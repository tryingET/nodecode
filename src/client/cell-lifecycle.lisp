;;;; cell-lifecycle.lisp --- terminal contributions, never gateway peripherals.
;;;; SPDX-License-Identifier: MIT
(in-package #:nodecode.evolved)

(defstruct (shell-cell-manager (:constructor make-shell-cell-manager (model)))
  model
  (lock (bt2:make-lock :name "shell-cell intentions"))
  (operations (bt2:make-lock :name "shell-cell resources"))
  (desired (make-hash-table :test #'equal))
  (failures (make-hash-table :test #'equal))
  (running nil)
  (epoch 0)
  (closed nil))

(defvar *shell-cell-managers* (make-hash-table :test #'eq))
(defvar *shell-cell-managers-lock* (bt2:make-lock :name "shell-cell managers"))
;; Successful teardown removes resource owners, not terminal-lifetime authority.
;; A delayed native notification cannot recreate an exited model's manager.
(defvar *shell-cell-closed-models* (make-hash-table :test #'eq :weakness :key))
;; Action metadata is owned by the exact manager, keeping its structure layout
;; stable. The manager's intention lock serializes the counter and drain wait.
(defstruct shell-cell-actions
  (pending 0)
  (drained (bt2:make-condition-variable :name "shell-cell actions drained")))
(defvar *shell-cell-actions* (make-hash-table :test #'eq :weakness :key))
(defvar *shell-cell-actions-lock* (bt2:make-lock :name "shell-cell action owners"))
;; Desired presence and configuration replacement are different obligations.
;; Newer wanted-state notifications inherit replacement until it succeeds.
(defvar *shell-cell-restart-requests* (make-hash-table :test #'eq :weakness :key))

(defun shell-cell-restarts-for (manager)
  (bt2:with-lock-held (*shell-cell-actions-lock*)
    (or (gethash manager *shell-cell-restart-requests*)
        (setf (gethash manager *shell-cell-restart-requests*)
              (make-hash-table :test #'equal)))))

(defun shell-cell-actions-for (manager)
  (bt2:with-lock-held (*shell-cell-actions-lock*)
    (or (gethash manager *shell-cell-actions*)
        (setf (gethash manager *shell-cell-actions*) (make-shell-cell-actions)))))

(defun shell-cell-manager-pending (manager)
  (shell-cell-actions-pending (shell-cell-actions-for manager)))

(defun (setf shell-cell-manager-pending) (value manager)
  (setf (shell-cell-actions-pending (shell-cell-actions-for manager)) value))

(defun shell-cell-manager-drained (manager)
  (shell-cell-actions-drained (shell-cell-actions-for manager)))
;; Keep RUNNING's (name . stop) contract and manager layout stable. Metadata is
;; owned by the exact entry, not the name: a replacement cannot inherit it.
(defvar *shell-cell-cleanup-pending* (make-hash-table :test #'eq :weakness :key))
(defvar *shell-cell-cleanup-lock* (bt2:make-lock :name "shell-cell cleanup state"))

(defun shell-cell-cleanup-pending-p (entry)
  (bt2:with-lock-held (*shell-cell-cleanup-lock*)
    (gethash entry *shell-cell-cleanup-pending*)))

(defun (setf shell-cell-cleanup-pending-p) (pending entry)
  (bt2:with-lock-held (*shell-cell-cleanup-lock*)
    (if pending (setf (gethash entry *shell-cell-cleanup-pending*) t)
        (remhash entry *shell-cell-cleanup-pending*)))
  pending)

(defun shell-cell-manager-for (model &optional create)
  (bt2:with-lock-held (*shell-cell-managers-lock*)
    (or (gethash model *shell-cell-managers*)
        (when (and create (not (gethash model *shell-cell-closed-models*)))
          (setf (gethash model *shell-cell-managers*) (make-shell-cell-manager model))))))

(defun exported-shell-cell-start (cell)
  "CELL's exported terminal entry point; START-CELL is deliberately never read."
  (when (eq :loaded (nlk:cell-state cell))
    (loop for system in (reverse (nlk:cell-systems cell))
          for package = (find-package (string-upcase system))
          when package
            do (multiple-value-bind (symbol status) (find-symbol "START-SHELL-CELL" package)
                 (when (and (eq :external status) (fboundp symbol))
                   (return (fdefinition symbol)))))))

(defun shell-cell-current-p (manager name epoch &optional wanted)
  (bt2:with-lock-held ((shell-cell-manager-lock manager))
    (let ((intent (gethash name (shell-cell-manager-desired manager))))
      (and (not (shell-cell-manager-closed manager))
           (eql epoch (car intent))
           (eq wanted (cdr intent))))))

(defun stop-shell-cell-entry (manager name)
  "Remove evidence only after its stop returned; a refusal stays retryable."
  (when-let (entry (assoc name (shell-cell-manager-running manager) :test #'equal))
    ;; Mark before calling: partial cleanup, including a non-local refusal,
    ;; must never be mistaken for a successfully running contribution.
    (setf (shell-cell-cleanup-pending-p entry) t)
    (funcall (cdr entry))
    (setf (shell-cell-manager-running manager)
          (remove entry (shell-cell-manager-running manager) :test #'eq))
    (setf (shell-cell-cleanup-pending-p entry) nil)))

(defun apply-shell-cell-intent (manager name epoch wanted restart config)
  ;; Loads can block. OFF records its epoch without this resource lock; after
  ;; a load, and again after acquisition, the captured start must still be live.
  (when (shell-cell-current-p manager name epoch wanted)
    (when wanted (nlk:load-cell name))
    (bt2:with-lock-held ((shell-cell-manager-operations manager))
      (when (shell-cell-current-p manager name epoch wanted)
        (when (or (not wanted) restart
                  (shell-cell-cleanup-pending-p
                   (assoc name (shell-cell-manager-running manager) :test #'equal)))
          (stop-shell-cell-entry manager name))
        (when (and wanted
                   ;; Retried cleanup can block while a newer OFF is recorded.
                   (shell-cell-current-p manager name epoch wanted)
                   (not (assoc name (shell-cell-manager-running manager) :test #'equal)))
          (when-let (cell (nlk:find-cell name))
            (when-let (start (exported-shell-cell-start cell))
              (let ((stop
                      (handler-case (funcall start config (shell-cell-manager-model manager))
                        (shell-cell-startup-cleanup-error (condition)
                          ;; A refused start can still own resources when rollback
                          ;; cannot join them. OFF/exit must retain and retry its inverse.
                          (let ((entry (cons name (shell-cell-startup-cleanup-stop condition))))
                            (push entry (shell-cell-manager-running manager))
                            (setf (shell-cell-cleanup-pending-p entry) t))
                          (error condition)))))
                (unless (functionp stop)
                  (error "cell ~a START-SHELL-CELL must return its stop function" name))
                (push (cons name stop) (shell-cell-manager-running manager))
                (unless (bt2:with-lock-held ((shell-cell-manager-lock manager))
                          (when (and (not (shell-cell-manager-closed manager))
                                     (eql epoch (car (gethash name (shell-cell-manager-desired manager)))))
                            ;; Currentness and acknowledgement are one decision:
                            ;; an old start cannot consume a newer replacement.
                            (when (eql restart (gethash name (shell-cell-restarts-for manager)))
                              (remhash name (shell-cell-restarts-for manager)))
                            t))
                  (stop-shell-cell-entry manager name))))))))))

(defun shell-cell-failure (manager name condition &optional epoch)
  "Only the current request may describe current state; teardown has no epoch."
  (when (bt2:with-lock-held ((shell-cell-manager-lock manager))
          (when (or (null epoch)
                    (and (not (shell-cell-manager-closed manager))
                         (eql epoch (car (gethash name (shell-cell-manager-desired manager))))))
            (setf (gethash name (shell-cell-manager-failures manager))
                  (princ-to-string condition))
            t))
    (warn "shell cell ~a: ~a" name condition)))

(defun dispatch-shell-cell-intents (model manager intents)
  "INTENTS already owns one pending slot, before OFF-LOOP can dispatch it."
  (let ((finished nil) (config-read nil) (config nil) (config-failure nil))
    (labels ((finish ()
               (bt2:with-lock-held ((shell-cell-manager-lock manager))
                 (unless finished
                   (setf finished t)
                   (decf (shell-cell-manager-pending manager))
                   (bt2:condition-notify (shell-cell-manager-drained manager)))))
             (configuration ()
               ;; OFF needs no configuration. Cache failure too, so mixed SYNC
               ;; still runs every inverse and records each affected start.
               (unless config-read
                 (setf config-read t)
                 (handler-case
                     (multiple-value-bind (value warning) (read-shared-config)
                       (setf config value)
                       (when warning (warn "shell cells: ~a" warning)))
                   (error (condition) (setf config-failure condition))))
               (when config-failure (error config-failure))
               config)
             (run ()
               (unwind-protect
                    (dolist (intent intents)
                      (destructuring-bind (name epoch wanted restart) intent
                        (handler-case
                            (progn
                              (apply-shell-cell-intent manager name epoch wanted restart
                                                       (when wanted (configuration)))
                              (bt2:with-lock-held ((shell-cell-manager-lock manager))
                                (when (eql epoch (car (gethash name (shell-cell-manager-desired manager))))
                                  (remhash name (shell-cell-manager-failures manager)))))
                          (error (condition) (shell-cell-failure manager name condition epoch)))))
                 (finish))))
      ;; Scheduling can fail before RUN begins; the idempotent inverse belongs
      ;; to this invocation, not to a mailbox continuation that exit may discard.
      (let ((dispatched nil))
        (unwind-protect
             (prog1 (off-loop model #'run
                              (lambda (ignored value) (declare (ignore ignored value)) :quiet))
               (setf dispatched t))
          (unless dispatched (finish)))))))

(defun request-shell-cell-action (model names &optional (action "start"))
  "Record ACTION now, then load/start/stop off-loop. Late loads cannot undo OFF.
SYNC is the gateway's complete desired set; START is an idempotent legacy ON."
  (unless (member action '("start" "stop" "restart" "sync") :test #'equal)
    (error "unknown shell cell action ~s" action))
  (let* ((manager (shell-cell-manager-for model t))
         (names (remove-duplicates (remove-if-not #'stringp names) :test #'equal))
         (intents nil))
    (unless manager (return-from request-shell-cell-action nil))
    (bt2:with-lock-held ((shell-cell-manager-lock manager))
      (unless (shell-cell-manager-closed manager)
        (let* ((epoch (incf (shell-cell-manager-epoch manager)))
               (known (when (equal action "sync")
                        (remove-duplicates
                         (append (mapcar #'nlk:cell-name (nlk:cells))
                                 (loop for key being the hash-keys of (shell-cell-manager-desired manager)
                                       collect key)) :test #'equal))))
          (dolist (name (remove-duplicates (append known names) :test #'equal))
            (let ((wanted (and (not (equal action "stop"))
                               (or (not (equal action "sync"))
                                   (member name names :test #'equal)) t)))
              (setf (gethash name (shell-cell-manager-desired manager)) (cons epoch wanted))
              (let ((restarts (shell-cell-restarts-for manager)))
                (cond ((not wanted) (remhash name restarts))
                      ((equal action "restart") (setf (gethash name restarts) epoch)))
                (push (list name epoch wanted (gethash name restarts)) intents))))
          (when intents (incf (shell-cell-manager-pending manager))))))
    (when intents (dispatch-shell-cell-intents model manager (nreverse intents)))
    manager))

(defun start-shell-cells (model)
  "Start loaded terminal halves inside the shell's own unwind scope."
  (request-shell-cell-action model (mapcar #'nlk:cell-name (nlk:cells))))

(defun stop-shell-cells (model &key (timeout 5))
  "Close authority, drain action thunks and attempt every inverse. True means quiet.
False and a reason means ownership remains for retry. TIMEOUT bounds waits, not
callbacks: each inverse must itself return or signal within a bounded interval."
  (let ((manager (bt2:with-lock-held (*shell-cell-managers-lock*)
                   (setf (gethash model *shell-cell-closed-models*) t)
                   (gethash model *shell-cell-managers*)))
        (deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (unless manager (return-from stop-shell-cells t))
    (flet ((remaining () (max 0.0 (/ (- deadline (get-internal-real-time))
                                     (float internal-time-units-per-second)))))
      (bt2:with-lock-held ((shell-cell-manager-lock manager))
        (setf (shell-cell-manager-closed manager) t)
        (clrhash (shell-cell-manager-desired manager))
        (loop while (plusp (shell-cell-manager-pending manager))
              do (when (zerop (remaining)) (return))
                 (bt2:condition-wait (shell-cell-manager-drained manager)
                                     (shell-cell-manager-lock manager) :timeout (remaining))))
      (let ((lock (shell-cell-manager-operations manager)))
        (unless (or (bt2:acquire-lock lock :wait nil)
                    (and (plusp (remaining)) (bt2:acquire-lock lock :timeout (remaining))))
          (return-from stop-shell-cells (values nil '(:resources-busy t))))
        (unwind-protect
             (progn
               (dolist (name (mapcar #'car (shell-cell-manager-running manager)))
                 (handler-case (stop-shell-cell-entry manager name)
                   (error (condition) (shell-cell-failure manager name condition))))
               (let ((pending (bt2:with-lock-held ((shell-cell-manager-lock manager))
                                (shell-cell-manager-pending manager)))
                     (cells (mapcar #'car (shell-cell-manager-running manager))))
                 (when (or (plusp pending) cells)
                   (return-from stop-shell-cells (values nil (list :pending pending :cells cells)))))
               (bt2:with-lock-held (*shell-cell-managers-lock*)
                 (when (eq manager (gethash model *shell-cell-managers*))
                   (remhash model *shell-cell-managers*)))
               t)
          (bt2:release-lock lock))))))

(defun note-shell-cell-shutdown (model)
  "Record refusal without throwing past independent shell teardown operations."
  (handler-case
      (multiple-value-bind (quiet reason) (stop-shell-cells model)
        (unless quiet
          (setf *shell-teardown-refusal*
                (format nil "terminal cell cleanup is incomplete (~s); profile handover refused" reason)))
        quiet)
    (error (condition)
      (setf *shell-teardown-refusal*
            (format nil "terminal cell cleanup failed (~a); profile handover refused" condition))
      nil)))
