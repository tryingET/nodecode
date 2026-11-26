;;;; Lifecycle intention obligations outlive a superseded dispatch thunk.
;;;; SPDX-License-Identifier: MIT
(in-package #:nodecode.test)

(defun call-with-held-shell-actions (body)
  "Hold native dispatch thunks, allowing exact FIFO/LIFO race ordering."
  (let ((saved (symbol-function 'nle::off-loop)) (queued nil))
    (labels ((run-one (&optional newest)
               (let ((thunk (if newest (pop queued)
                                (prog1 (car (last queued))
                                  (setf queued (butlast queued))))))
                 (when thunk (funcall thunk)))))
      (unwind-protect
           (progn
             (setf (symbol-function 'nle::off-loop)
                   (lambda (model thunk continuation)
                     (push (lambda () (funcall continuation model (funcall thunk))) queued)))
             (funcall body #'run-one))
        (loop while queued do (run-one))
        (setf (symbol-function 'nle::off-loop) saved)))))

(defun shell-demo-start-count ()
  (count :shell-start *shell-demo-log* :key (lambda (row) (and (consp row) (first row)))))

(defun assert-shell-restart-survives-wanted (model action newest &optional gateway-stop)
  ;; Given C running with A, when restart(B) is queued then a newer wanted
  ;; state arrives, then exactly one replacement using B must survive either order.
  (setf *shell-demo-config* (nlk:json-object "shell-demo" (nlk:json-object "mode" "A")))
  (nle::start-shell-cells model)
  (setf *shell-demo-config* (nlk:json-object "shell-demo" (nlk:json-object "mode" "B")))
  (call-with-held-shell-actions
   (lambda (run)
     (when gateway-stop (nle::request-shell-cell-action model '("nodecode-shell-demo") "stop"))
     (nle::request-shell-cell-action model '("nodecode-shell-demo") "restart")
     (nle::request-shell-cell-action model '("nodecode-shell-demo") action)
     (funcall run newest)
     (funcall run newest)
     (when gateway-stop (funcall run newest))))
  (is (= 2 (shell-demo-start-count)))
  (is (= 1 (count (list :shell-stop model) *shell-demo-log* :test #'equal)))
  (is (equal (list :shell-start model "B") (first *shell-demo-log*)))
  ;; Given successful replacement, repeated ordinary sync does not restart again.
  (nle::request-shell-cell-action model '("nodecode-shell-demo") "sync")
  (is (= 2 (shell-demo-start-count))))

(deftest shell-restart-obligation-survives-newer-sync-fifo (with-shell-cell-fixture (model))
  (assert-shell-restart-survives-wanted model "sync" nil))
(deftest shell-restart-obligation-survives-newer-start-lifo (with-shell-cell-fixture (model))
  (assert-shell-restart-survives-wanted model "start" t))
(deftest shell-gateway-stop-restart-obligation-survives-sync (with-shell-cell-fixture (model))
  (assert-shell-restart-survives-wanted model "sync" t t))

(deftest shell-newer-off-cancels-restart-without-reacquisition (with-shell-cell-fixture (model))
  ;; Given running A, when restart is superseded by OFF, then no replacement is acquired.
  (nle::start-shell-cells model)
  (call-with-held-shell-actions
   (lambda (run)
     (nle::request-shell-cell-action model '("nodecode-shell-demo") "restart")
     (nle::request-shell-cell-action model '("nodecode-shell-demo") "stop")
     (funcall run t)
     (funcall run t)))
  (is (= 1 (shell-demo-start-count)))
  (is (null (nle::shell-cell-manager-running (nle::shell-cell-manager-for model))))
  ;; A later ON is one new acquisition, not a stale replacement plus another ON.
  (nle::request-shell-cell-action model '("nodecode-shell-demo") "sync")
  (nle::request-shell-cell-action model '("nodecode-shell-demo") "start")
  (is (= 2 (shell-demo-start-count))))

(deftest shell-failed-replacement-retries-via-ordinary-sync (with-shell-cell-fixture (model))
  ;; Given a restart acquisition fails, when configuration is repaired and SYNC
  ;; remains wanted, then it acquires once and clears the old failure.
  (nle::start-shell-cells model)
  (setf *shell-demo-fail* t)
  (handler-bind ((warning #'muffle-warning))
    (nle::request-shell-cell-action model '("nodecode-shell-demo") "restart"))
  (let ((manager (nle::shell-cell-manager-for model)))
    (is (gethash "nodecode-shell-demo" (nle::shell-cell-manager-failures manager)))
    (setf *shell-demo-fail* nil)
    (nle::request-shell-cell-action model '("nodecode-shell-demo") "sync")
    (is (= 3 (shell-demo-start-count)))
    (is (null (gethash "nodecode-shell-demo" (nle::shell-cell-manager-failures manager))))
    (nle::request-shell-cell-action model '("nodecode-shell-demo") "sync")
    (is (= 3 (shell-demo-start-count)))))

(defun deliver-shell-intent (model action)
  (nlt:update model
              (make-instance 'nle::gw-projection-msg
                :frame (nlk:decode-sync-message
                        (nlk:encode-unsequenced-frame
                         "organism.cells" (nlk:json-object "names" #("nodecode-shell-demo")
                                                           "action" action))))))

(deftest shell-closed-model-rejects-late-native-notifications (with-shell-cell-fixture (model))
  ;; Given successful terminal teardown, when late native cell notifications
  ;; reach the same model, then terminal authority cannot be recreated.
  (nle::start-shell-cells model)
  (is (nle::stop-shell-cells model))
  (dolist (action '("start" "restart" "sync")) (deliver-shell-intent model action))
  (is (= 1 (shell-demo-start-count)))
  (is (null (nle::shell-cell-manager-for model)))
  ;; A distinct terminal model remains independently startable.
  (let ((other (make-instance 'nle:gateway-chat-model :ws nil :session-id "other")))
    (unwind-protect
         (progn (deliver-shell-intent other "start") (is (= 2 (shell-demo-start-count))))
      (nle::stop-shell-cells other))))

(deftest shell-never-started-closed-model-rejects-late-native-notifications (with-shell-cell-fixture (model))
  ;; Given teardown before any cell acquisition, delayed notifications still
  ;; cannot acquire resources for that terminal.
  (is (nle::stop-shell-cells model))
  (deliver-shell-intent model "sync")
  (is (zerop (shell-demo-start-count)))
  (is (null (nle::shell-cell-manager-for model))))

(deftest shell-replacement-obligation-survives-config-read-failure (with-shell-cell-fixture (model))
  ;; Given live A, when restart cannot read config, then later ordinary SYNC
  ;; still performs the owed replacement rather than silently retaining A.
  (nle::start-shell-cells model)
  (let ((saved (symbol-function 'nle::read-shared-config)))
    (unwind-protect
         (progn
           (setf (symbol-function 'nle::read-shared-config) (lambda () (error "unavailable")))
           (handler-bind ((warning #'muffle-warning))
             (nle::request-shell-cell-action model '("nodecode-shell-demo") "restart")))
      (setf (symbol-function 'nle::read-shared-config) saved)))
  (is (= 1 (shell-demo-start-count)))
  (setf *shell-demo-config* (nlk:json-object "shell-demo" (nlk:json-object "mode" "B")))
  (nle::request-shell-cell-action model '("nodecode-shell-demo") "sync")
  (is (= 2 (shell-demo-start-count)))
  (is (equal (list :shell-start model "B") (first *shell-demo-log*))))

(deftest shell-replacement-obligation-survives-dispatch-failure (with-shell-cell-fixture (model))
  ;; Given live A, when scheduling restart fails before its thunk begins,
  ;; then an ordinary start can discharge the retained replacement obligation.
  (nle::start-shell-cells model)
  (let ((saved (symbol-function 'nle::off-loop)))
    (unwind-protect
         (progn
           (setf (symbol-function 'nle::off-loop)
                 (lambda (&rest ignored) (declare (ignore ignored)) (error "dispatch refused")))
           (is (signals-error error
                 (nle::request-shell-cell-action model '("nodecode-shell-demo") "restart"))))
      (setf (symbol-function 'nle::off-loop) saved)))
  (setf *shell-demo-config* (nlk:json-object "shell-demo" (nlk:json-object "mode" "B")))
  (nle::request-shell-cell-action model '("nodecode-shell-demo") "start")
  (is (= 2 (shell-demo-start-count)))
  (is (equal (list :shell-start model "B") (first *shell-demo-log*))))

(deftest shell-newer-restart-during-acquisition-is-not-acknowledged-by-old-start
    (with-shell-cell-fixture (model))
  ;; Given restart B acquiring, when restart C plus SYNC arrive during that
  ;; acquisition, then B's completion cannot consume C's replacement obligation.
  (nle::start-shell-cells model)
  (setf *shell-demo-config* (nlk:json-object "shell-demo" (nlk:json-object "mode" "B")))
  (let ((saved (symbol-function 'nodecode-shell-demo::start-shell-cell)) (triggered nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'nodecode-shell-demo::start-shell-cell)
                 (lambda (config target)
                   (prog1 (funcall saved config target)
                     (unless triggered
                       (setf triggered t
                             *shell-demo-config*
                             (nlk:json-object "shell-demo" (nlk:json-object "mode" "C")))
                       (nle::request-shell-cell-action model '("nodecode-shell-demo") "restart")
                       (nle::request-shell-cell-action model '("nodecode-shell-demo") "sync")))))
           (call-with-held-shell-actions
            (lambda (run)
              (nle::request-shell-cell-action model '("nodecode-shell-demo") "restart")
              (funcall run)
              (funcall run t)
              (funcall run t))))
      (setf (symbol-function 'nodecode-shell-demo::start-shell-cell) saved)))
  (is (= 3 (shell-demo-start-count)))
  (is (= 2 (count (list :shell-stop model) *shell-demo-log* :test #'equal)))
  (is (equal (list :shell-start model "C") (first *shell-demo-log*)))
  (nle::request-shell-cell-action model '("nodecode-shell-demo") "sync")
  (is (= 3 (shell-demo-start-count))))
