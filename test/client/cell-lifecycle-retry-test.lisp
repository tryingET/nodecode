;;;; Actual generated shell scopes retain failed cleanup; no live host effects.
;;;; SPDX-License-Identifier: MIT
(in-package #:nodecode.test)
(defpackage #:nodecode-shell-retry-demo (:use #:cl))
(nlk:define-peripheral nodecode-shell-retry-demo::demo :not-running t)
(defvar *retry-log* nil)
(defvar *retry-fail-stop* nil)
(defvar *retry-fail-start* nil)
(defvar *retry-acquire-hook* nil)
(defvar *retry-stop-hook* nil)

(nle:define-cell nodecode-shell-retry-demo::demo
  (:section ("shell-retry-demo") (:owner "nodecode-shell-retry-demo"))
  (:shell-start
   (lambda (settings model)
     (declare (ignore settings))
     (push (list :start model) *retry-log*)
     (nle:on-stop (lambda () (push (list :first model) *retry-log*)))
     (nle:on-stop (lambda ()
                    (push (list :middle model) *retry-log*)
                    (when *retry-stop-hook* (funcall *retry-stop-hook*))
                    (when (member model *retry-fail-stop*) (error "still draining"))))
     (nle:on-stop (lambda () (push (list :last model) *retry-log*)))
     (when *retry-acquire-hook* (funcall *retry-acquire-hook*))
     (when *retry-fail-start* (error "acquisition refused")))))

(defmacro with-shell-retry-fixture (&body body)
  `(with-saved-globals ((*retry-log* nil) (*retry-fail-stop* nil)
                        (*retry-fail-start* nil) (*retry-acquire-hook* nil)
                        (*retry-stop-hook* nil))
     (unwind-protect (progn ,@body)
       (setf *retry-fail-stop* nil *retry-fail-start* nil *retry-acquire-hook* nil
             *retry-stop-hook* nil)
       (nodecode-shell-retry-demo::stop-shell-cell))))

(deftest shell-generated-stop-keeps-failed-inverse-and-retries-only-it
    (with-shell-retry-fixture)
  (let* ((model (list :model))
         (stop (nodecode-shell-retry-demo::start-shell-cell (nlk:json-object) model)))
    (setf *retry-fail-stop* (list model))
    (is (signals-error error (funcall stop)))
    (is (= 1 (hash-table-count nodecode-shell-retry-demo::*shell-stops*)))
    (is (equal '(:first :middle :last :start) (mapcar #'first *retry-log*)))
    (setf *retry-fail-stop* nil)
    (funcall stop)
    (funcall stop)
    (is (zerop (hash-table-count nodecode-shell-retry-demo::*shell-stops*)))
    (is (= 1 (count :first *retry-log* :key #'first)))
    (is (= 1 (count :last *retry-log* :key #'first)))
    (is (= 2 (count :middle *retry-log* :key #'first)))))

(deftest shell-generated-start-rollback-retains-scope-and-fences-reacquisition
    (with-shell-retry-fixture)
  (let ((model (list :model)))
    (setf *retry-fail-stop* (list model) *retry-fail-start* t)
    (is (signals-error error
          (nodecode-shell-retry-demo::start-shell-cell (nlk:json-object) model)))
    (is (= 1 (hash-table-count nodecode-shell-retry-demo::*shell-stops*)))
    (setf *retry-fail-start* nil)
    (is (signals-error error
          (nodecode-shell-retry-demo::start-shell-cell (nlk:json-object) model)))
    (is (= 1 (count :start *retry-log* :key #'first)))
    (is (= 1 (count :first *retry-log* :key #'first)))
    (is (= 1 (count :last *retry-log* :key #'first)))
    (setf *retry-fail-stop* nil)
    (nodecode-shell-retry-demo::stop-shell-cell model)
    (is (zerop (hash-table-count nodecode-shell-retry-demo::*shell-stops*)))
    (let ((stop (nodecode-shell-retry-demo::start-shell-cell (nlk:json-object) model)))
      (is (= 2 (count :start *retry-log* :key #'first)))
      (funcall stop))))

(deftest shell-generated-stop-all-continues-independent-models
    (with-shell-retry-fixture)
  (let ((bad (list :bad)) (good (list :good)))
    (nodecode-shell-retry-demo::start-shell-cell (nlk:json-object) bad)
    (nodecode-shell-retry-demo::start-shell-cell (nlk:json-object) good)
    (setf *retry-fail-stop* (list bad))
    (is (signals-error error (nodecode-shell-retry-demo::stop-shell-cell)))
    (is (gethash bad nodecode-shell-retry-demo::*shell-stops*))
    (is (not (gethash good nodecode-shell-retry-demo::*shell-stops*)))
    (is (= 1 (count (list :middle good) *retry-log* :test #'equal)))
    (setf *retry-fail-stop* nil)
    (nodecode-shell-retry-demo::stop-shell-cell)
    (is (zerop (hash-table-count nodecode-shell-retry-demo::*shell-stops*)))
    (is (= 1 (count (list :middle good) *retry-log* :test #'equal)))))

(deftest shell-generated-stop-failure-is-retained-by-native-manager
    (with-shell-cell-fixture (model))
  (with-shell-retry-fixture
    (setf nlk::*cells* (list (nlk::make-cell :name "nodecode-shell-retry-demo"
                           :systems '("nodecode-shell-retry-demo") :kind :peripheral
                           :start #'nodecode-shell-retry-demo::start-cell)))
    (let ((manager (nle::start-shell-cells model)))
      (is (= 1 (length (nle::shell-cell-manager-running manager))))
      (setf *retry-fail-stop* (list model))
      (handler-bind ((warning #'muffle-warning))
        (nle::request-shell-cell-action model '("nodecode-shell-retry-demo") "stop"))
      (is (= 1 (length (nle::shell-cell-manager-running manager))))
      (is (gethash "nodecode-shell-retry-demo" (nle::shell-cell-manager-failures manager)))
      (is (= 1 (hash-table-count nodecode-shell-retry-demo::*shell-stops*)))
      (setf *retry-fail-stop* nil)
      (nle::request-shell-cell-action model '("nodecode-shell-retry-demo") "stop")
      (is (null (nle::shell-cell-manager-running manager)))
      (is (not (gethash "nodecode-shell-retry-demo" (nle::shell-cell-manager-failures manager))))
      (is (zerop (hash-table-count nodecode-shell-retry-demo::*shell-stops*))))))

(deftest shell-generated-start-rollback-is-tracked-until-manager-off-succeeds
    (with-shell-cell-fixture (model))
  (with-shell-retry-fixture
    (setf nlk::*cells* (list (nlk::make-cell :name "nodecode-shell-retry-demo"
                           :systems '("nodecode-shell-retry-demo") :kind :peripheral
                           :start #'nodecode-shell-retry-demo::start-cell))
          *retry-fail-start* t *retry-fail-stop* (list model))
    (let ((manager (handler-bind ((warning #'muffle-warning)) (nle::start-shell-cells model))))
      (is (= 1 (length (nle::shell-cell-manager-running manager))))
      (is (gethash "nodecode-shell-retry-demo" (nle::shell-cell-manager-failures manager)))
      (is (= 1 (hash-table-count nodecode-shell-retry-demo::*shell-stops*)))
      (handler-bind ((warning #'muffle-warning))
        (nle::request-shell-cell-action model '("nodecode-shell-retry-demo") "restart"))
      (is (= 1 (count :start *retry-log* :key #'first)))
      (is (= 1 (length (nle::shell-cell-manager-running manager))))
      (setf *retry-fail-stop* nil *retry-fail-start* nil)
      (nle::request-shell-cell-action model '("nodecode-shell-retry-demo") "stop")
      (is (null (nle::shell-cell-manager-running manager)))
      (is (zerop (hash-table-count nodecode-shell-retry-demo::*shell-stops*)))
      (nle::request-shell-cell-action model '("nodecode-shell-retry-demo") "restart")
      (is (= 2 (count :start *retry-log* :key #'first)))
      (is (= 1 (length (nle::shell-cell-manager-running manager))))
      (nle::stop-shell-cells model)
      (is (null (nle::shell-cell-manager-for model))))))

(deftest shell-generated-start-rollback-is-tracked-through-shell-exit
    (with-shell-cell-fixture (model))
  (with-shell-retry-fixture
    (setf nlk::*cells* (list (nlk::make-cell :name "nodecode-shell-retry-demo"
                           :systems '("nodecode-shell-retry-demo") :kind :peripheral
                           :start #'nodecode-shell-retry-demo::start-cell))
          *retry-fail-start* t *retry-fail-stop* (list model))
    (let ((manager (handler-bind ((warning #'muffle-warning)) (nle::start-shell-cells model))))
      (handler-bind ((warning #'muffle-warning)) (nle::stop-shell-cells model))
      (is (nle::shell-cell-manager-closed manager))
      (is (eq manager (nle::shell-cell-manager-for model)))
      (is (= 1 (length (nle::shell-cell-manager-running manager))))
      (setf *retry-fail-stop* nil)
      (nle::stop-shell-cells model)
      (is (null (nle::shell-cell-manager-for model)))
      (is (zerop (hash-table-count nodecode-shell-retry-demo::*shell-stops*)))
      (is (= 1 (count :first *retry-log* :key #'first)))
      (is (= 1 (count :last *retry-log* :key #'first))))))

(deftest shell-generated-stale-stop-handle-does-not-stop-replacement
    (with-shell-retry-fixture)
  (let* ((model (list :model))
         (old-stop (nodecode-shell-retry-demo::start-shell-cell (nlk:json-object) model))
         (new-stop (nodecode-shell-retry-demo::start-shell-cell (nlk:json-object) model)))
    (is (= 1 (count :middle *retry-log* :key #'first)))
    (funcall old-stop)
    (is (= 1 (hash-table-count nodecode-shell-retry-demo::*shell-stops*)))
    (is (= 1 (count :middle *retry-log* :key #'first)))
    (funcall new-stop)
    (is (zerop (hash-table-count nodecode-shell-retry-demo::*shell-stops*)))
    (is (= 2 (count :middle *retry-log* :key #'first)))))

(deftest shell-generated-failed-acquisition-racing-off-retains-until-retry
    (with-shell-cell-fixture (model))
  (with-shell-retry-fixture
    (let ((entered (bt2:make-semaphore)) (release (bt2:make-semaphore))
          (start-thread nil) (stop-thread nil))
      (unwind-protect
           (progn
             (setf nlk::*cells* (list (nlk::make-cell :name "nodecode-shell-retry-demo"
                                    :systems '("nodecode-shell-retry-demo") :kind :peripheral
                                    :start #'nodecode-shell-retry-demo::start-cell))
                   *retry-fail-start* t *retry-fail-stop* (list model)
                   *retry-acquire-hook* (lambda ()
                                          (bt2:signal-semaphore entered)
                                          (bt2:wait-on-semaphore release)))
             (setf start-thread
                   (bt2:make-thread
                    (lambda () (handler-bind ((warning #'muffle-warning))
                                 (nle::start-shell-cells model)))))
             (is (bt2:wait-on-semaphore entered :timeout 2))
             (setf stop-thread
                   (bt2:make-thread
                    (lambda () (handler-bind ((warning #'muffle-warning))
                                 (nle::request-shell-cell-action
                                  model '("nodecode-shell-retry-demo") "stop")))))
             (let ((manager (nle::shell-cell-manager-for model)))
               (is (loop repeat 100
                         when (bt2:with-lock-held ((nle::shell-cell-manager-lock manager))
                                (null (cdr (gethash "nodecode-shell-retry-demo"
                                                    (nle::shell-cell-manager-desired manager)))))
                           return t
                         do (sleep 0.01)))
               (bt2:signal-semaphore release)
               (bt2:join-thread start-thread)
               (bt2:join-thread stop-thread)
               (is (= 1 (length (nle::shell-cell-manager-running manager))))
               (is (= 1 (hash-table-count nodecode-shell-retry-demo::*shell-stops*)))
               (is (= 1 (count :first *retry-log* :key #'first)))
               (is (= 1 (count :last *retry-log* :key #'first)))
               (is (= 1 (count :start *retry-log* :key #'first)))
               (setf *retry-fail-stop* nil)
               (nle::request-shell-cell-action model '("nodecode-shell-retry-demo") "stop")
               (is (null (nle::shell-cell-manager-running manager)))
               (is (zerop (hash-table-count nodecode-shell-retry-demo::*shell-stops*)))))
        (bt2:signal-semaphore release)
        (when start-thread (bt2:join-thread start-thread))
        (when stop-thread (bt2:join-thread stop-thread))))))

(deftest shell-generated-scope-is-registered-during-acquisition
    (with-shell-retry-fixture)
  (let ((model (list :model)) (registered nil))
    (setf *retry-acquire-hook*
          (lambda ()
            (setf registered (functionp (gethash model nodecode-shell-retry-demo::*shell-stops*)))))
    (funcall (nodecode-shell-retry-demo::start-shell-cell (nlk:json-object) model))
    (is registered)
    (is (zerop (hash-table-count nodecode-shell-retry-demo::*shell-stops*)))))

(deftest shell-retry-helper-does-not-change-historical-gateway-starting ()
  (let ((log nil) (warnings 0))
    (let ((stop (nle:starting
                  (nle:on-stop (lambda () (push :first log)))
                  (nle:on-stop (lambda () (push :bad log) (error "gateway refusal")))
                  (nle:on-stop (lambda () (push :last log))))))
      (handler-bind ((warning (lambda (condition)
                                (incf warnings) (muffle-warning condition))))
        (funcall stop))
      (funcall stop)
      (is (= 1 warnings))
      (is (equal '(:first :bad :last) log)))))

(defun check-generated-cleanup-reconciliation (model origin action)
  "Exercise the actual generated DSL, not a manually inserted fake stop handle."
  (let ((name "nodecode-shell-retry-demo"))
    (setf nlk::*cells* (list (nlk::make-cell :name name
                           :systems '("nodecode-shell-retry-demo") :kind :peripheral
                           :start #'nodecode-shell-retry-demo::start-cell)))
    (when (eq origin :startup)
      (setf *retry-fail-start* t *retry-fail-stop* (list model)))
    (let ((manager (handler-bind ((warning #'muffle-warning)) (nle::start-shell-cells model))))
      (when (eq origin :off)
        (setf *retry-fail-stop* (list model))
        (handler-bind ((warning #'muffle-warning))
          (nle::request-shell-cell-action model (list name) "stop")))
      (is (= 1 (length (nle::shell-cell-manager-running manager))))
      (is (= 1 (count :start *retry-log* :key #'first)))
      (is (gethash name (nle::shell-cell-manager-failures manager)))
      (let* ((pending-entry (assoc name (nle::shell-cell-manager-running manager) :test #'equal))
             (pending-stop (cdr pending-entry)))
        (is (nle::shell-cell-cleanup-pending-p pending-entry))
        (setf *retry-fail-start* nil)
        ;; Still-draining retries must attempt the retained inverse, not clear error.
        (handler-bind ((warning #'muffle-warning))
          (nle::request-shell-cell-action model (list name) action))
        (is (= 2 (count :middle *retry-log* :key #'first)))
        (is (= 1 (count :start *retry-log* :key #'first)))
        (is (gethash name (nle::shell-cell-manager-failures manager)))
        (is (eq pending-stop
                (cdr (assoc name (nle::shell-cell-manager-running manager) :test #'equal))))
        ;; Once the inverse can finish, the same ordinary action reacquires once.
        (setf *retry-fail-stop* nil)
        (nle::request-shell-cell-action model (list name) action)
        (is (= 2 (count :start *retry-log* :key #'first)))
        (is (= 3 (count :middle *retry-log* :key #'first)))
        (is (= 1 (count :first *retry-log* :key #'first)))
        (is (= 1 (count :last *retry-log* :key #'first)))
        (is (not (gethash name (nle::shell-cell-manager-failures manager))))
        (is (= 1 (hash-table-count nodecode-shell-retry-demo::*shell-stops*)))
        (is (not (nle::shell-cell-cleanup-pending-p pending-entry)))
        (is (not (nle::shell-cell-cleanup-pending-p
                  (assoc name (nle::shell-cell-manager-running manager) :test #'equal))))
        (is (not (eq pending-stop
                     (cdr (assoc name (nle::shell-cell-manager-running manager) :test #'equal)))))
        ;; Active ownership is still idempotent; do not mistake it for cleanup-only.
        (nle::request-shell-cell-action model (list name) action)
        (is (= 2 (count :start *retry-log* :key #'first)))
        (is (= 3 (count :middle *retry-log* :key #'first)))
        (nle::stop-shell-cells model)
        (is (null (nle::shell-cell-manager-for model)))
        (is (zerop (hash-table-count nodecode-shell-retry-demo::*shell-stops*)))))))

(deftest shell-generated-startup-rollback-ordinary-start-retries-before-reacquiring
    (with-shell-cell-fixture (model))
  (with-shell-retry-fixture (check-generated-cleanup-reconciliation model :startup "start")))

(deftest shell-generated-startup-rollback-sync-retries-before-reacquiring
    (with-shell-cell-fixture (model))
  (with-shell-retry-fixture (check-generated-cleanup-reconciliation model :startup "sync")))

(deftest shell-generated-partial-off-ordinary-start-retries-before-reacquiring
    (with-shell-cell-fixture (model))
  (with-shell-retry-fixture (check-generated-cleanup-reconciliation model :off "start")))

(deftest shell-generated-partial-off-sync-retries-before-reacquiring
    (with-shell-cell-fixture (model))
  (with-shell-retry-fixture (check-generated-cleanup-reconciliation model :off "sync")))

(deftest shell-generated-reconcile-blocked-cleanup-does-not-acquire-after-off
    (with-shell-cell-fixture (model))
  (with-shell-retry-fixture
    (let ((entered (bt2:make-semaphore)) (release (bt2:make-semaphore))
          (retry-thread nil) (off-thread nil) (name "nodecode-shell-retry-demo"))
      (unwind-protect
           (progn
             (setf nlk::*cells* (list (nlk::make-cell :name name
                                    :systems '("nodecode-shell-retry-demo") :kind :peripheral
                                    :start #'nodecode-shell-retry-demo::start-cell))
                   *retry-fail-start* t *retry-fail-stop* (list model))
             (let ((manager (handler-bind ((warning #'muffle-warning)) (nle::start-shell-cells model))))
               (setf *retry-fail-start* nil *retry-fail-stop* nil
                     *retry-stop-hook* (lambda ()
                                         (bt2:signal-semaphore entered)
                                         (bt2:wait-on-semaphore release)))
               (setf retry-thread
                     (bt2:make-thread
                      (lambda () (nle::request-shell-cell-action model (list name) "start"))))
               (is (bt2:wait-on-semaphore entered :timeout 2))
               (setf off-thread
                     (bt2:make-thread
                      (lambda () (nle::request-shell-cell-action model (list name) "stop"))))
               (is (loop repeat 100
                         when (bt2:with-lock-held ((nle::shell-cell-manager-lock manager))
                                (null (cdr (gethash name (nle::shell-cell-manager-desired manager)))))
                           return t
                         do (sleep 0.01)))
               (setf *retry-stop-hook* nil)
               (bt2:signal-semaphore release)
               (bt2:join-thread retry-thread)
               (bt2:join-thread off-thread)
               (is (= 1 (count :start *retry-log* :key #'first)))
               (is (= 2 (count :middle *retry-log* :key #'first)))
               (is (null (nle::shell-cell-manager-running manager)))
               (is (zerop (hash-table-count nodecode-shell-retry-demo::*shell-stops*)))
               (is (not (gethash name (nle::shell-cell-manager-failures manager))))))
        (setf *retry-stop-hook* nil)
        (bt2:signal-semaphore release)
        (when retry-thread (bt2:join-thread retry-thread))
        (when off-thread (bt2:join-thread off-thread))))))

(deftest shell-generated-superseded-start-cannot-clear-newer-cleanup-failure
    (with-shell-cell-fixture (model))
  (with-shell-retry-fixture
    (let ((entered (bt2:make-semaphore)) (release (bt2:make-semaphore))
          (old-thread nil) (calls 0) (name "nodecode-shell-retry-demo"))
      (unwind-protect
           (progn
             (setf nlk::*cells* (list (nlk::make-cell :name name
                                    :systems '("nodecode-shell-retry-demo") :kind :peripheral
                                    :start #'nodecode-shell-retry-demo::start-cell))
                   (symbol-function 'nlk:load-cell)
                   (lambda (&rest names)
                     (declare (ignore names))
                     (when (= 1 (incf calls))
                       (bt2:signal-semaphore entered)
                       (bt2:wait-on-semaphore release))))
             (setf old-thread
                   (bt2:make-thread
                    (lambda () (nle::request-shell-cell-action model (list name) "start"))))
             (is (bt2:wait-on-semaphore entered :timeout 2))
             (setf *retry-fail-start* t *retry-fail-stop* (list model))
             (let ((manager (handler-bind ((warning #'muffle-warning))
                              (nle::request-shell-cell-action model (list name) "start"))))
               (is (gethash name (nle::shell-cell-manager-failures manager)))
               (bt2:signal-semaphore release)
               (bt2:join-thread old-thread)
               (is (gethash name (nle::shell-cell-manager-failures manager)))
               (is (= 1 (length (nle::shell-cell-manager-running manager))))
               (is (= 1 (count :start *retry-log* :key #'first)))
               (setf *retry-fail-start* nil *retry-fail-stop* nil)
               (nle::request-shell-cell-action model (list name) "sync")
               (is (= 2 (count :start *retry-log* :key #'first)))
               (is (not (gethash name (nle::shell-cell-manager-failures manager))))))
        (bt2:signal-semaphore release)
        (when old-thread (bt2:join-thread old-thread))))))
