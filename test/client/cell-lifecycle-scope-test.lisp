;;;; Generated scope reentrancy and independent-model admission.
;;;; SPDX-License-Identifier: MIT
(in-package #:nodecode.test)
(defpackage #:nodecode-shell-scope-demo (:use #:cl))
(nlk:define-peripheral nodecode-shell-scope-demo::demo :not-running t)
(defvar *scope-log* nil)
(defvar *scope-acquire* nil)
(defvar *scope-cleanup* nil)
(nle:define-cell nodecode-shell-scope-demo::demo
  (:section ("shell-scope-demo") (:owner "nodecode-shell-scope-demo"))
  (:shell-start
   (lambda (settings model)
     (declare (ignore settings))
     (push (list :first-acquired model) *scope-log*)
     (nle:on-stop (lambda () (push (list :first-stopped model) *scope-log*)))
     (when *scope-acquire* (funcall *scope-acquire* model))
     (push (list :last-acquired model) *scope-log*)
     (nle:on-stop (lambda ()
                    (push (list :last-stopped model) *scope-log*)
                    (when *scope-cleanup* (funcall *scope-cleanup* model)))))))

(defmacro with-shell-scope-fixture (&body body)
  `(with-saved-globals ((*scope-log* nil) (*scope-acquire* nil) (*scope-cleanup* nil))
     (unwind-protect (progn ,@body)
       (setf *scope-acquire* nil *scope-cleanup* nil)
       (nodecode-shell-scope-demo::stop-shell-cell))))

(deftest shell-scope-reentrant-stop-during-acquisition-refuses-with-owned-rollback
    (with-shell-scope-fixture)
  ;; Given a partial acquisition, when it calls its exported STOP, then START
  ;; refuses and rolls back; a later inverse must never enter an orphaned scope.
  (let ((model (list :model)) (stop nil) (refused nil))
    (setf *scope-acquire* #'nodecode-shell-scope-demo::stop-shell-cell)
    (handler-case
        (setf stop (nodecode-shell-scope-demo::start-shell-cell (nlk:json-object) model))
      (error () (setf refused t)))
    (when stop (funcall stop))
    (is refused)
    (is (not (assoc :last-acquired *scope-log*)))
    (is (= 1 (count (list :first-stopped model) *scope-log* :test #'equal)))
    (is (zerop (hash-table-count nodecode-shell-scope-demo::*shell-stops*)))))

(deftest shell-scope-reentrant-start-during-acquisition-refuses-before-replacement
    (with-shell-scope-fixture)
  ;; Given an acquiring generation, when its callback tries replacement, then
  ;; replacement is rejected before either generation can lose its inverse.
  (let ((model (list :model)) (nested nil) (refused nil) (stop nil))
    (setf *scope-acquire*
          (lambda (incoming)
            (unless nested
              (setf nested t)
              (nodecode-shell-scope-demo::start-shell-cell (nlk:json-object) incoming))))
    (handler-case
        (setf stop (nodecode-shell-scope-demo::start-shell-cell (nlk:json-object) model))
      (error () (setf refused t)))
    (when stop (funcall stop))
    (is refused)
    (is (= 1 (count (list :first-acquired model) *scope-log* :test #'equal)))
    (is (= 1 (count (list :first-stopped model) *scope-log* :test #'equal)))
    (is (zerop (hash-table-count nodecode-shell-scope-demo::*shell-stops*)))))

(deftest shell-scope-reentrant-stop-during-cleanup-retains-only-failed-inverse
    (with-shell-scope-fixture)
  ;; Given cleanup that calls itself, when STOP runs, then it refuses recursion,
  ;; continues an independent inverse, and keeps only failed cleanup for retry.
  (let* ((model (list :model)) (nested nil)
         (stop (nodecode-shell-scope-demo::start-shell-cell (nlk:json-object) model)))
    ;; The guard makes the OLD implementation's red run finite, not stack overflow.
    (setf *scope-cleanup*
          (lambda (incoming)
            (unless nested
              (let ((old nested))
                (unwind-protect
                     (progn (setf nested t)
                            (nodecode-shell-scope-demo::stop-shell-cell incoming))
                  (setf nested old))))))
    (is (signals-error error (funcall stop)))
    (is (= 1 (count (list :last-stopped model) *scope-log* :test #'equal)))
    (is (= 1 (count (list :first-stopped model) *scope-log* :test #'equal)))
    (is (gethash model nodecode-shell-scope-demo::*shell-stops*))
    (setf *scope-cleanup* nil)
    (funcall stop)
    (is (= 2 (count (list :last-stopped model) *scope-log* :test #'equal)))
    (is (= 1 (count (list :first-stopped model) *scope-log* :test #'equal)))
    (is (zerop (hash-table-count nodecode-shell-scope-demo::*shell-stops*)))))

(deftest shell-scope-blocked-model-acquisition-does-not-delay-independent-shutdown
    (with-shell-scope-fixture)
  ;; Given B's completed scope and A blocked inside acquisition, when B's native
  ;; manager shuts down with a short deadline, then B finishes before A is released.
  (let* ((a (list :a)) (b (list :b))
         (entered (bt2:make-semaphore)) (release (bt2:make-semaphore))
         (done (bt2:make-semaphore)) (a-thread nil) (b-thread nil)
         (b-quiet nil) (before-release nil) (thread-errors nil)
         (stop-b (nodecode-shell-scope-demo::start-shell-cell (nlk:json-object) b))
         (manager (nle::shell-cell-manager-for b t)))
    (push (cons "nodecode-shell-scope-demo" stop-b) (nle::shell-cell-manager-running manager))
    (setf *scope-acquire*
          (lambda (model)
            (when (eq model a)
              (bt2:signal-semaphore entered)
              (bt2:wait-on-semaphore release))))
    (unwind-protect
         (progn
           (setf a-thread
                 (bt2:make-thread
                  (lambda ()
                    (handler-case
                        (nodecode-shell-scope-demo::start-shell-cell (nlk:json-object) a)
                      (error (condition) (push condition thread-errors))))))
           (is (bt2:wait-on-semaphore entered :timeout 2))
           (setf b-thread
                 (bt2:make-thread
                  (lambda ()
                    (unwind-protect
                         (handler-case (setf b-quiet (nle::stop-shell-cells b :timeout 0.02))
                           (error (condition) (push condition thread-errors)))
                      (bt2:signal-semaphore done)))))
           (setf before-release (bt2:wait-on-semaphore done :timeout 0.3)))
      (bt2:signal-semaphore release)
      (when a-thread (bt2:join-thread a-thread))
      (when b-thread (bt2:join-thread b-thread))
      (nodecode-shell-scope-demo::stop-shell-cell a)
      (nle::stop-shell-cells b))
    (is before-release)
    (is b-quiet)
    (is (null thread-errors))
    (is (= 1 (count (list :first-stopped b) *scope-log* :test #'equal)))
    (is (= 1 (count (list :last-stopped b) *scope-log* :test #'equal)))))

(deftest shell-scope-stop-of-busy-direct-acquisition-refuses-without-waiting
    (with-shell-scope-fixture)
  ;; Given a directly acquiring scope, when another thread requests STOP, then
  ;; it promptly refuses and leaves the owner intact until acquisition returns.
  (let ((model (list :busy)) (entered (bt2:make-semaphore))
        (release (bt2:make-semaphore)) (done (bt2:make-semaphore))
        (start-thread nil) (stop-thread nil) (before-release nil) (refused nil))
    (setf *scope-acquire* (lambda (model)
                           (declare (ignore model))
                           (bt2:signal-semaphore entered)
                           (bt2:wait-on-semaphore release)))
    (unwind-protect
         (progn
           (setf start-thread (bt2:make-thread
                               (lambda () (nodecode-shell-scope-demo::start-shell-cell
                                           (nlk:json-object) model))))
           (is (bt2:wait-on-semaphore entered :timeout 2))
           (setf stop-thread
                 (bt2:make-thread
                  (lambda ()
                    (handler-case (nodecode-shell-scope-demo::stop-shell-cell model)
                      (error () (setf refused t)))
                    (bt2:signal-semaphore done))))
           (setf before-release (bt2:wait-on-semaphore done :timeout 0.3)))
      (bt2:signal-semaphore release)
      (when start-thread (bt2:join-thread start-thread))
      (when stop-thread (bt2:join-thread stop-thread)))
    (is before-release)
    (is refused)
    (is (gethash model nodecode-shell-scope-demo::*shell-stops*))
    (nodecode-shell-scope-demo::stop-shell-cell model)
    (is (zerop (hash-table-count nodecode-shell-scope-demo::*shell-stops*)))))

(deftest shell-scope-stale-stop-is-inert-while-replacement-is-acquiring
    (with-shell-scope-fixture)
  ;; Given an obsolete handle and a replacement holding the same model lock,
  ;; when the old handle is called, then it returns harmlessly, not a busy refusal.
  (let* ((model (list :replace))
         (old (nodecode-shell-scope-demo::start-shell-cell (nlk:json-object) model))
         (entered (bt2:make-semaphore)) (release (bt2:make-semaphore))
         (thread nil) (refused nil) (new nil))
    (setf *scope-acquire* (lambda (model)
                           (declare (ignore model))
                           (bt2:signal-semaphore entered)
                           (bt2:wait-on-semaphore release)))
    (unwind-protect
         (progn
           (setf thread (bt2:make-thread
                         (lambda () (setf new (nodecode-shell-scope-demo::start-shell-cell
                                              (nlk:json-object) model)))))
           (is (bt2:wait-on-semaphore entered :timeout 2))
           (handler-case (funcall old) (error () (setf refused t)))
           (is (gethash model nodecode-shell-scope-demo::*shell-stops*)))
      (bt2:signal-semaphore release)
      (when thread (bt2:join-thread thread)))
    (is (not refused))
    (is (= 1 (count (list :first-stopped model) *scope-log* :test #'equal)))
    (funcall new)
    (is (= 2 (count (list :first-stopped model) *scope-log* :test #'equal)))
    (is (= 2 (count (list :last-stopped model) *scope-log* :test #'equal)))))
