;;;; cell-lifecycle-test.lisp --- terminal scopes and their native lifecycle.
;;;; SPDX-License-Identifier: MIT
(in-package #:nodecode.test)

(defpackage #:nodecode-shell-demo (:use #:cl))
(nlk:define-peripheral nodecode-shell-demo::demo :not-running t)
(defvar *shell-demo-log* nil)
(defvar *shell-demo-fail* nil)
(defvar *shell-demo-config* nil)

(nle:define-cell nodecode-shell-demo::demo
  (:section ("shell-demo") (:owner "nodecode-shell-demo")
    ("mode" :string :default "quiet" :doc "test setting"))
  (:start (lambda ()
            (push :gateway-start *shell-demo-log*)
            (nle:on-stop (lambda () (push :gateway-stop *shell-demo-log*)))))
  (:shell-start (lambda (settings model)
                  (push (list :shell-start model (getf settings :mode)) *shell-demo-log*)
                  (nle:on-stop (lambda () (push (list :shell-stop model) *shell-demo-log*)))
                  (when *shell-demo-fail* (error "shell demo refused")))))

(defmacro with-shell-cell-fixture ((model) &body body)
  `(let* ((,model (make-instance 'nle:gateway-chat-model :ws nil :session-id "shell-demo"))
          (saved-load (symbol-function 'nlk:load-cell))
          (saved-config (symbol-function 'nle::read-shared-config)))
     (with-saved-globals
         ((*shell-demo-log* nil) (*shell-demo-fail* nil)
          (*shell-demo-config* (nlk:json-object))
          (nlk::*cells* (list (nlk::make-cell :name "nodecode-shell-demo"
                                            :systems '("nodecode-shell-demo")
                                            :kind :peripheral
                                            :start #'nodecode-shell-demo::start-cell)))
          (nle::*shell-cell-managers* (make-hash-table :test #'eq)))
       (unwind-protect
            (progn
              (setf (symbol-function 'nlk:load-cell) (lambda (&rest names) (declare (ignore names)) nil)
                    (symbol-function 'nle::read-shared-config) (lambda () *shell-demo-config*))
              ,@body)
         (nodecode-shell-demo::stop-cell)
         (nodecode-shell-demo::stop-shell-cell)
         (nle::stop-shell-cells ,model)
         (setf (symbol-function 'nlk:load-cell) saved-load
               (symbol-function 'nle::read-shared-config) saved-config)))))

(deftest shell-cell-declaration-is-inert-and-exports-explicit-terminal-entry ()
  (is (null *shell-demo-log*))
  (is (eq :external (nth-value 1 (find-symbol "START-SHELL-CELL" "NODECODE-SHELL-DEMO"))))
  (is (eq :external (nth-value 1 (find-symbol "STOP-SHELL-CELL" "NODECODE-SHELL-DEMO"))))
  (is (zerop (hash-table-count nodecode-shell-demo::*shell-stops*))))

(deftest shell-cell-gateway-and-two-model-scopes-are-independent (with-shell-cell-fixture (model))
  (let* ((other (make-instance 'nle:gateway-chat-model :ws nil :session-id "other"))
         (config (nlk:json-object "shell-demo" (nlk:json-object "mode" "loud"))))
    (nodecode-shell-demo::start-cell config)
    (let ((first (nodecode-shell-demo::start-shell-cell config model))
          (second (nodecode-shell-demo::start-shell-cell config other)))
      (is (= 2 (hash-table-count nodecode-shell-demo::*shell-stops*)))
      (is (equal "loud" (nodecode-shell-demo::setting :mode)))
      (funcall first)
      (funcall first)
      (is (= 1 (hash-table-count nodecode-shell-demo::*shell-stops*)))
      (is (= 1 (count (list :shell-stop model) *shell-demo-log* :test #'equal)))
      (is (equal "loud" (nodecode-shell-demo::setting :mode)))
      (nodecode-shell-demo::stop-cell)
      (is (= 1 (hash-table-count nodecode-shell-demo::*shell-stops*)))
      (funcall second)
      (is (zerop (hash-table-count nodecode-shell-demo::*shell-stops*))))))

(deftest shell-cell-partial-start-unwinds-and-disabled-section-acquires-nothing (with-shell-cell-fixture (model))
  (setf *shell-demo-fail* t)
  (is (signals-error error (nodecode-shell-demo::start-shell-cell (nlk:json-object) model)))
  (is (equal (list :shell-stop model) (first *shell-demo-log*)))
  (is (zerop (hash-table-count nodecode-shell-demo::*shell-stops*)))
  (setf *shell-demo-log* nil *shell-demo-fail* nil)
  (nodecode-shell-demo::start-shell-cell
   (nlk:json-object "shell-demo" (nlk:json-object "enabled" nil)) model)
  (is (null *shell-demo-log*)))

(deftest shell-cell-native-manager-is-idempotent-and-never-starts-gateway (with-shell-cell-fixture (model))
  (let ((manager (nle::start-shell-cells model)))
    (nle::request-shell-cell-action model '("nodecode-shell-demo"))
    (is (= 1 (length (nle::shell-cell-manager-running manager))))
    (is (= 1 (count :shell-start *shell-demo-log* :key (lambda (entry) (and (consp entry) (first entry))))))
    (is (not (member :gateway-start *shell-demo-log*)))
    (is (not (nlk:cell-started-p (first nlk::*cells*))))
    (nle::request-shell-cell-action model '("nodecode-shell-demo") "stop")
    (is (null (nle::shell-cell-manager-running manager)))
    (nle::request-shell-cell-action model '("nodecode-shell-demo") "start")
    (is (= 1 (length (nle::shell-cell-manager-running manager))))
    (nle::stop-shell-cells model)
    (is (null (nle::shell-cell-manager-for model)))))

(deftest shell-cell-restart-rereads-settings-and-sync-removes-detached-cells (with-shell-cell-fixture (model))
  (nle::start-shell-cells model)
  (setf *shell-demo-config* (nlk:json-object "shell-demo" (nlk:json-object "mode" "new")))
  (nle::request-shell-cell-action model '("nodecode-shell-demo") "restart")
  (is (equal (list :shell-start model "new") (first *shell-demo-log*)))
  (nle::request-shell-cell-action model nil "sync")
  (is (null (nle::shell-cell-manager-running (nle::shell-cell-manager-for model))))
  (nle::request-shell-cell-action model '("nodecode-shell-demo") "sync")
  (is (equal (list :shell-start model "new") (first *shell-demo-log*)))
  (setf *shell-demo-config* (nlk:json-object "shell-demo" (nlk:json-object "enabled" nil)))
  (nle::request-shell-cell-action model '("nodecode-shell-demo") "restart")
  (is (equal (list :shell-stop model) (first *shell-demo-log*))))

(deftest shell-cell-off-invalidates-a-slow-load-before-it-acquires (with-shell-cell-fixture (model))
  (let ((entered (bt2:make-semaphore)) (release (bt2:make-semaphore)) (thread nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'nlk:load-cell)
                 (lambda (&rest names)
                   (declare (ignore names))
                   (bt2:signal-semaphore entered)
                   (bt2:wait-on-semaphore release)))
           (setf thread (bt2:make-thread
                         (lambda () (nle::request-shell-cell-action model '("nodecode-shell-demo")))))
           (is (bt2:wait-on-semaphore entered :timeout 2))
           (nle::request-shell-cell-action model '("nodecode-shell-demo") "stop")
           (bt2:signal-semaphore release)
           (bt2:join-thread thread)
           (is (null *shell-demo-log*))
           (is (null (nle::shell-cell-manager-running (nle::shell-cell-manager-for model)))))
      (bt2:signal-semaphore release)
      (when thread (bt2:join-thread thread)))))

(deftest shell-cell-exit-closes-authority-before-a-slow-load-finishes (with-shell-cell-fixture (model))
  (let ((entered (bt2:make-semaphore)) (release (bt2:make-semaphore)) (thread nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'nlk:load-cell)
                 (lambda (&rest names)
                   (declare (ignore names))
                   (bt2:signal-semaphore entered)
                   (bt2:wait-on-semaphore release)))
           (setf thread (bt2:make-thread
                         (lambda () (nle::request-shell-cell-action model '("nodecode-shell-demo")))))
           (is (bt2:wait-on-semaphore entered :timeout 2))
           (nle::stop-shell-cells model)
           (bt2:signal-semaphore release)
           (bt2:join-thread thread)
           (is (null *shell-demo-log*)))
      (bt2:signal-semaphore release)
      (when thread (bt2:join-thread thread)))))

(deftest shell-cell-one-refusal-does-not-strand-sibling-cleanup (with-shell-cell-fixture (model))
  (let* ((manager (nle::shell-cell-manager-for model t)) (stopped nil))
    (setf (nle::shell-cell-manager-running manager)
          (list (cons "bad" (lambda () (error "still draining")))
                (cons "good" (lambda () (setf stopped t)))))
    (handler-bind ((warning #'muffle-warning)) (nle::stop-shell-cells model))
    (is stopped)
    (is (equal '("bad") (mapcar #'car (nle::shell-cell-manager-running manager))))
    (is (nle::shell-cell-manager-for model))
    (setf (nle::shell-cell-manager-running manager) nil)))

(deftest shell-cell-protocol-retains-old-senders-and-checks-actions ()
  (dolist (action '(nil "start" "stop" "restart" "sync"))
    (let ((payload (nlk:json-object "names" #())))
      (when action (setf (gethash "action" payload) action))
      (is (nlk:decode-sync-message (nlk:encode-unsequenced-frame "organism.cells" payload)))))
  (dolist (action '(nil :null 3 "arbitrary"))
    (is (signals-error nlk:protocol-error
          (nlk:decode-sync-message
           (nlk:encode-unsequenced-frame "organism.cells"
                                        (nlk:json-object "names" #() "action" action)))))))

(deftest shell-cell-gateway-start-stop-and-attach-use-native-notifications (with-shell-cell-fixture (model))
  (let ((saved (symbol-function 'nle::broadcast-organism)) (frames nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'nle::broadcast-organism)
                 (lambda (kind payload &key session-id)
                   (declare (ignore session-id))
                   (when (equal kind "organism.cells") (push payload frames))))
           (nle::%start-cells '("nodecode-shell-demo"))
           (is (equal "restart" (gethash "action" (first frames))))
           (is (equal '("nodecode-shell-demo") (coerce (gethash "names" (first frames)) 'list)))
           (is (equal '("nodecode-shell-demo")
                      (coerce (gethash "names" (nle::shell-cells-state-payload)) 'list)))
           (nle::%stop-cells '("nodecode-shell-demo"))
           (is (equal "stop" (gethash "action" (first frames))))
           (is (zerop (length (gethash "names" (nle::shell-cells-state-payload))))))
      (setf (symbol-function 'nle::broadcast-organism) saved))))

(deftest shell-cell-native-reducer-routes-enable-and-disable (with-shell-cell-fixture (model))
  (labels ((deliver (action)
             (nlt:update model
                         (make-instance 'nle::gw-projection-msg
                           :frame (nlk:decode-sync-message
                                   (nlk:encode-unsequenced-frame
                                    "organism.cells"
                                    (nlk:json-object "names" #("nodecode-shell-demo")
                                                     "action" action)))))))
    (deliver "start")
    (is (= 1 (length (nle::shell-cell-manager-running (nle::shell-cell-manager-for model)))))
    (deliver "stop")
    (is (null (nle::shell-cell-manager-running (nle::shell-cell-manager-for model))))))

(deftest shell-cell-off-during-acquisition-undoes-the-late-scope (with-shell-cell-fixture (model))
  (let* ((saved (symbol-function 'nodecode-shell-demo::start-shell-cell))
         (entered (bt2:make-semaphore)) (release (bt2:make-semaphore))
         (start-thread nil) (stop-thread nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'nodecode-shell-demo::start-shell-cell)
                 (lambda (config target)
                   (let ((stop (funcall saved config target)))
                     (bt2:signal-semaphore entered)
                     (bt2:wait-on-semaphore release)
                     stop)))
           (setf start-thread (bt2:make-thread
                               (lambda () (nle::request-shell-cell-action model '("nodecode-shell-demo")))))
           (is (bt2:wait-on-semaphore entered :timeout 2))
           (setf stop-thread (bt2:make-thread
                              (lambda () (nle::request-shell-cell-action model '("nodecode-shell-demo") "stop"))))
           (let ((manager (nle::shell-cell-manager-for model)))
             (is (loop repeat 100
                       when (bt2:with-lock-held ((nle::shell-cell-manager-lock manager))
                              (null (cdr (gethash "nodecode-shell-demo"
                                                  (nle::shell-cell-manager-desired manager)))))
                         return t
                       do (sleep 0.01))))
           (bt2:signal-semaphore release)
           (bt2:join-thread start-thread)
           (bt2:join-thread stop-thread)
           (is (zerop (hash-table-count nodecode-shell-demo::*shell-stops*)))
           (is (= 1 (count (list :shell-stop model) *shell-demo-log* :test #'equal))))
      (bt2:signal-semaphore release)
      (when start-thread (bt2:join-thread start-thread))
      (when stop-thread (bt2:join-thread stop-thread))
      (setf (symbol-function 'nodecode-shell-demo::start-shell-cell) saved))))

(deftest shell-cell-sandbox-notifications-never-load-or-acquire (with-shell-cell-fixture (model))
  ;; Given a shell outside its organism's sandbox, when every lifecycle wire
  ;; action arrives, then neither disk loading nor terminal acquisition runs.
  (let ((loads 0)
        (nle::*sandboxed-shell* t)
        (nle::*sandbox-load-noted* nil))
    (setf (symbol-function 'nlk:load-cell)
          (lambda (&rest names) (declare (ignore names)) (incf loads)))
    (dolist (action '("start" "restart" "sync" "stop"))
      (nlt:update model
                  (make-instance 'nle::gw-projection-msg
                    :frame (nlk:decode-sync-message
                            (nlk:encode-unsequenced-frame
                             "organism.cells"
                             (nlk:json-object "names" #("nodecode-shell-demo")
                                              "action" action))))))
    (is (zerop loads))
    (is (null *shell-demo-log*))
    (is (null (nle::shell-cell-manager-for model)))
    (is nle::*sandbox-load-noted*)))
