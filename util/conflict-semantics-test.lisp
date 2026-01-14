(in-package :conflict-semantics)

;;; ============================================================
;;; Test suite
;;; ============================================================

(defvar *around-log* nil)
(defvar *aux-log* nil)


(defun run-tests ()
  "Run the conflict-semantics test suite."

  (format t "~&~%=== Testing conflict-detecting generic functions ===~%~%")

  ;; Clean up from previous runs
  (dolist (class-name '(test-a test-b test-c test-d test-e))
    (let ((class (find-class class-name nil)))
      (when class
        (setf (find-class class-name) nil))))

  ;; Define test class hierarchy:
  ;;
  ;;        test-a (method: "A")
  ;;       /      \
  ;;   test-b      test-c (method: "C")
  ;;       \      /
  ;;        test-d   <-- conflict!
  ;;           |
  ;;        test-e (method: "E")

  (defclass test-a () ())
  (defclass test-b (test-a) ())
  (defclass test-c (test-a) ())
  (defclass test-d (test-b test-c) ())
  (defclass test-e (test-d) ())

  (defgeneric/conflict test-speak (obj))

  (defmethod test-speak ((obj test-a))
    (format nil "Speaking from A"))

  (defmethod test-speak ((obj test-c))
    (format nil "Speaking from C"))

  (defmethod test-speak ((obj test-e))
    (format nil "Speaking from E"))

  ;; Test 1: Simple inheritance
  (format t "Test 1: Simple inheritance (A)~%")
  (let ((result (test-speak (make-instance 'test-a))))
    (assert (string= result "Speaking from A"))
    (format t "  PASS: ~S~%" result))

  ;; Test 2: Inherited method
  (format t "~%Test 2: Inherited method (B inherits from A)~%")
  (let ((result (test-speak (make-instance 'test-b))))
    (assert (string= result "Speaking from A"))
    (format t "  PASS: ~S~%" result))

  ;; Test 3: Own method shadows inherited
  (format t "~%Test 3: Own method (C has own)~%")
  (let ((result (test-speak (make-instance 'test-c))))
    (assert (string= result "Speaking from C"))
    (format t "  PASS: ~S~%" result))

  ;; Test 4: Conflict detection
  (format t "~%Test 4: Conflict detection (D inherits from B and C)~%")
  (handler-case
      (progn
        (test-speak (make-instance 'test-d))
        (format t "  FAIL: Expected conflict error~%"))
    (method-conflict-error (e)
      (format t "  PASS: Got conflict error: ~A~%" e)))

  ;; Test 5: Own method resolves conflict
  (format t "~%Test 5: Own method resolves inherited conflict (E)~%")
  (let ((result (test-speak (make-instance 'test-e))))
    (assert (string= result "Speaking from E"))
    (format t "  PASS: ~S~%" result))

  ;; Test 6: Explicit delegation resolves conflict
  (format t "~%Test 6: Explicit delegation (D delegates to C)~%")
  (delegate-to test-d #'test-speak test-c)
  (let ((result (test-speak (make-instance 'test-d))))
    (assert (string= result "Speaking from C"))
    (format t "  PASS: ~S~%" result))

  ;; Test 7: Remove delegation, conflict returns
  (format t "~%Test 7: Remove delegation, conflict returns~%")
  (undelegate test-d #'test-speak)
  (handler-case
      (progn
        (test-speak (make-instance 'test-d))
        (format t "  FAIL: Expected conflict error~%"))
    (method-conflict-error (e)
      (declare (ignore e))
      (format t "  PASS: Conflict returned after undelegate~%")))

  ;; Test 8: Auxiliary methods
  (format t "~%Test 8: Auxiliary methods~%")

  (defgeneric/conflict test-logged (obj))

  (defmethod test-logged ((obj test-a))
    (push :primary-a *aux-log*)
    "primary-a")

  (defmethod test-logged :before ((obj test-a))
    (push :before-a *aux-log*))

  (defmethod test-logged :after ((obj test-a))
    (push :after-a *aux-log*))

  (defmethod test-logged :before ((obj test-b))
    (push :before-b *aux-log*))

  (defmethod test-logged :after ((obj test-b))
    (push :after-b *aux-log*))

  (setq *aux-log* nil)
  (test-logged (make-instance 'test-b))
  (let ((expected '(:after-b :after-a :primary-a :before-a :before-b)))
    ;; Log is in reverse order due to push
    (format t "  Execution log: ~S~%" (reverse *aux-log*))
    (if (equal *aux-log* expected)
        (format t "  PASS: Auxiliary methods called in correct order~%")
        (format t "  FAIL: Expected ~S~%" expected)))

  ;; Test 9: Around methods
  (format t "~%Test 9: Around methods~%")

  (defgeneric/conflict test-around (obj))

  (defmethod test-around ((obj test-a))
    (push :primary *around-log*)
    :primary-result)

  (defmethod test-around :around ((obj test-a))
    (push :around-a-start *around-log*)
    (prog1 (call-next-method)
      (push :around-a-end *around-log*)))

  (defmethod test-around :around ((obj test-b))
    (push :around-b-start *around-log*)
    (prog1 (call-next-method)
      (push :around-b-end *around-log*)))

  (setq *around-log* nil)
  (let ((result (test-around (make-instance 'test-b))))
    (format t "  Result: ~S~%" result)
    (format t "  Execution log: ~S~%" (reverse *around-log*))
    ;; Correct order: B-start, A-start, primary, A-end, B-end
    ;; With push, list is built in reverse: (:around-b-end :around-a-end :primary :around-a-start :around-b-start)
    (if (and (eq result :primary-result)
             (equal *around-log*
                    '(:around-b-end :around-a-end :primary
                      :around-a-start :around-b-start)))
        (format t "  PASS: Around methods work correctly~%")
        (format t "  FAIL: Expected different execution order. Got: ~S~%" *around-log*)))

  ;; Test 10: Diamond inheritance without conflict
  (format t "~%Test 10: Diamond inheritance (same method via multiple paths)~%")

  ;; test-b and test-c both lead back to test-a's method
  ;; but test-c has its own, so without test-c's method, B would inherit A's
  ;; Let's make a clean diamond:

  (defclass diamond-top () ())
  (defclass diamond-left (diamond-top) ())
  (defclass diamond-right (diamond-top) ())
  (defclass diamond-bottom (diamond-left diamond-right) ())

  (defgeneric/conflict test-diamond (obj))

  (defmethod test-diamond ((obj diamond-top))
    "from-top")

  ;; diamond-bottom inherits from both diamond-left and diamond-right,
  ;; but they both just inherit from diamond-top, so no conflict
  (let ((result (test-diamond (make-instance 'diamond-bottom))))
    (assert (string= result "from-top"))
    (format t "  PASS: Diamond inheritance resolves correctly: ~S~%" result))

  ;; Describe resolution for debugging
  (format t "~%=== Resolution descriptions ===~%")
  (describe-resolution #'test-speak 'test-d)
  (describe-resolution #'test-logged 'test-b)

  (format t "~%=== All tests completed ===~%"))

;;; Run tests on load (comment out if not desired)
;; (run-tests)
