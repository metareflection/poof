;;;; conflict-semantics-tests.lisp

(in-package :conflict-semantics)

;;; ============================================================
;;; Test class hierarchy
;;;
;;;        test-a (method: "A")
;;;       /      \
;;;   test-b      test-c (method: "C")
;;;       \      /
;;;        test-d   <-- conflict!
;;;           |
;;;        test-e (method: "E")
;;; ============================================================

(defclass test-a () ())
(defclass test-b (test-a) ())
(defclass test-c (test-a) ())
(defclass test-d (test-b test-c) ())
(defclass test-e (test-d) ())

(defclass diamond-top () ())
(defclass diamond-left (diamond-top) ())
(defclass diamond-right (diamond-top) ())
(defclass diamond-bottom (diamond-left diamond-right) ())

;;; ============================================================
;;; Test generic functions
;;; ============================================================

(defgeneric/conflict test-speak (obj))
(defgeneric/conflict test-logged (obj))
(defgeneric/conflict test-around (obj))
(defgeneric/conflict test-diamond (obj))

;;; ============================================================
;;; Test state
;;; ============================================================

(defvar *aux-log* nil)
(defvar *around-log* nil)

;;; ============================================================
;;; Method definitions
;;; ============================================================

(defmethod test-speak ((obj test-a))
  (format nil "Speaking from A"))
(defmethod test-speak ((obj test-c))
  (format nil "Speaking from C"))
(defmethod test-speak ((obj test-e))
  (format nil "Speaking from E"))

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

(defmethod test-diamond ((obj diamond-top))
  "from-top")

;;; ============================================================
;;; Test runner
;;; ============================================================

(defun run-tests ()
  "Run the conflict-semantics test suite."
  (format t "~&~%=== Testing conflict-detecting generic functions ===~%~%")

  ;; Reset state
  (setq *aux-log* nil *around-log* nil)
  (dolist (gf (list #'test-speak #'test-logged #'test-around #'test-diamond))
    (clear-resolution-cache gf))

  (let ((passed 0) (failed 0))
    (flet ((pass (msg) (format t "  PASS: ~A~%" msg) (incf passed))
           (fail (msg) (format t "  FAIL: ~A~%" msg) (incf failed)))

      ;; Test 1
      (format t "Test 1: Simple inheritance (A)~%")
      (let ((r (test-speak (make-instance 'test-a))))
        (if (string= r "Speaking from A") (pass r) (fail r)))

      ;; Test 2
      (format t "~%Test 2: Inherited method (B inherits from A)~%")
      (let ((r (test-speak (make-instance 'test-b))))
        (if (string= r "Speaking from A") (pass r) (fail r)))

      ;; Test 3
      (format t "~%Test 3: Own method (C has own)~%")
      (let ((r (test-speak (make-instance 'test-c))))
        (if (string= r "Speaking from C") (pass r) (fail r)))

      ;; Test 4
      (format t "~%Test 4: Conflict detection (D inherits from B and C)~%")
      (handler-case
          (progn (test-speak (make-instance 'test-d)) (fail "No error raised"))
        (method-conflict-error (e) (pass (format nil "~A" e))))

      ;; Test 5
      (format t "~%Test 5: Own method resolves conflict (E)~%")
      (let ((r (test-speak (make-instance 'test-e))))
        (if (string= r "Speaking from E") (pass r) (fail r)))

      ;; Test 6
      (format t "~%Test 6: Explicit delegation (D delegates to C)~%")
      (delegate-to test-d #'test-speak test-c)
      (let ((r (test-speak (make-instance 'test-d))))
        (if (string= r "Speaking from C") (pass r) (fail r)))

      ;; Test 7
      (format t "~%Test 7: Remove delegation, conflict returns~%")
      (undelegate test-d #'test-speak)
      (handler-case
          (progn (test-speak (make-instance 'test-d)) (fail "No error raised"))
        (method-conflict-error () (pass "Conflict returned")))

      ;; Test 8
      (format t "~%Test 8: Auxiliary methods~%")
      (setq *aux-log* nil)
      (test-logged (make-instance 'test-b))
      (let ((expected '(:after-b :after-a :primary-a :before-a :before-b)))
        (format t "  Log: ~S~%" (reverse *aux-log*))
        (if (equal *aux-log* expected) (pass "Correct order") (fail *aux-log*)))

      ;; Test 9
      (format t "~%Test 9: Around methods~%")
      (setq *around-log* nil)
      (let ((r (test-around (make-instance 'test-b)))
            (expected '(:around-b-end :around-a-end :primary :around-a-start :around-b-start)))
        (format t "  Result: ~S~%  Log: ~S~%" r (reverse *around-log*))
        (if (and (eq r :primary-result) (equal *around-log* expected))
            (pass "Around methods work")
            (fail *around-log*)))

      ;; Test 10
      (format t "~%Test 10: Diamond inheritance~%")
      (let ((r (test-diamond (make-instance 'diamond-bottom))))
        (if (string= r "from-top") (pass r) (fail r)))

      ;; Summary
      (format t "~%=== Summary: ~D passed, ~D failed ===~%~%" passed failed)
      (zerop failed))))
