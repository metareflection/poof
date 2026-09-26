;;;; Code removed from pommette


;; Reversed (left-associative) fold — rop/list←op2 consumes a list, rop*←op2 is varargs.
(define (rop/list←op2 op2 id)
  (lambda (l)
    (if (null? l) id
        (let loop ((acc (car l)) (rest (cdr l)))
          (if (null? rest) acc (loop (op2 acc (car rest)) (cdr rest)))))))
(define (rop*←op2 op2 id)
  (let ((go (rop/list←op2 op2 id)))
    (lambda args (go args))))
;; Variants of rop/list←op2 / rop*←op2 for a curried operator that takes one arg then the next.
(define rop/list←op1.1 (lambda (op1.1 id)
  (rop/list←op2 (lambda (x y) (@ op1.1 x y)) id)))
(define rop*←op1.1 (lambda (op1.1 id)
  (rop*←op2 (lambda (x y) (@ op1.1 x y)) id)))


;;; TODO: find a simple yet meaningful example for recursive protos...
;;; and their further specialization, nested or not

#|
(define web-config-spec
  (mix*
   (field-spec 'database
      (mix*
        (constant-field-spec 'port 80)
        (field-spec 'allowed
        (record!-spec)))
   record!-spec))
   (override-
  (λ (self) (λ (super) (λ (method-id)
    (case method-id
      ((port) 80)
      ((database) (length (self 'parts)))
      (else (super method-id)))))))
|#
