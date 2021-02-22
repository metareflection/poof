;;; 0. Prelude -- basic definitions for poof
(define (displayln . l) (for-each display l) (newline))

(displayln "0. Defining common utilities")

(displayln "0.1. Testing support")

(define-syntax check!
  (syntax-rules ()
    ((_ (pred . args))
     (or (pred . args)
         (begin
           (display "Check failed: ") (write '(pred . args)) (newline)
           (display "Reduced to: ") (write (list pred . args)) (newline)
           (error 'check! "check failed:" '(pred . args) 'reduced-to (list pred . args)))))))

(displayln "0.2. General library functions")

;; Apparently this isn't defined in Chez Scheme (it's called fold-left instead).
(define (foldl Cons Nil l)
  (if (null? l) '() (Cons (car l) (foldl Cons Nil (cdr l)))))

(define (first l) (car l))
(define (second l) (cadr l))
(define (third l) (caddr l))
(define (fourth l) (cadddr l))
(define (fifth l) (caddrr (cdr l)))
