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

;; Apparently these are not defined in all Scheme implementations
;; (But e.g. are called fold-left and fold-right in Chez Scheme).
(define (foldl Cons Nil l)
  (if (null? l) Nil (foldl Cons (Cons (car l) Nil) (cdr l))))
(define (foldr Cons Nil l)
  (if (null? l) Nil (Cons (car l) (foldr Cons Nil (cdr l)))))

(check! (equal? (foldl cons '() '(1 2 3 4 5)) '(5 4 3 2 1)))
(check! (equal? (foldr cons '() '(1 2 3 4 5)) '(1 2 3 4 5)))

(define (first l) (car l))
(define (second l) (cadr l))
(define (third l) (caddr l))
(define (fourth l) (cadddr l))
(define (fifth l) (cadddr (cdr l)))

(check! (equal? (first '(1 2 3 4 5)) 1))
(check! (equal? (second '(1 2 3 4 5)) 2))
(check! (equal? (third '(1 2 3 4 5)) 3))
(check! (equal? (fourth '(1 2 3 4 5)) 4))
(check! (equal? (fifth '(1 2 3 4 5)) 5))

(displayln "0.3. Some macros")

;; This macro with keeping nesting and indentation under control.
;; https://fare.livejournal.com/189741.html
(define-syntax nest
  (syntax-rules ()
    ((nest (x ...) y z ...) (x ... (nest y z ...)))
    ((nest x) x)))

(check! (= (nest (+ 1 2) (* 3 4) (- 5 6)) -9))
