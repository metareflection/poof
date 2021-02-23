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
  (if (null? l) '() (foldl Cons (Cons (car l) Nil) (cdr l))))
(define (foldr Cons Nil l)
  (if (null? l) '() (Cons (car l) (foldr Cons Nil (cdr l)))))

(define (first l) (car l))
(define (second l) (cadr l))
(define (third l) (caddr l))
(define (fourth l) (cadddr l))
(define (fifth l) (caddrr (cdr l)))

(displayln "0.3. Some macros")

;; This macro with keeping nesting and indentation under control.
;; https://fare.livejournal.com/189741.html
(define-syntax nest
  (syntax-rules ()
    ((nest (x ...) y z ...) (x ... (nest y z ...)))
    ((nest x) x)))

(check! (= (nest (+ 1 2) (* 3 4) (- 5 6)) -9))
