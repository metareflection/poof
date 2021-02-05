(define (displayln . l) (for-each display l) (newline))

(displayln "0. Defining testing support")

(define-syntax check!
  (syntax-rules ()
    ((_ (pred . args))
     (or (pred . args) (error 'check! "check failed:" '(pred . args) 'reduced 'to (list 'pred . args))))))
