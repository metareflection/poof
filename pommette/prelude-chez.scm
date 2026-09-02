(import (rnrs base)
        (rnrs hashtables (6))
        (rnrs lists (6))
        ;; NB: the following require the chez-srfi collection
        (srfi :0 cond-expand)
        (srfi :1 lists))

(define (foldl f init lst)
  (fold-left (lambda (acc x) (f x acc)) init lst))

(define (foldr f init lst)
  (fold-right f init lst))

(define-syntax begin0
  (syntax-rules ()
    ((_ first rest ...)
     (let ((result first))
       (begin rest ...)
       result))))
