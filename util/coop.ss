(export #t)

(import :std/interactive :std/debug/DBG) ;; for debugging

(define-syntax define-identifier-macro
  (syntax-rules ()
    ((_ name symbol-expr list-expr)
     (define-syntax name
       (syntax-rules ()
         ((_ . x) (list-expr . x))
         (_ symbol-expr))))
    ((_ name expr)
     (define-identifier-macro name expr expr))))

(include "coop.scm")

;; Let Gerbil Scheme accept λ as short for l-a-m-b-d-a
;;(defalias λ lambda) ;; Emacs gerbil-mode already does it
;; def is already short for define in Gerbil
