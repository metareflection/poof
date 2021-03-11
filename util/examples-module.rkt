#lang racket/base

(provide declare-examples/module
         finalize-examples/module
         examples/module)

(require rackunit
         syntax/parse/define
         scribble/example
         (for-syntax racket/base
                     racket/match
                     syntax/transformer
                     syntax/srcloc))

(begin-for-syntax
  (define (stx-e stx) (if (syntax? stx) (syntax-e stx) stx))
  (define (restx ctx stx) (datum->syntax ctx (stx-e stx) ctx ctx))
  (struct examples/module-accumulator [evaluator lang [body #:mutable]]
    #:property prop:procedure
    (λ (self stx)
      ((make-variable-like-transformer (examples/module-accumulator-evaluator self))
       stx)))

  (define-splicing-syntax-class examples-options
    #:attributes [(options 1)]
    [pattern {~and {~seq options ...}
                   {~seq {~alt #:hidden
                               #:no-result
                               {~seq #:label _}
                               {~seq #:eval _}}
                         ...}}])

  (define (remove-comments stxs)
    (match stxs
      ['() '()]
      [(cons stx rst)
       (syntax-parse stx
         #:datum-literals [code:comment]
         [(code:comment . _) (remove-comments rst)]
         [(a ...)
          (cons (restx stx (remove-comments (attribute a)))
                (remove-comments rst))]
         [(a ...+ . b)
          (cons (restx stx (append (remove-comments (attribute a)) #'b))
                (remove-comments rst))]
         [_ (cons stx (remove-comments rst))])])))

(define-simple-macro (declare-examples/module name:id modlang:expr body:expr ...)
  (begin
    (define ev (make-base-eval #:lang 'modlang))
    (define-syntax name
      (examples/module-accumulator (quote-syntax ev)
                                   (quote-syntax modlang)
                                   (list (quote-syntax body) ...)))))

(define-simple-macro (finalize-examples/module name)
  #:declare name (static examples/module-accumulator? "examples/module name")
  #:with modlang (syntax-local-introduce
                  (examples/module-accumulator-lang (attribute name.value)))
  #:with body (map syntax-local-introduce
                   (examples/module-accumulator-body (attribute name.value)))
  (module name modlang . body))

(define-simple-macro
  (examples/module name opts:examples-options dat ...)
  #:declare name (static examples/module-accumulator? "examples/module name")
  #:with ev (examples/module-accumulator-evaluator (attribute name.value))
  #:do [(set-examples/module-accumulator-body!
         (attribute name.value)
         (append (examples/module-accumulator-body (attribute name.value))
                 (map syntax-local-introduce
                      (remove-comments (attribute dat)))))]
  (examples #:eval ev opts.options ... dat ...))
