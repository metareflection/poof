#lang racket
; -*- Scheme -*-

(require
  (only-in scribble/base ~ bold emph nested elem section subsection seclink verbatim linebreak)
  scriblib/bibtex
  (only-in scribble/core make-style)
  (only-in scribble/manual racket racketblock code codeblock litchar itemize item)
;; (only-in scribble/example examples make-base-eval)
;; (only-in scriblib/footnote note)
  (only-in scriblib/autobib make-bib authors define-cite)
  (only-in scribble-abbrevs appendix)
;; (only-in scribble/html-properties head-extra html-defaults)
  (only-in scribble-math/dollar $)
  scribble/html-properties
  scriblib/render-cond

;; scribble/minted
  syntax/parse/define
;;  "util/examples-module.rkt"
  "enumitem.rkt"
  "util.rkt"
  (for-syntax "util.rkt")
  (for-label racket))

(provide
  (all-defined-out)
  (all-from-out scriblib/bibtex)
;; (all-from-out scribble/core)
  (all-from-out scribble/manual)
;; (all-from-out scribble/example)
;; (all-from-out scribble/footnote)
  (all-from-out scriblib/bibtex)
  (all-from-out scriblib/autobib)
  (all-from-out scribble-abbrevs)
  (all-from-out scribble-math/dollar)
  (all-from-out syntax/parse/define)
;;  (all-from-out "examples-module.rkt")
  (all-from-out "enumitem.rkt")
  (all-from-out "util.rkt")
  (for-syntax (all-from-out "util.rkt")))

(define (tex-linebreak)
  (when/list (render-latex?) (linebreak)))
(define ~~
  (when/list (render-latex?) (list ~ ~)))

(define (anonymize x . y) y)
(define (GerbilScheme) (anonymize "our Scheme implementation" "Gerbil Scheme"))
(define (principle . x) (bold (emph x)))
(define (Quote . x) (apply nested #:style "quote" x))


(define-simple-macro (r a ...) (racket a ...))
(define (omega) "ω")
(define-simple-macro (c a ...) (elem #:style 'tt a ...))
(define-simple-macro (Code a ...) (verbatim a ...))
(define-simple-macro (Definitions a ...) (examples/module poof #:no-result a ...))
(define-simple-macro (Examples a ...) (examples #:eval poof #:no-result a ...))
(define-simple-macro (Checks a ...) (examples #:eval poof #:label #f a ...))
(define-simple-macro (λ formals body ...) (lambda formals body ...))
(define-simple-macro (TODO body ...) '())
(define-simple-macro (Xitemize body ...) (list body ...))
(define-simple-macro (Xitem body ...) (list " " body ... " "))
(define (ᵢ) (list (html-elem (c "ᵢ")) (tex "${}_i$")))
(define (Ri) (list (c "R") (ᵢ)))
(define (Pi) (list (c "P") (ᵢ)))
(define (⋂) (list (tex "$\bigcap$" (html-elem "⋂"))))
(define (⇝) (list (tex "$\rightsquigarrow$" (html-elem "⇝"))))

(define super 'super)
(define self 'self)
(define (XXXX . x)
   (list
     (section #:style 'unnumbered "XXX EDIT HERE XXX")
     (tex "\\noindent")
     (bold "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")))

(define-bibtex-cite "ltuo.bib" ~cite citet generate-bibliography)
(define (~nocite . x) (let ((_ (apply ~cite x))) (void)))

(define-simple-macro (defsection name tag text) (define (name (x text)) (seclink tag x)))
(defsection section1 "Prototypes_bottom_up" "section 1")

(define (favicon-style)
  (and (render-html?)
    (make-style "favicon" ; name is arbitrary, just needs to be non-false
      (list (head-extra
              '(link ([rel "icon"]
                            ;;[href "resources/pic/Half-Life_lambda_logo.svg"]
                            [href "resources/pic/cube.svg"]
                            [type "image/svg+xml"]))))))) ;; or image/x-icon, image/png
