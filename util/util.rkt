#lang racket ;; -*- Scheme -*-

(provide (all-defined-out))

(require (only-in scribble/base elem linebreak nested italic smaller bold)
         (only-in scribble/core make-paragraph make-style)
         (only-in scriblib/footnote note)
         (only-in scriblib/render-cond cond-element)
         (only-in scribble/html-properties css-addition html-defaults)
         (only-in scribble/latex-properties tex-addition make-tex-addition latex-defaults))

(require scribble/html-properties)
#;
(define-syntax-rule (when-not condition body ...)
  (when (not condition) body ...))

(define-syntax-rule (when/list condition body ...)
  (if condition (begin body ...) '()))

(define-syntax-rule (tex-only content)
  (cond-element (latex content) (else '())))
(define-syntax-rule (html-only content)
  (cond-element (html content) (else '())))

(define (exact-chars . args) (apply elem #:style (make-style #f '(exact-chars)) args))
(define (pretitle content) (make-paragraph (make-style 'pretitle '()) content))
(define (tex . args) (tex-only (apply exact-chars args)))
(define (noindent) (tex "\\noindent"))

;; TODO: find how to insert raw html
;;(define (Html . args) (html-only (apply elem #:style (make-style #f '(exact-chars)) args)))
(define (hhr) (html-only (elem #:style (make-style #f (list (make-alt-tag "hr"))))))

(define (xnote x . y)
  (list (apply note (append y (list (html-only (list (linebreak) (linebreak) (linebreak))))))
        x))

(define epigraph-style
  (make-style
   #f
   (list (make-tex-addition
          (bytes-append
           #"\n\\usepackage{epigraph}\n"
           #"\\setlength\\epigraphwidth{.6\\textwidth}\n"))
         (make-css-addition
          (bytes-append
           #".epigraph { margin: 2em 0; text-align: right; max-width: 60%;"
           #"  margin-left: auto; font-size: 1.1em; }\n"
           #".epigraph blockquote { font-style: italic; margin: 0; }\n"
           #".epigraph .epigraph-attribution { margin-top: 1em; font-style: normal;"
           #"  font-size: 0.95em; color: #555; }")))))

(define (epigraph #:author (author #f) . text)
  (elem
   #:style epigraph-style
   (tex-only
    (elem
     (exact-chars "\\epigraph{")
     text
     (exact-chars "}{")
     (or author '())
     (exact-chars "}")
     (noindent)))
   (html-only
    (elem
     #:style "epigraph"
     (italic text)
     (if author
       (elem #:style "epigraph-attribution"
             (apply smaller (list "— " author)))
       '())))))

;; scribble/report subsubsub*section does not work https://github.com/racket/scribble/issues/540
(define (Paragraph . x)
  (list (html-only (list (bold x) "  "))
        (list (tex "\\paragraph{") x (tex "}"))))
