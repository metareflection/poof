#lang racket ;; -*- Scheme -*-

(provide (all-defined-out))

(require (only-in scribble/base elem linebreak nested italic smaller bold)
         (only-in scribble/core make-paragraph make-style)
         (only-in scriblib/footnote note)
         (only-in scriblib/render-cond cond-element cond-block)
         (only-in scribble/html-properties css-addition html-defaults)
         (only-in scribble/latex-properties tex-addition make-tex-addition latex-defaults))

(require scribble/html-properties)
#;
(define-syntax-rule (when-not condition body ...)
  (when (not condition) body ...))

(define-syntax-rule (when/list condition body ...)
  (if condition (begin body ...) '()))

(define-syntax-rule (tex-block content)
  (cond-block (latex content) (else (nested))))
(define-syntax-rule (tex-elem content)
  (cond-element (latex content) (else '())))

(define-syntax-rule (html-block content)
  (cond-block (html content) (else (nested))))
(define-syntax-rule (html-elem content)
  (cond-element (html content) (else '())))

(define (exact-chars . args) (apply elem #:style (make-style #f '(exact-chars)) args))
(define (pretitle content) (make-paragraph (make-style 'pretitle '()) content))
(define (tex . args) (tex-elem (apply exact-chars args)))
(define (noindent) (tex "\\noindent"))

;; TODO: find how to insert raw html
;;(define (Html . args) (html-only (apply elem #:style (make-style #f '(exact-chars)) args)))
(define (hhr) (html-elem (elem #:style (make-style #f (list (make-alt-tag "hr"))))))

(define (xnote x . y)
  (list x
        (note #:number 'next
              (list y (html-elem (list (linebreak) (linebreak) (linebreak)))))
        ))

(define epigraph-style
  (make-style
   #f
   (list (make-tex-addition
          (bytes-append
           #"\n\\usepackage{epigraph}\n"
           #"\\setlength\\epigraphwidth{.70\\textwidth}\n"))
         (make-css-addition
          (bytes-append
           #".epigraph { "
           #"  width: 70%; max-width: 70%;"
           #"  font-size: 0.95em; "
           #"  margin-left: 30%; }\n"
           #".epigraph-attribution { margin-top: 1em; font-style: normal;"
           #"  font-size: 0.90em; color: #555; }\n"
           )))))

(define (epigraph-attribution attribution)
  (if attribution
    (elem #:style "epigraph-attribution"
          (smaller (list "— " attribution)))
    '()))

(define (epigraph #:attribution (attribution #f) #:- (- #f) . text)
  (nested
   #:style epigraph-style
   (tex-block
    (nested
     (exact-chars "\\epigraph{")
     text
     (exact-chars "}{")
     (or attribution - '())
     (exact-chars "}")
     (noindent)))
   (html-block
    (nested
     (nested
      #:style "epigraph"
      text
      (epigraph-attribution (or attribution -)))))))

;; scribble/report subsubsub*section does not work https://github.com/racket/scribble/issues/540
(define (Paragraph . x)
  (list (html-elem (list (bold x) "  "))
        (tex-elem (list (tex "\\paragraph{") x (tex "}")))))


(define book-abstract-style
  (make-style
   #f
   (list (make-css-addition
          (bytes-append
           #".book-abstract { "
           #"  margin: 2em auto 2em 2em; max-width: 90%;"
           #"  font-size: 0.92em; "
           #"  line-height: 1.2; }\n"
           )))))

(define (book-abstract . text)
  (nested
   #:style book-abstract-style
   (tex-block
    (nested
     (exact-chars
      (string-append "\n\\begin{center}\\begin{minipage}{0.90\\textwidth}"
                     "\\setlength{\\parindent}{15pt}\\noindent" ;; restore regular indentation
                     "\\small{}"))
     text
     (exact-chars "\\end{minipage}\\end{center}")))
   (html-block
    (nested
     (nested
      #:style "book-abstract"
      text)))))
