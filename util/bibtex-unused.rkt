#lang racket/base

;; Citation setup for scriblib/bibtex that can report uncited entries
;; in their original order in the .bib file.
;; This module does not modify Scribble and does not assume a bibliography path.

(require racket/list
         racket/port
         racket/string
         scriblib/autobib
         scriblib/bibtex)

(provide setup-bibtex
         bibtex-keys-in-file-order)

;; setup-bibtex : path-string?
;;             -> ~cite
;;                citet
;;                generate-bibliography
;;                unused-bibtex-keys
;;                report-unused-bibtex
;;
;; A typical invocation is:
;;
;;   (define-values (~cite
;;                   citet
;;                   generate-bibliography
;;                   unused-bibtex-keys
;;                   report-unused-bibtex)
;;     (setup-bibtex bibliography-path))
;;
;; Call report-unused-bibtex only after constructing the complete document:
;;
;;   @(generate-bibliography)
;;   @(report-unused-bibtex)
;;
;; It writes one unused citation key per line to current-error-port and
;; returns an empty Scribble element, so it can safely occur in a document.
(define (setup-bibtex bib-path)
  (define-cite autobib-cite autobib-citet generate-bibliography)
  (define-bibtex-cite* bib-path
    autobib-cite autobib-citet
    raw-~cite raw-citet)

  (define keys-in-order
    (bibtex-keys-in-file-order bib-path))
  (define used (make-hash))

  (define (record-keys! arguments)
    ;; define-bibtex-cite* splits each argument on whitespace.
    (for* ([argument (in-list arguments)]
           [key (in-list (string-split argument))])
      (hash-set! used (string-foldcase key) #t)))

  (define ((tracking-wrapper citer) . arguments)
    ;; Do not record a request that the underlying citer rejects.
    (begin0
      (apply citer arguments)
      (record-keys! arguments)))

  (define ~cite (tracking-wrapper raw-~cite))
  (define citet (tracking-wrapper raw-citet))

  (define (unused-bibtex-keys)
    (for/list ([key (in-list keys-in-order)]
               #:unless (hash-has-key? used (string-foldcase key)))
      key))

  (define (report-unused-bibtex [out (current-error-port)])
    (let ([ks (unused-bibtex-keys)])
      (when (pair? ks)
       (fprintf out "Unused bibliography entries:\n~a" (car ks))
       (for ([key (in-list (cdr ks))]) (fprintf out " ~a" key))
       (newline out)))
    ;; A value suitable for an at-expression in a Scribble document.
    null)

  (values ~cite
          citet
          generate-bibliography
          unused-bibtex-keys
          report-unused-bibtex))

;; Return citation keys with their original spelling and source order.
;;
;; This scanner reads only BibTeX entry headers.  It skips @string,
;; @preamble and @comment forms, ignores %-comments while looking for
;; headers, and skips complete entry bodies so that an @ in a field value
;; cannot be mistaken for a new entry.
(define (bibtex-keys-in-file-order bib-path)
  (define source
    (call-with-input-file bib-path port->string))
  (define length (string-length source))

  (define (at-end? position)
    (>= position length))

  (define (skip-line-comment position)
    (let loop ([position position])
      (cond
        [(at-end? position) position]
        [(char=? (string-ref source position) #\newline)
         (add1 position)]
        [else
         (loop (add1 position))])))

  (define (skip-space-and-comments position)
    (let loop ([position position])
      (cond
        [(at-end? position) position]
        [(char-whitespace? (string-ref source position))
         (loop (add1 position))]
        [(char=? (string-ref source position) #\%)
         (loop (skip-line-comment (add1 position)))]
        [else position])))

  (define (find-at position)
    (let loop ([position position])
      (cond
        [(at-end? position) #f]
        [(char=? (string-ref source position) #\@) position]
        [(char=? (string-ref source position) #\%)
         (loop (skip-line-comment (add1 position)))]
        [else
         (loop (add1 position))])))

  (define (identifier-end position)
    (let loop ([position position])
      (if (and (< position length)
               (or (char-alphabetic? (string-ref source position))
                   (char-numeric? (string-ref source position))
                   (memv (string-ref source position) '(#\- #\_))))
          (loop (add1 position))
          position)))

  (define (key-end position)
    (let loop ([position position])
      (cond
        [(at-end? position) #f]
        [(char=? (string-ref source position) #\,) position]
        [else (loop (add1 position))])))

  ;; Skip a whole {...} or (...) form. Braces shield parentheses in a
  ;; parenthesized entry, as required for values such as {A title (draft)}.
  (define (form-end opening-position opening)
    (define closing (if (char=? opening #\{) #\} #\)))
    (let loop ([position (add1 opening-position)]
               [depth 1]
               [brace-depth 0]
               [quoted? #f]
               [escaped? #f])
      (cond
        [(at-end? position) length]
        [else
         (define character (string-ref source position))
         (cond
           [escaped?
            (loop (add1 position) depth brace-depth quoted? #f)]
           [(char=? character #\\)
            (loop (add1 position) depth brace-depth quoted? #t)]
           [(char=? character #\")
            (loop (add1 position) depth brace-depth (not quoted?) #f)]
           [quoted?
            (loop (add1 position) depth brace-depth #t #f)]
           [(and (char=? opening #\() (char=? character #\{))
            (loop (add1 position) depth (add1 brace-depth) #f #f)]
           [(and (char=? opening #\()
                 (char=? character #\})
                 (positive? brace-depth))
            (loop (add1 position) depth (sub1 brace-depth) #f #f)]
           [(positive? brace-depth)
            (loop (add1 position) depth brace-depth #f #f)]
           [(char=? character opening)
            (loop (add1 position) (add1 depth) brace-depth #f #f)]
           [(char=? character closing)
            (if (= depth 1)
                (add1 position)
                (loop (add1 position) (sub1 depth) brace-depth #f #f))]
           [else
            (loop (add1 position) depth brace-depth #f #f)])])))

  (let loop ([position 0] [reversed-keys null])
    (define at-position (find-at position))
    (cond
      [(not at-position)
       (reverse reversed-keys)]
      [else
       (define type-start
         (skip-space-and-comments (add1 at-position)))
       (define type-end (identifier-end type-start))
       (define type
         (string-foldcase (substring source type-start type-end)))
       (define opening-position
         (skip-space-and-comments type-end))
       (cond
         [(or (at-end? opening-position)
              (not (memv (string-ref source opening-position)
                         '(#\{ #\())))
          (loop (add1 at-position) reversed-keys)]
         [else
          (define opening (string-ref source opening-position))
          (define next-position
            (form-end opening-position opening))
          (cond
            [(member type '("comment" "preamble" "string"))
             (loop next-position reversed-keys)]
            [else
             (define start
               (skip-space-and-comments (add1 opening-position)))
             (define end (key-end start))
             (if end
                 (let ([key (string-trim (substring source start end))])
                   (loop next-position
                         (if (string=? key "")
                             reversed-keys
                             (cons key reversed-keys))))
                 (loop next-position reversed-keys))])])])))
