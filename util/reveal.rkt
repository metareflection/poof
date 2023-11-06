#lang at-exp racket ;; -*- Scheme -*-

(provide
 slide-group slide gslide x-slide
 L R t C CB ~ ~~ L~ Li
 Url image fragment color th* td* tL tR tC simple-table
 *white* *gray* *blue* *light-blue* *red* *light-red* *green* *light-green*
 spacing* spacing
 comment email
 reveal reveal-url)

(require
 scribble/html
 net/url
 (for-syntax syntax/parse))

;; http://docs.racket-lang.org/scribble/extra-style.html

;; Reveal and new html stuff
(define/provide-elements/not-empty section video) ; more tags here

;; Register sections (but only at the top-level)
(define-values [get-sections register-section]
  (let ([sections '()])
    (values (λ () (reverse sections))
            (λ (section) (set! sections (cons section sections))))))
(define section-toplevel? (make-parameter #t))
(define-syntax-rule (slide (options ...) stuff ...)
  (do-slide (list options ...) (λ () (list stuff ...))))
(define (do-slide options thunk)
  (let ((toplevel? (section-toplevel?)))
    (parameterize ([section-toplevel? #f])
       (let ((section (apply section (append options (thunk)))))
         (if toplevel?
             (register-section section)
             section)))))
(define group-title (make-parameter #f))
(define-syntax-rule (slide-group title stuff ...)
  (do-slide-group title (λ () (list stuff ...))))
(define (do-slide-group title thunk)
  (slide ()
   #;(slide () @(h1 title))
   (parameterize ([group-title title])
     (thunk))))

(define (do-group-title)
  (when (group-title)
    (p align: 'right valign: 'top (font size: 4 (b (group-title))))))
(define-syntax-rule (gslide (options ...) stuff ...)
  (slide (options ...) (do-group-title) stuff ...))
(define-syntax-rule (when-not condition body ...)
  (when (not condition) body ...))

(define (resource-url . text)
  (cons "resources/" text))

(define (reveal-url . text)
  #; (cons "http://cdn.jsdelivr.net/reveal.js/3.0.0/" text)
  #; (cons "/home/fare/src/fare/reveal.js-master/" text)
  (resource-url "reveal/" text))

;; Quick helpers
(define-syntax-rule (defcodes lang ...)
  (begin (define (lang . text) (pre (code class: 'lang text)))
         ...))
(defcodes scheme javascript haskell)

(define (pic-url name url)
  (let ((file (string-append "resources/pic/" name)))
    (unless (file-exists? file)
      (define out (open-output-file file #:exists 'truncate))
      (call/input-url (string->url url)
                      get-pure-port
                      (λ (in) (copy-port in out)))
      (close-output-port out))
    file))

(define (L . x) (apply div align: 'left x))
(define (R . x) (apply div align: 'right x))
(define (t . x) x)
(define (C . x) (apply div align: 'center x))
(define (CB . x) (C (apply b x)))

(define (Url . x) (a href: x (tt x)))
;;(define (comment . x) '())
(define (email . x) (code class: 'email x))

(define (image name url . size)
  (img src: (pic-url name url) alt: name
       ;;height: (if (empty? size) "75%" size)
       class: "r-stretch"))

(define (fragment #:index (index 1) . body)
  (apply span class: 'fragment data-fragment-index: index body))

(define *white* "#ffffff")
(define *gray* "#7f7f7f")
(define *blue* "#0000ff")
(define *light-blue* "#b4b4ff")
(define *red* "#ff0000")
(define *light-red* "#ffb4b4")
(define *green* "#00ff00")
(define *light-green* "#b4ffb4")

(define (nbsp (repetitions 1)) (make-string repetitions #\u00A0))
(define (~ (repetitions 1)) (make-string repetitions #\u202F)) ; U+202F NARROW NO-BREAK SPACE > <
(define (~~) (~ 10))
(define (L~ . x) (apply L (~~) x))
(define (Li . x) (L (apply li x)))

(define (spacing* l (space (br)))
  (cond
    ((null? l) (list space))
    ((pair? l) (append (list space)
                       (if (pair? (car l)) (car l) (list (car l)))
                       (spacing* (cdr l))))
    (else (error 'spacing*))))

(define (spacing l)
  (if (list? l)
      (cdr (spacing* (filter-not null? l)))
      l))

(define (color text #:fg (fgcolor #f) #:bg (bgcolor #f))
  (if (or fgcolor bgcolor)
      (span style: (list (if fgcolor (list "color:" fgcolor ";") '())
                     (if bgcolor (list "background-color:" bgcolor ";") '()))
            text)
      text))

(define (gray . text) (color text #:fg *gray*))

(define (bg-slide text fgcolor bgcolor)
  (λ x
    (gslide (data-background: bgcolor)
     (spacing x)
     (div align: 'right valign: 'bottom (color #:fg fgcolor text)))))

(define-syntax-rule (x-slide (options ...) x ...)
  (gslide (options ...) (spacing (list x ...))))

;;(define th-width "4%")
;;(define td-width "48%")
;;(define table-width "114%")
(define th-width "8%")
(define td-width "46%")
(define table-width "104%")

(define (th* name)
  (if name (th width: th-width (font size: "6" (color name #:fg *white*))) (td)))

(define (td* x) (td (color x #:fg *white*)))

(define (row name left right
             #:left-bg (left-bg #f) #:right-bg (right-bg #f) #:fragment? (fragment? #f))
  (tr
   (th* name)
   (td
    width: td-width (when left-bg bgcolor:) (when left-bg left-bg)
    (spacing left))
   (if right
       (td
        width: td-width bgcolor: right-bg
        (when fragment? class:) (when fragment? 'fragment)
        (when fragment? data-fragment-index:) (when fragment? 1)
        (spacing right))
       (td width: td-width))))

(define (tC (x '())) (td style: "text-align: center; border: none;" x))
(define (tL (x '())) (td style: "text-align: left; border: none;" x))
(define (tR (x '())) (td style: "text-align: right; border: none;" x))

(define (simple-table contents)
  (letrec ((line (lambda (th . tds) (cons (th* th) (map td* tds))))
           (lines (lambda (titles . xss)
                    (apply table align: 'right width: "100%"
                       (tr (map th* titles))
                       (map (lambda (xs) (apply tr (apply line xs))) xss)))))
    (apply lines contents)))

(define (reveal Title Sections)
  (output-xml
   @html{
     @head{
       @meta[http-equiv: "Content-Type" content: "text/html; charset=utf-8"]
       @meta[name: 'viewport content: "width=device-width, initial-scale=1"]
       @link[rel: 'icon href: "resources/pic/mukn-icon.svg" type: "image/svg+xml"]
       @link[rel: 'stylesheet href: @resource-url{my.css}]
       @link[rel: 'stylesheet href: @reveal-url{css/reveal.css}]
       @link[rel: 'stylesheet href: @reveal-url{css/theme/black.css}]
       @link[rel: 'stylesheet href: @reveal-url{lib/css/zenburn.css}]
       @link[rel: 'stylesheet href: @resource-url{my.css}]
       @title[Title]
     }
     @body[style: (string-append "background-repeat: no-repeat; "
                                 "background-size: 3%; "
                                 "background-origin: padding-box; "
                                 "background-position: bottom 1% right 1%; "
                                 "background-image: url('resources/pic/mukn-icon-whitebg.png'); ")]{
       @div[class: 'reveal]{@div[class: 'slides Sections]}
       @script[src: @reveal-url{lib/js/head.min.js}]
       @script[src: @reveal-url{js/reveal.min.js}]
       @;@script[src: @reveal-url{reveal.js}]
       @script/inline{
         Reveal.initialize({
           dependencies: [
             {src: "@reveal-url{plugin/highlight/highlight.js}",
              async: true,
              controls: true,
              controlsLayout: 'bottom-right',
              callback: () => hljs.initHighlightingOnLoad()}],
           controls: false})}}}))
