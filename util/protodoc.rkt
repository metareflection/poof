#lang racket ;; -*- Scheme -*-

(provide (all-defined-out)) ;; export everything for now... will refine later

(require
  scribble/html
  "reveal.rkt"
  "util.rkt"
  "coop.rkt")

#; ;; Document
(define-type Doc
  (Record
    title: String ;; title of this section (NB: not Xexpr)
    parent: (OrFalse Doc) ;; parent section
    path: (List Nat) ;; path from top document to current section
    slide: Xexpr ;; top slide (thunked)
    n-subdocs: Nat ;; number of subdocs in this document
    subdocs-r: (Fun Nat -> Doc))) ;; subdocs of this document

;; Empty / Top value of type doc
;; : Doc
(def top-doc
  (record
    title #f
    parent #f
    path '()
    slide #f
    n-subdocs 0
    subdocs (λ (i) top-doc)))

;; : Doc -> Doc
(def (doc-top doc)
 (def parent (doc'parent))
  (if parent (doc-top parent) doc))

;; : Doc -> (List Section)
(def (doc-subdocs doc)
  (when/list doc
    (for/list ((i (in-range (doc'n-subdocs))))
      (doc 'subdocs i))))

;; : Doc -> (List Section)
(def (doc-contents doc)
  (def slide (doc'slide))
  (def subdocs-contents (map doc-contents (doc-subdocs doc)))
  (if slide
    (cons (section slide) subdocs-contents)
    (section subdocs-contents)))

;; : String -> $Proto Doc
(def $title ($kv 'title))

;; : Doc -> Xexpr
(def (title-para doc)
  (def title (doc'title))
  (when/list title
    (p align: 'right valign: 'top (font size: 4 (b title)))))

;; : (Proto Doc Doc) ... -> Proto Doc Doc
(define ($subdoc . protos)
  (λ (self super)
    (def i (super'n-subdocs))
    (def (doc _)
      (apply docfix
        ($record
          parent self
          path (cons i (super'path)))
        protos))
    (record n-subdocs (+ i 1)
            subdocs (rcons* i doc (super'subdocs))
            super)))

;; : Xexpr (Proto Doc Doc)... -> Proto Doc Doc
(define ($section title . protos)
  (apply $subdoc ($title title) protos))

;; : Doc Doc -> Xexpr
(def (plan-slide self super)
  (def plan-title "Plan of the Talk") ; (or (super'parent'title) "Plan")
  (def current-section (car (super'path)))
  (def subdocs (doc-subdocs (self'parent)))
  (def len (length subdocs))
  (def titles (map (getField 'title) subdocs))
  (def hilit-titles
    (for/list ((t titles) (i (in-naturals)))
      (if (= i current-section) (li class: 'current-section (b t)) (li t))))
  (def items (if (<= len 5)
               (spacing* hilit-titles)
               hilit-titles))
  (list
   (title-para (super'parent))
   (h1 plan-title)
   (ul items)))

;; : Proto Doc Doc
(def ($plan-slide self super)
  (@ ($subdoc
      ($record
       title "Plan"
       slide (plan-slide self super)))
     self super))

;; : Xexpr Xexpr ... -> Proto Doc Doc
(define ($xslide title . exprs)
  (λ (self super)
    (@ ($subdoc
        ($record
         title title
         slide (list*
                (title-para super)
                (h1 title)
                exprs)))
       self super)))

;; : Xexpr Xexpr ... -> Proto Doc Doc
(define ($slide title . exprs)
  (apply $xslide title
         (if (<= (length exprs) 5)
             (spacing* exprs)
             exprs)))

;; : (Proto Doc Doc) ... -> Doc
(define (docfix . l)
  (fix (apply rmix* l) top-doc))

;; : Doc -> Unit
(def (reveal-doc doc)
  (reveal (doc'title) (doc-contents doc)))
