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

(def ($top-doc self super) top-doc)

;; : Doc -> Doc
(def (doc-top doc)
 (def parent (doc'parent))
  (if parent (doc-top parent) doc))

;; : Doc -> (List Section)
(def (doc-subdocs doc)
  (if doc
    (for/list ((i (in-range (doc'n-subdocs))))
      (doc 'subdocs i))
    '()))

;; : Doc -> (List Section)
(def (doc-contents doc)
  ;;(eprintf "doc-contents ~s ~s ~a ~a\n" (doc'path) (doc'title) (doc'n-subdocs) (and (doc'slide) #t))
  (def slide (doc'slide))
  (def subdocs-contents (map doc-contents (doc-subdocs doc)))
  (if slide
    (cons (section slide) subdocs-contents)
    (section subdocs-contents)))

;; : String -> $Proto Doc
(def $title ($kv 'title))

(def (parent-title doc)
  (def parent (doc'parent))
  (and parent (parent'title)))

(def (parent-title-para doc)
  (def title (parent-title doc))
  (when/list title
    (p align: 'right valign: 'top (font size: 4 (b title)))))

(define ($subdoc . protos)
  (λ (self super)
    (def i (super'n-subdocs))
    ;;(eprintf "$subdoc ~s ~s ~a ~a ~s\n" (super'path) (super'title) (super'n-subdocs) i 0)
    (def (doc _)
      (apply docfix
        ($record
          parent self
          path (cons i (super'path)))
        protos))
    ;;(eprintf "$subdoc2 ~s ~s\n" (doc'path) (doc'title))
    (record n-subdocs (+ i 1)
            subdocs (rcons* i doc (super'subdocs))
            super)))

(define ($section title . protos)
  (apply $subdoc ($title title) protos))

(def (plan-slide self super)
  (def plan-title (or (parent-title super) "Plan"))
  (def current-section (car (super'path)))
  (def subdocs (doc-subdocs (self'parent)))
  (def len (length subdocs))
  (def titles (map (getField 'title) subdocs))
  ;;(eprintf "plan-slide ~s ~s ~s ~s\n" (super'path) (super'title) current-section len)
  (def hilit-titles
    (for/list ((t titles) (i (in-naturals)))
      (if (= i current-section) (li class: 'current-section (b t)) (li t))))
  (list*
   (parent-title-para super)
   (h1 plan-title)
   (ul hilit-titles)))

(def ($plan-slide self super)
  (@ ($subdoc
      ($record
       title "Plan"
       slide (plan-slide self super)))
     self super))

(define ($slide title . exprs)
  (λ (self super)
    ;;(eprintf "$slide ~a\n" title)
    (@ ($subdoc
        ($record
         title title
         slide (list*
                (parent-title-para super)
                (h1 title)
                (spacing* exprs))))
       self super)))

(define (docfix . l) (fix (apply rmix* l) top-doc))

(def (reveal-doc doc)
  (reveal (doc'title) (doc-contents doc)))
