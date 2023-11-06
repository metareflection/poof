#lang at-exp racket @; -*- Scheme -*-
#|
Prototypes: Object-Orientation, Functionally

Slides for presentation at NJPLS 2023 in Princeton, 2023-11-10

To compile it, use:
  racket njpls2023-slides.rkt > njpls2023-slides.html

To test interactively, try:
  racket -i -l scribble/reader -e "(use-at-readtable)" -l racket


This document is available under the bugroff license.
  http://www.oocities.org/soho/cafe/5947/bugroff.html
|#

(require scribble/html
         "util/util.rkt"
         "util/coop.rkt"
         "util/protodoc.rkt"
         "util/reveal.rkt")

(def doc
  (docfix
    $top-doc
    ($title "Prototypes: Object-Orientation, Functionally")
    (app
      $kv 'slide
      (list
       @div[class: 'logo]{
       @img[src: "resources/pic/mukn-name.svg"
            alt: "Mutual Knowledge Systems"
            width: "50%"
            valign: 'middle
            style: "
    vertical-align: middle;
    background-color: white;
    padding-left: .5em;
    padding-right: .5em;
    padding-top: .5em;
    padding-bottom: .5em;
"]}
       @; TODO: hide the menu behind a button at the top when on mobile (?)
       @div[class: 'title
            style: "color: #55f; vertical-align: middle; text-align: center; font-size: 150%"
            @b{Prototypes: @(br) Object-Orientation, @(br) Functionally}]
       @(br clear: 'all)
       @p{@small{@(~)}}
       @L{@code{@(~ 18) fix = (λ (p t) (Y (λ (s) (p s t))))} @(br)
          @code{@(~ 18) mix = (λ (p q) (λ (s t) (p s (q s t))))}}
       @p{@small{@(~)}}
       @C[style: "font-size: 66%"]{
           François-René Rideau @(email "<fare@mukn.com>") @(br)
           Alex Knauth @(email "<alexander@knauth.org>") @(br)
           Nada Amin @(email "<namin@seas.harvard.edu>")}
       @C{@small{@Url{http://github.com/metareflection/poof}}}
       @div[style: "font-size: 50%;" (~)]
       @C{@small{NJPLS 2023-11-10}}
       @div[style: "font-size: 50%;" (~)]
       @table[style: "text-align: left; padding-left: 0; margin-left: 0; width: 100%; font-size: 50%;"
         (tr @td{@code{PgDn}: next} @td{@code{PgUp}: previous} @td{@code{↑ ↓ ← → ESC ⏎}
             @td{Touchscreen: swipe down until you must swipe right}})]))))

#|
(slide-group*
(x-slide () @h1{Paper: OO System@em{s} in λ-calculus}
  @Li{@em{Simple} Constructive Semantics of OO in pure FP}
  @Li{Demystify fundamental concepts of OO}
  @Li{Prototypes before Classes, Purity before Mutation}
  @Li{@em{Why} it matters, @em{Why} it's that way}
 @comment{
(Thesis)
})

(x-slide () @h1{Language Wars: OO vs FP}
  @Li{Two sides. Gabriel: “Incommensurable paradigms”}
  @Li{Insults, misunderstanding}
  @Li{Videos, Books, University Courses... all wrong.}
  @Li{And yet, in practice: mutual adoption!})

(x-slide () @h1{OO for FP}
  @Li{Academics tend to understand FP better}
  @Li{That's my tribe — probably yours}
  @Li{Yet in my experience OO matters}
  @Li{And yet, in practice: mutual adoption!})

(x-slide () @h1{What OO isn't about}
  @Li{Classes}
  @Li{“Encapsulation”, “Information Hiding”}
  @Li{Inheritance vs Composition vs ...}
  @Li{Mutation everywhere})
)
|#

(reveal-doc doc)
