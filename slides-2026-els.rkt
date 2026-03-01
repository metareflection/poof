#lang at-exp racket @; -*- Scheme -*-
#|
Lambda: the Ultimate Paradigm

Slides for presentation at the European Lisp Symposium, 2026-05-11

To compile it, use:
  racket slides-2025-lambdaconf.rkt > build/slides-2025-lambdaconf.html

To test interactively, try:
  racket -i -l scribble/reader -e "(use-at-readtable)" -l racket


This document is available under the bugroff license.
  http://www.oocities.org/soho/cafe/5947/bugroff.html
|#

(require scribble/html
         "util/util.rkt"
         "util/coop.rkt"
         (rename-in "util/coop.rkt" (|@| $))
         "util/protodoc.rkt"
         "util/reveal.rkt")

(def (make-table lists)
  (table style: "border-bottom-width:0;"
   (map (lambda (l)
          (tr style: "border-bottom-width:0;"
              (map (lambda (x) (td style: "border-bottom-width:0;" x)) l)))
        lists)))

(def doc
  (docfix
    ($title "Lambda, the Ultimate Paradigm")
    ($ $kv 'slide
      (list
       @div[class: 'logo]{
       @img[src: "resources/pic/mukn-icon-whitebg.png"
            alt: "MuKn.com"
            width: "35%"
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
            @b{Lambda, the Ultimate Paradigm}]
       @(br clear: 'all)
       @p{@small{@(~)}}
       @C[style: "font-size: 66%"]{
           François-René Rideau @(email "<fare@mukn.com>")}
       @C{@small{@Url{http://github.com/metareflection/poof}}}
       @div[style: "font-size: 50%;" (~)]
       @C{@small{LambdaConf 2025-05-13}}
       @div[style: "font-size: 50%;" (~)]
       @table[style: "text-align: left; padding-left: 0; margin-left: 0; width: 100%; font-size: 50%;"
         (tr @td{@code{PgDn}: next} @td{@code{PgUp}: previous} @td{@code{↑ ↓ ← → ESC ⏎}
             @td{Touchscreen: swipe down until you must swipe right}})]))
    ($section "Introduction: The Lambda Nature"
     $plan-slide
     ($slide "The Great Lisp Revolution"
        @L{Symbolic Processing / “Artificial Intelligence”}
        @L{Functional Programming}
        @L{Garbage Collection}
        @L{Interactive Programming}
        @L{Knowledge-based / Rule-based / Declarative Programming}
        @L{Concurrency- / Process- / Actor- oriented Programming}
        @L{Object-Oriented Programming (KRL, Actors, Flavors, T, CLOS)}
        @L{Graphical User Interfaces}
        @L{Hypertext}
        @L{Massively Parallel Programming}) @; Thinking Machines’s Connection Machine
     ($slide "The Talk I’ll Be Giving Instead"
        @L{Why care about OO}
        @L{Why care about (Multiple) Inheritance}
        @L{Some historical context}
        @L{The Best Inheritance Algorithm}))

   ($section "Conclusion: Paradigm War and Peace"
     $plan-slide
     ($slide "Why did I care about Prototype OO?"
        @Li{Great Usenet Flamewars of OO vs FP}
        @Li{Dysfunctional Programming vs @br
            @(~ 18) Objectionable Disorientation}
        @Li{Talking Past Each Other… for Decades}
        @Li{1970s Lisp had both (2000s Scala: with types)}
        @Li{The paper I wished I could have read younger})
     ($slide "Why So Much Blindness?"
        @Li{Industry doesn’t care enough about Correctness}
        @Li{Academia doesn’t understand Programming In The Large}
        @Li{Both like to ignore the Human Factor}
        @Li{All get stuck in their paradigms}
        @Li{Opposition in mindset, not in logic})
     ($slide "Two Mindsets"
        @Li{Compositionality for reducible problems}
        @Li{Incrementality for irreducible problems}
        @Li{Brighter than ambitious vs more ambitious than bright}
        @Li{History: OO invented to model complex SIMULAtions}
        @Li{History: FP to formalize computations down their simplest})
     ($xslide "Inhabiting Constraints — But Which?"
        @C{Every task involves constraint,}
        @C{Solve the thing without complaint@";"}
        @C{There are magic links and chains}
        @C{Forged to loose our rigid brains.}
        @C{Structures, strictures, though they bind,}
        @C{Strangely liberate the mind.}
        @R{— James Falen, as quoted by dughof})
     ($slide "Meta-Thesis"
        @Li{Humility, not fanaticism}
        @Li{Incommensurable paradigms? Go wider!}
        @Li{Simplicity matters — so does Complexity}
        @Li{Paradigm: “Systems” vs “Programming Language”}
        @Li{Scheme: λ’s for Semantics, macros for Syntax})
     ($slide "Thank You!"
        @Li{Theory: @(~ 8) @Url{https://github.com/metareflection/poof}}
        @Li{Practice: @(~ 5) Gerbil Scheme @Url{https://cons.io}}
        @Li{X: @Url{https://x.com/ngnghm}  Blog: @Url{https://ngnghm.github.io}}
        @Li{Hire me — or be hired by me! @(~ 5) @code{<fare@"@"mukn.com>}}
        @Li{Plenty more research ideas, code and papers to write…}))))

(reveal-doc doc)


1950s:
Symbolic Processing, “Artificial Intelligence”, Conditional Expressions,
Functional Programming, Recursion, Dynamic Typing, Garbage Collection,
Interactive Programming, Code as Data.

1960s:
Metacircular interpreters, bootstrapped compilers, macros, REPLs, Portable Programming,
Dynamic Scoping, Error handling, Symbolic Differentiation, Knowledge Representation,
Dynamic Code Extension.

1970s:
Rule-based / Declarative Programming, Concurrency- / Process- / Actor- oriented Programming,
timesharing, non-local exits, first-class continuations, backtracking, lexical scoping,
proper tail-calls, interactive debugging, object-oriented programming, graphical user interfaces,
tagged microarchitectures, static typesystems.

1980s:
Commercial Lisp Workstations, network programming, massively parallel programming,
hypertext, CAD, movie CGI, symbolic algebra, expert systems, OO: CLOS,
dynamic code upgrade, orthogonal persistence, macro hygiene, resumable exceptions,
delimited control, Emacs, theorem provers,


1990s:
ANSI CL standard, OO: AMOP & C3,
partial evaluation,


2000s
Language-oriented programming (Racket),
module systems with proper phase separation for metalevels (Racket),
mixing static and dynamic types (Racket),


lazy evaluation (Friedman & Wise 1976)
partial evaluation
fexprs and first-class environments

lots of automated theorem proving work, like LCF, CAML, NuPRL, and Nqthm

Vau calculus, proper separation of metalevels / phases for procedural macros (Racket)


Symmathesy

