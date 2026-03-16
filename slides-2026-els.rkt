#lang at-exp racket @; -*- Scheme -*-
#|
Lambda, the Ultimate Paradigm

Slides for presentation at the European Lisp Symposium, 2026-05-11

To compile it, use:
  racket slides-2026-els.rkt > build/slides-2026-els.html

To test interactively, try:
  racket -i -l scribble/reader -e "(use-at-readtable)" -l racket


This document is available under the bugroff license.
  http://www.oocities.org/soho/cafe/5947/bugroff.html

Abstract:

From its discovery in 1958, Lisp has been at the forefront of innovation in topics as diverse as
Artificial Intelligence, Memory Management, Object-Oriented Programming,
Control Structures, Human Computer Interaction—and much more.
Then, in the 1990s, progress largely stopped in Lisp, to happen in other ecosystems.
Some Lisp technology was abandoned and forgotten;
and while there remains a niche community of Lisp hackers who keep producing wonderful innovation,
it is largely not at the forefront of technological progress anymore.
What happened? What made and makes Lisp such a good platform for creation?
What advantages does Lisp still have? How can they be further amplified?
What advantages did it lose? How can the effect be reduced or reversed?
And what does the recent advent of Artificial Intelligence that can write software
mean for the future of Lisp?
Back in the day, Lispers used the slogan “Lambda the Ultimate <something>” to
boast about how Lisp could get to the very essence of so many issues that others barely understood.
While some Functional Programmers have tried to claim the “Lambda” slogan for themselves,
I’ll argue why indeed Lisp has the “Lambda Nature” in ways that
no other programming language does—precisely because Lisp is more than a programming language.

Bio:
Once maintainer and rewriter of Common Lisp’s build system ASDF, and current co-maintainer of Gerbil Scheme, François-René Rideau also recently wrote the book “Lambda the Ultimate Object: Object-Orientation Elucidated”. While active as a developer in the software industry, he has always kept a foot in research, and has written on topics including Free Software, Reflection, Semantics, Object Orientation, Programming Language Design, Security, Interactive Protocols, Software Evolution, and Orthogonal Persistence.
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

Lisp is more than a programming language: it is a computing system.

Lisp rejects the “command and control” paradigm of an omniscient programmer above the machine slavishly doing his bidding, and embraces the “interaction” paradigm of humans and machines cooperating with each other based on comparative advantage.

See Gabriel’s 2012 “The Structure of a Programming Language Revolution” for how people completely misconstrue Lisp or OO based on trying to make sense of them with the blinders of the extremely poor paradigm of “programming language”.


Patterns mean “I have run out of language.” — Rich Hickey

A design pattern is when programmers become manual compilers for some feature not supported by the language, and must follow a recipe by hand, ensure its consistency, and maintain that consistency manually as the program inevitably evolves.

“I object to doing things that computers can do.” — Olin Shivers

A good language has only one single design pattern: use, extend and evolve the language and its libraries.

Any language without macros is a bad language, because you can't extend it, and thus need design patterns beyond the One Design Pattern.


Macros 1963
The original macro paper... has bugs! https://dspace.mit.edu/handle/1721.1/6111
A transcription and bug report:
https://github.com/acarrico/ai-memo/blob/master/AIM-057.txt

The "programming language" paradigm demands no developer interaction after the program starts—of course it cannot deal with evolution, by definition.

The "system" paradigm supports evolution after the program starts—of course it allows developer interaction after program start.

Once in a while, I complain about developers so intelligent they deal with a lot of software complexity, yet not so intelligent that they know to ruthlessly simplify that complexity away. And not just the infamous three-star or three-comma programmers.
LLMs only amplify that.

Homoiconic vs bicameral

If factories were managed according to the Programming Language paradigm, each time you had to adjust a tolerance on a component, you'd nuke the factory from orbit, killing all the workers and destroying the inventory, then start a new factory from scratch with brand new hires.
