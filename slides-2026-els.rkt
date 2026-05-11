#lang at-exp racket @; -*- Scheme -*-
#|
Lambda, the Ultimate Paradigm

Slides for presentation at the European Lisp Symposium, 2026-05-11
  https://european-lisp-symposium.org/2026/

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

(define (pic file width alt . opts)
  (apply img
         src: (string-append "resources/pic/els2026/" file)
         alt: alt
         width: width
         valign: 'middle
         style: "
    vertical-align: middle;
    padding-left: 0em;
    padding-right: 0em;
    padding-top: 0em;
    padding-bottom: 0em;
"
         opts))

(def doc
  (docfix
    ($title "Lambda, the Ultimate Paradigm")
#|    ($ $kv 'headers
      (list
       @link[rel: "stylesheet" href: "https://try.gambitscheme.org/codemirror/lib/codemirror.css"]
       @link[rel: "stylesheet" href: "https://try.gambitscheme.org/UI.css"]
       @meta[name: "viewport" content: "width=device-width, initial-scale=1.0"]
       @script[src: "https://try.gambitscheme.org/browserfs.min.js"]
       @script[src: "https://try.gambitscheme.org/codemirror/lib/codemirror.js"]
       @script[src: "https://try.gambitscheme.org/codemirror/mode/scheme/scheme.js"]
       @script[src: "https://try.gambitscheme.org/codemirror/addon/edit/matchbrackets.js"]
       @script[src: "https://try.gambitscheme.org/codemirror/keymap/emacs.js"]
       @script[src: "https://try.gambitscheme.org/UI.js"]
       @script[src: "https://try.gambitscheme.org/VM.min.js"]
       @script[src: "https://code.jquery.com/jquery-3.6.0.min.js" integrity: "sha256-/xUj+3OJU5yExlq6GSYGSHk7tPXikynS7ogEvDej/m4=" crossorigin: "anonymous"]))
    ($ $kv 'onload "main_vm.init('#ui');")|#
    ($ $kv 'slide
      (list
       ;; https://www.computerhistory.org/pdp-1/spacewar/
       @div[class: 'logo
       @pic["Dan Edwards, Peter Samson, Spacewar! on PDP-1.avif" "200%"
            "Dan Edwards and Peter Samson playing Spacewar! on a PDP-1"]]
       @div[class: 'title
            style: "color: #55f; vertical-align: middle; text-align: center; font-size: 100%"
            @b{Lambda, the Ultimate Paradigm}]
       @p{@small{@(~)}}
       @C[style: "font-size: 66%"]{
           François-René Rideau @(email "<fare@mukn.com>")}
       @C{@small{@Url{https://github.com/metareflection/poof}}}
       @div[style: "font-size: 50%;" (~)]
       @C{@small{European Lisp Symposium 2026-05-11}}
       @div[style: "font-size: 50%;" (~)]
       @table[style: "text-align: left; padding-left: 0; margin-left: 0; width: 100%; font-size: 50%;"
         (tr @td{@code{PgDn}: next} @td{@code{PgUp}: previous} @td{@code{↑ ↓ ← → ESC ⏎}
             @td{Touchscreen: swipe down until you must swipe right}})]))
    ($section "Introduction: The Lambda Nature"
     $plan-slide
     ($slide "Lambdist Koans (1)"
       ;; https://www.chessprogramming.org/images/0/07/2-4.Greenblatt-Richard_Knight.1978.L02645385.MIT.lg.jpg
       @pic["Richard Greenblatt, Tom Knight, Lisp Machine 1978.jpg" "36%"
            "Greenblatt and Tom Knight before the Lisp Machine"]
       @comment{
  A student, in hopes of understanding the Lambda-nature, came to Greenblatt.
  As they spoke a Multics system hacker walked by.
  “Is it true”, asked the student,
  “that PL/I has many of the same data types as Lisp?”
  Almost before the student had finished his question,
  Greenblatt shouted, “FOO!”, and hit the student with a stick.
})
     ($slide "Lambdist Koans (2)"
       ;; https://www.historyofinformation.com/image.php?id=8490
       @pic["Connection Machine.png" "42%"
            "The Connection Machine"]
       @comment{
Years before he built Connection Machines,
Danny Hillis composed this and other “Koans”.
Instead of Buddhist Zen Masters and Disciples,
they featured Lisp hackers from the MIT AI Lab.
He launched a genre:
dispensing wisdom about modern computing,
in forms inherited from the ancients.
})
     ($slide "Lambdist Koans (3)"
       ;; https://www.historyofinformation.com/image.php?id=8490
       @pic["Lambda the Ultimate Imperative.png" "90%"
            "LAMBDA: the Ultimate Imperative"]
       @comment{
Around the same time, Sussman and Steele wrote their famous series of papers:
“Lambda the Ultimate Imperative”,
“Lambda the Ultimate Declarative”,
“Lambda the Ultimate GOTO”...
They launched another genre:
showing how to reduce programming paradigms down to Lambdas.
})
     ($slide "The Question"
       ;; https://billykaplan.io/blog/lambda-the-ultimate/
       @pic["SICP.webp" "75%"
            "SICP"]
       @comment{
So what is this LAMBDA that imbues some software with its blessed Lambda Nature?
What is this LAMBDA that gives LISP its Ultimate Power?
Is it… Functional Programming?

Nope.
These days, not just ML or Haskell support Functional Programming.
Python and JavaScript, and even C++ and Java,
now possess first class anonymous functions.
Do *they* possess the Lambda Nature?
And if the Lambda Nature is about Functional Programming,
how can LISP itself have claimed to possess it,
when it didn't actually have TRUE LAMBDAs until 1975?
})
     ($slide "My Answer (1)"
       ;; https://makezine.com/article/craft/an-actual-turing-machine/
       @pic["Turing Machine.webp" "80%"
            "Turing Machine"]
       @comment{
Sure, you can reduce all computations to LAMBDAs.
That's remarkable. And LAMBDAs are elegant, and logically simple.
But you could as well reduce all computations to... Turing Machines.
(Though they were designed to be boring rather than elegant.)
Machine code would work too, and would be more useful;
but machine code is specific to one kind of machine;
and machines gets old quick.
All that means is that LAMBDAs, Turing Machines, Machine Code, are universal.

Now having a universal subset is a pretty basic requirement
for a general purpose programming language.
Surely, that's not what makes Lisp special!?
})
     ($slide "My Answer (2)"
      ;; https://i.pinimg.com/236x/56/d6/d8/56d6d8d7ede94abea35b410a026d3832--alan-watts-to-the-moon.jpg
      @pic["Pointing to the Moon.jpg" "30%"
           "Pointing to the Moon"]
       @comment{
“When a wise man points at the Moon, the fool looks at the finger”.
But when any fool can point at the Moon, the wise man marvels at the finger.
The LAMBDAs are the Moon, which is beautiful.
But the magic was in the pointing all along:
in the ability to translate semantics into those elegant LAMBDAs,
rather than in the elegant but ubiquitous LAMBDAs themselves.
}))
   ;; The Tao that can be told is not the true Tao - Laozi
   ($section "Does Lisp have the Lambda Nature?"
     $plan-slide
     ($slide "So, Was Lisp Great?"
       @comment{
Well, we can look at achievements by Lispers.
There are too many, so I can only single out a few.
})
     ($slide "Was Lisp Great? (1950s)"
      @L{@b{“Artificial Intelligence”}, @br
         @b{Conditional Expressions}, @br
         @b{Metacircular Interpreters}, @br
         @b{Recursion}, @br
         Functional Programming, @br
         List Processing and Symbolic Processing, @br
         Dynamic Typing, @br
         Interactive Programming, @br
         Code as Data...
})
     ($slide "Was Lisp Great? (1960s)"
      @L{@b{Automatic Storage Management} (Garbage Collection), @br
         @b{Macros}, @br
         @b{REPLs} (then READ-EVAL-PRINT cycle), @br
         @b{Error handling}, @br
         Bootstrapped Compilers,
         Portable Programming, @br
         Dynamic Scoping and explicit environment capture, @br
         Symbolic Differentiation,
         Knowledge Representation, @br
         Dynamic Code Extension,
         Logo, @br
         Structure Editing,
         Time-sharing...
})
     ($slide "Was Lisp Great? (1970s)"
      @L{
@b{Object-Orientation}, Prototype, Method Combination, @br
@b{First-Class Continuation}, Backtracking, Non-local Exit, @br
@b{Lexical scoping}, proper tail-call, @br
@b{Extensible Editors}, GUIs, Programmable Syntax, @br
Interactive Source-Level Debugging, @br
Rule-based Programming, Expert Systems, @br
Pattern-Matching, Constraint Propagation, @br
Concurrency- / Process- / Actor- oriented Programming, @br
Tagged Microarchitectures (Lisp Machines), @br
Lazy Evaluation, Typesystems, Theorem Provers... @br
})
     ($slide "Was Lisp Great? (1980s)"
      @L{
@b{CLOS}—the greatest Object System ever designed—, @br
@b{Massively Parallel Programming}, @br
@b{Macro Hygiene}, @br
Commercial Lisp Machines,
Network Programming, @br
Hypertext, CAD, Movie CGI, Symbolic Algebra, @br
Incremental Compilation, Dynamic Code Upgrade,@br
Resumable Exceptions, Delimited Control, @br
Genetic Programming, Orthogonal Persistence...
})
     ($slide "Was Lisp Great? (1990s)"
      @L{
@b{Meta-Object Protocols}, @br
@b{Web programming}—especially so with continuations—, @br
ANSI CL standard, @br
C3 Linearization (Dylan), @br
Partial Evaluation (Scheme)...
})
     ($slide "Was Lisp Great? (2000s)"
      @L{
@b{Module Systems with Phase Separation} (Racket), @br
Language-Oriented Programming (Racket), @br
Mixing Static Types and Dynamic Contracts (Racket), @br
Popularizing Persistent Data Structures (Clojure)...
})
     ($slide "Was Lisp Great? (2010s)"
      @L{
Solver-aided programming (Racket (Rosette)), @br
Gradual typing matured (Racket), @br
Higher-order software contracts (Racket)...
})
     ($slide "Was Lisp Great? (2020s)"
      @L{
Rhombus (Racket) — more infix syntax with macros @br
...
})
     ($slide "Now, wait a minute..."
       @comment{
First, we see a sharp decline in the 1990s and since.
If Lisp was once great, is it still?
Does Lisp still have the Lambda Nature?
Can the Lambda Nature wane as well as wax?
I’ll have to get back to that.
Then again...
})
     ($slide "COPIED !?"
       ;; https://ebiquity.umbc.edu/_file_directory_/papers/1470.pdf
       @pic["TLTM.png" "60%"
            "Newell & Simon paper on IPL"]
       @comment{
Did all these innovations really come from Lisp?
IPL had List Processing before LISP.
And was Bobrow first to publish about Object-Oriented Programming,
or did he just take ideas from Simula and Smalltalk?
How many of these are really Lisp innovations?
Aren’t they mostly copycats?

Maybe more importantly, look at LAMBDA itself!
})
     ($slide "The LAMBDA Story"
      ;; https://xach.livejournal.com/170311.html
      @pic["doing-it-wrong.jpg" "35%"
           "McCarthy 'Programming - You're Doing It Completely Wrong' Meme"]
      @comment{
McCarthy didn't invent LAMBDA at MIT in 1958.
Rather he *misimplemented* a first-order description by Kleene
of the 1930s calculus by Church;
and thus he inadvertently discovered *dynamic binding*.
If needed, Steve Russell’s FUNCTION macro would expensively
capture the entire environment in a “FUNARG device”.
(Same Steve Russell who wrote the first LISP implementation, and Spacewar!)

Landin's 1966 ISWIM was the first programming language
built around the actual λ-calculus;
first implemented in 1968 as PAL by Arthur Evans’s BCPL team also at MIT.

Lisp only had proper LAMBDAs, with *lexical binding*,
in 1975, when Sussman and Steele invented Scheme.
And Lisp only adopted lexical binding after Steele showed
how to compile it *efficiently* in 1978.
So, for its first 20 years, LISP didn't even have proper LAMBDAs.

And yet... these were the most productive years of LISP.
})
     ($slide "Adopt Any Paradigm"
      ;; https://a-z-animals.com/animals/chameleon/pictures/
      @pic["Wild-Chameleon-Reptile-With-Beautiful-Colors.jpg" "75%"
           "Wild Chameleon Reptile With Beautiful Colors"]
      @comment{
So maybe LAMBDA was copied, and badly, at first.
And so on for List Processing, Rule-based Programming, and Theorem Provers,
and any of those many paradigms that were developed in Lisp.
But that's the point.

Lisp has this long history of epic failures...
that turn into epic successes...
because the language can keep evolving...
from the inside, by regular users.

Non-Lispers each have to invent a programming language from scratch;
and they can only afford to implement one paradigm in it—also badly at first.
Afterwards, any language evolution is left in the hands of
a small class of anointed language implementors...
while regular users are left to watch from the sidelines.

Lisp could quickly absorb any programming paradigm that anyone invented, even badly;
Lisp could keep refining that paradigm until it wasn't bad anymore;
And Lisp could integrate it well with other paradigms
... all that faster than possible outside Lisp.
})
     ($slide "Lisp Makes Paradigms Friends"
      @L{Objects are a poor man’s closures. @br @C{— Norman Adams @~ @~ @~}}
      @L{Closures are a poor man’s objects. @br @C{— Christian Quéinnec}}
      @comment{
One can argue that Bobrow didn’t invent Object-Oriented Programming.
Yet he unarguably contributed much to OOP;
with CLOS, he brought it to heights
far beyond what was ever achieved outside Lisp...
with the help of many other Lispers...
often unsuspected nobodies that no one had asked to do it...
as with Howard Cannon’s Flavors.
There was more Object-Oriented experimentation in Lisp
than in all other languages combined (except for types).

Lisp had all the paradigms, then or now popular or unpopular,
deservedly or undeservedly:
functional, object-oriented, rule-based, actors,
declarative, imperative, logic, graphical, what have you.
It had all these paradigms long before any bigoted imbecile
invented oppositions between some of them
(such as Functional Programming vs Object-Oriented Programming).
The only opposition that actually exists between those paradigms is that,
outside Lisp, you must build each language from scratch;
and with resource constraints,
you can usually only afford one paradigm at most.

Lisp lifts those constraints.
It is not a static language built by the anointed;
it is an interactively evolving system into which
anyone can dynamically build more languages.
})
     ($oslide "The Serendipity of Parentheses (1)"
      (list data-transition: "slide-in none-out")
      @L{1958 LISP, MEXP, manually compiled:
         @br @code{foo=cons[bar;foo]}}
      @~
      @Lc{(SETQ FOO (CONS BAR FOO)) ; 1960 LISP I, SEXP, interpreted
}
      @~
      @Lc{MACRO (( ; 1963 LISP 1.5 @br
         (STASH (LAMBDA (FORM)
             (LIST (QUOTE SETQ)(CADAR FORM)
                   (LIST (CONS (CADR FORM) @br
         (CADAR FORM))) ))) @br
))}
      @comment{
What made Lisp great was this ability to program programming languages,
with a permanent common system underneath.
Lisp is not just “code as data” or “homoiconicity”,
but an **extensible** protocol for code as data.
Shriram Krishnamurthi calls that a “bicameral” syntax,
wherein you read syntax, so you can (extensibly) expand it,
before you actually parse it.
These features were not deliberately designed by McCarthy,
but serendipitously discovered over the years.

The parenthetical syntax of symbolic expressions, or S-expressions, or SEXP,
was initially meant only for data.
Code was to use meta expressions, or M-expressions, or MEXP,
a more traditional infix syntax that was never unambiguously formalized.
But McCarthy wrote a metacircular interpreter for code represented as data;
it was a curiosity a first, for a Turing-style theorem;
but it was turned into a practical implementation by Steve Russell in 1959;
and it used SEXP as input.

Here is some pattern of assignment code that comes often, to add an item to a list.
There isn't even a MEXP for it, because the MEXP language has no assignment, no side-effect.

To automate away this pattern, Tim Hart invents macros in 1963.
And here is the very first macro STASH, in all its glory, as first published, with all its bugs.
LISP starting by transforming SEXP with an outside metaprogram.
Now there was a protocol to extend the SEXP transformation from within LISP.

No indentation, that's before text editors.
Also notice that toplevel forms are special in Lisp 1.5;
they are processed by EVALQUOTE:
it is like a batch processor, with a command name followed by argument constants...
except the arguments can have a recursive list structure,
setting them decades ahead of mainframe and Unix shells still.
})
     ($oslide "The Serendipity of Parentheses (2)"
      (list data-transition: "none")
      @Lc{(DEFPROP PUSH ; 1966 PDP-6 LISP @br
          @~ (LAMBDA (FORM) @br
          @(~ 3) (LIST (QUOTE SETQ) (CADDR FORM) @br
          @(~ 5)       (LIST (QUOTE CONS) @br
          @(~ 11)            (CADR FORM) (CADDR FORM)))) @br
          @~ MACRO)}
      @comment{
L. Peter Deutsch wrote the first interactive interpreter, PDP-1 LISP, in 1964.
Now users thought primarily in SEXP.

In 1966, Peter Samson's PDP-6 LISP replaced EVALQUOTE by EVAL,
so the toplevel isn't special anymore.
You define functions and macros with an ordinary form.
After that, the pretense of MEXP was wholly abandoned.
})
     ($oslide "The Serendipity of Parentheses (3)"
      (list data-transition: "none")
      @Lc{(DEFUN PUSH MACRO (FORM) ; 1969 MACLISP @br
          @~ (LIST 'SETQ (CADDR FORM) @br
          @(~ 3)   (LIST 'CONS @br
          @(~ 6)        (CADR FORM) (CADDR FORM))))}
      @comment{
In 1969, Greenblatt's MACLISP streamlines syntax.
He introduces DEFUN and the QUOTE character.
By now LISP is not just an experiment. It is an actual system.
Ergonomics matter.
})
     ($oslide "The Serendipity of Parentheses (4)"
      (list data-transition: "none")
      @Lc{(defmacro push (item place) @br
          @~ ;; 1979 Lisp Machine Lisp @br
          @~ `(setq ,place (cons ,item ,place)))}
      @comment{
Ten years later, lowercase is now common, and we have
defmacro that destructures its arguments,
and backquotes for code templates.
That is also when lexical scoping was adopted and LET is introduced.
})
     ($oslide "The Serendipity of Parentheses (5)"
      (list data-transition: "none")
      @div[align: 'left style: "font-size: 90%"
        ]{(defmacro push (item place &environment env) ; 1984 CLtL1 @br
          @(~ 3) (multiple-value-bind @br
          @(~ 19)        (temps vals stores store-form access-form) @br
          @(~ 13)      (get-setf-method place env) @br
          @(~ 7)   (let ((item-var (gensym "ITEM-"))) @br
          @(~ 13)      `(let* ((,item-var ,item) @br
          @(~ 29)              ,@"@"(mapcar #'list temps vals) @br
          @(~ 29)              (,(car stores) (cons ,item-var ,access-form))) @br
          @(~ 19)         ,store-form))))}
      @comment{
Common Lisp standardizes a notion of places that can be computed,
and machinery to handle it without reevaluation of side-effects.
})
     ($oslide "The Serendipity of Parentheses (6)"
      (list data-transition: "none")
      ;; TODO: no sliding for reveal
      @div[align: 'left style: "font-size: 90%"
        ]{(defmacro push (item place &environment env) ; 1991 ANSI CL @br
          @(~ 3) (multiple-value-bind @br
          @(~ 19)        (temps vals stores store-form access-form) @br
          @(~ 13)      (get-setf-expansion place env) @br
          @(~ 7)   (let ((item-var (gensym "ITEM-"))) @br
          @(~ 13)      `(let* ((,item-var ,item) @br
          @(~ 29)              ,@"@"(mapcar #'list temps vals) @br
          @(~ 29)              (,(car stores) (cons ,item-var ,access-form))) @br
          @(~ 19)         ,store-form))))}
      @comment{
ANSI Common Lisp renames a support macro to avoid clash with
methods having a meaning in CLOS.

It took a full decade for LISP to become LISP;
over two decades to look somewhat modern;
three to become today's Common Lisp.
Common Lisp hasn't changed much in nearly four decades since, and probably never will.
})
     ($oslide "The Serendipity of Parentheses (7)"
      (list data-transition: "none-in slide-out")
      @Lc{(define-syntax push ; Scheme @br
          @(~ 1)  (syntax-rules () @br
          @(~ 3)    ((push item place) @br
          @(~ 4)     (set! place (cons item place)))))}
      @comment{
As for Scheme, it doesn't support side-effects or places as much;
but it has hygienic macros — a form of lexical scoping across metalevels.
})
     ($slide "A Machine for Thinking (1)"
      @L{I object to doing things that computers can do.
           @br @C{— Olin Shivers}}
      @L{Patterns mean “I have run out of language.”
           @br @C{— Rich Hickey@~}}
      @comment{
McCarthy, like Turing, dreamt of inventing Thinking Machines.
Instead he discovered a Machine for Thinking:
A language design that minimizes overhead in expressing new ideas;
where ideas can be incremental and interactive extensions to existing ones;
where ideas can be cheaply refined into better ideas.

A language that minimizes drudgery:
no more building parsers, and runtimes;
no more expanding design patterns;
no more dealing with boilerplate;
where programmers can focus on the semantics of their systems;
where they can go straight to the notions that matter.
})
     ($slide "A Machine for Thinking (2)"
      ;; https://www.afrenchcollection.com/the-thinker-by-auguste-rodin-rodin-museum/
      @pic["Le penseur de Rodin.webp" "27%"
           "Le penseur de Rodin"]
      @comment{
*This* is what made Lisp a great substrate for innovation and development:
its “meta” capabilities.
Reflection, macros, embedded domain-specific languages, etc.
A low barrier to entry for new ideas,
that you could easily copy from any other language,
as well as grow in-house.
T.S. Eliot wrote “Immature poets imitate; mature poets steal”.
Lisp enables mature software poetry.
}))
   ($section "Fractured Symmathesy"
     $plan-slide
     ($slide "Why Did It Stop?"
      @comment{
Remember my slides about Lisp innovation across decades?
It slows down in the 1980s, then almost stops in the 1990s and later.

Yet Lisp hasn't changed since the 1990s.
At least not much, and certainly not for the worse.
If anything, the many dialects of Lisp,
their implementations, their libraries,
as well as the extent literature,
have improved in the last 30 years, albeit modestly.

Still the fount of innovation has all but stopped.
Lisp is no longer relevant like it used to be to programming at large.
WHY?

If Lisp still has the same Lambda Nature,
there must have been other factors at stake.
Factors that were once present, and then disappeared.
})
     ($slide "Symmathesy (1)"
      ;; https://www.facebook.com/BruceLee/posts/it-is-like-a-finger-pointing-away-to-the-moon-dont-concentrate-on-the-finger-or-/10157904832570634/
      @pic["Bruce Lee, Enter the Dragon Finger Moon.jpg" "75%"
            "Bruce Lee, Enter the Dragon Finger pointing to the Moon"]
      @comment{
I previously likened the power of Lisp as that of
being able to point a finger at semantics you're interested in.
Pointing a finger is something you do for a spectator.
One spectator is the machine that then evaluates the semantics.
But other spectators are importantly, the humans who will understand
enough of the semantics to keep developing software based on it.
})
     ($slide "Symmathesy (2)"
      ;; https://thefactbase.com/the-internet-was-originally-called-arpanet-advanced-research-projects-agency-network-designed-by-the-us-department-of-defense/
      @pic["ARPANET Geographic Map September 1973.jpg" "60%"
           "ARPANET Geographic Map September 1973"]
      @comment{
Lisp was initially developed at MIT, Stanford, BBN, Xerox PARC, etc.,
elite, well-endowed institutions.
These interconnected communities were lavishly funded by DARPA;
they gathered a critical mass of exceptionally talented researchers.
})
     ($slide "Symmathesy (3)"
      ;; https://antigonejournal.com/2023/02/raphael-school-of-athens/
      @pic["SoARf.png" "75%"
           "Raphael’s The School of Athens"]
      @comment{
Given long time horizons, access to the best computer hardware available, and
superior electronic communication channels,
they formed what Nora Bateson called a *symmathesy* —
a living, mutually reinforcing culture in which people learn from one another,
while breaking new grounds that mainstream computing would not reach for decades.
})
     ($slide "The AI Winter (1)"
      ;; http://xahlee.info/kbd/symbolics_keyboard_rev_c.html
      @pic["Symbolics_keyboard_PN_365407_130df.jpg" "75%"
           "Symbolics keyboard"]
      @comment{
By 1987, luxury Lisp Machines were losing ground
to much cheaper Unix workstations,
while Lisp-based research programs were losing funding
due to unfulfilled promises of AI.
})
     ($slide "The AI Winter (2)"
      ;; https://www.wqxr.org/story/unplanned-fall-berlin-wall/
      @pic["Berlin Wall fall.jpg" "75%"
           "Berlin Wall fall"]
      @comment{
With the Fall of the Berlin Wall, the US Government,
reaping the “peace dividend”, further slashed its defense spending.
Remaining AI research money shifted toward more immediate, practical goals.

Lisp talent was dispersed into an industry that had grown
around business workstations and mass market PCs in turn catching up:
efficiency-first cultures driven by short-term competition,
very different from the subsidized laboratories of yore.

Languages like BASIC, Pascal, C, C++, Perl already occupied niches
that advanced Lisp hackers had afforded neglecting.
Only small pieces of Lisp survived by inspiring Java and JavaScript.
A minority of researchers, hobbyists and a few odd people in the industry
kept using one of the many dialects of Lisp.
But there was no longer any active force driving Lisp forward,
or keeping a community together.
})
     ($slide "Tower of Babel"
      ;; https://www.wikiart.org/en/pieter-bruegel-the-elder/the-tower-of-babel-1563
      @pic["The Great Tower of Babel, by Pieter Bruegel.jpg" "60%"
           "The Great 'Tower of Babel', by Pieter Bruegel 1563"]
      @comment{
There now exist hundreds of dialects of Lisp, tens of which are active.
The once dominant Common Lisp standard still has a dozen active implementations,
different enough from each other, yet with some degree of interoperation.
Scheme has too many standards, and more implementations than one can count,
each quite incompatible with the others.
Application-specific dialects such as Emacs Lisp or AutoLISP
survive (or die) with their application.
The practically-minded Clojure has had enduring industrial success.
And Racket has been developing a symmathesy of its own in some academic circles.

And of course, all these Lisps are incompatible not just with each other,
but even more so with mainstream languages,
introducing an impedance mismatch and non-negligible cost
for one to use code written in the others.

This breakdown of the community has also prevented the resurgence
of any unified symmathesy around Lisp as in the 1960s or 1970s
when the community could gather around handful of dialects that mattered.
})
     ($slide "The Lone Wolf Curse"
      @L{If you give someone Fortran, he has Fortran. @br
         If you give someone Lisp, he has any language he pleases. @br
         @C{— Guy L. Steele Jr.}}
      @L{However, he’ll have a hard time finding someone else speaking @br
         that same language he pleases. @br
         @C{— Faré (me) @(~ 4)}}
      @comment{
The fragmentation of the community isn’t just linguistic — it’s cultural.
Lisp selects for brilliant individualists.
The very confidence that makes a great Lisper becomes a curse
without the institutional structures that once channeled it into collaboration.

Each hacker builds their own cathedral. None of them interoperate.
The collective output is far less than the sum of its parts.
Mark Tarver diagnosed this as the “bipolar Lisp programmer”.
Without the shared hallways of MIT or PARC,
the McCarthys, Bobrows, Hewitts, Sussmans and Steeles
don’t have anyone to cross-pollinate.
There are only lone wolves.
})
     ($slide "Not Below the Line"
      ;; https://windows10spotlight.com/images/31072
      @pic["iceberg.jpg" "75%"
           "Iceberg"]
      @comment{
Interestingly, another phenomenon has limited
the amount of cooperative experimentation open to Lispers:
The Common Lisp standard has drawn a line, with conformant code above
and conformant implementations below, such that
it is still quite easy for Lispers to experiment above the line,
but what is below is a black box, wherein experimentation is both
extremely hard and totally non-portable—high cost, low benefit.

To a point, the increasing efficiency expectations laid on compilers,
and the complexity of code capable of fulfilling these expectations,
has led to a similar line on every dialect and implementation
of Lisp or of any language.
Lisp makes it exceptionally easy to extend the language "from above",
but doesn't help extend it "from below",
as it used to until the 1980s.
Only a handful hackers dare hack each Lisp implementation.
}))
   ($section "The Bright AI Lisp Future"
     $plan-slide
     ($slide "Turning the Tide"
      ;; https://www.flickr.com/photos/suebsang/8640016968
      @pic["empty_SoA.jpg" "55%"
           "Empty School of Athens"]
      @comment{
Is the Lisp symmathesy forever gone?
Is its LAMBDA Nature all for nought?
Are its serendipitous discoveries to be forever lost,
a memory kept by old dodderers and eccentric loners?

I will argue that the age of AI, though it was not brought by Lisp
as anticipated until the 1980s, is a boon for Lisp in the 2020s.
})
     ($slide "A Thinking Machine for Thinking Machines"
      ;; https://www.google.com/imgres?q=rodin%20thinker%20robot&imgurl=https%3A%2F%2Ft4.ftcdn.net%2Fjpg%2F16%2F28%2F44%2F75%2F360_F_1628447521_OpkLCfGtYoQBO8uzmRDy3EZ3qv0GZNIx.jpg&imgrefurl=https%3A%2F%2Fstock.adobe.com%2Fbe_fr%2Fsearch%3Fk%3Dthinker%2Brobot&docid=sAf8ejYmfUcGHM&tbnid=IQsSYDYCvgi6YM&vet=12ahUKEwj5u-2azamUAxWlV6QEHT4sMcEQnPAOegQIGBAB..i&w=514&h=360&hcb=2&itg=1&ved=2ahUKEwj5u-2azamUAxWlV6QEHT4sMcEQnPAOegQIGBAB#sv=CAMSXhoyKhBlLS1pdEp0bGYyY3ZXWG5NMg4taXRKdGxmMmN2V1huTToOUEdxM1ZGdnZvNVp5Uk0gBCokCg5JUXNTWURZQ3ZnaTZZTRIQZS0taXRKdGxmMmN2V1huTRgAMAEYByCH4Z7qCEoIEAEYASABKAE
      @pic["Robot Thinker.jpg" "27%"
           "Robot variant of Rodin’s Thinker"]
      @comment{
The Lambda Nature of Lisp was its ability
to collapse the distance between a concept and its expression.
This ability holds for Artificial Intelligences
as well as for Natural Stupidities.

As great as an AI may be,
it will always be finite in size and limited by costs.
It will always be important to save on neurons, tokens, and whatever resources,
while tackling increasingly hard problems with combinatorial explosion
when handled naively.

The issue of representing ideas precisely and concisely
becomes an objective and reproducible matter of costs and capabilities for AIs,
when the same matter was subjective and personal for humans.

If like me you understand what makes Lisp great,
you should see that as an opportunity.
})
     ($slide "Objective Costs"
      ;; https://www.reddit.com/r/ProgrammerHumor/comments/120mb3s/are_there_10x_developers/
      @pic["Are there 10x developers.webp" "36%"
           "Are there 10x developers?"]
      @comment{
Anecdotal experience shows that a human has about
the same failure rate per line of code in any programming language,
but needs an order of magnitude fewer lines of code
in Lisp compared to non-Lisp languages (or “blub” as Paul Graham dubbed them).
I conjecture that the same observation will hold for AIs, and
that you can save an order of magnitude in AI costs
by programming in Lisp vs “blub” languages.

I haven’t tested this hypothesis yet, but I invite you to test it with me,
if you have the tokens to spare.
})
     ($slide "Impedance Matching"
      ;; Wikipedia
      @pic["Pont du Gard.jpg" "80%"
           "Pont du Gard"]
      @comment{
Now of course, there remains the problem of interoperation.
Using a marginal language means you cannot benefit
from code written in other languages (whether Lisp or “blub”),
without spending scarce resources in writing a wrapper or a translation.
But AI just vastly lowered the cost of such wrappers and translations.
What previously took weeks of skilled work can be achieved
with hours of semi-automation.
Testing can ensure that AI-generated code is solid.
Reviews are still time-consuming but made easier by interactive AI explanations.
And while the result is not perfect, it is improving at a rapid pace.

Moreover, the cost of those wrappers and translations is constant
(or vary linearly with the amount of stable code you need, and its updates),
while the tenfold savings from using them increase linearly as you keep using Lisp.

AI may thus solve language fragmentation issues
like computers solved unit conversion issues, by automating them away.
})
     ($slide "Diving Assistance"
      ;; https://www.uboatworx.com/photos/antarctica-submarine-diving-m-y-legend
      @pic["Submarine dives Antarctica 2018.jpg" "75%"
           "Submarine dives Antarctica 2018"]
      @comment{
AI can help untangle legacy code;
at times refactoring it into cleaner code;
at times documenting it, explaining it to newcomers;
at times adding tests, specifications, or correctness proofs.

Modifying a language implementation below the line of its documented interface
will become easier as AI improves.
It will likely still be significantly harder than working above the line.
Yet the cost of going deep into an existing implementation will sink.

For instance, you or I may want at some point
to unify Common Lisp classes and structs using my C4 linearization algorithm.
Before AI, this would have been a task way too large and costly
for me to undertake unless paid, and no one else would pay me for that.
But with AI, I am confident that I can navigate the internals of SBCL and
any other relevant CL implementation if needed, as well as fix any library
that might be broken by the slightly incompatible change,
all in an acceptable amount of time and effort.
Maybe you will instead want to tweak the Garbage Collector of your Lisp
to better support lazy evaluation.
Tasks previously out of reach are now feasible.
})
     ($slide "The Amplifier Problem (1)"
      ;; Wikipedia
      @pic["Op-amp_symbol.svg.png" "75%"
           "Op-amp symbol"]
      @comment{
AI is an amplifier. You can have it amplify good code, or bad code.
You can generate lots of unmaintainable boilerplate,
or you can generate well-factored uses of macros and DSLs.
})
     ($slide "The Amplifier Problem (2)"
      ;; https://www.annieandre.com/michelin-3-star-restaurants-paris/
      @pic["3-star-michelin-restaurants-paris.avif" "75%"
           "3 star Michelin Restaurants Paris"]
      @comment{
I like to rant about “three-comma programmers”,
the Lisp equivalent of C “three-star programmers”.
It may sound like the praise of a three-star restaurant;
but it means that in C, three-star programmers have code that includes “***”
— three levels of pointer indirection — hard to follow.
In Lisp, three-comma programmers have code that includes “,,,”
— three levels of unquoting — even harder to follow.
It’s a backhanded compliment for programmers bright enough to handle complexity,
but not enough to ruthlessly simplify it away.
Well, AI can be a four-comma programmer, and beyond.
Or it can help you refactor code so it is easier to understand.
})
     ($slide "The Amplifier Problem (3)"
      ;; https://www.engineering.com/could-robotic-exoskeletons-achieve-widescale-use-by-2022/
      @pic["Ripley Power Loader.webp" "75%"
           "Ripley Power Loader"]
      @comment{
AIs can yield code that is
too stupid or too clever,
correct or incorrect,
complete or incomplete, relevant or irrelevant,
simple or complex,
maintainable or unmaintainable.

It can amplify your good and bad experiences as Lispers,
as participants in a Symmathesy.
}))
   ($section "A New AI-assisted Lisp Symmathesy"
     $plan-slide
     ($slide "AI-assisted Lisp Symmathesy"
      ;; https://chatgpt.com/c/69fd59be-0534-83ea-826b-07e89c6a684d
      ;; Please generate a variant of Raphael's "The School of Athens" where the characters are replaced by Anthropomorphic Robot versions of themselves, maybe in the style of 1980s scifi illustration. Otherwise, same color palette, composition, style as Raphael.
      @pic["Robot School of Athens.png" "60%"
           "Robot School of Athens"]
      @comment{
I believe that AI code generation can enable a new age of Lisp-based
cooperation in the exploration of software concepts—a rejuvenated Lisp community,
that innovates faster than blub languages
in topics that are relevant to the world at large.

But this is not automatic. This requires deliberate effort, by you and me.
We still must want to exchange ideas with others,
though AI may facilitate publishing our ideas,
or finding and understanding the ideas of others.
We must still produce code, though a lot of it may be generated,
using tools that themselves may be usefully published and shared.
})
     ($slide "Prophecy and Challenge (1)"
      @L{An extreme optimist is a man who believes that humanity @br
         will probably survive even if it doesn’t take his advice. @br
         @C{— John McCarthy}}
      @comment{
John McCarthy, discoverer of Lisp, was an “extreme optimist”.
I do not always share his extreme optimism. But even when I do,
I remember that change happens not outside of people, but through them.
And so, Lisp or the equivalent may win despite no one listening to my advice;
yet if no one does, it will still happen through some people (or AIs)
rediscovering the same ideas.
And at that point, what I'm offering you is just the *opportunity*
to get in early onto the train of the AI-assisted Lisp Symmathesy.
})
     ($slide "Prophecy and Challenge (2)"
      @pic["caza_stargazing_robot.jpg" "36%"
           "Philippe Caza: Le Robot qui rêvait"]
      @comment{
Come, and enjoy with me, the days of innovation and cooperation
through expressing programming concepts as directly as possible,
through incrementally extending languages with new concepts,
through absorbing any and all existing paradigms into Lisp,
faster than others can develop them outside Lisp.
With barriers lowered through AI.
})
     ($slide "Thank You!"
        @L{Slides: @(~ 8) @Https{github.com/metareflection/poof}}
        @L{Join me: @(~ 5) Gerbil Scheme @Url{https://cons.io}}
        @L{Blog: @Https{ngnghm.github.io} @~[3] X: @Https{x.com/ngnghm}}
        @L{Hire me: @(~ 5) @code{<fahree@"@"gmail.com>}}
  @;    @div[id: "ui"] @; would need to hack try.gambitscheme.org/UI.js for that.
  @;(iframe src: "https://feeley.github.io/gambit-in-the-browser/" style: "width:80%; height:50vh; border:none; border-radius:8px;")
        @comment{
I can type (- 2026 1984) and it will return 42
}))))

(reveal-doc doc)
