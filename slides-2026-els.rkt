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


lazy evaluation (Friedman & Wise 1976)
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


Lisp had OOP, FP, Actors, Logic programming, Graphical User Interfaces, and many more paradigms, all together, already back in the 1970s, before any idiot invented an “opposition” between paradigms.

The “opposition” between paradigms is just the resource limitation of idiots using the “programming language” paradigm and therefore having to allocate their limited resources to only one paradigm. When you adopt the “systems” paradigm of Lisp, you have a system that you extend in many ways, with no opposition between extensions, only complementarity.
tg
For 20 years, Lisp did not have the true LAMBDA, yet it had the LAMBDA nature(!)
1958-1979 is when most of the great discoveries were made in Lisp land,
and Lisp only adopted lexically scoped LAMBDA between 1975-1979.


@;{
## Introduction

### Lambdist Koans

  A student, in hopes of understanding the Lambda-nature, came to Greenblatt.
  As they spoke a Multics system hacker walked by.
  “Is it true”, asked the student,
  “that PL/I has many of the same data types as Lisp?”
  Almost before the student had finished his question,
  Greenblatt shouted, “FOO!”, and hit the student with a stick.

Danny Hillis, years before he built Connection Machines,
composed a series of koans where Lisp hackers from the MIT AI Lab
took the place of Buddhist Zen Masters and Disciples. He launched a genre.
Around the same time, Sussman and Steele wrote their famous series of papers:
'Lambda the Ultimate Imperative',
'Lambda the Ultimate Declarative',
'Lambda the Ultimate GOTO'...
launching another genre: showing how to reduce programming paradigms down to Lambdas.

### The Question

So what is this LAMBDA that can imbue software with its blessed nature,
and gives LISP its power?
Is it… functional programming?

But not just ML, Haskell but even Python and JavaScript—and these days,
even C++ or Java—support functional programming,
with first class anonymous functions.
Do *they* possess the Lambda Nature?
And if the Lambda Nature is about functional programming,
how can LISP itself claim to possess it,
when it didn't actually have TRUE LAMBDAs until 1975?

### My Answer

Well, sure you can reduce all computations to LAMBDAs. That's remarkable.
You could reduce them all to Turing Machines instead,
but they were designed to be boring, rather than elegant.
Assembly works too, but it's specific to one kind of machine,
and is ephemeral as technology evolves.
And so there is value in what computations are reduced to.

But looking back, @emph{that} was not the ultimate power of Lisp.
“When a wise man points at the moon, the fool looks at the finger”.
But when any fool can point at anything, the wise man marvels at the finger.
The LAMBDAs are the Moon, which is beautiful. But the magic was in the pointing.

## The LAMBDA Nature

### So, was Lisp great?

Well, we can look at achievements by Lispers.
I can only single out a few of them.

1950s:
List Processing and Symbolic Processing,
***“Artificial Intelligence”***,
***Conditional Expressions***,
Functional Programming,
***Recursion***,
Dynamic Typing,
Automatic Storage Management (***Garbage Collection***),
Interactive Programming,
***Metacircular Interpreters***,
Code as Data.

1960s:
Bootstrapped Compilers,
***Macros***,
***REPLs***,
Portable Programming,
Dynamic Scoping,
***Error handling***,
Symbolic Differentiation,
Knowledge Representation,
Dynamic Code Extension,
Logo,
Structure Editing,
Time-sharing.

1970s:
Rule-based / Declarative Programming,
Pattern-Matching,
Constraint Propagation,
Concurrency- / Process- / Actor- oriented Programming,
Non-local Exits,
***First-Class Continuations***,
Backtracking,
Lexical Scoping,
Proper Tail-calls,
Interactive Debugging,
***Object-Oriented Programming***, Prototypes, Method Combination,
Graphical User Interfaces,
Tagged Microarchitectures (Lisp Machines),
Lazy Evaluation,
Static Typesystems,
Programmable Syntax,
***Extensible Editors***.

1980s:
Commercial Lisp Workstations,
Network Programming,
***Massively Parallel Programming***,
Hypertext,
CAD,
Movie CGI,
Symbolic Algebra,
Expert Systems,
Multiple Dispatch,
***CLOS***—the greatest Object System ever designed—,
Dynamic Code Upgrade,
Orthogonal Persistence,
***Macro Hygiene***,
Resumable Exceptions,
Delimited Control,
Theorem Provers,
Incremental Compilation,
Genetic Programming.

1990s:
ANSI CL standard,
***Meta-Object Protocols***,
C3 Linearization,
***Web programming—especially so with continuations***,
Partial Evaluation

2000s:
Language-Oriented Programming (Racket),
***Module Systems with Proper Phase Separation for Metalevels*** (Racket),
Mixing Static and Dynamic Types with Contracts (Racket),
Persistent Data Structures (Clojure).

### Now, wait a minute

First, we see a sharp decline in the 1990s and since.
If Lisp was once great, is it still?
Does Lisp still have the LAMBDA nature?
How can the LAMBDA nature even wane as well as wax?

Then again, do all these innovations really come from Lisp?
IPL had List Processing before LISP.
While Object-Oriented Programming was first described as such
by Bobrow and Winograd in Lisp, they took the name from Alan Kay
and many concepts from Dahl and Nygaard's Simula.
But maybe more importantly, look at LAMBDA itself!

### The LAMBDA Story

McCarthy didn't invent LAMBDA in 1958;
rather he *misimplemented* a first-order description by Kleene
of the 1930s calculus by Church—and ended up
inadvertently discovering *dynamic binding*.
Landin's 1966 ISWIM was the first programming language
built around the actual λ-calculus,
implemented (also at MIT) in 1968.
Lisp only had proper LAMBDAs, with *lexical binding*, in 1975,
with Sussman and Steele's Scheme, and only adopted it
after Steele showed how to compile it efficiently in 1978.
So, for its first 20 years, LISP didn't even have proper LAMBDAs.

And yet, these were the most productive years of LISP.

### Copy Any Paradigm ###

So maybe LAMBDA was copied, and badly, at first.
And List Processing, Rule-based Programming, and Theorem Provers,
and any of those many paradigms that were developed in Lisp.
But that's the point.
Other researchers each had to invent a programming language from scratch
that would only implement one paradigm—also badly at first.
Lisp could quickly absorb any programming paradigm that anyone invented;
and Lisp could keep refining that paradigm until it wasn't bad anymore,
and integrated well with other paradigms, as fast or faster
than non-Lispers could.

One can argue that Bobrow didn't invent Object-Oriented Programming.
Yet he unarguably contributed much to OOP, and together with other Lispers,
brought it to heights far beyond what was ever achieved in any non-Lisp language.


### The Serendipity of Parentheses ###

What made Lisp great was this ability to program the programming language.
It is not just “code as data” or “homoiconicity”,
but an **extensible** protocol for code as data;
what Shriram Krishnamurthi calls the “bicameral” syntax,
wherein you read code, so you can (extensibly) expand it,
before you actually parse it.
These were not deliberately designed by McCarthy,
but serendipitously discovered over the years.

The parenthetical symbolic expression syntax, or S-expression, or SEXP,
was initially meant only for data.
Code was to use the meta expression syntax, or M-expression, or MEXP,
a more traditional infix syntax.
But McCarthy's metacircular interprenter, a curiosity a first,
was made into a practical implementation by Steve Russell in 1959,
that used SEXP as input.
Tim Hart discovered macros in 1963,
wherein you could extend the SEXP syntax from within LISP.
With L. Peter Deutsch's first interactive interpreter, PDP-1 LISP, in 1964,
users thought primarily in SEXP.
After Peter Samson's PDP-6 LISP in 1966 made toplevel definitions into
ordinary expressions, MEXP was abandoned.
@;{Using EVAL instead of EVALQUOTE; use DEFPROP to define functions, macros, etc.}
Greenblatt only introduced DEFUN in 1969, in MACLISP.

It took a full decade for LISP to become LISP.


### A Machine for Thinking ###

McCarthy, like Turing, dreamt of inventing Thinking Machines.
Instead he discovered a Machine for Thinking:
A language design that minimizes the overhead
of expressing new ideas as incremental and interactive extensions to existing ones,
and refining them into better ideas.
A language that minimizes the drudgery of building parsers and runtimes,
so that programmers can focus on the semantics of their systems,
and go straight to the notions that matter.

This is what made Lisp a great substrate for innovation and development:
the “meta” aspect—reflection, macros,
embedded domain-specific languages, etc.
A low barrier to entry for new ideas that
could easily be copied from any other language,
as well as grown in-house.
T.S. Eliot wrote “Immature poets imitate; mature poets steal”.
Lisp enables mature software poetry.

## Fractured Symmathesy

### Why did it stop?

Since the 1990s, Lisp hasn't changed, at least not much, and not for the worse.
If anything, the many dialects, their implementations, their libraries,
and the extent literature, have improved in the last 30 years, albeit modestly.
Yet the fount of innovation has largely slowed down,
or stopped being as relevant as before to the larger programming world.

Thus, there must have been other factors beyond the LAMBDA Nature of Lisp,
that were once present, and then disappeared.

### Symmathesy

I previously likened the power of Lisp as that of being able to
point a finger at semantics you're interested in.
Pointing a finger is something you do for a spectator.
One spectator is the machine that then evaluates the semantics.
But other spectators are importantly, the humans who will understand
enough of the semantics to keep developing software based on it.

Lisp was developed at MIT, Stanford, BBN, Xerox PARC, Yale, Indiana University,
and other elite, well-endowed institutions.
These interconnected communities were lavishly funded by DARPA,
gathering a critical mass of exceptionally talented researchers.
Given long time horizons, access to the best computer hardware available, and
superior electronic communication channels,
they formed what Nora Bateson called a @emph{symmathesy} —
a living, mutually reinforcing culture in which people learn from one another,
while breaking new grounds that mainstream computing would not reach for decades.

### The AI Winter

By 1987, luxury Lisp Machines were losing ground
to much cheaper Unix workstations,
while Lisp-based research programs were losing funding
due to unfulfilled promises of AI.
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

### Tower of Babel

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

### The Lone Wolf Curse

The fragmentation of the community isn’t just linguistic — it’s cultural.
Lisp selects for brilliant individualists.
The very confidence that makes a great Lisper becomes a curse
without the institutional structures that once channeled it into collaboration.

“If you give someone Fortran, he has Fortran.
If you give someone Lisp, he has any language he pleases.” (Steele)
“However, (I’ll add) he’ll have a hard time finding someone else speaking
that same language he pleases.”

Each hacker builds their own cathedral. None of them interoperate.
The collective output is far less than the sum of its parts.
Mark Tarver diagnosed this as the “bipolar Lisp programmer”.
Without the shared hallways of MIT or PARC,
the Hewitts, Sussmans, Steeles and Bobrows don't have anyone to cross-pollinate.
There are only lone wolves.

### Not Below the Line

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

## The Bright AI Lisp Future

### Turning the Tide

Is the Lisp symmathesy forever gone?
Is its LAMBDA Nature all for nought?
Are its serendipitous discoveries to be forever lost,
a memory kept by old dodderers and eccentric loners?

I will argue that the age of AI, though it was not brought by Lisp
as anticipated until the 1980s, is a boon for Lisp in the 2020s.

### A Thinking Machine for Thinking Machines

The LAMBDA Nature of Lisp was its ability
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

### Objective Costs

Anecdotal experience shows that a human has about
the same failure rate per line of code in any programming language,
but needs an order of magnitude fewer lines of code
in Lisp compared to non-Lisp languages (or “blub” as Paul Graham dubbed them).
I conjecture that the same observation will hold for AIs, and
that you can save an order of magnitude in AI costs
by programming in Lisp vs “blub” languages.

I haven’t tested this hypothesis yet, but I invite you to test it with me,
if you have the tokens to spare.

### Impedance Matching

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

### Diving Assistance

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

### A New AI-assisted Lisp Symmathesy

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

### Prophecy and Challenge

John McCarthy, discoverer of Lisp, was an “extreme optimist”:
“a man who believes that humanity will probably survive
even if it doesn’t take his advice.”

I do not always share his extreme optimism. But even when I do,
I remember that change happens not outside of people, but through them.
And so, Lisp or the equivalent may win despite no one listening to my advice;
yet if no one does, it will still happen through some people (or AIs)
rediscovering the same ideas.
And at that point, what I'm offering you is just the @emph{opportunity}
to get in early onto the train of the AI-assisted Lisp Symmathesy.

Come, and enjoy with me, the days of innovation and cooperation
through expressing programming concepts as directly as possible,
through incrementally extending languages with new concepts,
through absorbing any and all existing paradigms into Lisp,
faster than others can develop them outside Lisp.
With barriers lowered through AI.

}
