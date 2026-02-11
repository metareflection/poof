#lang scribble/report
@; -*- Scheme -*-
@(require "util/ltuo_lib.rkt")

@;{ TODO: use bookcover to create a book cover. }

@title[#:style (ltuo-style)]{
  Lambda: the Ultimate Object
    @linebreak[] @tex-linebreak[]
    @smaller{Object Orientation Elucidated@|~~|}
    @linebreak[] @tex-linebreak[]
    @(when/list (render-latex?) (cube-logo))}

@author{François-René Rideau}

@(when/list (render-html?) (cube-logo))
@noindent[]
@italic{This book is a work in progress.
Please send feedback to fahree at gmail.}@xnote[""]{
  For your convenience, a current draft is available
  in PDF at @url{http://fare.tunes.org/files/cs/poof/ltuo.pdf}
  and in HTML at @url{http://fare.tunes.org/files/cs/poof/ltuo.html}.
  The source code is at @url{https://github.com/metareflection/poof}.
}
@linebreak[]@tex{\\{}}

@book-abstract{
As a software practitioner, you have not only heard of Object Orientation (OO),
but seen it or used it, loved it or hated it.
Yet you may have been frustrated that there never seems to be clear answers as to
what exactly OO is or isn’t, what it is @emph{for}, when and how to use it or not use it.
There are many examples of OO; but everyone does it differently;
every OO language offers an incompatible variant.
There is no theory as to what common ground there is if any,
even less so one on the best way to do OO.
Certainly, none that two computer scientists can agree about.
By comparison, you well understand Functional Programming (FP).

Can you explain OO in simple terms to an apprentice, or to yourself?
Can you reason about OO programs, what they do, what they mean?
Can you make sense of the tribal warfare between OO and FP advocates?
Maybe you’ve enjoyed OO in the past, or been teased by colleagues who have,
and are wondering what you are or aren’t missing?
Maybe you’d fancy implementing OO on top of the OO-less language
you are currently using or building, but from what you know it looks too complicated?
Indeed do you really understand why to implement which of no inheritance, single inheritance,
mixin inheritance, or multiple inheritance?
Can you weigh the arguments for multiple inheritance done C++ or Ada style,
versus Lisp, Ruby, Python or Scala style?
Is there a best variant of inheritance anyway?
And do concepts like prototypes, method combinations and multiple dispatch seem natural to you,
or are they mysteries that challenge your mental model of OO?
Last but not least… have you had enough of us Lispers bragging about how our 1988 OO system
is still decades ahead of yours?

If any of these questions bother you, then this book is for you.
This book offers a Theory of OO that it elucidates in simple terms on top of FP—as
Intra-linguistic Modular Extensibility.
A mouthful, but actually all simple concepts you already use,
though you may not have clear names for them yet.
This Theory of OO can answer all the questions above, and more.
The answers almost always coincide with
@emph{some} existing academic discourse or industry practice;
but obviously, they cannot possibly coincide with
@emph{all} the mutually conflicting discourses and practices out there;
and, often enough, this theory will reject currently prevalent majority views and
promote underrated answers.

But this Theory of OO is not just connecting previously known yet disparate lore;
nor is it yet another @italic{a posteriori} rationalization
for the author’s arbitrary preferences.
This theory is @emph{productive}, offering new, never before articulated ways to think about OO,
based on which you can implement OO in radically simpler ways,
in a handful of short functions you can write in any language that has higher-order functions;
and it can @emph{objectively} (hey!) justify every choice made.
This theory reconciles Class OO, Prototype OO, and even a more primitive classless OO
that few computer scientists are even aware exists.
What is easily underappreciated, this theory can demarcate
this common domain of OO from a lot of related but quite distinct domains
that may look like OO and even share some of its vocabulary,
yet can be shown to be conceptually foreign.
The crown of this Theory of OO though is a new algorithm, C4, that allows combining
single and multiple inheritance in a way that is better—and provably so—than
the alternatives used in any programming language so far.
}

@tex{\tableofcontents{}}

@include-section{ltuo_01_introduction.scrbl}
@include-section{ltuo_02_what_oo_is_not.scrbl}
@include-section{ltuo_03_what_oo_is_informal_overview.scrbl}
@include-section{ltuo_04_oo_as_internal_extensible_modularity.scrbl}
@include-section{ltuo_05_minimal_oo.scrbl}
@include-section{ltuo_06_rebuilding_oo_from_minimal_core.scrbl}
@include-section{ltuo_07_inheritance_mixin_single_multiple_or_optimal.scrbl}
@include-section{ltuo_08_extending_the_scope_of_oo.scrbl}
@include-section{ltuo_09_efficient_object_implementation.scrbl}
@include-section{ltuo_10_conclusion.scrbl}
@include-section{ltuo_11_annotated_bibliography.scrbl}
