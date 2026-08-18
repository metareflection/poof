#lang scribble/report
@; -*- Scheme -*-
@(require "util/ltuo_lib.rkt")

@title[#:style (ltuo-style)]{
  Lambda: the Ultimate Object
    @linebreak[] @tex-linebreak[]
    @smaller{Object Orientation Elucidated@|~~|}
    @linebreak[] @tex-linebreak[]
    @(when/list (render-latex?) (cube-logo))}

@author{François-René Rideau}

@(when/list (render-html?) (cube-logo))

@dedication{
  To William R. Cook, who first formalized inheritance in the λ-calculus,
  and the discrepancy between inheritance and subtyping,
  and also mixin inheritance—the foundations on which this book is built.
  He was, I discovered too late,
  the one person with whom I most wanted to argue about the ideas herein.
}
@noindent[]
@italic{This book, though well advanced, is still a work in progress}@xnote["."]{
  For your convenience, a current draft is available
  in PDF at @url{https://fare.tunes.org/files/cs/poof/ltuo.pdf}
  and in HTML at @url{https://fare.tunes.org/files/cs/poof/ltuo.html}.
  The source code is at @url{https://github.com/metareflection/poof}.
  Please send feedback to fahree@"@"gmail.
}
@linebreak[]@tex{\\{}}

@book-abstract{
As a software practitioner, you have not only heard of Object Orientation (OO),
but seen it or used it, loved it or hated it.
Yet you may have been frustrated that there never seem to be clear answers as to
what exactly OO is or isn’t, what it is @emph{for}, when and how to use it or not use it.
There are many examples of OO—but everyone does it differently;
every OO language offers an incompatible variant.
There is no theory describing what common ground there is, if any,
much less one describing the best way to do OO—certainly,
none that two computer scientists can agree on.
By comparison, you well understand Functional Programming (FP).

Can you explain OO in simple terms to an apprentice, or to yourself?
Can you reason about OO programs and what they mean?
Can you make sense of the tribal warfare between OO and FP advocates?
Maybe you’ve enjoyed OO in the past, or have heard enough from colleagues who have,
and are wondering what you are or aren’t missing?
Maybe you’d fancy implementing OO on top of the non-OO language
you are currently using or building, but from what you know this looks too complicated?
Indeed do you really understand which to implement of no inheritance, single inheritance,
mixin inheritance, or multiple inheritance, and why?
Can you weigh the arguments for multiple inheritance done C++, Ada or PHP style,
versus Lisp, Ruby, Python or Scala style?
Is there a best variant of inheritance anyway?
And do prototypes, method combinations and multiple dispatch seem natural to you,
or are they mysteries that challenge your notion of OO?
Last but not least… are you tired of us Lispers bragging about how our 1988 OO system
is still decades ahead of yours?

If any of these questions bothers you, then this book is for you.
It offers a Theory of OO, explained in simple terms on top of FP—as Internal Modular Extensibility.
A mouthful, but actually all simple concepts you already use,
though you may not have clear names for them yet.
This Theory of OO can answer all the questions above, and more.
The answers almost always coincide with
@emph{some} existing academic discourse or industry practice;
but obviously, they cannot possibly coincide with
@emph{all} the mutually conflicting discourses and practices out there;
and, often enough, this theory will reject currently prevalent majority views and
promote underrated answers.

But this Theory of OO is not merely connecting previously known yet disparate lore;
nor is it yet another @italic{a posteriori} rationalization
for the author’s arbitrary preferences.
This theory is @emph{productive}, offering new, never-before-articulated ways to think about OO,
based on which you can implement OO in radically simpler ways,
in a handful of short functions you can write in any language that has higher-order functions;
and it can @emph{objectively} (hey!) justify every design choice.
This theory reconciles Class OO, Prototype OO, and even a more primitive classless OO
that few computer scientists are even aware exists.
What is easily underappreciated, this theory can demarcate
this common domain of OO from a lot of related but quite distinct domains
that may look like OO and even share some of its vocabulary,
yet can be shown to be conceptually foreign.
The crown of this Theory of OO, though, is a new algorithm, C4, that combines
single and multiple inheritance in a way that is better—and provably so—than
the alternatives used in any programming language so far.
}

@tex{\tableofcontents{}}

@include-section{ltuo_01_introduction.scrbl}
@include-section{ltuo_02_what_oo_is_informal_overview.scrbl}
@include-section{ltuo_03_what_oo_is_not.scrbl}
@include-section{ltuo_04_oo_as_internal_extensible_modularity.scrbl}
@include-section{ltuo_05_minimal_oo.scrbl}
@include-section{ltuo_06_rebuilding_oo_from_minimal_core.scrbl}
@include-section{ltuo_07_inheritance_mixin_single_multiple_or_optimal.scrbl}
@include-section{ltuo_08_types_for_oo.scrbl}
@include-section{ltuo_09_extending_the_scope_of_oo.scrbl}
@include-section{ltuo_10_efficient_object_implementation.scrbl}
@include-section{ltuo_11_conclusion.scrbl}
@include-section{ltuo_12_annotated_bibliography.scrbl}
