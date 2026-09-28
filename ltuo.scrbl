#lang scribble/book
@; -*- Scheme -*-
@(require "util/ltuo_lib.rkt")

@title[#:style (ltuo-style)]{
  Lambda: The Ultimate Object
    @linebreak[] @tex-linebreak[]
    @smaller{Object Orientation Elucidated@|~~|}
    @linebreak[] @tex-linebreak[]
    @(when/list (render-latex?) (cube-logo))}

@author{François-René Rideau}

@(when/list (render-html?) (cube-logo))

@dedication{
  To William R. Cook, who first formalized inheritance in the λ-calculus,
  showed how it differs from subtyping,
  introduced mixin inheritance, and more—the foundations on which this book is built—yet
  who never believed in the significance of inheritance.
  He was, I discovered too late,
  the one person with whom I most wanted to argue about the ideas in this book.
}
@book-abstract{
@noindent[]
@italic{This book, though well advanced, is still a work in progress}@xnote["."]{
  You are reading manuscript @tt[(git-version "ltuo-*")].
  The latest draft is available in PDF at @url{https://fare.tunes.org/files/cs/poof/ltuo.pdf}
  and in HTML at @url{https://fare.tunes.org/files/cs/poof/ltuo.html}.
  The source code is at @url{https://github.com/metareflection/poof}.
  Please send feedback to fahree@"@"gmail.
}
@linebreak[]@linebreak[]@;@tex{\\{}}
You have seen or used Object Orientation (OO), loved it or hated it.
But do you understand what exactly OO is,
what it is @emph{for}, and when and how (not) to use it?
OO languages each offer an incompatible variant,
with no theory to explain their common ground…
that two computer scientists can agree on.
By contrast, Functional Programming (FP) has the λ-calculus.

Can you explain OO to yourself, or to an apprentice?
Reason about OO programs and what they mean?
Make sense of the tribal warfare between OO and FP advocates?
Implement OO in a language that lacks it?
@emph{Objectively} (hey!) choose between single, mixin and multiple inheritance in its many forms?
Fit prototypes, method combinations and multiple dispatch into your notion of OO?
Last but not least… are you tired of us Lispers bragging about how
our 1988 OO system is still decades ahead of yours?

If these questions bother you, this book has the answers.
It develops a Theory of OO on top of FP,
defining OO as @emph{Internal Modular Extensibility}:
a mouthful for simple concepts you already use,
though you may not yet have clear names for them.
It connects disparate academic theories and industry practices,
while challenging some widely accepted views.

More than a synthesis of existing lore, this theory is @emph{productive}:
it offers new ways to reason about OO and implement OO
in a few lines of code in any language with higher-order functions.
It reconciles Class OO, Prototype OO—and a more primitive OO without objects(?!)
that few computer scientists even know exists.
It also demarcates OO from related but conceptually distinct domains that share its vocabulary.

Its crowning achievement is C4, a new inheritance algorithm
that combines single and multiple inheritance,
and is provably better than any used before.
}

@tex{\tableofcontents{}}

@include-section{ltuo_01_introduction.scrbl}
@include-section{ltuo_02_what_oo_is_informal_overview.scrbl}
@include-section{ltuo_03_what_oo_is_not.scrbl}
@include-section{ltuo_04_oo_as_internal_modular_extensibility.scrbl}
@include-section{ltuo_05_minimal_oo.scrbl}
@include-section{ltuo_06_rebuilding_oo_from_minimal_core.scrbl}
@include-section{ltuo_07_inheritance_mixin_single_multiple_or_optimal.scrbl}
@include-section{ltuo_08_types_for_oo.scrbl}
@include-section{ltuo_09_extending_the_scope_of_oo.scrbl}
@include-section{ltuo_10_efficient_object_implementation.scrbl}
@include-section{ltuo_11_conclusion.scrbl}
@include-section{ltuo_12_annotated_bibliography.scrbl}
