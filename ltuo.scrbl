#lang scribble/acmart @manuscript @review @nonacm
@; -*- Scheme -*-
@;{ For review: @anonymous @review
    Default already: @10pt @natbib @screen @acmsmall
    @anonymous @authordraft @authorversion @timestamp @review @nonacm }
@;{ Someday put it in scribble/report or scribble/book format instead? }

@; TODO: a ltuo.bib with per-entry summary and commentary tailored to the relevance of the cited piece
@; to the current one.

@;{ TODO: get this header in the HTML???
@head-extra[
  (list 'link
        (list (list 'rel "icon")
              (list 'type "image/svg+xml")
              (list 'href "resources/pic/Half-life_lambda_logo.svg")))]}

@title[
]{Lambda, the Ultimate Object
@; @linebreak[] @;
} @subtitle{ @; <- for scribble/acmart
       A theory of Object-Orientation, with a modest contribution on Optimal Inheritance}

@author[
    #:email (email "fare@mukn.com")
    #:affiliation (affiliation #:institution @institution{@emph{MUKN, Inc.}}
                               #:country "USA")
]{François-René Rideau}

@abstract{
  We present our new C4 algorithm to best combine
  single and multiple inheritance in Object-Oriented languages.
  To explain what our algorithm does and why it matters,
  we first recapitulate a complete theory of Object-Orientation (OO),
  sufficient to establish what we mean by “best”:
  what OO is and isn’t, what is its purpose, what is inheritance, what are known variants of it,
  what makes each variant desirable or not, what are the relevant challenges with
  using, implementing or combining them.
  In particular, we reconstruct from first principles OO
  as a mechanism for reified (in-language) Modular Extensibility,
  with examples in a subset of Scheme that is a minimal extension of the untyped λ-calculus.
  We explain why multiple inheritance is more expressive and modular than single inheritance,
  why (prototype or class) linearization is desirable
  and what are known desired properties of linearization.

  Our algorithm simply combines two well-known ideas:
  C3 linearization and the treatment of single and multiple inheritance by Scala 3.
  It also brings new insight into the nature single and multiple inheritance,
  and what matters about each of them.
  While this contribution is quite modest in light of prior art,
  it illustrates how our theory of OO is productive of new solutions and explanations for them,
  and not just a posteriori justifications of existing practices.
  Our theory yields answers that mostly coincide with
  prevalent academic discourse and industrial practice,
  but sometimes goes against either or both.
  Our unified theory of OO might therefore be the bigger contribution,
  compared to getting the one last concept of inheritance just right.
}

@(require scriblib/bibtex
@;        (only-in scribble/core make-style)
          (only-in scribble/manual racket racketblock code codeblock litchar itemize item)
          (only-in scribble/example examples make-base-eval)
          (only-in scriblib/footnote note)
          (only-in scribble-abbrevs appendix)
@;        (only-in scribble/html-properties head-extra html-defaults)
          (only-in scribble-math/dollar $)
@;          scribble/minted
          syntax/parse/define
          "util/examples-module.rkt"
          "util/enumitem.rkt"
          "util/util.rkt"
          (for-label racket))

@(define (anonymize x . y) x)
@(define (GerbilScheme) @anonymize["our Scheme implementation"]{Gerbil Scheme})
@(define (principle . x) (bold (emph x)))

@(define-simple-macro (r a ...) (racket a ...))
@(define (omega) "ω")
@(define-simple-macro (c a ...) (elem #:style 'tt a ...))
@(define-simple-macro (Code a ...) (verbatim a ...))
@(define-simple-macro (Definitions a ...) (examples/module poof #:no-result a ...))
@(define-simple-macro (Examples a ...) (examples #:eval poof #:no-result a ...))
@(define-simple-macro (Checks a ...) (examples #:eval poof #:label #f a ...))
@(define-simple-macro (λ formals body ...) (lambda formals body ...))
@(define-simple-macro (TODO body ...) '())
@(define-simple-macro (Xitemize body ...) (list body ...))
@(define-simple-macro (Xitem body ...) (list " " body ... " "))
@(define (ᵢ) (list @(html-only @c{ᵢ}) @tex{${}_i$}))
@(define (Ri) (list @c{R}(ᵢ)))
@(define (Pi) (list @c{P}(ᵢ)))
@(define (⋂) (list @tex{$\bigcap$}@html-only{⋂}))
@(define (⇝) (list @tex{$\rightsquigarrow$}@html-only{⇝}))

@(define super 'super)
@(define self 'self)

@;; Suppress the page count for the Camera-ready version by uncommenting the below.
@;@tex{\thispagestyle{empty}\pagestyle{empty}}
@;; Instead, we could set the start page for the document with:
@;@pageStart{42}

@(define-bibtex-cite "ltuo.bib" ~cite citet generate-bibliography)
@(define (~nocite . x) (let ((_ (apply @~cite x))) (void)))

@(define-simple-macro (defsection name tag text) (define (name (x text)) (seclink tag x)))
@(defsection section1 "Prototypes_bottom_up" "section 1")

@pretitle{
@tex{
\acmYear{2025}
\copyrightyear{2025}
\acmConference[Scheme]{Scheme Workshop}{2025}{online}
\acmISBN{978-1-xxxx-xxxx-x/21/10}
\acmPrice{00.00}
\acmDOI{https://dx.doi.org/xx.xxxx/xxxxxxx.xxxxxxx}
\setcopyright{none}
}}

@section[#:tag "Intro"]{Introduction}

@subsection{Our Contribution: An Optimal Inheritance Mechanism — and a Theory Why}

@subsubsection{Context: Inheritance}
Object-Oriented Programming (OOP), or
@principle{Object-Orientation (OO), is a programming style
  characterized by the use of @emph{inheritance}},
a mechanism for programming by modularly extending
partial specifications of code @~cite{inheritance1996}@xnote["."]{
  A notable dissident to this characterization is William Cook,
  a respected academic who made many key contributions to understanding the semantics of inheritance
  @~cite{Cook1989 cook1989inheritance cook1989denotational bracha1990mixin Cook1994}
  yet later argued that inheritance was orthogonal to OO @; Also Cook1989?
  and that OO is about “classes” of “objects” that can only be accessed through “interfaces”
  @~cite{Cook2009 Cook2012}.

  However, coding against an SML “module” would count as OO by Cook’s criteria,
  and indeed Cook explicitly calls the untyped λ-calculus “the first object-oriented language”,
  while dismissing Smalltalk as not OO enough because its integers are not pure objects@~cite{Cook2009}.
  Cook’s definition, that embraces the modular aspect of OO while rejecting
  its extensible or dynamic aspect, runs contrary to all practice.
  It brings no light on any of the languages commonly considered OO
  yet cursed by Cook as not being OO enough,
  no light on any of the Functional Programming (FP) languages blessed by Cook as actually being OO
  to the surprise of their users, and no light on the difference between the two.

  Cook’s many works on OO over the years also systematically neglect important concepts
  in OO, such as prototypes, multiple inheritance, method combination or multiple dispatch.
  In the end, Cook’s PhD and subsequent academic career grew out of
  brilliantly modeling the key mechanism of OO (inheritance)
  from the foreign point of view of FP;
  but his wilful ignorance and deep misunderstanding of the OO tradition, indeed missing the point,
  were such that they have become proverbial: immortalized in Gabriel’s essay
  “The Structure of a Programming Language Revolution” @~cite{gabriel2012}
  as a prototypical failure to understand a phenomenon when viewed
  through a scientific paradigm incommensurable with the one that produced it.

  In the end, Cook is well worth mentioning precisely to illustrate the lack of
  common vocabulary, common concepts, and common paradigms among those
  who practice and study OO, even or especially
  among notable academics with deep expertise in the field.
  And yet, there are undeniably common practices, common phenomena, common concepts,
  common language features, common design patterns,
  worth understanding, conceptualizing, defining and naming
  in the rich (though sometimes mutually conflicting) traditions
  that grew around OO.
  There is thus a need to elucidate the key concepts of OO behind the hype and confusion;
  such is the main purpose of this essay.
}
OO usually depends on explicit support from the programming language at hand,
then called an Object-Oriented language.
Now programmers (resp. programming language designers)
have faced a dilemma since almost the very beginning,
of which of several variants of inheritance to use (resp. implement), if any at all:
many prefer single inheritance for its simplicity and performance
@~cite{Simula1967 kay1996early};
others prefer multiple inheritance
for its greater expressiveness and modularity @~cite{Bobrow1976 Cannon1979}.
A few outliers prefer mixin inheritance, a variant in some sense intermediary between the two above,
but also more fundamental, more composable @~cite{bracha1990mixin}.
Some prefer no inheritance at all.
@; Cite https://freedium.cfd/https://medium.com/better-programming/object-oriented-programming-the-trillion-dollar-disaster-92a4b666c7c7
@; XXX Reverse Inheritance as trivially expressible in terms of mixin or multiple inheritance.

@subsubsection{Problem: Optimal Inheritance?}
Is there a form of inheritance that is objectively better than the others,
with respect to expressiveness, modularity, extensibility, and runtime performance?
Is one of the usual variants superior to the others in every way?
If not, is there a combination of them, or a superset of them, that is?
Some languages notably combine both single inheritance and multiple inheritance,
though with some constraints @~cite{cltl2 scalableComponentAbstractions2005};
would the best way to do inheritance subsume these combinations?
Importantly, multiple inheritance usually (but not always!) involves a computation called
the @emph{linearization} of the inheritance graph.
Is that linearization necessary?
What becomes of it in this optimal inheritance?
And of course, OO has many detractors who claim that the best inheritance is no inheritance.
What are the reasons to use or not use inheritance to begin with?

@subsubsection{Claim: A Theory of OO}
So that we may offer a solution to the problem of optimal inheritance,
we will first recapitulate a theory of OO sufficient to state the problem
and the criteria that may make a solution better than others.
In a way, we will only be stating lore that has been known for decades.
Yet no element of that lore is uniformly known to all;
some may never have been told explicitly in academic literature;
a few elements actually go radically against prevalent opinions in academic literature
without being necessarily wholly original;
and they have never been formulated together.
What more, @principle{We offer a Theory of OO that is @emph{Productive}}:
it does not merely restate and explain old knowledge,
but shapes it into a coherent whole capable of generating and justifying new knowledge.

@subsubsection{Claim: Constructive OO}
Furthermore, @principle{Our Theory of OO is also Constructive}:
we include code in Scheme, using as few features of Scheme as possible,
so that it may be easily adapted to any programming language or dialect.
We explain which features we need beyond the mere applicative λ-calculus, why,
and how to typically implement them in existing programming languages.
Remarkably, the main feature we need is lazy evaluation, or ways to emulate it,
as OO is most naturally defined in a pure lazy functional setting,
and eager evaluation of OO without side-effects leads to exponential recomputations.
One aspect for which we do not provide a construction, however, is static typing;
however, others have provided good foundational theories
for typing OO@~cite{iloop1995 allen2011type},
and we will provide semi-formal designs for what better static types for OO should look like
on top of such foundations.
Good OO types should work for not just for Second-Class Classes,
but also for First-Class Prototypes;
they do not require dependent types, but do require recursive types and subtyping.
@;{ TODO evaluate Scala DOT in this context. Ask nada.}

@subsubsection{Claim: C4 is Optimal}
We claim that (a) indeed there is a best way to combine single and multiple inheritance,
that (b) indeed it involves linearization of the inheritance graph,
that (c) there are enough constraints on linearization for the optimal algorithm
to be well-defined up to some heuristic, and
that (d) even then there are good reasons to prefer a specific heuristic.
@principle{Our C4 algorithm implements this optimal variant of inheritance.}
We implemented C4, that is included as part of
the builtin object system of @(GerbilScheme)@xnote["."]{
  Scheme @; XXX CITE lambda-the-ultimate r4rs r7rs-small
  is a language with a rather minimalist definition.
  There are dozens of implementations of the language
  (with various degrees of compliance to the various standards),
  and each implementation provides its own extensions on top of this common core,
  to offer a usable programming environment.
  The extensions are usually mutually incompatible,
  though there are trends, similarities, variants of shared code between the many implementation,
  and even standards for some of these extensions.
  However, there is no standard object system,
  instead plenty of different object systems that span the entire design space for OO—except for
  their generally lacking static types.
}

Once our theory of OO is (re)stated, the C4 algorithm and the justification of its benefits
should seem rather obvious, though still novel.
Yet without that (re)statement, neither the algorithm nor its benefits would be obvious.
C4 is thus a comparatively modest yet meaningful innovation on top of our theory,
that crowns the theory as productive.

@subsection{Plan of the Article}

In section 2, we dispel common misconceptions about OO,
to ensure that our theory isn’t met with misunderstanding due to misconceptions
or disagreements about what is being theorized.

In section 3, we provide a quick overview of Object Orientation,
and the three variants of Inheritance in common use.
This section serves as a map of the concepts and of the words we use to describe them,
necessary because there is no common theory of OO, and
no unambiguous shared vocabulary to name what common concepts there are.
We notably introduce the essential yet oft-ignored notion of
Conflation between Specification and Target value, and describe
the relationship between Specifications, Prototypes, Classes and Objects.

In section 4, we explain what we mean by Extensible Modularity,
the why and wherefore of OO.
This section remains informal, but lays the conceptual groundwork
for the formal approach we take in the rest of this essay.

In section 5, we introduce a minimal formal models of Modularity and Extensibility
in terms of pure Functional Programming (FP), using Scheme syntax,
and derive from first principles a minimal OO system, in two lines of code.
This minimal OO system uses Mixin Inheritance, and, remarkably,
has neither Objects nor Prototypes, even less Classes,
only Specifications and Targets.

In section 6, we rebuild all the familiar features and appurtenances of OO
as additions or modifications to the minimal system from section 5:
prototypes, classes, types, mutation, etc.
We notably explain away the too easy confusion between subtyping and subclassing,
and discuss the actual relationship between OO and imperative programming,
when the natural framework for OO is actually pure lazy functional programming.

In section 7, we discuss in detail the main forms of inheritance:
single inheritance, multiple inheritance and mixin inheritance.
We examine issues surrounding method conflict and resolution, or harmonious combination.
We explain the known consistency constraints that matter
for linearization algorithms in the context of multiple inheritance,
and state of the art in satisfying them, the C3 algorithm.
Finally, we discuss how to combine multiple and single inheritance.
We discuss existing solutions solutions respectively adopted by
Common Lisp @~cite{cltl2}, Ruby, @; TODO cite
and Scala @~cite{scalableComponentAbstractions2005}.
We then propose our solution, a linearization algorithm we call C4,
that satisfies all the constraints we discussed for a good linearization algorithm,
and for combining single and multiple inheritance.
We explain why the residual heuristic we also adopt from C3 is arguably the best one.

In section 8, we discuss more advanced topics including
Focused Modular Extensions, Method Combination, Multiple Dispatch (Multimethods),
Monotonicity, Orphan Typeclasses, and Global Fixpoints.

Finally, in section 9 we conclude by recapitulating our findings.

@; TODO: Describe Appendices


@subsection{Note on Nomenclature}

As we restate well-known and less-known lore of Object Orientation,
we will endeavour to precisely define the terms we use.
Defining terms is especially important since various authors
from diverse communities around many OO languages
each use conflicting terminologies,
with different words for the same concepts,
or, which is worse, the same words for different concepts.
This tower of Babel can cause much confusion
when trying to communicate ideas across communities,
as people ascribe opposite presuppositions and connotations to the words
used by other people, and talk past each other
while incorrectly believing they understood what the other said.

Thus, when multiple nomenclatures conflict, we will give precedence to
the @emph{least ambiguous} word for a concept,
even if it is neither the most popular word for the concept, nor the oldest,
even if we sometimes make one up just for this article.
The words we choose will hopefully cause readers to pause and reflect,
rather than unwittingly misunderstand the sometimes subtle points we make,
as they might if we had used a treacherously familiar word.

In particular, we will conspicuously avoid using the unqualified words
“object” and “class” unless strictly necessary,
because they connote for each reader, depending on the traditions he has adopted,
a different set of assumptions,
that are parasitic to the theory we are laying out.
We will also reject the word “class” to mean the most general kind
of entity subject to inheritance,
since a class is but a quite limited special case of a @emph{prototype},
that is itself derivative of what we’ll call a
(modular extensible) @emph{specification}.

@; TODO for submission, move to appendix?
@section{What Object-Orientation is @emph{not}}

Before we explain in detail what OO @emph{is},
let us cast aside a lot of things it @emph{isn’t}
that too many people (both proponents and opponents)
falsely identify with OO.
This is important, because attempts at building or explaining a theory of OO often fail
due authors and readers having wrong expectations about what OO is supposed to be.

@subsection{Whatever C++ is}

The most popular OO language in the decades that OO was a popular trend (roughly 1980 to 2010),
C++ indeed supports some form of OOP.
But C++ is a rich language with many aspects completely independent of OO
(e.g. efficient bit-banging, RAII, template metaprogramming or pointer aliasing and a memory model),
whereas the OO aspect that it undoubtly offers
is very different from how OO works in most other OO languages,
and colloquial C++ often goes against the principles of OO@xnote["."]{
  Alan Kay famously declared at OOPSLA ’97, near peak C++ popularity:
  “I made up the term ‘object-oriented’, and I can tell you I didn’t have C++ in mind.”}
Therefore, C++ is in no way representative of OO, and
if what you know of “Object Orientation” comes from C++,
please put it aside, at least while reading this article, and come with a fresh mind.

This is especially true with regard to multiple inheritance,
that will be an important topic later in this essay.
C++ boasts support for multiple inheritance, and many people,
when thinking of multiple inheritance, think of what C++ offers.
Yet, while C++ supports single inheritance well, what it calls “multiple inheritance”
is not at all the same as what most everyone else calls “multiple inheritance”:
it is actually a modified kind of mixin inheritance
with some kind of “duplication” of superclasses (for non-@c{virtual} classes)@xnote[","]{
  C++ actually copies bad ideas from Snyder’s CommonObjects @~cite{Snyder1986Encapsulation}
  in “duplicating” superclasses to force inheritance into a tree rather when it is a DAG.
  You can try to make sense of it in terms of actual multiple inheritance,
  by maintaining copies of superclasses wherein you rename their fields and methods
  according to the path being followed in the inheritance tree.
  But really, the attitude seems to be
  “here is a simple but incorrect solution to a problem that only solves easy cases;
  let us keep this convenient ‘solution’ and pretend we are allowed to redefine the problem
  so our ‘solution’ becomes correct,
  then leave our users to deal with the discrepancy between their actual problem and
  our incorrect solution.”
  That is the very opposite of any good science or engineering,
  which would instead be to identify the actual problem and its correct solution.
}
and a subset of multiple inheritance (for @c{virtual} classes and members).
Moreover, C++ crucially lacks the proper method resolution
that enables a lot of the modularity of multiple inheritance in other languages.

Now, you can use C++’s powerful template language to reconstitute actual mixin inheritance
and its method resolution on top of C++’s weird variant of inheritance@~cite{smaragdakis2000mixin};
and you could no doubt further implement proper multiple inheritance on top of that@xnote["."]{
  One could achieve multiple inheritance as a design pattern on top of mixin inheritance,
  as we will describe later in this article,
  wherein developers would manually compute and specify
  each class’s superclass precedence list;
  but this cancels some of the modularity benefits of multiple inheritance
  versus single and mixin inheritance.
  Alternatively, someone could extend the above technique to also reimplement
  the entire superclass linearization apparatus
  within the C++ template metaprogramming language.
  Template metaprogramming is most definitely powerful enough for the task,
  though it will take a very motivated developer to do the hard work,
  and the result will still a burden for any developer who wants to use it.
  Moreover, for all that cost, classes defined that way would only interoperate
  with other classes following the exact same pattern.
  Maybe the library implementing the pattern could eventually be included
  in some semi-standard library, until, if it gets any traction,
  the language itself is eventually amended to do the Right Thing™.
}
But this technique is quite uncolloquial, syntactically heavy, slower than the colloquial ersatz,
and programmers have to rigorously
follow, enforce and maintain some complex design patterns.

Finally, and at the very least, consider that
unless you explicitly tag your classes and their members @c{virtual},
C++ will deliberately eschew the “dynamic dispatch” of OO
and use “static dispatch” instead for the sake of “going fast”.
In the end, C++ is many great and not-so-great things, but only few of those things are OO,
and even most of those that look like OO are often different enough that
@principle{C++ does not reliably inform about OO in general}@xnote["."]{
  The situation is similar for Ada, that adopted multiple inheritance in 2003
  by seemingly copying the general design of C++.
  Now even when C++ got multiple inheritance wrong@~cite{stroustrup1989multiple},
  ignorance was no valid excuse,
  since Lisp got it right ten years earlier@~cite{Cannon1979}.
  Ignorance is even less forgivable in the case of Ada
  copying C++’s “multiple inheritance” yet 14 years later.
  Meanwhile, many languages got it right: Ruby, Python, Scala, etc.
}

@subsection[#:tag "classes_only"]{Classes Only}

Many claim that classes, as first implemented by Simula 67@~cite{Simula1967}
(though implementing a concept previously named by Hoare@~cite{hoare1965record}),
are essential to OO, and only ever care to implement, use, formalize,
study, teach, propagandize, or criticize class-based OO (a.k.a. Class OO).
Books from summities in Programming Languages @~cite{Pierce2002TAPL plai eopl3},
in their chapter about OO, barely even mention any other kind of OO if at all,
much less study it.

Yet KRL@~cite{Winograd1975},
the second recognizable precursor to OO,
whose authors first applied the words “inheritance” and “prototypes” to their language
(though the words were used descriptively without their latter technical definition),
has what we would now recognize as prototype-based OO (a.k.a. Prototype OO).
The modern concept of OO
can be traced back to Smalltalk adopting inheritance in 1976
and popularizing the word and concept of it among programming language designers;
and Smalltalk was class-based.
Yet contemporary with Smalltalk or immediately after it
were prototype-based languages Director @~cite{Kahn1976 Kahn1979} and
ThingLab @~cite{Borning1977 Borning1979 Borning1981}@xnote["."]{
  ThingLab was built on top of Smalltalk by members of the same team at PARC,
  and oscillated between having or not having classes in addition to prototypes.}
Plenty more Prototype OO or “class-less” OO languages followed
@~cite{Rees82t:a adams88oopscheme chambers1989efficient Lawall89SelfInScheme Salzman05prototypeswith jsonnet nix2015 poof2021}.
There are lot more Prototype OO languages than we could have time to review @~cite{WikiProto},
but prominent among them is JavaScript @~cite{eich1995javascript},
one of the most used programming language in the world @~cite{TopPL2022},
maybe the top one by users
(though it relatively recently also adopted classes on top of prototypes @~cite{EcmaScript:15}).

What more, we will argue below that Prototype OO @~cite{Borning1986}
is more general than Class OO, that is but a special case of it @~cite{Lieberman1986}.
And we will even argue that you can recognizably have OO
with neither prototypes nor classes, as in T @~cite{adams88oopscheme}.
Despite common ignorant opinions to the contrary,
@principle{Class-less OO is part and parcel of the OO tradition},
historically, conceptually, and popularly.

Now of course, classes, while not @emph{essential} to OO,
are still @emph{important} in its tradition.
The situation is similar to that of types in Functional Programming (a.k.a. FP):
the historical preexistence and continued relevance of the untyped λ-calculus
and the wide adoption of dynamically typed functional languages like Scheme or Nix
are ample evidence that types are not essential to FP;
yet types are undoubtly an important topic that occupies much of the theory and practice of FP.
Actually, the analogy goes further since, as we’ll see,
classes are precisely an application of OO to types.

@subsection{Imperative Programming}

Many people assume that OO requires mutation,
wherein all attributes of all objects should be mutable, or at least be so by default,
and object initialization must happen by mutation.
Furthermore, they assume that OO requires the same applicative (eager) evaluation model
for procedure calls and variable references as in every common imperative language.
@; TODO{CITE? C++ Perl5 Python Java JavaScript Scala Ruby Go (see GitHub)}
Meanwhile, many have of late claimed that purity (the lack of side-effects including mutable state)
is essential to FP, making it incompatible with OO.
Some purists even argue that normal-order evaluation (call-by-name or call-by-need)
is also essential for “true” FP, making it (they say) even more incompatible with OO.

However, there are many good historical reasons,
having to do with speed and memory limitations at both runtime and compile-time,
why early OO and FP languages alike, from the 1960s to the 1980s,
as well as most languages until relatively recently,
were using mutable state everywhere, and an eager evaluation model, at least by default.
And with early 1980s slogans like “objects are a poor man’s closures” and
“closures are a poor man’s objects”@~cite{adams88oopscheme},
the problem back then was clearly not whether OO could be done purely with functions,
but whether it made practical sense to program purely without side-effects in general.
That question that would only be slowly answered positively,
in theory in the early 1990s @; TODO CITE Moggi
and in practice in the mid 2000s to mid 2010s,
as Haskell grew up to become a practical language@xnote["."]{
  Some may identify darcs (2003) as the first widely used real-world application of Haskell.
  After it came innovations such as bytestring (2005), cabal (2005)
  (and the “cabal hell” it started causing around 2006 until later solved by stack),
  ghc6 (2006), that made Haskell much more practical to use, and
  new notable applications appeared like pandoc (2006), or xmonad (2007).
  A turning point maybe was the publication of “Real World Haskell” (2008). @; TODO CITE
  Eventually, Stack (2015) made non-trivial haskell programs and scripts repeatable.
  Now there’s obviously a lot of subjectivity in deciding
  when exactly Haskell became “practical”—but one should expect
  the transition to practicality to be an S curve, such that
  whichever reasonable yet somewhat arbitrary threshhold criteria you choose,
  the answer would be at about the same time.
  In any case, making a practical language pure functional was just not an option before 2010 or so,
  and it is absurd to claim that any programming language concept is intrinsically stateful
  just because its practical implementations before 2010 were all stateful.
  You could similarly claim that logic programming is intrinsically stateful,
  or that functional programming itself is intrinsically stateful.
}

Yet, there are (a) pure models of OO such as those of
Kamin, Reddy, Cook and Bracha@~cite{Kamin1988 ObjectsAsClosures Cook1989 bracha1990mixin},
(b) pure lazy dynamic OO languages such as Jsonnet or Nix@~cite{jsonnet dolstra2008nixos},
and pure lazy OO systems for Scheme@~cite{poof2021}
@; TODO maybe mention foreshadowing by Oleg Kiselyov ?
and (c) languages happily combining OO and FP such as Common Lisp or Scala,
@;TODO cite ScalaZ, etc.
with plenty of libraries restricting themselves to pure functional objects only.
These provide ample evidence that OO does not at all require mutation,
but can be done in a pure setting, and is very compatible with FP, purity,
and even with laziness and normal-order evaluation.
@; Haskell typeclasses embody half of OO@~cite{typeclasses LIL2012},
@; and could be tweaked to embody all of it.
@; though its designers might not wholly embrace the OO tradition. @; TODO{CITE}
@; TODO: add inheritance to Haskell typeclasses or Rust traits, or just cite CL gf, Clojure protocols.
Actually, we will argue based on studying of the semantics of OO that
@principle{Pure Lazy Functional Programming is the natural setting for OO}. @;{TODO secref}

@subsection{Encapsulation}

Many OO pundits claim that an essential concept in OO
is “encapsulation” or “information hiding”@~cite{DeRemerKron1975},
though there is no consensus as to what this or these concepts mean,
and no clear definition. @; TODO{CITE}

Inasmuch as “encapsulation” informally denotes but part or all of
modularity—the ability to code against an interface,
with code on either side not caring which way the other side implements its part of the interface
(or not even being able to distinguish between multiple such implementations)—then
yes, this is half of the essence of OO, as per our definition
(the other half being extensibility).
Some may also call this concept “data abstraction” or some other kind of “abstraction”.
@; XXX cite Liskov??? Mary Shaws???

However, inasmuch as some people identify encapsulation as the presence
of specific visibility mechanisms such as found in C++ or Java
(with some attributes or methods being @c{public}, @c{private} or something in–between,
on the precise semantics of which designers of different languages cannot agree),
we’ll easily dismiss such mechanisms as not actually essential to OO,
since many quintessential OO languages like Smalltalk or Common Lisp
lack any such specific mechanism,
whereas many non-OO languages possess mechanisms to achieve the same effect,
in the form of modules defining but not exporting identifiers
(e.g. not declaring them @c{extern} in C),
or simply lexical scoping@~cite{rees1995}.
@; TODO{cite Simula? JS?}

Now, these mechanisms themselves can be very useful,
worthy features to add to an OO language, to use and study, etc.
They are just not essential to OO and not specific to it,
though of course their adaptation to OO languages will follow
the specific shape of OO constructs not found in non-OO languages.
Misidentifying OO as being about these mechanisms rather
than about the modularity they do or do not support can only lead to
sacrificing the ends to the means.

@subsection{Inheritance as opposed to Composition}
Some argue that there is an essential conflict between OO and FP,
between Inheritance and Composition,
wherein OO is about model every possible domain in terms of inheritance,
especially so where it can be preferred compared to FP and composition.

But OO and FP are just distinct concepts neither of which subsumes the other,
that thus fit distinct sets of situations.
@;Each distinct concept has its set of situations that it fits,
@;distinct from that of any other concept (or else they are actually the same concept);
@;a concept that fits all situations has no content and is useless;
@;and two concepts like OO and FP neither of which subsumes the other,
@;cover sets of situations neither of which is a subset of the other.
It makes no sense to oppose them, especially not when we see that
OO can be expressed in a few lines of FP, whereas
most modern OO languages contain FP as a subset—and
Lisp has harmonously combined OO and FP together since both their emergences in the 1970s,
decades before anyone had the idea to fantasize a conflict between the two.

The argument is actually a distortion of a legitimate question of OO design, @; TODO cite
wherein one has to decide whether some aspect of a class (respectively prototype or pattern)
embodied as attributes or methods, should be included directly in the class
(a) by inheriting from another class defining the aspect
(the class @emph{is-a} subclass of it — inheritance of classes), or
(b) indirectly by the class having as an attribute an object of that other class
(the class @emph{has-a}n attribute that is it —
composition of classes seen as constructor functions).

The answer of course depends on expectations about how the class will be further specialized
within a static or dynamically evolving schema of data structures and algorithms.
If the schema is small, static, well-understood and won’t need to evolve,
it doesn’t really matter which technique is used to model it.
But as it grows, evolves and boggles the mind,
a more modular and extensible approach is more likely to enable adapting the software
to changing situations, at which point thoughtful uses of inheritance can help a lot@xnote["."]{
  @emph{Is} a car a chassis (inheritance),
  or does it @emph{have} a chassis while not @emph{being} it (composition)?
  If you’re writing a program that is only interested in the length of objects,
  you may model a @c{car} as a @c{lengthy} object with a @c{length} slot,
  and a @c{chassis} too. Now if your program will only ever be interested
  but in the length of objects, you may altogether skip any object modelling:
  and only use numeric length values directly everywhere for all program variables.
  Is a car a chassis? Yes, they are both their length, which is the same number,
  and you may unify the three, or let your compiler’s optimizer unify the two variables
  as they get initialized them from the same computation.
  Now if you know your program will evolve to get interested in
  the width of objects as well as their length,
  you might have records with length and width rather than mere numbers,
  and still unify a car and its chassis.
  But if your program eventually becomes interested in the height, weight or price of objects,
  you’ll soon enough see that the two entities may somehow share some attributes
  yet be actually distinct: ultimately, both @c{car} and @c{chassis} @emph{are} @c{lengthy},
  but a @c{car} @emph{has} a @c{chassis} and @emph{is not} a @c{chassis}.

  There is also an old slogan of OO design,
  notably found in the famous “Gang of Four” (“GoF”) book @~cite{GoF1994},
  that you should “favor object composition over class inheritance”.
  GoF argues not to create an exponential number of subclasses
  that specialize based on static information about what is or could be a runtime value,
  because classes are compile-time and human-developer-time objects
  that are less flexible and costlier in human effort than runtime entities.
  These arguments of course do not apply for regular Prototype OO,
  wherein umpteen combinations of prototypes (and classes as a particular case)
  can be generated at runtime at no additional cost in human effort.
  Still, the point can be made that if a programmer is confused about which of is-a or has-a
  to use in a particular case, it’s a good heuristic to start with has-a,
  which will quickly lead to an obvious showstopper issue if it doesn’t work,
  whereas picking is-a where has-a was the better choice can lead to a lot of complications
  before it is realized that it won’t work right.
  Yet it is always preferable to understand the difference between “is” and “has”,
  and to use the correct one based on understanding of the domain being modeled,
  rather than on vague heuristics that substitute for lack of understanding.
  At any rate, this slogan, though oft quoted out of context in online debates,
  actually has nothing to do with the OO vs FP debate, it is about using OO effectively.
}

@subsection{Message Passing}
Alan Kay, who invented Smalltalk and coined the term “Object-Oriented Programming” circa 1967
notably explained@~cite{Kay2020} that by that he originally meant
a metaphor of computation through independent (concurrent, isolated) processes
communicating by passing asynchronous messages.
This metaphor also guided the modifications originally
brought by Simula to Algol@~cite{Simula1966}.
It is also present in notable early object systems such as
Director @~cite{Kahn1976 Kahn1979} and
ThingLab @~cite{Borning1977 Borning1979 Borning1981}.

However, neither Simula, nor Smalltalk nor any popular claimed OO language
actually fits that metaphor, though some obscure and later Actor languages might.
@; cite ABCL/1 ?
Instead, the only popular language ever to fit this metaphor
is Erlang@~cite{OOP2010};
yet Erlang is not part of the OO tradition,
and its authors have instead described its paradigm as “Concurrency-Oriented Programming”.
Meanwhile the theory of computation through message-passing processes
was studied with various “process calculi”,
@; TODO cite pi calculus, join calculus, rho calculus, CHAM, etc.
that are also foreign to the OO tradition,
and largely unembraced by the OO community.
Indeed Erlang crucially lacks inheritance, or support for the “extreme late binding of all things”
that Alan Kay also once mentioned was essential for OO@xnote["."]{
  In Erlang, each process is a dynamic pure applicative functional language
  enriched with the ability to send and receive messages to and from other processes.
  Now, as we’ll see, you need fixed-points to express the semantics of OO;
  but in a pure applicative context, you cannot directly express sharing the results of a computation,
  so the pure fixed-point combinators lead to exponential recomputations as the fixed-point
  involves deeper self-references.
  OO is therefore possible using the applicative pure functional fragment of the language
  within an Erlang process, but the result will not scale very well;
  see for instance the example “object-via-closure” that Duncan McGreggor wrote as part of LFE.
  @; https://github.com/lfe/lfe/blob/1d0de5e04a9d5b8f1430063c45b561f08123a4e0/examples/object-via-closure.lfe
  Or OO could be achieved indirectly, by using a preprocessor that expands it away,
  or a compile-time only extension to the compiler, as in most static Class OO languages.
  Or OO could be achieved as a design pattern of maintaining some global table
  to store the state of the many shared lazy computations in each process.
  Or, more in line with the Actor model that Erlang embodies,
  OO could be achieved by spawning one or multiple processes
  for each shared lazy or stateful computation (including each super-object of each object),
  which might require some strict object lifetime discipline (not colloquial in Erlang),
  or garbage collection of processes (not part of the Erlang language, beyond the process tree);
  see for instance the example “object-via-process” that Duncan McGreggor wrote as part of LFE.
  @; https://github.com/lfe/lfe/blob/1d0de5e04a9d5b8f1430063c45b561f08123a4e0/examples/object-via-process.lfe
  None of these solutions would qualify as supporting OO much more than
  assembly language “supports” OO or any Turing-universal language “supports” any paradigm, though.
  In the end, the essence of OO, which is Prototype OO,
  directly fits in the pure lazy functional paradigm,
  but only fits indirectly in other paradigms,
  including the pure applicative functional paradigm.
}

Moreover, many OO languages generalize and extend their method dispatch mechanism
from “single dispatch” to “multiple dispatch”@~cite{
  Bobrow86CommonLoops bobrow88clos CecilMultimethods allen2011type}.
Their “multimethods” are attached to tuples of prototypes or classes,
and there is no single prototype, class, or single independent entity of any kind
capable of either “receiving” or “sending” a message.
Instead, they are attached to a “generic function”
that handles the dispatch based on the types of its arguments@xnote["."]{
  The “generic function” functionality from in the Common Lisp Object System (CLOS) @; TODO cite
  can be viewed as isomorphic to the “protocols” functionality of Clojure;
  and Common Lispers also use the word “protocol” informally to designate a set of generic functions.
  They would in turn be isomorphic to the “typeclasses” of Haskell
  or the “traits” of Rust... @; TODO cite
  if only these latter two suppored inheritance, which they don’t.
  These idioms denote a set of related function names and type signatures,
  that get implemented differently for different types of arguments
  (and, in Haskell, also different types of expected results).
  Other the crucial property, these traits, typeclasses or protocols
  can be defined @emph{after the fact},
  so that new traits, typeclasses or protocols can be defined for existing types,
  and new types can be added to existing typeclasses.
  This second property is in sharp contrast with “interfaces” in Java or C#,
  wherein the author of the class must know in advance
  all the interfaces that the class will implement,
  which must yet cannot anticipate any of the future extensions that users will need.
  Users with needs for new protocols will then have to keep reinventing
  variants of existing classes, or wrappers around existing classes, etc.
  — and again when yet another protocol is needed.
  Protocols are therefore much more modular than Java-style “interfaces”,
  and more extensible than Rust “traits” or Haskell “typeclasses”,
  making them modular at a finer grain (protocol extensions rather than protocol definitions),
  which in turn makes them more modular.
  Note also how what Rust recently popularized as “trait” is
  something completely different from what Smalltalk, Mesa or Scala call “trait”,
  that do support inheritance, but only describe operations on a single type,
  and cannot be defined after the fact, but have to be made explicit parts of a class definition.
  Once again, be careful that there is no common vocabulary
  across programming language communities.
}
While multimethods are obviously not essential to OO
since there are plenty of OO languages without them,
they are a well-liked, age-old extension
in many OO languages (CLOS, CECIL, Dylan, Fortress, Clojure, Julia)
and extensions exist for C++, Java, JavaScript, TypeScript, C#, Python, Ruby, etc.
@; TODO cite stroustrup2007multimethods https://en.wikipedia.org/wiki/Multiple_dispatch
The “message passing” paradigm, having no place for multimethods,
thus falls short compared to other explanations of OO that accommodate them.
Now, the message passing paradigm @; TODO cite PLANNER, Actors
can be extended with a notion of “group messaging”
where one object sends a “message” to a “group” of objects
(rather than each member of the target group)
@; TODO cite ABCL group messaging
or to a “chemical” paradigm where a “chemical reaction” may involve
multiple entities in and multiple entities out, with “message” entities
conveying the changes in intermediary steps. @; TODO cite CHAM
But even with these extensions to the paradigm,
you would still have to also specifically shoe-horn extensibility and method resolution
into the paradigm to fit OO and its method inheritance,
whether with single dispatch or multiple dispatch.

In conclusion, whatever historical role one it may have had in inspiring the discovery of OO,
@principle{the paradigm of message-passing processes is wholly distinct from OO},
with its own mostly disjoint tradition and very different concerns,
that describes a different set of programming languages and patterns@xnote["."]{
  Now, there is no doubt, from their later testimonies as well as then published papers,
  that Erlang’s Concurrency Oriented Programming is clearly
  what the authors of Simula, Smalltalk, Actors, etc., were all @emph{aiming at}.
  But, due to hardware as well as software limitations of the 1960s and 1970s,
  they all failed to actually reach that goal until the mid 1980s,
  and instead on their way stumbled on something altogether different,
  that they identified and developed—Object Oriented Programming.

  That’s how invention always works:
  if you knew in advance the thing you’d find later, it would already have been invented.
  An invention is always surprising, original, and never, ever,
  exactly what you knew in advance it would be—or else
  the invention happened earlier and @emph{then} was surprising and original.
  Also, an invention is shaped by the technical constraints of the time—some of which it may lift,
  but not always those anticipated.
}

@subsection[#:tag "modeling_the_world"]{Modeling the World}

Some have claimed that OO is meant to be @emph{the} way to model the world,
often in association with the concurrent message passing model
we already established above was not quite OO,
or with some class-based OO framework they sell.

However, while OO can indeed be of great use in modeling a lot of problems,
especially where the modeling language needs modularity and extensibility,
it by no means is supposed to be a Theory of Everything that subsumes
Relativity and Quantum Mechanics, Category Theory, Darwinism, Aristotelian Poetics, etc.
Even if we stick to software, there are plenty of paradigms other than OO that OO does not subsume:
functional programming, logic programming, machine learning,
operational research, relational databases, reactive programming, temporal logic,
concurrent programming, dataflow programming, reactive programming,
homomorphic encryption, etc.
Inasmuch as OO languages can be used to implement any of these paradigms,
so can any Turing Tar-Pit. And inasmuch as any of these paradigms
can be harmonously combined with OO, that does not make either a subset of the other.
People seriously studying OO should not take at face value the claims of
Snake Oil and Silver Bullet salesmen, either about what their products can do,
or about whether these products indeed embody OO. Mostly, they do not.

Consider methodologies such as UML that claim to do OO model design, @; TODO cite
drawing diagrams of relations between classes including inheritance.
Beside the fact that classes are not essential to OO as seen previously,
UML and similar languages do not even meaningfully have classes:
there is no proper semantics to inheritance,
especially in presence of fields that recursively refer back to a class:
should the child class have a link to the parent class or to the child class?
Assume a classic case of modeling humans as animals,
wherein animals can have spawns that are animals of the same kind:
Should human spawns be modeled as arbitrary animals,
or should they be modeled as human only?
Conversely, if some animals eat other animals,
does that mean that humans automatically eat humans, or only some other animals?
In presence of recursion, UML falls apart,
by failing to distinguish between subclassing and subtyping,
between self-reference and reference to a constant.

Interestingly, Amílcar Sernadas or Bart Jacobs’s categorical theories
of “objects” and “inheritance”
@~cite{sernadas1994 Jacobs1995ObjectsAC Jacobs1996InheritanceAC} @; TODO cite Goguen OBJ
actually model UML and refinement,
and not at all actual Objects and Inheritance as used in Programming Languages;
a hijacking of the same words for completely different meanings,
with the only similarity being that both sets of meanings
involve arrows between specifications.
At least Jacobs explicitly embraces early on the limitation whereby
self-reference or recursion is prohibited from field definitions. @; TODO cite
Just like UML, his co-algebra utterly fails to model OO;
but at least his theory is internally consistent if not externally.

@principle{UML, co-algebras and other similar methodologies
are actually relational data modeling @; TODO cite
disguised as OO}.
As we’ll see later, their “classes” are extensible indeed,
but in a trivial way that fails to support modularity@xnote["."]{
  Note that there is nothing wrong at all with relational data modeling as such:
  it is a fine technique for many purposes,
  despite being deliberately limited in abstraction (and, therefore, modularity)—and
  sometimes @emph{thanks to this limitation}.
  Restrictions to expressiveness can be very useful,
  in the necessarily restricted or imprecise cases that they apply.
  Indeed, in some cases, relational data modeling, not OO,
  is what you need to organize your data and your code.
  However, what is very wrong, and intellectually dishonest,
  was to sell relational data modeling as OO back when OO was trendy, based on
  a superficial and at times deliberate misunderstanding of OO
  by either or both sellers and buyers, resulting in more confusion.
}
UML and co-algebras describe the “easy case” of OO, where objects are just a convenient way
of merging records of elementary data types
(or “constant” data types, for co-algebras)
— an easy case without recursion, where subclassing indeed coincides with subtyping.
But these methodologies avoid crucial features of OO programming,
where records can recursively refer to other records,
where the operations of interest are higher-level than getting or setting fields,
where you incrementally extend not just data types but also algorithms,
where there are “binary methods” that involve two objects at once,
or even more elaborate higher-order functions, etc.
More broadly, these methodologies lack any effective semantics of inheritance,
of method resolution in computing properties of objects along their class hierarchies,
or of anything that has the precision required to specify code
that can actually run and be reasoned about.
But specifying code is exactly where the conceptual difficulties and gains of OO
are both to be found with respect to software construction.
In fact, these handwaving methodologies@xnote[""]{
  Not all uses of Category Theory in “OO” are handwaving.
  Goguen, who invokes Category Theory in his papers,
  and is cited by these later categorical imitators,
  develops precise formal specification of code, refinement of such specifications,
  and actual implementation of executable code.
  @;{TODO cite}
  On the other hand, Goguen’s claim to do “OO” is dubious,
  despite some attempts in later works to retrofit some actual OO concepts into his systems;
  instead, what he developed turns out to be
  a completely different and orthogonal paradigm—term rewriting.
  Term rewriting is a wonderfuly interesting paradigm to study,
  but has never seen any adoption for practical programming,
  though it has found uses in reasoning about programs.
  What Goguen calls “inheritance” most of the time is actually code refinement,
  a technique that can be used to build proven-correct compilers,
  though it is not a general theory of code implementation applicable
  to arbitrary such compilers.
}
are specifically designed to make
those incapable or unwilling to wrestle with computation
believe they understand all there is to know about software modeling.
Yet the nature and correctness of software lies precisely
in this gap they are unable or unwilling to explore.

An actual theory of types for OO must confront not just products of elementary data types,
but sum types, function types, subtyping, constrained type parameters,
existential and universal types, and more—including, especially, fixed-points (recursion).
And you can always go beyond with session types, substructural types, temporal types,
separation types, dependent types, etc.
In the end, if you care about modeling the types in your software (and you usually should),
you should write your software in a language with a rich and strong type system,
one that is logically consistent or at least whose inconsistencies are well mapped and can be avoided,
one that is statically enforced by the compiler or at least
that you will systematically enforce socially.
Then you should use that type system to describe not just
records of elementary data types over the wire or on disk,
but all the rich entities within your software, their interactions and interrelations.
This will provide much more help with design and safety than any code-less methodology can.
And if you picked an OO-capable language like C++, Java, C# or Scala,
(or, with manually enforced dynamic types, Lisp, Ruby or Python),
you can actually use OO as you do it.

@section{What Object-Orientation @emph{is} — Informal Overview}

In this section we map out the important concepts of OO,
as developed in the rest of this essay.
The little explanations we offer are what is strictly necessary
to relate concepts to one another, and avoid misinterpreting them.
More details explanations, and justifications, will follow in subsequent sections.

@subsection{Extensible Modular Specifications}
Object-Orientation (“OO”) is a technique that enables the specification of programs
through extensible and modular @emph{partial} specifications,
embodied as entities @emph{within} a programming language:

@subsubsection{Partial specifications}
A program is made of many parts that can be written independently,
enabling division of labor,
as opposed to all logic being expressed in a single big monolithic loop@xnote["."]{
  The entire point of partial specifications is that they are not complete,
  and trying to “instantiate” them before all the information has been assembled should fail.
  Type systems and semantic frameworks incapable of dealing with such incomplete information
  are thereby incapable of apprehending OO.
}

@subsubsection{Modularity (Overview)}
A programmer can write or modify one part (or “module”)
while knowing very little information about the contents of other parts,
enabling specialization of tasks. Modularity is achieved by having modules
interact with each other through well-defined “interfaces” only,
as opposed to having to understand in detail the much larger contents
of the other modules so as to interact with them.

@subsubsection{Extensibility (Overview)}
A programmer can start from the existing specification and only need contribute
as little incremental information as possible when specifying a part
that modifies, extends, specializes or refines other parts,
as opposed to having to know, understand and repeat existing code almost in extenso
to make modifications to it.

@subsubsection{Internality}
Those partial programs and their incremental extensions
are entities @emph{inside} the language,
as opposed to merely files edited, preprocessed or generated @emph{outside} the language itself,
which can be done for any language.
@subsection{Prototypes and Classes}
@subsubsection{Prototype OO vs Class OO}
These in-language entities are called @emph{prototypes} if first-class
(manipulated at runtime, and specifying values, most usually records),
and @emph{classes} if second-class
(manipulated at compile-time only, and specifying types, most usually record types).
A language offers prototype-based object orientation (“Prototype OO”) if it has prototypes,
and class-based object orientation (“Class OO”) if it only has classes.

The first OO language used classes @~cite{Simula1967},
but the second one used prototypes @~cite{Winograd1975},
and some provide both @~cite{EcmaScript:15}.
Class OO is the more popular form of OO,
but the most popular OO language, JavaScript,
started with Prototype OO, with Class OO only added on top twenty years later.

@subsubsection{Classes as Prototypes for Types}
@principle{A class is a compile-time prototype for a type descriptor}:
a record of a type and accompanying type-specific methods,
or some meta-level representation thereof across stages of evaluation.
@;{ TODO see appendix XXX }

Class OO is therefore a special case of Prototype OO,
which is therefore the more general form of OO @~cite{Lieberman1986 poof2021}.
And indeed within Prototype OO, you can readily express
prototypes for runtime type descriptors for your language,
or prototypes for type descriptors for some other language you are processing as a meta-language.

In this paper, we will thus discuss the general case of OO,
and thus will seldom mention classes, that are a special case of prototypes,
except when specifically elucidating the relationship between Class OO and Prototype OO,
or mentioning existing systems that are somehow restricted in expressiveness
so they only support classes@xnote["."]{
  There would be plenty to discuss about classes and types from the point of view of
  library design and ecosystem growth, patterns that OO enables and antipatterns to avoid,
  tradeoffs between expressiveness and ease of static analysis, etc.
  There are plenty of existing books and papers on Class OO,
  OO software libraries, techniques to type and compile OO, to get inspiration from,
  both positive and negative.
  While some of the extent OO lore indeed only applies to defining classes,
  a whole lot of it easily generalizes to any use of inheritance,
  even without classes, even without prototypes at all,
  even when the authors fail to suspect any OO exists beyond Class OO.
}
Still our discussion of OO, and our exploration of inheritance in particular,
directly applies just as well to the special case that is Class OO.

@subsubsection{Classes in Dynamic and Static Languages}
Dynamic languages with reflection, such as Lisp, Smalltalk or JavaScript,
can blur the distinction between runtime and compile-time,
and thus between Prototype OO and Class OO.
Indeed many languages implement Class OO on top of Prototype OO
exactly that way, with classes being prototypes for type descriptors. @;{ TODO cite JavaScript ?}

Static languages, on the other hand, tend to have very restricted sublanguages at compile-time
with termination guarantees, such that their Class OO is vastly less expressive than Prototype OO,
in exchange for which it is amenable to somewhat easier static analysis.
A notable exception is C++, that has a full-fledged
Pure Functional Lazy Dynamically-Typed Prototype OO language at compile-time: templates.
On the other hand, Java and after it Scala are also Turing-universal at compile-time,
but not in an intentional way that enables practical metaprogramming,
only an unintentional way that defeats guarantees of termination
@~cite{grigore2016javagenericsturingcomplete}.

@subsection{More Fundamental than Prototypes and Classes}
@subsubsection{Specifications and Targets}
As we reconstruct the semantics of OO from first principles,
we will see that more so than prototype, class, object, or method,
@principle{the fundamental notions of OO are @emph{specification} and @emph{target}}:
@emph{target} computations are being specified by
extensible and modular, partial @emph{specifications}.

The target can be any kind of computation, returning any type of value.
In Prototype OO, the target most usually computes a record,
which offers simpler opportunities for modularity than other types.
In Class OO, the target is instead a record @emph{type},
or rather a type descriptor, record of a record type and its associated methods,
or of their compile-time or runtime representation.

A (partial or complete) specification is a piece of information that
(partially or completely) describes a computation.
Multiple specifications can be combined together into a larger specification.
From a complete specification, a target computation is specified that can be extracted.
A specification is modular inasmuch as it can define certain aspects of the target
while refering to other aspects to be defined in other specifications.
A specification is extensible inasmuch as it can refer to some aspects of the target
as partially defined so far by previous specifications, then amend or extend them.
In Prototype OO, access to this modular context and to the previous value being extended
are usually refered to through respective variables often named @c{self} and @c{super}.
In Class OO, such variables may also exist, but there is an extra layer of indirection
as they refer to an element of the target type rather than to the target itself.

@subsubsection[#:tag "PaC"]{Prototypes as Conflation}
A specification is not a prototype, not a class, not a type, not an object:
it is not even a value of the target domain.
It is not a record and does not have methods, fields, attributes or any such thing.

The target value in general is not an object either,
it’s just an arbitrary value of that arbitrary domain.
It needs not be any kind of record nor record type;
it needs not have been computed as the fixed-point of a specification;
indeed it is not tied to any specific way to compute it.
It cannot be inherited from (incrementally extended)
according to any of the design patterns that characterize OO.

Rather, @principle{a prototype
(a.k.a. “object” in Prototype OO, and “class” but not “object” in Class OO)
is the @emph{conflation} of a specification and its target},
i.e. an entity that depending on context at various times refers to
either the specification (when you extend it)
or its target (when you use its methods).

Formally speaking, a conflation can be seen as a cartesian product
with implicit casts to either component factor depending on context.
In the case of a prototype, you’ll implicitly refer to its specification
when speaking of extending it or inheriting from it;
and you’ll implicitly refer to its target when speaking of calling a method,
looking at its attributes, reading or writing a field, etc.

@subsubsection{Modularity of Conflation}

In first-class OO without conflation,
developers of reusable libraries are forced to decide in advance and in utter ignorance
which parts of a system they or someone else may want to either extend or query in the future.
Having to choose between specification and target at every potential extension point
leads to an exponential explosion of bad decisions to make:
Choosing target over specification everywhere would of course defeat extensibility;
but choosing specification over target for the sake of extensibility,
especially so without the shared computation cache afforded by conflation,
would lead to an exponential explosion of runtime re-evaluations.
By letting programmers defer a decision they lack information to make,
the conflation of specification and target is an essential pragmatic feature of Prototype OO:
it increases intertemporal cooperation between programmers
and their future collaborators (including their future selves)
without the need of communication between the two (that would require time-travel).
Thus, in first-class OO, @principle{Conflation Increases Modularity}.

This modularity advantage, however, is largely lost in
static languages with second-class Class OO["."]{
  Note that this does not include dynamic Class OO languages
  like Lisp, Smalltalk, Ruby or Python.
  In these languages, compilation can keep happening at runtime,
  and so programmers still benefit from Conflation.
}
In such languages, all class extensions happen at compile-time,
usually in a restricted language.
Only the type descriptors, targets of the specifications,
may still exist at runtime (if not inlined away).
The specifications as such are gone and cannot be composed or extended anymore.
Thus, you can always squint and pretend that conflation is only a figure of speech
used when describing the system from the outside
(which includes all the language documentation),
but not a feature of the system itself.
However, this also means that in these static OO languages,
conflation of specification and target is but a minor syntactic shortcut
with little to no semantic benefit.
Yet its major cost remain: the confusion it induces,
not just among novice developers, but also experts, and even language authors.

@subsubsection{Conflation and Confusion}

@principle{Conflation without Distinction is Confusion}.
Those who fail to distinguish between two very different concepts being conflated
will make categorical errors of using one concept or its properties
when the other should to be used.
Inconsistent theories will lead to bad choices.
Developers will pursue doomed designs they think are sound,
and reject rejecting actual solutions their theories reject.
@;{ TODO examples, citations }
At times, clever papers will offer overly simple solutions that deal with only one entity,
not noticing an actual solution needed to address two.
Seldom, heroic efforts will lead to overly complicated solutions
that correctly deal with both conflated entities at all times.
Either way, incapable of reasoning correctly about OO programs,
developers will amend bad theories with myriad exceptions and “practical” recipes,
rather than adopt a good theory that no one is offering.
Murky concepts will lead to bad tooling.
Confused developers will write subtle and persistent application bugs.
Researchers will waste years in absurd quests and publish nonsense along the way,
while fertile fields lay unexplored.
See the @secref{NNOOTT} regarding decades of confusion
between subtyping and subclassing due to
confusing target (subtyping) and specification (subclassing).
@;{TODO cite Meyer OOSC, see other section}

Still when you clearly tease the two notions apart,
and are aware of when they are being conflated for practical purposes,
so you can distinguish which of the two aspects should be invoked in which context,
then the semantics of OO becomes quite simple.
Shockingly, conflation was only first explicitly discussed in @~cite{poof2021} even though
(a) the concept is implicitly older than OO, going at least as far back as @~cite{hoare1965record},
and (b) the implementation of various Prototype OO systems has to explicitly accommodate for it
(see e.g. the @c{__unfix__} attribute in @~cite{nix2015})
even when the documentation is silent about it.

@; TODO example Nix specification?

@subsection{Objects}
@subsubsection{An Ambiguous Word}
In Prototype OO, a prototype, conflation of a specification and its target,
is also called an “object” or at times “instance”, especially if the target is a record.
Note that laziness is essential in computing the target record or its entries,
since most specifications, being partial, do not specify
a complete computation that terminates without error in finite time,
yet this expected non-termination should not prevent the use
of the conflated entity, to extract and extend its specification.

In Class OO, a prototype, conflation of a specification and its target,
is instead called a “class”,
and the target is specifically a type descriptor rather than an arbitrary record or non-record value.
What in Class OO is called an “object” or “instance” is an element of a target type as described.
Regular prototype fields and methods are called “class fields” or “class methods”
(or “static” fields and methods, after the keyword used in C++ and Java)—but
remember they only involve the target type, not the specification.
“Object methods” are semantically regular methods that take one implicit argument in front,
the object (i.e. element of the target type).
“Object fields” are regular fields of the object as a record.

Finally, many languages, systems, databases, articles or books call “object” some or all
of the regular runtime values they manipulate, that may or may not be records,
and that are in no way part of an actual OO system extensible with inheritance,
at least none available to the user, whether Prototype OO or Class OO.
The authors will not usually claim that these objects are part of an OO framework
actual or imagined, but then again sometimes they may.

@subsubsection{OO without Objects}

Therefore, @principle{the word “object” is practically useless when talking of OO in general},
being too ambiguous absent the context of a specific language, system, document, etc.
Furthermore, the fundamental patterns of OO can exist and be usefully leveraged in a language
that lacks any notion of object, merely with the notions of specification and target:
Indeed, Yale T Scheme has a class-less “object system” @~cite{adams88oopscheme},
wherein the authors call “objects” any language value,
and “instance” those records of multiple function entry points used as the non-extensible targets
of their extensible specifications, themselves called “components”,
that use mixin inheritance.

To avoid confusion, we will be careful in this article to only speak of
“specification”, “target”, “prototype”, and (target type) “element”
and to avoid the word “object”—an unnecessary notion as well as a useless word.
As already mentioned above, we will also avoid the word “class” unless necessary,
since it is a special case of the more general prototypes we are studying.

This is all particularly ironic when the field we are studying is called “Object Orientation”,
in which the most popular variant involves classes.
But fields of knowledge are usually named as soon as the need is felt
to distinguish them from other fields,
long before they are well-understood, and thus based on misunderstandings;
this misnomer is thus par for the course@xnote["."]{
  The wider field of study is similarly misnamed, as per the famous E. W. Dijkstra quote:
  “Computer Science is no more about computers than astronomy is about telescopes.”
  Hal Abelson would add that it is not a science, either.
}

@subsection{Inheritance Overview}
@subsubsection{Inheritance as Modular Extension of Specifications}
Inheritance is the mechanism by which partial modular specifications are
incrementally extended into larger modular specifications,
until the point where a complete specification is obtained
from which the specified target computation can be extracted.

There have historically been three main variants of inheritance,
with each object system using a variation on one or two of them:
single inheritance, multiple inheritance and mixin inheritance.
For historical reasons, we may speak of classes, subclasses and superclasses
in this subsection, especially when discussing previous systems using those terms;
but when discussing the general case of prototypes and specifications,
and we will prefer speaking of specifications and their
parents, ancestors, children and descendents (also specifications).

@subsubsection{Single Inheritance Overview}

Historically, the first inheritance mechanism discovered was @emph{single inheritance},
though it was not known by that name until a decade later.
In an influential paper@~cite{hoare1965record},
Hoare introduced the notions of “class” and “subclass” of records
(as well as, infamously, the @c{null} pointer).
The first implementation of the concept appeared in Simula 67 @~cite{Simula1967}.
Alan Kay later dubbed this mechanism “single inheritance” and adopted it for Smalltalk 76
as a compromise instead of the more general but then less well understood multiple inheritance
@~cite{kay1996early}. @; TODO or was is Bobrow who named it? Do a deeper search.
Many other languages adopted it after Smalltalk,
including Java that made it especially popular circa 1995. @;{@~cite{}. @TODO{or C#}}

In Simula, a class is defined starting from a previous class as a “prefix”.
The effective text of a class (hence its semantics) is then the “concatenation”
of the direct text of all its transitive @emph{prefix classes},
including all the field definitions, method functions and initialization actions,
in order from least specific superclass to most specific@xnote["."]{
  SIMULA later on also allows each class or procedure
  to define a “suffix” as well as a “prefix”,
  wherein the body of each subclass or subprocedure
  is the “inner” part between this prefix and suffix,
  marked by the @c{inner} keyword as a placeholder.
  This approach by SIMULA and its successor BETA @~cite{kristensen1987beta}
  (that generalized classes to “patterns” that also covered method definitions the same way)
  is in sharp contrast with how inheritance is done in about all other languages,
  that copy Smalltalk.
  The “prefix” makes sense to initialize variables, and to allow procedure definitions
  to be overridden by latter more specialized definitions;
  the “suffix” is sufficient to do cleanups and post-processing,
  especially when procedures store their working return value
  in a variable with the same name as the procedure being defined, in ALGOL style;
  the entire “inner” setup also makes sense in the context of spaghetti code with GOTOs,
  before Dijkstra made everyone consider them harmful in 1968.
  But frankly, it is both limited and horribly complex to use in the post-1968 context
  of structured code blocks, not to mention post-1970s higher-order functions, etc.,
  even though in BETA you can still express what you want in a round-about way
  by explicitly building a list or nesting of higher-order functions
  that re-invert control back the way everyone else does it,
  that you call at the end.
  And so, while Simula was definitely a breakthrough, its particular form of inheritance
  was also a dead-end;
  no one but the Simula inventors want anything resembling @c{inner} for their language;
  no other known language uses it,
  although it is easily expressible using user-defined method combinations
  in e.g. CLOS @~cite{bobrow88clos}.
  Thus we can rightfully say that inheritance, and OO,
  were only invented and named but with Alan Kay’s Smalltalk 76.
  In the end, Simula should count as a precursor to OO, or at best an early draft of it,
  but either way, not the real, fully-formed concept.
  Dahl and Nygaard did not invent OO as we know it
  anymore than Columbus discovered the continent of America;
  still they made the key discovery that later made OO possible.
}
In modern terms, we call the prefix a superclass,
or more generally, a parent, or, when considering transitive parents, an ancestor. @; TODO CITE
We call @emph{inheritance hierarchy} of a specification,
the set of its ancestors;
when using single inheritance, this hierarchy then constitutes a list,
and the ancestor relation is a total order.

Single inheritance is easy to implement without higher-order functions:
method lookup can be compiled into a simple and efficient array lookup at a fixed index
— as opposed to some variant of hash-table lookup in the general case
for mixin inheritance or multiple inheritance.
Moreover, calls to the “super method” also have a simple obvious meaning.
In olden days, when resources were scarce and before FP was mature,
these features made single inheritance more popular
than the more expressive but costlier and less understood alternatives.

Even today, most languages that support OO only support single inheritance, for its simplicity.
@;{TODO cite}

@subsubsection[#:tag "MULIO"]{Multiple Inheritance Overview}

Discovered a few years later, and initially just called @emph{inheritance},
in what in retrospect was prototype OO, in KRL @~cite{Winograd1975 Bobrow1976},
multiple inheritance allows a specification (frame, class, prototype, etc.)
to have multiple direct parents.
The notion of (multiple) inheritance thus predates Smalltalk-76 @~cite{Smalltalk78}
adopting the term, retroactively applying it to SIMULA,
and subsequently inventing the terms “single” and “multiple” inheritance
to distinguish the two approaches as well as recognize their commonality.

Although some more early systems @~cite{Kahn1976 Borning1977 Traits Borning1982Multiple}
used multiple inheritance,
@principle{multiple inheritance only became usable with the epochal system Flavors}
@~cite{Cannon1979}
and successor Lisp object systems Loops@~cite{bobrow1983loops},
New Flavors@~cite{Moon1986Flavors}, CommonLoops@~cite{bobrow86commonloops}
and CLOS@~cite{bobrow88clos cltl2}.
Since then, many languages including Ruby, Perl, Python and Scala
correctly adopted the basic design of Flavors (though none of its more advanced features)—we
will call them @emph{flavorful} (our word).
On the other hand, influential or popular languages including Smalltalk, Self, C++ and Ada
failed to learn from Flavors and got multiple inheritance largely wrong—we
will call them @emph{flavorless} (idem).

With multiple inheritance, the structure of a specification and its ancestors is
a Directed Acyclic Graph (“DAG”).
The set of all classes is also a DAG, the subclass relation is a partial order.
Most OO systems include a common system-wide base class
at the end of their DAG; but it is possible to do without one.

The proper semantics for a specification inheriting multiple different methods
from a non-linear ancestry proved tricky to get just right.
The older (1976) “flavorless” viewpoint sees it as a conflict between methods you must override.
The newer (1979) “flavorful” viewpoint seeing it as a cooperation between methods you can combine.
Unhappily, too many in both academia and industry are stuck in 1976,
and haven’t even heard of the flavorful viewpoint, or, when they have, still don’t understand it.
For this reason, despite it being more expressive and more modular than single inheritance,
flavorful multiple inheritance still isn’t as widely adopted.@xnote["."]{
  Out of the top 50 most popular languages in the TIOBE index 2025, @;{TODO cite}
  6 support flavorful multiple inheritance (Python, Perl, Ruby, Lisp, Scala, Solidity),
  3 only support flavorless multiple inheritance (C++, Ada, PHP),
  2 require a non-standard library to support multiple inheritance (JavaScript, Lua),
  17 support single inheritance only (Java, C#, VB, Delphi, R, MATLAB, Rust, COBOL, Kotlin, Swift, SAS, Dart, Julia, TypeScript, ObjC, ABAP, D),
  and the rest don’t support inheritance at all (C, Go, Fortran, SQL, Assembly, Scratch, Prolog, Haskell, FoxPro, GAMC, PL/SQL, V, Bash, PowerShell, ML, Elixir, Awk, X++, LabView, Erlang).
}

@subsubsection[#:tag "MIXIO"]{Mixin Inheritance Overview}

Mixin inheritance was discovered last @~cite{bracha1990mixin},
probably because it relies on a more abstract pure functional view of OO;
maybe also because it was one of the first successful attempts at elucidating inheritance
in the paradigm of programming language semantics,
when the concept had previously been developed in paradigm of computing systems @~cite{gabriel2012}.
Yet, for the same reasons,
@principle{Mixin Inheritance is more fundamental than the other two variants}.
It is the simplest kind of inheritance to formalize @emph{given the basis of FP},
in a couple of higher-order functions.
Specifications are simple functions, inheritance is just chaining them, and
extracting their target computation is just computing their fixpoint.
Mixin inheritance also maps directly to the concepts
of Modularity and Extensibility we are discussing,
and for these reasons we will study it first when presenting a formal semantics of OO
(@secref{MOO}).

Mixins equipped with a binary inheritance operator constitute a monoid
(associative with neutral element),
and the inheritance structure of a specification is just
the flattened list of elementary specifications chained into it,
as simple as single inheritance.
Mixin inheritance works better at runtime, either with Prototype OO,
or for Class OO in a dynamic and somewhat reflective system.

Mixin inheritance is in some way simpler than single inheritance
(but only if you understand FP yet are not bound by limitations of most of today’s FP type systems),
and as expressive as multiple inheritance
(arguably slightly more, though not in a practically meaningful way),
but is less modular than multiple inheritance because it doesn’t automatically handle
transitive dependencies but forces developers to handle them manually,
effectively making those transitive dependencies part of a specification’s interface.

For all these reasons adoption of Mixin Inheritance remains relatively limited,
to languages like
StrongTalk @~cite{Bracha1993Strongtalk Bak2002Mixins},
Racket @~cite{Mixins1998 Flatt2006Mixins},
Newspeak @~cite{bracha2008newspeak},
GCL @~cite{gclviewer2008},
Jsonnet @~cite{jsonnet},
and Nix @~cite{nix2015}.
Yet it still has outsized outreach, for just the use of GCL at Google means
a large part of the world computing infrastructure
is built upon configurations written using mixin inheritance.
One may also construe the way C++ non-“virtual” repeated superclasses
as a form of mixin inheritance with automatic renaming,
at which point Mixin Inheritance is actually very popular, just not well-understood.

@subsubsection{False dichotomy between inheritance and delegation}
Many authors have called “delegation” the mechanism used by Prototype OO,
@; TODO CITE SELF, Castagna Cardelli 1996, …
as distinct from the “inheritance” mechanism of Class OO.
However, Lieberman, in one of the papers that popularized this dichotomy@~cite{Lieberman1986},
discusses the two joined concepts of prototype-delegation vs class-inheritance@xnote[","]{
Lieberman also explores at length the philosophical underpinnings of these two idea-complexes,
which is quite interesting from a historical and psychological point of view,
but has little relevance to a technical discussion on the semantics of OO,
its proper use or implementation decades later.
}
and explains in detail how classes and their “inheritance” can be expressed
as a special case of prototypes and their “delegation”,
while classes cannot express prototypes.
He gives multiple examples of behaviors expressible with prototypes but not with classes,
wherein prototypes enable dynamic extension of individual “objects” (prototypes) at runtime,
while classes only allow extension at compile-time, and only
for an entire type (“class”) of “objects” (elements of the type).
But while it is extremely important indeed to understand the distinction and the relationship
between the general notion of prototypes from the special case of classes,
it only begets confusion to treat inheritance and delegation
as separate concepts when they have identical concerns of semantics, implementation or performance,
whether used for prototypes versus classes.

One confounding factor is that of mutable state.
Early OO, just like early FP, was usually part of systems with ubiquitous mutable state;
prototype inheritance (or “delegation”) algorithms thus often explicitly allow or cope with
interaction with such state, including
mutation and sharing or non-sharing of per-object or per-class variables,
and especially tricky, mutation and sharing of a prototype’s inheritance structure.
However, class systems often had all their inheritance semantics resolved at compile-time,
during which there is no interaction with user-visible side-effects, and
it doesn’t matter whether the compiler does or doesn’t itself use mutable state:
from the user point of view it is as if it were pure functional and there is no mutation
in the inheritance structure or state-sharing structure of classes,
at least not without using “magic” reflection primitives.
One may then have been tempted then to see prototype delegation as intrinsically stateful,
and class inheritance as intrinsically pure (though at compile-time).

Yet, recent pure functional Prototype OO systems @~cite{jsonnet nix2015 poof2021}
prove constructively that prototypes can be pure, and that they use
the very same inheritance mechanisms as classes,
indeed with classes as a particular case of prototypes with the usual construction.
Meanwhile, old reflective Class OO systems like Lisp and Smalltalk
@~cite{Kahn1976 kay1996early gabriel1991clos AMOP}
also support mutable state to modify the inheritance structure at runtime,
for the sake of dynamic redefinition of classes at runtime,
in what remains semantically a pure functional model once when the structure is set.
See how in CLOS you can define methods on generic function
@c{update-instance-for-redefined-class} to control how data is preserved, dropped or transformed
when a class is redefined. @;{TODO XXX @~cite{}}
Mutable state and mutable inheritance structure in particular are therefore
clearly an independent issue from prototypes vs classes,
though it might not have been obvious the time.
As we introduce formal models of OO, we will start with pure functional models, and
will only discuss the confounding matter of side-effects much later@xnote["."]{
  It might be interesting to explain @emph{why} previous authors failed so hard to
  identify delegation and inheritance, when the similarities are frankly obvious,
  and the relationship between classes and prototypes is well-known
  to anyone who implemented classes atop prototypes.
  But lacking direct access to those authors’ brains, our explanations must remain speculative.

  First, pioneers are eager to conceptualize and present their experiments as original
  and not just the same concept in a different context.
  They necessarily have to sell their ideas as historical package deals,
  before the underlying concepts are clearly identified and separated from each other.
  They are too close to the matter to tell which of the features they built would be immortalized
  through the ages as fundamental concepts
  vs just contingent implementation details soon to be forgotten.
  In the brief time that publishing about Prototypes was trendy,
  scientists studying pioneering works may have focused too much
  on the specifics of Actors, SELF, or other successful Prototype language du jour,
  and failed to properly conceptualize a general notion of Prototype.
  Unlike the pioneers themselves, they deserve blame for their myopia,
  and so do the followers who cite and repeat their “findings” without criticism.
  However this explanation is not specific to the topic at hand,
  and is valid for every field of knowledge.

  Second, and with more specificity to Prototypes,
  Computer Scientists following the Programming Language (PL) paradigm@~cite{gabriel2012}
  might have been incapable of unifying the prototypes and classes
  when delegation happens at runtime while inheritance happens at compile-time:
  not only does the machinery look very different to users and somewhat different as implementers,
  written in different languages with different formalisms,
  but PL people tend to deeply compartmentalize the two.
  They may have looked at low-level mutable state
  (omnipresent in any practical language until the mid-2000s)
  as essential when happening at runtime,
  when they could clearly conceptualize it away as an implementation detail
  when happening at compile-time.
  Systems paradigm people (including the old Lisp, Smalltalk and SELF communities)
  who freely mix or interleave runtime and compile-time in the very same language,
  might have had no trouble unifying the two across evaluation times,
  but they tend not to publish articles about PL semantics,
  and not to be read and understood by PL semanticians when they do.

  Revisiting these topics several decades after they were in vogue,
  and finding their then-treatment lacking, with errors from the time still uncorrected to this day,
  makes me wonder about what other false ideas I, like most people, assume are true
  in all the other topics I haven’t revisited, whether in Computer Science or not,
  where I just blindly assume the “experts” to be correct due to Gell-Mann amnesia.
}

As for which words to keep, the word “inheritance” was used first for the general concept,
in a language with “prototypes”, KRL @~cite{Winograd1975}.
The word “delegation” stems from the Actor message-passing model,
and is both later and less general,
from after the words “inheritance” and “prototypes” were better established,
and is strongly connoted to specific implementations using the message-passing paradigm.
It also fell out of fashion some time in the 1990s,
after JavaScript became a worldwide phenomenon, and (correctly) used the term “inheritance”
rather than delegation (as it isn’t particularly “message passing”, just calling functions).
@;{TODO cite ECMA-262 ECMAScript, 1st edition 1997 https://ecma-international.org/publications-and-standards/standards/ecma-262/}

@subsection{Epistemological Digression}
Many people will inevitably quibble about our definition or characterization of OO.
@itemlist[#:style'ordered
@item{Is our definition correct?}
@item{What does it even mean for a definition to be correct?
Aren’t definitions “just” arbitrary conventions?}
@item{Is there an authority on those words?}
@item{Shouldn’t we use the same definition as Alan Kay,
who invented the expression “Object Oriented Programming”?}
@item{Shouldn’t we just let others define “OO” however they want,
and stick to “inheritance” when discussing the field characterized by the use of inheritance?}]

Though a treatise of epistemology is beyond the scope of this article, @;{TODO cite}
we can briefly answer these questions as follow:
@itemlist[#:style'ordered
@item{Yes, our definition is correct:
it accurately identifies what people mean usually by those words,
and distinguishes situations where they apply from situations where they do not,
in the contexts that people care about.}
@item{No, the phenomena that effectively affect people,
that they care to name, discuss, think about and act on, are not arbitrary.
Thus the important part of definitions isn’t convention at all:
it is the structure and understanding of these phenomena, rather than the labels used for them.
A correct definition precisely identifies the concepts that are relevant to people’s concerns,
that help them make better decisions that improve their lives,
whereas an incorrect definition misleads them into counterproductive choices.}
@item{No, there is no authority on software vocabulary, person or committee,
that can decree different words for others to use,
even less so different phenomena for others to care about.
And people care about OO even if you try to change the name for it,
or denature the name “OO” not to identify the same phenomenon anymore;
they will keep caring about the same phenomenon under a different name,
rather than care about whatever those who corrupt the name may want them to.}
@item{Not quite. We should definitely listen to Alan Kay@xnote[","]{
  Alan Kay, who coined the term “object-oriented programming” in 1967, explains in 2003 @~cite{Kay2003}
  that “OOP to me means only messaging, local retention and protection and hiding of state-process,
  and extreme late-binding of all things”.

  Our interpretation is that the first part of this definition (until the last comma)
  corresponds to modularity, the ability to think about programs in terms of separate
  “local” entities each with its own “state-process” wherein interactions only happen
  through well-delimited interfaces (“messaging”).
  The second part “extreme late-binding of all things” indirectly references
  the in-language and extensible aspect of modules:
  extreme late-binding means that the value of those units of modularity may change at runtime,
  which means not only dynamic dispatch of method invocation
  depending on the runtime class of an object,
  but also the ability to dynamically
  define, incrementally extend, refine or combine those units in the language.
  Those units may be first-class prototypes, and even when they are only second-class classes,
  there is a first-class reflection mechanism to define and modify them.
  When this extensibility is only available at compile-time,
  as in the object system of many static languages, then
  the OOP only happens in the meta-language (as in e.g. C++ templates),
  or the language lacks complete support for OOP.

  Note that Kay didn’t immediately adopt SIMULA’s inheritance mechanism in Smalltalk-72
  (it wasn’t called that yet in SIMULA, either);
  but he did adopt it eventually in Smalltalk-76, and
  this adoption is what launched OO as a phenomenon.
  Kay stated adopting single inheritance over multiple inheritance
  was a compromise @~cite{kay1996early},
  yet still didn’t adopt multiple inheritance later, though his team experimented with it.
  More broadly, Kay didn’t endorse any specific inheritance mechanism,
  and never focused on that part of the design. To Kay it was only a means to an end,
  which is what Kay called “extreme late binding”: the fact that behavior definition
  happens and takes effect dynamically up to the last moment based on values computed at runtime.
  Inheritance, the practical means behind the late behavior definition that is late bound,
  and the precise form it takes, is secondary to Kay;
  what matters to Kay is the role it plays in enabling dynamic code specialization.
  But inheritance becomes a primary concern to us as we formalize the concepts behind OO,
  and refine the intuitions of a pioneer into codified knowledge after decades of practice.
  And if other means are found to arguably satisfy Kay’s “extreme late binding”,
  then we’ll have to give them a name that distinguishes them from what is now called OO.
}
and to other pioneers and experts;
but none is an ultimate authority, and
we should look at what the pioneers and experts do and not (just) what they say,
especially since you could recursively apply
the same skepticism to the words they use in their definitions.
Also, be wary that pioneers provide inspiration, insight and discoveries,
but seldom well-rounded neatly-conceptualized theories:
often these theories arise only after lots of
experience, filtering, and reformulation@xnote["."]{
As the jest goes, “Every technique is first developed,
then used, important, obsolete, normalized, and finally understood.”
}}
@item{No, it is no good to let an ignorant majority “define” the term “Object-Orientation”
to mean what little they know of it—for instance, to pick the most popular elements:
Class OO only, always mutable records,
only single-inheritance or C++ style flavorless “multiple inheritance”,
single dispatch, no method combination, etc.
Letting those who don’t know and don’t care define technical words
would be knowledge bowing to ignorance;
it would be for those who know and care to abdicate their responsibility
and follow the masses when they should instead lead them;
it would be ceding terrain to the Enemy—snake oil salesmen, chaosmongers,
corrupters of language, manipulators, proud spreaders of ignorance, etc.—who if let loose
would endlessly destroy the value of language and make clear meaning incommunicable.
Beside, if you retreat to “inheritance” in the hope that at least that for that term
you can get people to agree on a clear unambiguous meaning@xnote[","]{
  The term “inheritance” is already corrupted, since Goguen uses it at times to mean refinement,
  and others use it to mean the (non-modular) extension of database tables or equivalent.
  And there are plenty of legitimate non-OO uses of the word “inherit”, to
  mean that some entity derives some property from a historical origin, an enclosing context, etc.
}
you’ll find that if you
have any success defining a useful term that way, the soldiers of entropy will rush
to try to defile it in very proportion to your success;
you will have given up precious lexical real estate for no gain whatsoever,
only terrible loss@xnote["."]{
  Indeed, if you don’t know to stand your ground, you will constantly retreat,
  and be made to use ever increasingly flowery “politically correct” vocabulary
  as a humiliation ritual before those who will wantonly take your words away
  to abuse you and thereby assert their dominance over you.
}}]

So what phenomena count as OO?
The design patterns used by programmers when they write code in an OO language;
the interactions they have with computers and with each other.
the decision trees that are enabled or disabled when evolving a program into another.
In the case of OO, these phenomena are what is captured by
the intralinguistic extensible modularity as defined above:
(a) the ability to “code against an interface” and
pass any value of any type that satisfies the interface
(modularity, be it following structural or nominative rules),
(b) the ability to extend and specialize existing code by creating a new entity
that “inherits” the properties of existing entities and only needs specify
additions and overrides in their behavior rather than repeat their specifications,
wherein each extension can modularly refer to functionality defined
in other yet-unapplied extensions; and
(c) the fact that these entities and the primitives to define, use and specialize them
exist @emph{within} the programming language rather than as an external preprocessing layer.

We contend that the above is what usually meant by OO,
that matches the variety of OO languages and systems
without including systems that are decidedly not OO like Erlang, SML or UML.
Whatever clear or murky correspondance between names and concepts others may use,
this paradigm is what we will call OO—it is what we will discuss in this article,
and systematically reduce to elementary concepts.

@section{OO as Internal Extensible Modularity}
@subsection{Modularity}

@subsubsection{Division of Labor}

Modularity@~cite{Parnas1972 Dennis1975} is the organization of software source code
in order to support division of labor, dividing it into “modules” that can each be
understood and worked on mostly independently from other modules,
by one or multiple developers over time.
The “interface” of each module defines a semi-formal contract between the users and implementers
of the module, such that users need only know and understand the interface to enjoy the functionality
of the module, whereas the implementers have freedom to build and modify the module internals
as long as they satisfy the interface.


@subsubsection{First- to Fourth-class, Internal or External}

A few languages offer a builtin notion of modules as @emph{first-class} entities,
that can be manipulated as values at runtime.
But popular modern programming languages usually only offer
@emph{some} builtin notion of modules as @emph{second-class} entities,
entities that exist at compile-time but are not available as regular runtime values@xnote["."]{
  In between the two, some languages offer a “reflection” API that gives some often limited
  runtime access to representations of the module entities.
  This API is often limited to introspection only or mostly;
  for instance, it won’t normally let you call the compiler
  to define new modules or the linker to load them.
  Yet some languages support APIs to dynamically evaluate code,
  that can be used to define new modules;
  and some clever hackers find ways to call a compiler and dynamic linker,
  even in languages that don’t otherwise provide support APIs for it. @; TODO cite Goo
}
Either first-class or second-class entities are considered @emph{internal} to the language,
part of its semantics, handled by its processors (compiler, interpreter, type system, etc.).

However many languages offer no such internal notion of modules.
Indeed modules are a complex and costly feature to design and implement,
and few language designers and implementers will expend the necessary efforts toward it
at the start of language’s development;
only the few that have success and see their codebase grow
in size and complexity generally bother@xnote["."]{
  Unless they develop their language within an existing modular framework
  for language-oriented programming, such as Racket,
  @;TODO{cite. Also Stratego http://strategoxt.org/ ? Pypy https://pypy.org/ ?}
  from which they inherit the module system.
}

Now modularity is foremost a @emph{meta-linguistic} concept:
Even in a language that provides no support whatsoever for modules
@emph{within} the language itself (such as C),
programmers will find means to express modules as @emph{third-class} entities,
automated by tools @emph{outside} the language:
a preprocessor, an object file linker, editor macros, “wizards” or LLMs.
And even if they somehow don’t because they can’t use or can’t afford to use such automation,
developers may achieve modules as @emph{fourth-class} entities,
ones that they handle manually, with design patterns, editors, copy-paste, and lots of debugging.
Either third-class or fourth-class entities are considered @emph{external} to the language,
not part of its semantics, not handled by its processors,
yet conceptually present in the minds of the programmers@xnote["."]{
  Whereas the terms “first-class” and “second-class” are well-established in the literature,
  @;{TODO cite}
  we are hereby introducing the terms “third-class” and “fourth-class”,
  as well as the distinction between “internal” and “external” entities.
  Of course, depending on where you draw the line for “the language”,
  the very same entity processed by the very same tools may shift between these classes:
  thus, in C++ classes are second-class entities in;
  but if the language being considered is C, and C++ is seen as an external metalanguage
  (as was the case in the original implementation @c{cfront} of C++,
  a preprocessor that generated C code),
  then the very same classes are third-class entities for C;
  and if @c{cfront} were updated to support templates, classes would be first-class entities
  for the compile-time C++ template language;
  and if done by hand as a set of conventions on top of C, they would be fourth-class entities.
}
Thus, programmers will:
@itemize[
@item{copy and paste sections of code, or prefix identifiers, as poor man’s modules;}
@item{preprocess files to concatenate, transform and generate code fragments;}
@item{transclude “include” files to serve as libraries or interfaces to later-linked libraries;}
@item{divide code in files they independently compile then “link” or “load” together;}
@item{orchestrate incremental (re)building of software with utilities such as “make”;}
@item{bundle software into “packages” they exchange and distribute with “package managers”;}
@item{“containerize” consistent package installations into “images”.}]

When for the sake of “simplicity”, “elegance”, or ease of development or maintenance,
support for modularity is left out of a language,
or of an ecosystem around one or multiple languages,
this language or ecosystem becomes but
the weak kernel of a haphazard collection of tools and design patterns cobbled together
to palliate this lack of internal modularity with external modularity.
The result inevitably ends up growing increasingly
complex, ugly, and hard to use, develop and maintain.

@subsubsection[#:tag "CfM"]{Criterion for Modularity}
@principle{A design is more modular if it enables developers to cooperate more while coordinating less}
compared to alternative designs that enable less cooperation or require more coordination,
given some goals for developers, a space of changes they may be expected to enact in the future, etc.
More ability to code one’s part, while requiring less knowledge about other people’s parts.

For instance, the object-oriented design of the Common Lisp build system
ASDF@~cite{ASDF2 ASDF3}
made it simple to configure, to extend, and
to refactor to use algorithms in @emph{O(n)} rather than @emph{O(n³)} or worse,
all of it without any of the clients having to change their code.
This makes it arguably more modular than its predecessor MK-DEFSYSTEM@~cite{kantrowitz1991}
that shunned use of objects (possibly for portability reasons at the time),
was notably hard to configure, and resisted several attempts to extend or refactor it.

Note that while external modularity enables cooperation during development,
internal modularity also enables cooperation during deployment,
with conditional selection and reconfiguration of modules,
gradual deployment of features, user-defined composition of modules,
automated A/B testing, and more.

For a more complete theory of Modularity, see @citet{ngnghm9}.

@subsubsection{Historical Modularity Breakthroughs}

Programmers developers these days are very aware of files, and file hierarchies,
as units of modularity they have to think about quite often,
naming, opening, visiting and closing them in their editors,
manipulating them in their source control, etc.
Although files as such embody a form of modularity external
to most simple programming languages that they use,
the more advanced programming languages, that they use most often,
tend to have some notion of those files,
runtime and/or compile-time representation for those files,
and often some correspondence between file names and names of internal module entities
that result from compiling those files.
In other words, for most languages that matter,
files embody modularity internal to programming languages@xnote["."]{
  Note that while a file may be a unit of modularity, this is not always the case:
  often, actual modules span multiple files;
  at other times, there may be multiple modules in a single file.
  For instance, compiling a C program typically requires each file to be registered
  into some “build” file (e.g. a @c{Makefile}) to be considered as part of the software;
  a C library also comes with an accompanying “header” file, though there need not be
  a 1-to-1 correspondence between C source files and C header files
  (as contrasted with source and interface files in OCaml).
  A C “module” therefore typically spans multiple files,
  and some files (headers and build files) can have bits from multiple modules.
  Of course, having to manually ensure consistency of information between multiple files
  makes the setup less modular than a language that doesn’t require as much maintenance.
  Then again, the C language also recently adopted some notion of namespace,
  which, though it doesn’t seem used that much in practice,
  can constitute yet another notion of modules, several of which can be present in a file.
  At a bigger scale, groups of files in one or multiple directories may constitute a library,
  and a “package” in a software “distribution” may include one or several libraries, etc.
  Thus, Even within a single language, there can be many notions of modularity
  at several different scales of software construction,
  enabling division of labor across time for a single person, for a small team, for a large team,
  between several teams, etc., each time with different correspondences
  between modules and files, that are never quite 1-to-1:
  even “just a file” is not merely a blob of data, but also the meta-data of
  a filename registered in a broader namespace, and some intended usage.
}

Now, while files go back to the 1950s and hierarchical filesystems to the 1960s,
@; First filesystem??? IBM disk???
@; Semi-Hierarchical: ERMA Mark 1 1958 https://dl.acm.org/doi/10.1145/1458043.1458058 - but 100 fixed categories
@; Hierarchical: Multics 1965 https://www.multicians.org/fjcc4.html OS/360, Burroughs B5000
there was an even earlier breakthrough in modularity,
so pervasive that programmers use it without even thinking about using it:
subroutines,
already conceptualized by Ada Lovelace as “operations”@~cite{Lovelace:1843:NTL}, @;
@; that could be “subsidiary”,
@;{Scientific Memoirs, Selected from the Transactions of Foreign Academies of Science and Learned Societies, and from Foreign Journals, R. and J. E. Taylor , Vol III, 1843, L. F. Menabrea on Babbage’s Analytical Engine. Notes of the translator (= Augusta Ada (King, Countess of) Lovelace, A.A.L. not A.L.L.)
  Note A p693 "operation"… general notion of pure computation from between
  "First the symbols of *operation* are frequently *also* the symbols of the *results* of operations.
  We say that symbols are apt to have both a *retrospective* and a *prospective* signification. […]"
  [[[past vs present/future; passive vs active; values vs continuations]]]
  Secondly […] the symbols of *numerical magnitude*, are frequently *also* the symbols of *operations*,
  as when they are the indices of powers."
  [[[She already has to deal with confusion between ambiguous terms!]]]
  [[[She anticipates symbolic computations… "symbolical results|data" (outputs|inputs) vs "numerical"]]]
  [[[She calls computer science "the science of operations"]]]
  [[[She understands that infinite programmability (no word for it) is what the Analytical engine vastly
     superior to the finite-configuration Difference Engine,
     making it "the executive right hand of abstract algebra"]]]
} formalized early on by Wilkes et al. in the 1940s @; TODO cite
used by the earliest programmers in manually assembled code,
part of the first programming language Plankalkül, @; TODO cite
and of FORTRAN II. @; TODO cite
Subroutines allow programmers to divide bigger problems into smaller ones
at a fine grain, that can each be solved by a different programmer,
while other programmers only have to know its calling convention.

No less important than subroutines as such yet also taken for granted
was the concept of a return address allowing
calls to a single copy of a subroutine from multiple sites,
@; TODO CITE Turing 1946 report on ACE; implemented by Wilkes
and later of a call stack of such return addresses and other caller and callee data.
The call stack enabled reentrancy of subroutines,
such that a subroutine can be recursively called by a subroutine it itself called;
@; that only became common after LISP and ALGOL in the late 1950s.
@; TODO cite McCarthy LISP 1957, Dijkstra ALGOL 1958
thus, programmers do not have to care that their subroutines
should not be reentered while being executed
due to a direct or indirect subroutine call.
Indeed, subroutines can now be reentered, not just accidentally, but intentionally,
to express simpler recursive solutions to problems.
The ability to cooperate more with less coordination
with programmers from other parts of the software:
that’s the very definition of modularity.

Sadly, the generalization of call stacks to first-class continuations in the 1970s,
@; TODO cite Steve Russell 1960, van Wijngaarden 1964, Landin 1964, Strachey 1974, Steele 1978
and later to delimited control in 1988 and beyond @; TODO cite Felleisen 1988
are still not available @; TODO cite
in most programming languages, eschewing another progress in modularity,
wherein programmers could otherwise abstract over the execution of some fragment of code
without having to worry about transformations to their control structure,
e.g. for the sake of non-deterministic search, @; TODO cite
robust replaying of code in case of failure, @; TODO cite
security checks of code behavior, @; TODO cite
dynamic discovery and management of side-effects, etc. @;{TODO cite}

On a different dimension,
separately compiled object files, as implemented by FORTRAN (1956), @; TODO cite
provides third-class modularity through an external linker.
Later developments like
typechecked second-class Modules a la Modula-2 (1978), @; TODO cite Wirth;
@; TODO also look into Liskov’s CLU, Mary Shaws’ Alphart, etc.
Higher-Order Modules a la ML (1985), @; TODO cite McQueen 1985
typeclasses a la Haskell (or traits in Rust),
interfaces a la Java, and much more,
@; TODO cite
provide second-class modularity as part of a language itself.

@;{  objects in Smalltalk 72, etc.
  First-class modules in ML (199x?), etc.
  provide first-class modularity. }

These are all examples of modularity that don’t involve extensibility as such.
By combining modularity with extensibility, we can also add all kinds of
OO classes and prototypes since SIMULA 1967, but also precursor breakthroughs like
the “locator words” of the Burroughs B5000 @~cite{lonergan1961 barton1961}, and
Ivan Sutherland’s Sketchpad’s “masters and instances” @~cite{sketchpad1963},
that both inspired Kay, or Warren Teitelman’s Pilot’s ADVISE facility @~cite{teitelman1966},
that was influential at least in the Lisp community
(eventually leading to method combination in Flavors and CLOS).
@;{
  I wonder how much the Smalltalk and Interlisp teams did or did not interact at PARC.
  I can’t imagine they didn’t, and yet, Interlisp seems to have had more influence
  on the the MIT Lispers than the co-located Smalltalkers.
}

@subsubsection{Modularity and Complexity}

Modularity used correctly can tremendously simplify programs and improve their quality,
by enabling the solutions to common problems to be solved once by experts at their best,
and then shared by everyone,
instead of the same problems being solved over and over, often badly,
by amateurs or tired experts.

Use of modules also introduces overhead, though,
at development time, compile-time and runtime alike:
boiler plate to define and use modules,
name management to juggle with all the modules,
missing simplifications in using overly general solutions
that are not specialized enough to the problems at hand,
duplication of entities on both sides of each module’s interface,
extra work to cover the “impedance mismatch” between
the representation used by the common solution
and that used by the actual problems, etc.

Modularity used incorrectly, with too many module boundaries,
module boundaries drawn badly,
resolved at the wrong evaluation stage,
can increase complexity rather than reduce it:
more parts and more crossings between parts cause more overhead;
the factoring may fail to bring simplification in how programmers may reason about the system;
subtle underdocumented interactions between features,
especially involving implicit or mutable state,
can increase the cognitive load of using the system;
attempts at unifying mismatched concepts only to then re-distinguish them
with lots of conditional behavior will cause confusion rather than enlightenment;
modules may fail to offer code reuse between different situations;
programmers may have to manage large interfaces to achieve small results@xnote["."]{
  A notable trend in wrongheaded “modularity” is @emph{myopic} designs,
  that achieve modest savings in a few modules under focus
  by vastly increasing the development costs left out of focus,
  in extra boilerplate and friction, in other modules having to adapt to the design,
  in inter-module glue being made more complex, or
  in module namespace curation getting more contentious.

  For instance, the once vastly overrated and overfunded notions “microkernels” or “microservices”
  consist in dividing a system in a lot of modules, “servers” or “services”
  each doing a relatively small task within a larger system,
  separated by extremely expensive hardware barriers
  wherein data is marshalled and unmarshalled across processes or across machines.
  Each service in isolation, that you may focus on,
  looks and is indeed simpler than the overall software combining those services;
  but this overall software kept out of focus
  was made significantly more complex by dividing it into those services.
  Another myopic design that is much rarer these days is to rewrite a program
  from a big monolithic heap of code in a higher level language
  into a lot of subroutines in a lower-level language (sometimes assembly language),
  boasting how vastly simpler each of these subroutines is to the previous program;
  yet obviously the sum total of those routines is much more complex to write and maintain
  than a similarly factored program written in an appropriate higher level language
  (that indeed the original language might not be).

  While myopic designs might offer some small performance improvement in terms of
  pipelining, locality and load-balancing for very large worksets,
  they offer no improvement whatsoever in terms of modularity, quite the contrary:
  @Xitemize[@;#:style enumparenalph
  @Xitem{(1)
    The modules must already exist or be made to exist at the language level,
    before the division into services may be enacted.
    Far from @emph{providing} a solution to any problem for the programmer,
    these techniques @emph{require} programmers to already have solved
    the important problem of module factoring before you can apply them,
    at which point they hinder rather than help.
  }
  @Xitem{(2)
    Given the factoring of a program into modules,
    these techniques add runtime-time barriers
    that do not improve safety compared to compile-time barriers
    (e.g. using strong types, whether static or dynamic).
  }
  @Xitem{(3)
    Yet those runtime barriers vastly decrease performance, and even more so
    by the compile-time optimizations they prevent (whether builtin or user-defined with e.g. macros).
  }
  @Xitem{(4)
    The interface of each module is made larger for having to include a specific marshalling,
    and the constraints related to being marshalled.
    This problem can be alleviated by the use of “interface description languages”,
    but these languages tend to be both poorer and more complex
    than a good programming language’s typesystem,
    and introduce an impedance mismatch of their own.
  }
  @Xitem{(5)
    Any alleged benefits from dividing in multiple distributed processes,
    whether in terms of failure-resistance, modularity, safety or performance
    are wholly lost if and when (as is often the case) persistent data
    all ends up being centralized in database services,
    that become a bottleneck for the dataflow.
  }
  @Xitem{(6)
    The overall system is not made one bit smaller for being divided in smaller parts;
    actually, all the artificial process crossings, marshallings and unmarshallings,
    actually make the overall system noticeably larger in proportion to how small those parts are.
    Lots of small problems are added a both runtime and compile-time,
    while no actual problem whatsoever is solved.
  }
  @Xitem{(7)
    These designs are thus actually detrimental in very proportion to how much they are followed,
    and in proportion to how beneficial they claim to be. They are, technically, a lie.
  }
  @Xitem{(9)
    In practice, “successful” microkernels import all services into a monolithic “single server”,
    and successful microservices are eventually successfully rebuilt
    as vertically integrated single services.
  }]

  In the end, the technical justification for these misguided designs stem from the inability
  to think about meta-levels and distinguish between compile-time and runtime organization of code:
  Modularity is a matter of semantics, as manipulated by programmers at programming-time,
  and by compilers at compile-time;
  the runtime barrier crossings of these myopic designs
  cannot conceivably do nothing to help about that, only hinder.
  @;{ TODO cite something about HURD? Footnote? }

  Now, there are sometimes quite valid socio-economical (and not technical) justifications
  to dividing a program into multiple services:
  when there are many teams having distinct incentives, feedback loops, responsibilities,
  service level agreement their are financially accountable for, etc.,
  it makes sense for them to deploy distinct services.
  Indeed, Conway’s Law @;TODO CITE ?
  states that the technical architecture of software follows its business or management architecture.

  Finally, it is of note that some systems take a radically opposite approach
  to modularity, or lack thereof:
  instead of gratuitous division into ever more counter-productive modules,
  some prefer wanton avoidance of having to divide code into modules at all.
  For instance, APL reduces the @emph{need} for modules or even subroutines by being extremely terse,
  and by shunning abstraction in favor of concrete low-level data representations
  (on top of its high-level virtual machine rather than of the lower-level model
  of more popular languages),
  to the point of replacing many routine names by common idioms, known sequences of combinators.
  @;{TODO cite - ask arcfide / sacrideo}
  Admittedly, there is only so much room in this direction:
  as the software grows in intent and the features
  to simplify and monomorphize code
  until it is both so simple and task-specific that there is no need for shared modules:
  at some point, you reach the intrinsic complexity of the task at hand,
  the point at which it is too big to fit wholly in any programmer’s mind;
  then it must be chipped away by moving parts into other modules,
  notably by reusing common algorithms and data structures from libraries
  rather than inline specialized versions.
  But in practice, a lot of problems can be tackled this way by bright enough developers,
  especially after having been divided into subproblems for socio-economic reasons.
}

@subsubsection{Implementing Modularity}

To achieve modularity, the specification for a software computation
is divided into independently specified modules.
Modules may reference other modules and the subcomputations they define,
potentially specified by many different people at different times,
including in the future.
These references happen through a module context, a data structure
with many fields or subfields that refer to each module, submodule,
or subcomputation specified therein.
When comes time to integrate all the module specifications into a complete computation,
then comes the problem of implementing the circularity between the definition of the module context
and the definitions of those modules.

With third-class modularity, the modules context and module context are resolved
before the language processor starts,
by whatever preprocessor external to the language generates the program.
With second-class internal modularity, they are resolved by some passes of the
language-internal processor, before the program starts.
In either of the above cases, a whole-program optimizer might fully resolve
those modules such that no trace of them is left at runtime.
Without a whole-program optimizer, compilers usually generate global linkage tables
for the module context, that will be resolved
statically by a static linker when producing an executable file,
or dynamically by a dynamic loader when loading shared object files,
in either case, before any of the programmer’s regular code is run
(though some hooks may be provided for low-level programmer-specified initialization).

At the other extreme, all third-class or second-class module evaluation
may be deferred until runtime, the runtime result being no different
than if first-class internal modularity was used,
though there might be benefits to keeping modularity compile-time only
in terms of program analysis, synthesis or transformation.
For local or first-class modules, compilers usually generate some kind of
“virtual method dispatch table”
(or simply “virtual table” or “dispatch table” or some such)
for each modularly defined entity, that, if it cannot resolve statically,
will exist as such at runtime.
Thus, Haskell typeclasses become “dictionaries” that may or may not be fully inlined.
The case of OO, prototypes are indeed typically represented
by such “dispatch table” at runtime—which in Class OO
would be the type descriptor for each object, carried at runtime for dynamic dispatch.

In a low-level computation with pointers into mutable memory,
the module context is often being accessed through a global variable,
or if not global, passed to functions implementing each module as a special context argument,
and each field or subfield is initialized through side-effects,
often with some static protocol to ensure that they are (hopefully, often all) initialized
before they are used.
In a high-level computation without mutation,
each module is implemented as a function taking the module context as argument,
the fields are implemented as functional lenses@;{TODO cite},
and the mutual recursion is achieved using a fixpoint combinator@;{TODO cite};
lazy evaluation may be used as a dynamic protocol to ensure that
each field is initialized before it is used@xnote["."]{
  It is hard to ensure initialization before use;
  a powerful enough language makes it impossible to predict whether that will be the case.

  In unsafe languages, your program will happily load nonsensical values from
  uninitialized bindings, then silently corrupt the entire memory image,
  dancing a fandango on core, causing a segmentation fault and other low-level failures,
  or even worse, veering off into arbitrary undefined behavior and yielding
  life-shatteringly wrong results to unsuspecting users.
  The symptoms if any are seen long after the invalid use-before-init,
  making the issue hard to pin-point and debugging extremely difficult
  unless you have access to time-travel debugging at many levels of abstraction.

  In not-so-safe languages, a magic NULL value is used, or for the same semantics
  just with additional syntactic hurdles, you may be forced to use some option type;
  or you may have to use (and sometimes provide out of thin air) some default value
  that will eventually prove wrong.
  In the end, your program will fail with some exception,
  which may happen long after the invalid use-before-init,
  but at least the symptoms will be visible and you’ll have a vague idea
  what happened, just no idea where or when.

  Actually safe languages, when they can’t prove init-before-use,
  will maintain and check a special “unbound” marker in the binding cell or next to it,
  possibly even in a concurrency-protected way if appropriate,
  to identify which bindings were or weren’t initialized already, and
  eagerly raise an exception immediately at the point of use-before-init,
  ensuring the programmer can then easily identify where and when the issue is happening,
  with complete contextual information.

  In even safer languages, lazy evaluation automatically ensures that you always have init-before-use,
  and if the compiler is well done may even detect and properly report finite dependency cycles;
  you may still experience resource exhaustion if you generate infinite new dynamic dependencies,
  but so would you in the less safe alternatives if you could reach that point.

  Safest languages may require you to statically prove init-before-use,
  which may be very hard with full dependent types,
  or very constraining with a more limited proof system,
  possibly forcing you to fall back to only the “not-so-safe” alternative.
  But this technology is onerous and not usable by programmers at large.

  Some languages may let you choose between several of these operation modes depending
  on compilation settings or program annotations.

  Interestingly, whichever safety mode is used, programmers have to manually follow
  some protocol to ensure init-before-use.
  However the lazy evaluation approach minimizes artificial constraints on such protocol,
  that when not sensible might force him to fall back to the not-so-safe variant.

  The init-before-issue issue is well-known and exists outside of OO: it may happen
  whenever there is mutual recursion between variables or initial elements of data structures.
  However, we’ll see that “open recursion” @~cite{cardelli1992extensible Pierce2002TAPL},
  i.e. the use of operators meant to be the argument of a fixpoint combinator,
  but also possibly composition before fixpointing,
  is ubiquitous in OO,
  wherein partial specifications define methods that use other methods that are yet to be defined
  in other partial specifications, that may or may not follow any particular protocol
  for initialization order.
  Thus we see that the simplest, most natural and safest usable setting for OO is:
  lazy evaluation.

  This may come at a surprise to many who mistakenly believe the essence of OO
  is in the domain it is commonly applied to
  (defining mutable data structures and accompanying functions as “classes”),
  rather than in the semantic mechanism that is being applied to these domains
  (extensible modular definitions of arbitrary code using inheritance).
  But this is no surprise to those who are deeply familiar with C++ templates, Jsonnet or Nix,
  and other systems that programmers to directly use unrestricted OO
  in a more fundamental purely functional setting wherein OO can be leveraged
  into arbitrary programming and metaprogramming, not just for “classes” stricto sensu.
  In the next subsubsection, we argue the case from the point of view of modularity
  rather than initialization safety;
  however, both can be seen as two aspects of the same argument about semantics.
}
The more programmers must agree on a protocol to follow, the less modular the approach.
In a stateful applicative language, where the various modular definitions
are initialized by side-effects, programmers need to follow some rigid protocol
that may not be expressive enough to follow the modular dependencies between internal definitions,
often leading to indirect solutions like “builder” classes in Java,
that stage all the complex computations before
the initialization of objects of the actually desired class.

@subsection[#:tag "extensibility"]{Extensibility}
@subsubsection{Extending an Entity}
Extensibility is the ability to take a software entity and create a new entity
that includes all the functionality of the previous entity,
and adds new functionality, refines existing functionality,
or otherwise modifies the previous entity.
Extensibility that can be realized inside or outside of a programming language.

@subsubsection{First-class to Fourth-class Extensibility}

External Extensibility is the ability to extend some software entity
by taking its code then reading it, copying it, editing it,
processing it, modifying it, (re)building it, etc.
Extensibility is much easier given source code meant to be thus read and written,
but is still possible to reverse-engineer and patch binary code,
though the process can be harder, less robust, less flexible and less reusable.
Thus, external extensibility is always possible for any software written in any language,
though it can be very costly, in the worst case
as costly as rewriting the entire extended program from scratch.
When done automatically, it’s third-class extensibility.
When done manually, it’s fourth-class extensibility.
The automation need not be written in the same language as the software being extended,
or as its usual language processor, though either may often be the case.

Internal or Intra-linguistic extensibility, either first-class or second-class, by contrast,
is the ability to extend, refine or modify a software entity
without having to read, rewrite, or modify any of the previous functionality,
indeed without having to know how it works or have access to its source code,
only having to write the additions, refinements and modifications.
Second-class extensibility happens only at compile-time, and is quite restricted,
but enables more compiler optimizations and better developer tooling.
First-class extensibility may happen at runtime, and is less restricted
than second-class extensibility but more so than external extensibility
in terms of what modifications it affordably enables,
yet less restricted than either in terms of enabling last-minute customization
based on all the information available to the program.

These notions of extensibility are complementary and not opposite, for often to extend a program,
you will first extract from the existing code a shared subset that can be used as a library
(which is extra-linguistic extension),
and re(write) both the old and new functionality as extensions of that library
(which is intra-linguistic extension),
as much as possible at compile-time for efficiency (which is second-class extensibility),
yet what you need to runtime (which is first-class extensibility).

Finally, and as for modularity, the use of reflection, “live” interactive environments,
or even for a “regular” computing system considered from a wider point of view
that includes not just “dead” programs from “end-user” programmers,
but also all human interaction loops from operating system and compiler writers
as well as those end-users, can blur the lines between “external” and “internal”,
between “second-class” and “first-class”, or reclassify them as demarcations
in the division of labor that may be opportune to some of the participating humans
but not to others.

@subsubsection[#:tag "CfE"]{A Criterion for Extensibility}

@principle{A design is more extensible if it enables developers
to enact more kinds of change through smaller more local modifications}
compared to alternative designs that require larger (costlier) rewrites
or more global modifications (or prohibit change, same as making its cost infinite).

Extensibility should be understood within a framework of what changes
are or aren’t “small” for a human (or AI?) developer, rather than
for a fast and mindless algorithm.

Thus, for instance, changing some arithmetic calculations to use
bignums (large variable-size integers) instead of fixnums (builtin fixed-size integers)
in C demands a whole-program rewrite with non-local modifications to the program structure.
in Java it involves some changes all over though straightforward
and preserving the local program structure;
in Lisp requires minimal local changes, and Haskell requires one local change only.
Thus with respect to this and similar kinds of change, if expected,
Haskell is more extensible than Lisp that is more extensible than Java that is more extensible than C.
@;{TODO examples for C, Java, Lisp, Haskell}

Extensibility does not necessarily mean that a complex addition or refactoring
can be done in a single small change;
rather, code evolution can be achieved in many small changes, wherein
the system can assist the developer into only having to care
about a small change at a time, while the system tracks down what are all
the small remaining changes necessary.

For instance, a rich static type system can often be used as a tool
to guide large refactorings by dividing them in manageably small changes
(as extralinguistic code modifications),
making the typechecker happy one redefinition at a time after a type modification.
This example also illustrates how
@principle{Extensibility and Modularity usually happen through
meta-linguistic mechanisms rather than linguistic mechanisms},
i.e. through tooling outside the language rather than
expressions inside the language.
Even then, having the locus of such extensible modularity be internalized within the language
enables dynamic extension, runtime code sharing, user-guided specialization
as intralinguistic deployment processes that leverage the result of
the extralinguistic development process.

@subsubsection{Historical Extensibility Breakthroughs}

While any software is externally extensible by patching the executable binary,
the invention of assembly code made it much easier to extend programs,
especially so with automatic offset calculations from labels.
Compilers and interpreters that process source code meant for human consumption and production,
preprocessors, interactive editors, and all kinds of software tooling,
also much facilitated external extensibility.
And software that requires configuration through many files
that must be carefully kept in synch manually
is less extensible than one that can be configured through a single file
from which all required changes are automatically propagated in a coherent manner
(whether that configuration is internal or external, first-class to fourth-class);
if multiple files must be modified together
(such as interface and implementation files in many languages),
then the units of modularity are not each single file, but
each group of files that need be modified together;
or sometimes, groups that include parts of multiple files, which is even more cumbersome.

Higher-level languages facilitate external extensibility compared to lower-level ones,
by enabling programmers to make smaller more local changes for larger effects;
thus FORTRAN enables more extensible code than assembly, and Haskell more than FORTRAN.
Languages with higher-order functions or object-orientation enable more internal extensibility
by enabling the creation of new in-language entities built from existing entities
in more powerful ways.
One particular way that OO can be showed to enable more extensibility than lack thereof,
is when some course on data structures explains variants of a data structure require
complete rewrite of each variant and all associated functions from scratch
when using a functional language with simple Hindley-Milner typechecking, @; TODO cite Okasaki book?
but can be simply written as a series of classes refining previous classes
with a couple of changes at each step when using OO.

@subsubsection{Extensibility without Modularity}

Before we go deep into how OO the brings together extensibility and modularity,
we may want to explain what extensibility without modularity means.

Operating Systems, applications or games sometimes deliver updates using some kind of binary patch
format that minimizes the size that has to be transmitted
while maximizing the changes made to the software.
Such patches embody extensibility yet total lack of modularity:
they are meaningful only in the context of the exact previous version of the software.
Text-based diff files can similarly be used as patches for source code,
and are somewhat more modular, being meaningful even in presence
of independent changes to the source code, but overall remain not very modular,
due to being fragile and making changes without respecting any semantic code interface;
actually their power to extend software lies precisely in their not having to respect such interfaces.

The ultimate in extensibility without modularity would be to specify modifications to the software
as bytes concatenated at the end of a compressed stream (e.g. using @c{gzip} or some AI model)
of the previous software:
a few bits could specify maximally large changes, ones that can reuse large amounts of information
from the compressor’s model of the software so far;
yet those compressed bits would mean nothing outside the exact context
of the compressor—maximum extensibility, minimum modularity.

Interestingly, these forms of external extensibility without modularity,
while good for distributing software, are totally infeasible for humans to directly create:
they vastly increase rather than decrease the cognitive load required to write software.
Instead, humans use modular forms of extensibility, such as editing source code
as part of a edit-evaluate-debug development loop, until they reach a new version
of the software they want to release, after which point they use automated tools
to extract from the modularly-achieved new version some non-modular compressed patches.

As for internal extensibility without modularity,
UML, co-Algebras or relational modeling, as previously discussed in @secref{modeling_the_world},
fail to model OO precisely because the “classes” they specify are mere types:
they lack the modularity context that enables self-reference in actual OO classes.
As in-language entities, they can be extended by adding new attributes,
but these attributes have to be constants:
attributes being defined cannot vary modularly by referring to the class being defined it-“self”
or its many other attributes being defined.

@subsubsection{Extensibility and Complexity}

Extensibility of software entities means that variants of these software entities
may be reused many times in as many different contexts,
so more software can be achieved for fewer efforts.

Now, external extensibility through editing (as opposed to e.g. preprocessing)
means that these entities are “forked”,
which in turn reintroduces complexity in the maintenance process,
as propagating bug fixes and other changes to all the forks
becomes all the harder as the number of copies increases.
External extensibility through preprocessing,
and internal extensibility, do not have this issue,
and, if offered along a dimension that matters to users,
enable the development of rich reusable libraries
that overall decrease the complexity of the software development process@xnote["."]{
  Interestingly, detractors of OO will often count “code reuse” as a bad aspect of OO:
  they will claim that they appreciate inheritance of interfaces, but not of implementation.
  Many OO language designers, as of Java or C#, will say the same of multiple inheritance
  though not of single inheritance.
  Yet, many practitioners of OO have no trouble reusing and extending libraries of OO classes and prototypes,
  including libraries that rely heavily on multiple inheritance for implementation.

  It is quite likely that users bitten by the complexities yet limitations
  of multiple inheritance in C++ or Ada may see that
  it does not bring benefits commensurable with its costs;
  and it is also quite likely that users fooled by the absurd “inheritance is subtyping” slogan
  found that code written under this premise does not quite work as advertised.
  These disgruntled users may then blame OO in general
  for the failings of these languages to implement OO,
  or for their believing false slogans and false lessons propagated by bad teachers of OO.
}

Note that often, the right way to reuse an existing software entity
is not to reuse it as is (internal extensions),
but first to refactor the code (external extensions) so that both the old and new code
are extensions of some common library (internal extensions).
Then the resulting code can be kept simple and easy to reason with.
Internal and external extensibility are thus mutual friends, not mutual enemies.

By decreasing not only overall complexity, but the complexity of @emph{incremental} changes,
extensibility also supports shorter software development feedback loops,
therefore more agile development.
By contrast, without extensibility, developers may require more efforts
before any tangible incremental progress is achieved,
leading to loss of direction, of motivation, of support from management
and of buy-in from investors and customers.
Extensibility can thus be essential to successful software development.

@subsubsection{Implementing Extensibility}

To enable extensibility, a software entity must be represented in a way
that allows semantic manipulation in dimensions meaningful to programmers.
By the very nature of programming, any such representation is enough to
enable arbitrary extensions, given a universal programming language,
though some representations may be simpler to work with than others:
they may enable programmers can express concisely and precisely the changes they want,
at the correct level of abstraction, detailed enough that they expose the concepts at stake,
yet not so detailed that the programmer in drowned in minutia.

Now, in the case of second-class internal extensibility,
or of first-class internal extensibility in a less-than-universal language,
representations are no longer equivalent,
as only a subset of computable extensions are possible.
This kind of limited extensibility is not uncommon in computing systems;
the restrictions can help keep the developers safe and simplify the work of language implementers;
on the other hand, they may push developers with more advanced needs towards
relying external extensibility instead, or picking an altogether different language.

In a pure functional model of extensibility,
an extension is a function that takes the original unextended value
and returns the modified extended value.
In a stateful model of extensibility,
an extension can instead be a procedure that takes a mutable reference, table or structure
containing a value, and modifies it in-place to extend that value—sometimes
using “hot-patches” that were not foreseen by the original programmer.

@;{TODO examples}

@subsection[#:tag "extensible_modularity"]{Extensible Modularity}
@subsubsection{A Dynamic Duo}

Modularity and Extensibility work hand in hand:
@principle{Modularity means you only need to know
a small amount of old information to make software progress.
Extensibility means you only need to contribute
a small amount of new information to make software progress.}
Together they mean that one or many finite-brained developers can better divide work
across time and between each other,
and make more software progress with a modular and extensible design,
tackling larger problems while using less mental power each,
compared with using less-modular and less-extensible programming language designs.
Together they enable the organic development of cooperative systems
that grow with each programmer’s needs without getting bogged down in coordination issues.

@subsubsection{Modular Extensible Specifications}

A modular extensible specification specifies how to extend a previous value,
but in a modular way, wherein the extension is able to refer
through some modular context to values defined by other people,
and those other definitions being referenced can themselves be extended.

At this point, we reach the end of what can be clearly explained while remaining informal,
and have to introduce a formal model of modularity, extensibility, and the two together,
to further convey the ideas behind OO.
The last task to carry informally is therefore a justification of our approach to formalization.

@subsection{Why a Minimal Model of First-Class OO using FP?}

@subsubsection{Why a Formal Model?}

The current section was largely appealing to well-established concepts in Computing,
and inasmuch as it can be understood, it is only because we expect our readers
to be seasoned programmers and scientists familiar with those concepts.
Our most complex explanations were in terms of functions from a modular context to a value,
or from some (original) value to another (extended) value (of essentially the same type).
As we try to combine these kinds of functions, the complexity is larger
than can be explained away in a few words, and the devil will be in the details.

A formal model will allow us to precisely define and discuss the building blocks of OO,
in a way that is clear and unambiguous to everyone.
This is all the more important when prevailing discourse about OO is full of
handwaving, imprecision, ambiguity, confusions, and
identical words used with crucially different meanings by different people.

@subsubsection{Why a Minimal Model?}

We seek a @emph{minimal} model because a non-minimal model means there are still concepts
that haven’t been teased apart from each other, but are confused and confusing.

If we @emph{can} express classes in terms of prototypes, prototypes in terms of specifications,
or multiple and single inheritance in terms of mixin inheritance,
then we @emph{must} do so, until we reduce OO to its simplest expression,
and identify the most fundamental building blocks within it,
from which all the usual concepts can be reconstituted, explained, justified, evaluated,
generalized, and maybe even improved upon.

@subsubsection{Why Functional Programming?}

Functional Programming (FP) is a computational model
directly related to formal or mathematical logic
whereby we can precisely reason about programs, their semantics (what they mean),
how they behave, etc.
The famous Curry-Howard Correspondence establishes a direct relationship
between the terms of the λ-calculus that is the foundation of FP,
and the rules of logical deduction. That correspondence can also be extended
to cover the Categories of mathematics, and more.

Therefore, in an essential sense, FP is indeed the “simplest” paradigm
in which to describe the semantics of OO and other programming paradigms:
it is simultaneously amenable to both computation and reasoning
with the least amount of scaffolding to bridging between the two.

@subsubsection{Why an Executable Model?}

In the words of Donald Knuth:
“Actually a person does not @emph{really} understand something
until teaching it to a @emph{computer}, i.e. expressing it as an algorithm.”.

While we could achieve a slightly simpler algorithmic model
by using a theoretical variant of the λ-calculus,
we and other people would not be able to directly run and test our algorithms
without a costly and error-prone layer of translation or interpretation.

By using an actual programming language that while very close to the λ-calculus
comes with both additional features and additional restrictions,
we will introduce some complexity, and
a barrier to entry to people not familiar with this particular language.
But the code we offer in this essay will be easy for us and our readers
to run, to test, to debug, to interact with,
to develop an intuition on how it runs and what it does,
and later to extend and to adapt.

Our code will also provide a baseline for implementers who would want to use our ideas,
and who may just port our code to their programming language of choice,
and be able to debug their port by comparing its behavior to that of the original we provide.
They can achieve implement basic OO in two lines of code in any modern programming language,
add have a full-featured OO system in a few hundreds of lines of code.

@subsubsection{Why First-Class?}

The most popular form of OO is second-class, wherein
all inheritance happens at compile-time when defining classes.
But for some programmers to use OO as a second-class programming construct,
language implementers still have to implement OO as a first-class construct
within their compilers and other semantic processors.
@emph{Anyone’s second-class entities are someone else’s first-class entities.}
And you still don’t fully understand those entities until you have implemented them,
at which point they are first class.
Thus, every useful minimal semantic model is always a first-class model,
even if “only” for the implementation of a meta-level tool such as a typechecker.

@subsubsection{What Precedents?}

We were flabbergasted when we first saw
basic OO actually implemented in two function definitions,
in the Nix standard library @~cite{nix2015}.
These two definitions can be ultimately traced in a long indirect line
to the pioneering formalization by Bracha and Cook @~cite{bracha1990mixin},
though the author wasn’t aware of the lineage, or indeed even that he was doing OO@xnote[";"]{
  Peter Simons, who implemented prototypes as a user-level library in Nix
  as “extensions”, wrote in a private communication that
  he did not not know anything about their relationship to Prototypes, Mixins or OO,
  but semi-independently reinvented them and their use,
  inspired by examples and by discussions with Andres Löh and Conor McBride;
  the latter two unlike Simons were well-versed in OO literature,
  though they are usually known to advocate FP over OO.
}
however, Nix also implements conflation, a crucial element missing from Bracha and Cook.

We will deconstruct and reconstruct this formalization.
Then, on top of this foundation, we will be able to add all the usual concepts of OO,
developed by their respective authors, whom we will cite.

@subsubsection{Why Scheme?}

The Algorithmic Language Scheme @; TODO cite original paper, Steele thesis, R{1-7}RS
is a minimal language built around a variant of the applicative λ-calculus,
as a dialect in the wider tradition of LISP. @; CITE
It has many implementations, dialects and close cousins, @; cite Racket
a lot of documentation, modern libraries, @; TODO cite SRFIs
decades of established lore, code base, user base, academic recognition.
and also macro systems @; TODO cite
that allow to tailor the syntax of the language to your needs.

Some other languages, like ML or Haskell, @; CITE
are closer to the theoretical λ-calculus, but come with builtin “typesystems”
that are way too inexpressive to accept the λ-terms we use.
We suspect that using dependent types such as in Rocq, Agda or Lean @; CITE
have sufficiently expressive typesystems, but the types involved might be unweildy
and would make it harder to explain the basic concepts we present.
We leave it as an exercise to the reader to port our code to such platforms,
and look forward to the result.

Nix, that directly provides λ-calculus with dynamic typing,
is lazy, which actually makes the basic concepts simpler.
But it would require more care for implementers trying to port such an implementation
to most programming contexts that are applicative.
Also, Nix is more recent, less well-known, its syntax and semantics less recognizable;
the Lindy effect @; TODO cite
means it will probably disappear sooner that Scheme,
making this essay harder to read for potential readers across time.
Finally, Nix as compared to Scheme, is missing a key feature beyond the basic λ-calculus,
that we will use when building multiple inheritance:
the ability to test the equality of two specifications.

Therefore, we pick Scheme as the best compromise in which to formalize OO.

@section[#:tag "MOO"]{Minimal OO}

Now that we’ve given an informal explanation of OO and
what it is for (Internal Extensible Modularity),
we can introduce formal semantics for it, starting with a truly minimal model:
(1) No classes, no objects, just specifications and targets.
(2) The simplest and most fundamental form of inheritance, mixin inheritance.
Mixin inheritance is indeed simplest from the point of view of post-1970s formal logic,
though not from the point of view of implementation using mid-1960s computer technology,
at which point we’d be using single-inheritance indeed.
@; XXX remove the above paragraph, check its content is available

@subsection{Minimal First-Class Extensibility}

@subsubsection{Extensions as Functions}

Let us start with formalizing First-Class Extensibility in pure FP,
as it will be easier than modularity, and a good warmup.
To make clearer what kind of computational objects we are discussing,
we will be using semi-formal types as fourth-class entities,
i.e. purely as design-patterns to be enforced by humans.
We leave a coherent typesystem as an exercise to the reader,
or will later direct him to relevant literature.

Now, to extend to some computation returning some value of type @c{V},
is simply to do some more computation, starting from that value,
and returning some extended value of some possibly different type @c{W}.
Thus, in general an “extension” is actually an arbitrary transformation,
which in FP, will be modeled as a function of type @c{V → W}.

However, under a stricter notion of extension,
@c{W} must be the same as @c{V} or a subtype thereof,
such that you can add or refine information about the entity being extended,
or adjust it in minor ways,
but not in ways that invalidate the information specified so far,
at least none of the information encoded in the type.
when that is the case, we will then speak of a “strict extension”.

Obviously, if types are allowed to be too precise,
then any value @c{v} is, among other things, element of
the singleton type that contains only @c{v}, at which point
the only allowed transformation is a constant non-transformation.
Still, in a system in which developers can @emph{explicitly} declare
a type @c{V} if any (i.e. if not @c{Any})
that encodes information they require to be preserved,
it makes sense to require only extensions that strictly respect that type,
i.e. functions of type @c{V → V}, or @c{W ⊂ V ⇒ V → W}
(meaning @c{V → W} under the constraint that @c{W} is a subtype of @c{V},
for some type @c{W} to be declared, in which case the function is also of type @c{V → V}).

@subsubsection{Coloring a Point}

The prototypical type @c{V} to (strictly) extend would be the type @c{Record} for records.
Assuming for the moment some syntactic sugar, and postponing discussion of precise semantics,
we could define a record as follows:
@Code{(define point-p (record (x 2) (y 4)))}
i.e. the variable @c{point-p} is bound to a record that associates
to symbol @c{x} the number @c{2} and to symbol @c{y} the number @c{4}.

An sample (strict) extension would be the function @c{paint-blue} below,
that extends a given record (lexically bound to @c{p} within the body of the function)
into a record that is a copy of the previous
with a new or overriding binding associating to symbol @c{color} the string @c{"blue"}:
@Code{(define (paint-blue p) (extend-record p 'color "blue"))}

Obviously, if you apply this extension to that value with @c{(paint-blue point-p)}
you obtain the a record equal to what you could have directly defined as:
@Code{(record (x 2) (y 4) (color "blue"))}

Readers familiar with the literature will recognize the “colored point” example
used in many OO papers. Note however, that in the present example,
as contrasted to most such papers, and to further examples in subsequent sections:
(a) we are extending a point @emph{value} rather than a point @emph{type},
(b) the value is a regular record, and not an “object” by any means, and
(c) indeed we haven’t started modeling the modularity aspect of OO yet.

@subsubsection{Extending Arbitrary Values}

The type @c{V} of some values being extended could be anything.
The possibilities are endless, but here are a few simple real-life examples
of strict extensions for some given type:
@itemize[
@item{The values could be numbers, and then
your extensions could be adding some increment to a previous number,
which could be useful to count the price, weight or number of parts in a project being specified.}
@item{The values could be bags of strings, and
your extensions could append part identifiers to the list of spare parts or ingredients to order
before to start assembly of a physical project.}
@item{The values could be lists of dependencies, where each dependency
is a package to build, action to take or node to compute,
in a build system, a reactive functional interface or a compiler.}]

A record (indexed product) is just a common case because it can be used to encode anything.
Mathematicians will tell you that products (indexed or not)
give a nice “cartesian closed” categorical structure to the set of types
for values being extended. What it means in the end is that
you can decompose your specifications into elementary aspects that you can combine
together in a record of how each aspect is extended.

@subsubsection{Applying or Composing Extensions}

The ultimate purpose of an extension @c{ext} is
to be applied to some value @c{val},
which in Scheme syntax is written @c{(ext val)}.

But interestingly, extensions can be composed, such from two extensions
@c{ext1} and @c{ext2} you can extract an extension @c{(compose ext1 ext2)},
also commonly written @c{ext1 ∘ ext2},
that applies @c{ext1} to the result of applying @c{ext2} to the argument value.
And since we are discussing first-class extensions in Scheme,
we can always define the @c{compose} if not yet defined, as follows,
which is an associative operator with the identity function @c{id} as neutral element:
@Code{
(define compose (λ (ext1 ext2) (λ (val) (ext1 (ext2 val)))))
(define id (λ (val) (val)))}

Now if we were discussing second-class extensions in a restricted compile-time language,
composition might not be definable, and not expressible unless available as a primitive.
That would make extensions a poorer algebra than if they could be composed.
With composition, strict extensions for a given type of values are a monoid
(and general extensions are a category).
Without composition, extensions are just disjointed second-class constants without structure.
We will see later that this explains why in a second-class setting,
single inheritance is less expressive than mixin inheritance and multiple inheritance.

@subsubsection{Top Value}

Now, sooner or later, composed or by itself, the point of an extension is
to be applied to some value.
When specifying software in terms of extensions,
what should the initial base value be, to which extensions are applied?
One solution is for users to be required to somehow specify an arbitrary base value,
in addition to the extension they use.

A better solution is to identify some default value containing
the minimum amount of information for the base type @c{V}
that is being strictly extended.
Each extension then transform their “inherited” input value
into the desired extended output value,
possibly refining the type @c{V} along the way,
such that the initial type is the “top” type of this refinement hierarchy.
The initial base value is also called a “top value” @c{⊤},
and somewhat depends on what monoidal operation is used to extend it
as well as the domain type of values.

@itemize[
@item{For the type @c{Record} of records, @c{⊤ = (record-empty)} the empty record.}
@item{For the type @c{Number} of numbers, @c{⊤ = 0}, seen additively, or @c{1} if multiplicatively,
or @c{-∞} (IEEE floating-point number) seen with @c{max} as the operator, or @c{+∞} with @c{min}.}
@item{For the type @c{Pointer} of pointers into a graph of records, @c{⊤ = null},
the universal null pointer@xnote["."]{
  Hoare called his 1965 invention of null his “billion dollar mistake”. @; CITE both
  Now, to Hoare’s credit, his invention of classes in the same article,
  for which he vaguely suggests the semantics of single inheritance
  as Dahl and Nygaard would implement after his article,
  yet that he (and they) wrongfully assimilate to subtyping,
  was a trillion dollar happy mistake.
  Overall, the effect of his article @~cite{hoare1965record} was probably net vastly positive.
}}
@item{For the type @c{Type} of types (in a compiler, at the meta-level),
@c{⊤ = Any}, the top type (“contains everything, about which you know nothing”) that you refine,
or bottom type @c{⊤ = Nothing} (“contains nothing, about which you know everything”) that you extend.}
@item{For any function type in a language with partial functions, @c{⊤ = abort},
a function that never returns regularly,
and instead always abort regular evaluation and/or throws an error.}
@item{For the type @c{Lazy} of lazy computations, which would be an appropriate default
in a language with lazy evaluation,
even and especially a pure functional language with partial functions,
such as Haskell or Nix,
@c{⊤ = (lazy ⊥)} is a universal top value, where @c{⊥ = (abort)} is a computation
that never terminates normally. Indeed, @c{(lazy ⊥)} carries no useful information
but can be passed around, and has “negative infinite” information
if you try to force, open or dereference it, which is much less than
the 0 information as provided by a regular null value.
In Scheme, one may explicitly use the @c{delay} primitive to express such laziness,
though you must then explicitly @c{force} the resulting value rather than
having the language implicitly force computations whenever needed.}
@item{Last but not least for the type @c{Any} of arbitrary values of any type,
which would be an appropriate default in a language with eager evaluation,
any value could do as default, but often there is a more colloquial default.
In C, you would use 0 in integer context and the @c{NULL} pointer in pointer context.
In modern C++, similarly but with the @c{nullptr} instead of @c{NULL}.
In Java, @c{null}.
In Lisp, @c{NIL}.
In Scheme, the false boolean @c{#f}, or, in some dialects, a special unit value @c{(void)}
(the null value @c{'()} being traditionally used only for lists).
In JavaScript, @c{undefined}, @c{null} or an empty record @c{{}}.
In Python, @c{None}.
Various languages may each offer some value that is easier to work with as a default,
or some library may offer an arbitrary value to use as such.
Some strongly typed applicative languages will offer no universal such value, though,
and then some ad hoc arbitrary value must be provided for each type used.}]

In the context of extensions for objects,
OO languages usually use @c{Record} as their top type,
or some more specific @c{Object} subtype thereof that carries
additional information as instance of Prototype or Class (depending on the language),
with some null value or empty object as their top value.
YMMV, but in our examples below, we will be more ambitious as to how widely applicable
OO techniques can be, and, using Scheme as our language, we will choose
the @c{Any} type as our top type, and the false boolean @c{#f} as our top value:
@Code{
(define top #f)}

@subsubsection{Here there is no Y}

Looking at the type signature @c{(V → V) → V}
for the process of obtaining a value from a strict extension
(and every extension is strict for the top type),
one may be tempted to say “I know, this is the type of the fixpoint combinator Y”
and propose the use of said combinator.

The idea would indeed work in many cases, as far as extracting a value goes:
for any lazy type @c{V} (and similarly for function types),
any extension that would return a useful result applied to @c{(lazy ⊥)}
passing it as argument to the lazy @c{Y} combinator would also yield a result.
And indeed the results will be the same if the extension wholly ignores its argument,
as is often the intent in those situations:
typically, you’d compose extensions, with one “to the right”
ignoring its argument, overriding any previous value
(or lack thereof, as the default default is a bottom computation),
and returning some default value that is more useful in context
(including not being a bottom computation);
further extensions “to the left” then build something useful from that default value.

However, if there is no such overriding extension, then
the results would not necessarily the same between applying the extension or passing it to @c{Y}.
For instance, given the extension @c{(λ (x) (lazy-cons 1 x))}
for some @c{lazy-cons} function creating a co-inductive stream of computations
(as opposed to an inductive list of values as with the regular Scheme @c{cons}),
applying the extension to @c{(lazy ⊥)} yields a stream you can destructure once,
yielding @c{1} as first value, and “exploding at your face” if you try to destructure the rest;
meanwhile, applying the @c{Y} combinator yields an infinite stream of 1’s.

Then comes the question of which answer is more appropriate.
Using the @c{Y} combinator only applies to functions and lazy values,
the latter being isomorphic to nullary functions
that always return the same cached result;
it doesn’t apply and therefore isn’t appropriate in the general case of eager values.
Meanwhile, applying extensions to a top value is always appropriate;
it also corresponds better to the idea of specifying a value by starting from
no specific information then refining bit by bit until a final value is reached;
it does require identifying a type-dependent top value to start from,
but there is an obvious such value for the types where the fixpoint combinator applies;
finally, it is easier to understand than a fixpoint combinator and arguably more intuitive.

All in all, the approach of applying extensions to a top value
is far superior to the approach of using a fixpoint combinator
for the purpose of extracting a value from an extension.
Thus, as far as we care about extensibility:
@emph{here, there is no Y}@xnote["."]{
  (With apologies to Primo Levi.)
}

@subsection[#:tag "MFCM"]{Minimal First-Class Modularity}

@subsubsection{Modeling Modularity (Overview)}

To model Modularity in terms of FP,
we need to address several issues, each time with functions:
@itemize[
@item{First, we need to be able to define several independent entities.
For that, we introduce records, as functions from identifiers to values.}
@item{Second, programmers need to be able to use existing modularly-defined entities.
For that, we introduce the notions of module context as a record,
and modular definition as function from module context to entity.}
@item{Third, programmers need to be able to publish the entities they define
as part of the modular context that can be used by other programmers.
For that, we introduce to notion of module as record,
and (open) modular module definition, as function from module context to module.}
@item{Last, programmers need to be able to link together
those independent modular definitions into complete programs that end-users can run.
For that, we introduce closed modular module definitions,
and show how to resolve the open references.}]

The end-user is provided with a “linked” program as a resolved module context
from which he can invoke a programmer-defined “main” entry-point with his choice of arguments.

@subsubsection{Records}

Before we model Modularity as such, let us delve deeper into the modeling of Records,
that are the usual substrate for much of Modularity.

@subsubsub*section{Record Nomenclature}

We will follow Hoare @~cite{hoare1965record} in calling
“record” the concrete representation of data that contains zero, one or many “fields”.
A typical low-level implementation of records is as
consecutive “words” (or “characters”) of “computer store” or “storage space”,
all of them typically mutable.
But for the sake of studying semantics rather than performance,
this paper will discuss higher-level implementations, immutable,
and without notion of consecutive words.
By contrast, Hoare uses “object” to refer to more abstract entities
from the “problem” being “modeled”, and an “object” has higher-level “attributes”.
An object and some or all of its attributes might be simply represented
as a record and its fields, but other representations are possible.

Other authors at times have used other words for records or other representations of the same concept:
struct, structure, data structure, object, table, hash-table,
tuple, named tuple, product, indexed product, entity, rows, etc.
Their fields in various traditions and contexts may have been called such things as:
methods, slots, attributes, properties, members, variables, functions, bindings, entries, items,
keys, elements, components, columns, etc.
Mapping which terms are used in any given paper to those used in this or another paper
is left as an exercise to the reader.

@subsubsub*section{Typing Records}

We could be content with a simple type @c{Record}, but then
every access would be require some kind of type casting.
Instead, we will have slightly more elaborate types.

Given types @c{V}, @c{W}, @c{X}... for values,
@c{I}, @c{J}... for identifiers or other indexes,
let us consider records or sets of bindings as values of an index product
@c{∏R = i:I → R@(ᵢ)} wherein to each value @c{i} in the index set @c{I},
the record will associate a value of type @(Ri),
where @c{R} is a schema of types, a function from @c{I} to @c{Type}.

To simplify our model, a pure functional record of type @c{∏R}
can be seen as an indexed function from the codomain @c{I} of indexes
to @(Ri) for each @c{i}.
When invoked on a value not in @c{I}, the function may return any value, or diverge.
To further simplify, and for the sake of modeling records as first-class functions,
when using Scheme, for indexes we will use symbols (interned strings)
instead of identifiers (that in Scheme are symbols enriched with
scoping and provenance information, used for second-class macro-expansion).

For example, where @c{Number} is the type of numbers and @c{String} the type of strings,
we could have a record type
@Code{
type ∏R = {x: Number, y: Number, color: String}
}
and a point @c{point-q} of type @c{∏R} defined as follows:
@Code{
(define point-q
  (record (x 3) (y 4) (color "blue")))
}

@subsubsub*section{Implementing Records}

Let’s assume that there is a type for identifiers
and primitives for deciding equality between them,
and evaluating conditionally based on equality or not.
In Scheme, we will use symbols for identifiers (or, as we’ll see, sometimes, booleans),
the @c{equal?} primitive for testing equality, and
the @c{(if @emph{condition then-clause else-clause})} special form for conditionals.
Programming languages have the equivalent
(though they might use uninterned strings, or number constants, instead of symbols),
and you if you care about the pure λ-calculus there are many embeddings and encodings
of unary or binary numbers, and lists or trees thereof, that will do.

Programming languages usually already provide some data structure for records
with second-class identifiers, or “dictionaries” with first-class identifiers,
or have some library that does.
For instance, while the core Scheme language has no such data structure,
each implementation tends to have its own extension for this purpose, and
there are multiple standard extensions for records or hash tables.
Many papers and experiments use a linked list of (key, value) pairs,
known in the Lisp tradition as an alist (association list),
as a simple implementation for records;
alists don’t scale, but don’t need to in the context of such experiments.
Nevertheless to make the semantics of records clear, we will provide
a trivial purely functional implementation, that also doesn’t scale,
but that is even simpler.

The basic reference operator is just function application:
@Code{
(define record-ref (λ (rec) (λ (key) (rec key))))}

The empty record can be represented as a function that always fails. In Scheme:
@Code{
(define record-empty (λ (_) (error "empty record")))}

To extend a record with one key-value binding, you can use
@Code{
(define record-cons (λ (key) (λ (val) (λ (rec) (λ (i)
  (if (eqv? key i)
     val
     (rec i)))))))}

Note how this trivial implementation does not support
getting a list of bindings, or removing a binding.
Not only won’t we need these features to implement OO@xnote[","]{
  We generate HTML for our presentations using exactly this implementation strategy.
  The Scheme implementation we use has builtin record support, and
  there are libraries now somewhat portable libraries for records in Scheme,
  but we made it a point to use a minimal portable object system
  to show the feasability and practicality of the approach.
}
they constitute a “reflection” API
that if exposed would interfere with various compiler optimizations,
the use of which is actively rejected when statically typing records.
However, there are other reasons why our implementation is not practical for long-running programs:
it leaks space when a binding is overridden,
and the time to retrieve a binding is proportional to the total number of bindings
(including overridden ones) instead of being logarithmic in the number of visible bindings only,
for a pure functional implementation based on balanced trees,
or constant time, for a linear or stateful implementation
based on either known field offsets or hashing@xnote["."]{
  The nitpicky would also account for an extra square root factor
  due to the limitations of physics@~cite{MythOfRAM2014}.
}

@subsubsub*section{Merging Records}

Given a list of bindings as pairs of an identifier and a value,
you can define a record that maps each identifier to the value in the first
binding with that identifier in the list, if any
(or else, errors out, diverges, returns null, or does whatever a record does by default).
Conversely, given a list of identifiers and a record, you can get a list of bindings
as pairs of an identifier and the value that the record maps it to.
Most practical implementations of records support
extracting from a record the list of identifiers it binds,
and such a list of all bindings in the record;
but this “reflection” feature is not necessary for most uses of records.
Indeed, the trivial implementation we will use,
wherein records are functions from identifier to value, doesn’t;
and even more elaborate implementations often deliberately not only support runtime reflection,
only second-class knowledge of what are the identifiers bound by a record.

Finally, given a list of records and for each record a set of identifiers
(that may or may not be the set of all identifiers bound by it,
possibly extracted via reflection), you can merge the records along those sets of identifiers,
by converting in order each record to a list of bindings for its given set of identifiers,
appending those lists, and converting the appended result to a record.

@subsubsection{Modular definitions}

Now we can introduce and model the notion of modular definition:
a modular definition is a way for a programmer to specify
how to define an entity of some type @c{E} given some modular context,
or just @emph{module context}, of type @c{C}.
The module context contains all the available software entities,
that were defined in other modules by other programmers
(or even by the same programmer, at different times,
who doesn’t presently have to hold the details of them in his limited brain).
And the simplest way to model a modular definition as a first-class value,
is as a function of type @c{C → E}, from module context to specified entity@xnote["."]{
  A Haskeller may well interpret “modular” in this paper as meaning
  “in the reader monad of a module context”, in that a modular something
  will be a function from the module context to that something.
  This is correct, but beware that this is only half the story.
  The other half is in what it means for something to be a module context,
  rather than any random bit of context.
  And we’ll see shortly that the answer involves open recursion and fixpoints.
  Informally, modular means that locally you’re dealing with part of a whole,
  and globally, the whole will be made out of the parts in the end.
}

Typically, the module context @c{C} is a set of bindings mapping identifiers
to useful values, often functions and constants,
whether builtin the language runtime or available in its standard library.
Now, we already have types for such sets of bindings: record types.
And as a language grows in use and scope, the module context will include
further libraries from the language’s wider ecosystem,
themselves seen as sets of bindings of their respective record types,
accessed hierarchically from a global module context,
that now contains “modules” (records) as well as other values
(or segregated from regular values at different levels of the module hierarchy).
In any case, the global module context is typically a record of records, etc.,
and though many languages have special restrictions on modules as second-class entities,
for our purpose of modeling the first-class semantics of modularity,
we may as well consider that at runtime at least, a module is just a regular record,
and so is the global module context, of type @c{C = ∏R}.

For instance, we could modularly specify a function @c{ls-sorted} that returns
the sorted list of filenames (as strings) in a directory (as a string),
from a module context of type @c{∏R} that provides
a function @c{ls} of type @c{String → List(String)} and
a function @c{sort} that sorts a list of strings:
@;{@Code{
type R = { ls: String → List(String), sort: List(String) → List(String) }
}}
@Code{
(define ls-sorted (λ (ctx) (compose (ctx 'sort) (ctx 'ls))))
}

Note how in the above code snippet, we model records is functions from symbol to value,
and to extract a binding from a record, we thus call it with a symbol as single argument.
The functions extracted are then chained together, as in the @c{compose} function
we defined earlier (that we could have used if we extracted it from the module context,
or could otherwise assume it was a language builtin).

@subsubsection[#:tag "OMD"]{Open Modular Definitions}

Now, programmers usually do not just specify just a single entity of type @c{E},
but many entities, that they distinguish by associating them to identifiers.
i.e. they modularly define a @emph{module} of type @c{∏P}.
A modular module definition is thus “just” a function from record to record:
the input record is the modular context of type @c{∏R}, and
the output record is the specified module of type @c{∏P}.
We will say that the identifiers bound in @c{∏R}
are @emph{required} by the modular definition (or @emph{referenced} by it),
whereas the identifiers bound in @c{∏P} are @emph{provided} by the modular definition
(or @emph{defined} by it).

In general, we call a modular definition “open”,
inasmuch as some entities may be @emph{required} that are not @emph{provided}
by the modular definition, and must be provided by other modular definitions.
Now, the notion that the modular definition “provides” entities supposes
that these entities will be available, bound to well-known identifiers in the module context.
There are many strategies to realize this provision:
@itemize[
@item{The modular definition can be a paired with a single identifier,
under which the entity is intended to be bound in a complete module context.}
@item{The modular definition can provide multiple bindings,
wherein the entity defined is a module, to be merged as a record into
the complete module context.}
@item{The first strategy above can be adapted to a hierarchical namespace of nested records,
by pairing the modular definition with a path in the namespace, i.e. a list of identifiers.}
@item{For multiple bindings, the second strategy can also be paired with a path;
or the merging of records can be recursive, at which point a strategy must be devised
to determine how deep to recurse, for instance by somehow distinguishing “modules”
from regular “records”.}]

In the end, we could let the programmer specify how his modular definition will extend
the module context, with whatever merges he wants, however deeply nested—which
will lead us to modular extensibility.
But for now, let us abstract over which strategy is used to
assemble many modular definitions together.

@subsubsection{Linking Modular Module Definitions: Y}

We will call a modular module definition “closed” when,
after whatever assembly of individual modular definitions happened,
it specifies the global module context of an entire program,
wherein every entity required is also provided.
A closed modular module definition is thus of type @c{∏R → ∏R}.

Then comes the question: how can we, from a closed modular module definition,
extract the actual value of the module context, of type @c{∏R},
and thereby realize the program that was modularly specified?

This module realization function we are looking for is
of type @c{(C → C) → C} where @c{C = ∏R}.
Interestingly, we already mentioned a solution:
the fixpoint combinator @c{Y}.
And whereas it was the wrong solution to resolve extensions,
it is exactly what the doctor ordered to resolve modular definitions:
the @c{Y} combinator “ties the knots”,
links each reference requiring an entity to the definition providing it,
and closes all the open loops.
It indeed does the same in a FP context that an object linker does
in the lower-level imperative context of executable binaries:
link references in open modular definitions to defined values in the closed result.

If there remain identifiers that are required but not provided,
there will be an error—or non-termination, or some default value that will not make sense,
at runtime, or at compile-time, depending on how sophisticated the exact implementation is
(a typechecker might catch the missing requirement, for instance).
If there remain identifiers that are provided but not required,
and they are not otherwise (meant to) be used via reflection,
then a “tree shaker” or global dead code optimizer may eliminate them.

@subsubsection[#:tag "DSF"]{Digression: Scheme and FP}

Here are two ways in which Scheme departs from the theoretical model of Functional Programming,
that also apply to many (but not all) other programming languages, and that affect
their suitability to modeling Object Orientation.

@subsubsub*section{Many Y combinators}

First, there are many variants to the fixpoint (or fixed-point) combinator Y,
and the pure applicative Y combinator you could write in Scheme’s
pure subset of the λ-calculus is actually quite bad in practice.
Here is the applicative Y combinator,
expressed in terms of the composition combinator B and the duplication combinator D@xnote[":"]{
  A simple way to test the @c{applicative-Y} combinator,
  or the subsequent variants @c{applicative-Y-expanded} and @c{stateful-Y}
  is to use it to define the factorial function:
  @c{(define fact (applicative-Y (λ (f) (λ (n) (if (<= n 1) n (* n (f (1- n))))))))}
  and you can then test that e.g. @c{(fact 6)} returns @c{720}.
}
@Code{
(define B (λ (x) (λ (y) (λ (z) (x (y z))))))
(define applicative-D (λ (x) (λ (y) ((x x) y))))
(define applicative-Y (λ (f) (applicative-D ((B f) applicative-D))))
(define applicative-Y-expanded
  (λ (f) ((λ (x) (λ (y) ((x x) y)))
          (λ (x) (f (λ (y) ((x x) y)))))))
}
@; Test: ((applicative-Y (λ (f) (λ (n) (if (<= n 1) n (* n (f (1- n))))))) 6) ;==> 720
@; Test: ((applicative-Y-expanded (λ (f) (λ (n) (if (<= n 1) n (* n (f (1- n))))))) 6) ;==> 720
The Y combinator works by composing the argument function @c{f}
with indefinite copies (duplications) of itself (and accompanying plumbing).
In this applicative variant, the first, minor, issue with this combinator is
that it only works to compute functions,
because the only way to prevent a overly eager evaluation of a computation
that would otherwise diverge is to protect this evaluation under a λ.
We happen to have chosen a representation of records as functions,
such that the applicative Y still directly applies;
if not, we may have had to somehow wrap our records in some sort of function,
at which point we may as well use the lazy Y below,
or switch to representing modular contexts as records of functions,
instead of functions implementing or returning records.

The second, major, issue with the applicative Y is that the pure applicative λ-calculus
by itself has no provision for sharing non-fully-reduced computations,
only for sharing (fully-reduced) values;
therefore the fixed-point computations are duplicated,
and any information used along the way will have to be recomputed
as many times as computations are duplicated, which can grow exponentially fast
as the computation involves deeper sub-computations.
In some cases, the eager evaluation may never terminate at all when lazy evaluation would,
or not before the end of the universe.
And of course, if there are any non-idempotent side effects,
they too will be potentially duplicated a large number of times.

There are several potential alternatives to
the practically inapplicable applicative Y combinator:
(1) a stateful Y combinator,
(2) a lazy Y combinator, or
(3) a second-class Y combinator.

A stateful Y combinator is what the @c{letrec} construct of Scheme provides
(and also its @c{letrec*} variant, that the internal @c{define} expands to):
it uses state mutation underneath to create and initialize a mutable cell
that will hold the shared fixpoint value, with the caveat that you should be careful
not to access the variable before it was initialized@xnote["."]{
  A variable won’t be accessed before it is used if you’re immediately binding it variable
  to a λ expression, but may happen if you bind the variable to a function application expression,
  wherein the variable is passed as argument without wrapping it in a λ,
  or the λ it is wrapped in is called before the evaluation of this expression completes.
  The Scheme language does not protect you in this case,
  and, in general, could not protect you
  without either severely limiting the language expressiveness,
  or solving the halting problem.
  Various languages and their implementations,
  depending on various safety settings they might have or not,
  may raise a “variable not bound” exception,
  use a special value such as @c{#!void} or @c{null} or @c{undefined}
  that is not usually part of the expected type,
  or access uninitialized memory potentially returning nonsensical results
  or causing a latter fandango on core, etc.
}
If the variable is only accessed after it is initialized,
and the rest of the program is pure and doesn’t capture intermediate continuations
with Scheme’s famous @c{call/cc}, the mutation cannot be exposed as a side-effect,
and the computation remains overall pure (deterministic, referentially transparent),
though not definable in terms of the pure applicative λ-calculus.
Note however how in the definition below, @c{p} below still needs be a function,
and we must η-convert it into the equivalent but protected @c{(λ (y) (p y))}
before to pass it to @c{f} to prevent access to the variable @c{p} before its initialization:
@Code{
(define (stateful-Y f) (letrec ((p (f (λ (y) (p y))))) p))
}
@; Test: ((stateful-Y (λ (f) (λ (n) (if (<= n 1) n (* n (f (1- n))))))) 6) ;==> 720

A second solution is to use a lazy Y. In a language like Nix
(where @c{λ (f)} is written @c{f:}, and @c{let} like Scheme @c{letrec}
recursively binds the variable in the definition body), you can simply define
@c{Y = f: let p = f p; in p}. In Scheme, using the convention that
every argument variable or function result must be protected by @c{delay},
and you must @c{force} the delayed referenc to extract the result value,
we would write@xnote[":"]{
  Again, a simple way to test the lazy Y combinator is to use it
  to define the factorial function:
  @c{(define fact (lazy-Y (λ (f) (λ (n) (if (<= n 1) n (* n ((force f) (1- n))))))))}
  and you can then test that e.g. @c{(fact 6)} returns @c{720}.
  Note that we do without wrapping of @c{n} in a @c{delay},
  but @c{f} itself is a delayed function value to fit the calling convention of @c{lazy-Y},
  and we therefore must @c{force} it before we call it.
  The subsequent variants of @c{lazy-Y} can be tested in the same way.
}
@Code{
(define (lazy-Y f) (letrec ((p (f (delay p)))) p))
}
Or, if you want a variant based on combinators:
@Code{
(define lazy-B (λ (x) (λ (y) (λ (z) ((force x) (delay ((force y) z)))))))
(define lazy-D (λ (x) ((force x) x)))
(define lazy-Y-with-combinators
  (λ (f) (lazy-D (delay ((lazy-B f) (delay lazy-D))))))
(define lazy-Y-expanded
  (λ (f) ((λ (x) ((force x) x))
          (delay (λ (x) ((force f) (delay ((force x) x))))))))
}
@; Test: ((lazy-Y-with-combinators (λ (f) (λ (n) (if (<= n 1) n (* n ((force f) (1- n))))))) 6) ;==> 720
@; Test: ((lazy-Y-expanded (λ (f) (λ (n) (if (<= n 1) n (* n ((force f) (1- n))))))) 6) ;==> 720
One advantage of a lazy Y is that evaluation is already protected by the @c{delay}
primitive and thus can apply to any kind of computation, not just to functions;
though if you consider that @c{delay} is no cheaper than a @c{λ} and indeed uses
a @c{λ} underneath, that’s not actually a gain, just a semantic shift.
What the @c{delay} does buy you, on the other hand, is sharing of computations
before they are evaluated, without duplication of computation costs or side-effects@xnote["."]{
  Whether wrapped in a thunk, an explicit delay, an implicitly lazy variable,
  or some other construct, what is interesting is that
  ultimately the fixpoint combinator indefinitely iterates a @emph{computation},
  and this wrapping is a case of mapping computations into values in an otherwise
  call-by-value model that requires you to talk about values.
  In a calculus such as call-by-push-value@~cite{conf/tlca/Levy99},
  where values and computations live in distinct type universes,
  the fixpoint combinator would clearly be mapping
  computations to computations without having to go through the universe of values.
}
(Note that @c{delay} can be easily implemented on top of any stateful applicative language,
though a thread-safe variant, if needed, is somewhat trickier to achieve.)

A third solution, often used in programming languages with second-class OO only
(or languages in which first-class functions must terminate), is
for the @c{Y} combinator (or its notional equivalent) to only be called at compile-time,
and only on modular definitions that abide by some kind of structural restriction
that guarantees the existence and well-formedness of a fixpoint,
as well as e.g. induction principles to reason about said fixpoint.
Also, the compile-time language processor usually doesn’t expose any side-effect to the user,
such that there is no semantic difference whether its implementation is itself pure or impure,
and uses a fixpoint combinator or any other representation for recursion.
Since we are interested in first-class semantics for OO, we will ignore this solution
in the rest of this essay, and leave it as an exercise for the reader.
@;{TODO CITE Aaron Stump from U Iowa, etc.}

@subsubsub*section{Function Arity}

Functional Programming usually is written with unary functions (that take exactly one argument),
and to express more than one arguments, you “curry” it:
you define a function of one argument that returns a function that processes the next argument, etc.,
and when all the arguments are received you evaluate the desired function body.
Then to apply a function to multiple arguments, you apply to the first argument,
and apply the function returned to the second argument, etc.
The syntax for defining and using such curried functions is somewhat heavy in Scheme,
involving a lot of parentheses, when the usual convention for Functional Programming languages
is to do away with these extra parentheses:
in FP languages, two consecutive terms is function application, which is left-associative,
so that @c{f x y} is syntactic sugar for @c{((f x) y)};
and function definition is curried, so that @c{λ x y . E} is syntactic sugar for @c{λ x . λ y . E}.

Thus, there is some syntactic discrepancy that makes code written in
the “native” Functional style look ugly and somewhat hard to follow in Scheme.
Meanwhile, colloquial or “native” Scheme code may use any number of argument as function arity,
and even variable numbers of argument, or, in some dialects, optional or keyword arguments,
which does not map directly to mathematical variants of Functional Programming;
but it is an error to call a function with the wrong number of arguments.

One approach to resolving this discrepancy is to just cope with
the syntactic ugliness of unary functions in Scheme, and just use them nonetheless,
despite Lots of Insipid and Stupid Parentheses.

A second approach is to adopt a more native Scheme style over FP style,
with a variety of different function arities, making sure that a function is always called
with the correct number of arguments. This approach melds best with the rest of the Scheme
ecosystem, but may hurt the eyes of regular FP practitioners, and
require extra discipline (or extra debugging) to use.

A third approach is to use Scheme macros to automatically curry function definitions
and function applications, such that a function called with insufficient arguments
becomes the same function partially applied, whereas a function called with too many arguments
becomes a call of the result of calling the function with the correct number of arguments,
with the rest of the arguments. Such macros can be written in a few tens of lines of code,
though they incur a performance penalty; an efficient variant of these macros
that statically optimize function calls could be much larger,
and might require some level of symbiosis with the compiler.

We have implemented variants of our minimal OO system in many combinations
of the above solutions to these two issues, in Scheme and other languages.
For the rest of this essay, we will adopt a more “native” Scheme style,
assuming @c{stateful-Y} and using multiple function arities
that we will ensure are carefully matched by function callers;
yet or to keep things simple and portable, we will avoid variable arity
with optional arguments, rest arguments or keyword arguments.
As a result, the reader should be able both to easily copy and test
all the code in this essay at their favorite Scheme REPL,
and also easily translate it to any other language
that sports first-class higher-order functions.

@subsection[#:tag "MFCME"]{Minimal First-Class Modular Extensibility}

@subsubsection{Modular Extensions}

Let us combine the above extensibility and modularity in a minimal meaningful way,
as modular extensibility.
We will call “modular extension” a modular definition for an extension.
Thus, given a module context of type @c{C} (typically a record with @c{C = ∏R}),
a type @c{V} for the “inherited” value being extended
and @c{W} for the extended value being “provided”,
an (open) modular extension is a function of type @c{C → V → W}.
When @c{W} is the same as @c{V}, or a subtype thereof, we will call it
a strict (open) modular extension.
When @c{W = V = ∏P} for some record type @c{∏P},
we will call it a modular module extension.
When @c{C = V = W} we will call it a closed modular extension,
which as we will see can be used as a specification for a value of that type.

@subsubsection{Composing Modular Extensions}

While you could conceivably merge such modular extensions,
the more interesting operation is to compose them, or more precisely,
to compose each extension under the module context and bound identifier,
an operation that for reasons that will soon become obvious,
we will call mixin inheritance for modular extensions:
@Code{
(define mix (λ (c p) (λ (s) (λ (t) (c s (p s t))))))}
The variables @c{c} and @c{p} stand for “child” and “parent” specifications,
wherein the value “inherited” by the composed function
will be extended (right to left, with the usual function-as-prefix syntax)
first by @c{(p s)} then by @c{(c s)},
where @c{s} is the module context
(also called @c{self} in many contexts, for reasons that will become obvious@xnote[")"]{
  The @c{self} argument is the one involved in open recursion or “late binding”;
  it embodies the @emph{modular} side of OO.
  It is called @c{self} because it is destined to be bound as the fixpoint variable
  of a fixpoint operator, wherein it refers to the entity being defined itself.
  The name @c{self} is used in Smalltalk, Scheme, SELF, Python, Jsonnet, Nix,
  many more languages, and in a lot of the literature about OO semantics.
  In SIMULA, and after it, in C++, Java, JavaScript or Scala, the @c{this} keyword is used instead.
  Note however, that we are currently discussing a variant of Prototype OO,
  as in SELF, Jsonnet, Nix, JavaScript, where the @c{self} or @c{this}
  is indeed the open recursion variable.
  In Class OO language, the definition being one of a type descriptor, not of a record,
  the open recursion variable would instead be something like @c{Self}, @c{MyType} or @c{this.type},
  though there is even less standardization in this area.
  See below the discussion of the meaning of “object” in Prototype OO vs Class OO.
  @; TODO secref Classes
}
and @c{t} is the inherited value
(also called @c{super} in the same contexts, for the same reasons@xnote[")."]{
  The @c{super} argument refers to the partial value computed so far
  from @emph{parent} specifications (composed to the right, with the usual composition order);
  the rightmost seed value of @c{super} when computing the fixed point is a “base” or “top” value,
  typically an empty record, possibly enriched with metadata or ancillary fields for runtime support.
  @c{super} embodies the @emph{extensible} side of OO,
  enabling a specification to build on values @emph{inherited} from parent specifications,
  add aspects to them, override aspects of them, modify aspects of them, etc.,
  in an extension to the computation so far.
  @c{super} is the variable or keyword used in Smalltalk
  and in many languages and object systems after it,
  such as Java or Python, to access inherited methods.
  In CLOS you’d use @c{call-next-method}.
  In C++ you can’t quite directly express that concept in general, because you have to name
  the superclass whose method (or “(virtual) member function”) you want to call,
  so you can’t directly write traits that inherit “super” behavior along the class precedence list;
  but it works if you restrict yourself to inheritance, or if you use template metaprogramming
  to arrange to pass a superclass, or list or DAG of superclasses, as argument to your template,
  and manually reimplement mixin inheritance @~cite{smaragdakis2000mixin},
  or if you’re adventurous, multiple inheritance, on top of C++.
}
The function can also be written with @c{compose}, eliding the “super” variable:
@Code{
(define mix (λ (c p) (λ (s) (compose (c s) (p s)))))}

General modular extensions for a given context form a category,
and strict modular extensions for a given context and type form a monoid,
wherein the operation is composition with the @c{mix} function,
and the neutral element @c{idMExt} is the specification that “extends”
any and every value by returning it unchanged, as follows@xnote[":"]{
  As usual, a change of representation from @c{p} to @c{mp = mix p},
  with inverse transformation @c{p = mp idMExt},
  would enable use of the regular @c{compose} function
  for composition of specifications.
  Haskellers and developers using similar composition-friendly languages
  might prefer this kind of representation,
  the way they like van Laarhoven lenses @~cite{oconnor2012lenses};
  yet, Oliveira @~cite{MonadsMixins} or
  the @c{Control.Mixin.Mixin} library (part of the @c{monadiccp} package),
  instead both use a slightly different representation that compared to ours
  swaps the order of arguments of the @c{self} and @c{super} arguments.
  We will stick with our representation, also shared by the Nix standard library, as it makes
  our explanations, and, in later sections, the types of specifications, slightly simpler.
}
@Code{
(define idMExt (λ (s) (λ (t) t)))}

@subsubsection{Closing Modular Extensions}

A closed modular extension is
a function of type @c{C → C → C},
i.e. a modular extensible module specification where @c{C = V = W}.
In the common case that @c{C} is a record, this means that
wherein every identifier required is also extended.

As before for closed modular module definitions, the question is:
how do you get from such a closed modular module extension
to an actual module definition where all the loops are closed,
and every identifier is mapped to a value of the expected type?
And the way we constructed our model, the answer is simple:
first, under the scope of the module context,
you apply your extension to the top value for a module context
(usually, that’s the empty record);
then you have reduced your problem to a regular modular module definition
@c{∏R → ∏R}, at which point you only have to compute the fixpoint.
We will call this operation instantiation for modular extensions:
@Code{(define fix (λ (t) (λ (m) (Y (λ (s) ((m s) t))))))}
In this expression,
@c{t} is the top value for the type being specified (typically the empty record, for records),
@c{m} is the modular extension, and
@c{s} is the fixpoint variable for the module context we are computing.

@subsubsection{Default and non-default Top Type}

Assuming some common top type @c{Top} and default value @c{top} in that type
(we will use @c{Any} and @c{#f} in our example Scheme implementation),
we will define the common instantiation operation for modular extensions:
@Code{(define fixt (fix top))}
or to inline @c{fix}:
@Code{(define fixt (λ (m) (Y (λ (s) ((m s) top)))))}

Note that if the language-wide top type is too wide in some context,
for instance in our choice of Scheme and @c{Any} because that you want
to work with the narrower @c{Record} so as to define individual methods,
and a default value @c{empty-record} instead of @c{#f}, then you can
compose your modular extension with a modular extension as follows to the right,
that throws away the previous value or computation (ignores its @c{super} argument)
and returns the new default value regardless of context (ignores its @c{self} argument;
unless that default is extracted from the context):
@Code{(define record-spec (λ (self) (λ (super) empty-record)))}
We could then equivalently define a variant of fix specialized for records
in any of the following ways:
@Code{(define fix-record (fix empty-record))}
@Code{(define fix-record (λ (m) (Y (λ (s) ((m s) empty-record)))))}
@Code{(define fix-record (λ (m) (fixt (mix m record-spec))))}
Note that because it ignores its @c{super} argument and thus throws away any inherited value,
the @c{record-spec} modular extension must appear last, or at least
after any modular extension the result of which isn’t to be ignored.

Why not make @c{empty-record} the language-wide default?
Because we will the language-wide default will apply not just to the specification of records,
but also to the specification of individual fields of each record,
and in this more general context, the default value @c{#f} is
possibly more efficient at runtime, and definitely more colloquial—therefore more efficient
in the most expensive resource, human-time.


@subsubsection[#:tag "MOI"]{Minimal OO Indeed}

The above functions @c{mix} and @c{fix} are indeed isomorphic
to the theoretical model of OO from Bracha and Cook @~cite{bracha1990mixin}
and to the actual implementation of “extensions” in nixpkgs @~cite{nix2015}@xnote["."]{
  Our presentation of mixin inheritance is actually slightly more general than what
  Bracha, Cook or Simons did define, in that our definition is not specialized for records.
  Indeed, our closed modular extensions work on values of any type,
  even their modularity aspect is indeed more useful for types
  that somehow directly or indirectly encode.
  But our theory also importantly considers not just closed modular extensions,
  but also the more general open modular extensions, for which it is essential
  that they universally apply to target values of any type.
  We can therefore claim as our innovation a wider, more general understanding of mixin inheritance,
  of which there is no evidence in earlier publications.
}.
This style of inheritance was dubbed “mixin inheritance” by Bracha and Cook@xnote[";"]{
  The name “mixin” originally comes from Flavors @~cite{Cannon1979},
  inspired by the ice cream offerings at Emack & Bolios
  (as for the concept itself, it was inspired both by
  previous attempts at multiple inheritance in KRL @~cite{Bobrow1976} or Ani @~cite{Kahn1979},
  combined with the ADVISE facility @~cite{teitelman1966}).
  However, Flavors offers full multiple inheritance (and was the first system to do it right),
  whereas the “mixins” of Bracha and Cook are a more rudimentary and more fundamental concept,
  that does not include automatic linearization of transitive dependencies.
  Also, “mixins” in Flavors are not distinguished by the language syntax or by the compiler;
  they are just classes that are intended to be typically used with multiple inheritance,
  as part of many disjoint hierarchies, and might indeed not otherwise make sense as base classes.
  Since the word implies no special processing by the compiler or by the human operator,
  it can be dispensed with in the original context, and gladly assigned
  a new, useful, technical meaning.
  But that doesn’t mean the context that made the word superfluous should be forgotten,
  quite the contrary.
  We will get back to Flavors when we discuss multiple inheritance. @;TODO secref
}
and the two functions, that can easily be ported to any language with first-class functions,
are enough to implement a complete object system.

How do we use these inheritance and instantiation functions?
By defining, composing and closing modular extensions of type @c{C → V → V)} where
@c{C} is the type of the module context,
and @c{V} that of the value under focus being extended:
@Code{
(define my-spec (λ (self) (λ (super) body ...)))}
where @c{self} is the module context,
@c{super} is the inherited value to be extended,
and @c{body ...} is the body of the function, returning the extended value.

In the common case that @c{V = ∏P},
and with our trivial representation of such records as @c{∏P = I → P}
where @c{I} is the type of identifiers,
a typical modular module extension will look like:
@Code{
(define my-spec (λ (self) (λ (super) (λ (method-id) body ...))))}
where @c{method-id} is the identifier for the method to be looked up,
and the body uses @c{(super method-id)} as a default when no overriding behavior is specified.

Alternatively, this can be abstracted in terms of using a mix of one or multiple
calls to this method-defining specification, that specifies a single method
with given @c{key} as name for a recognized value of @c{method-id},
and given open modular extension function @c{compute-value}
that takes the @c{self} context and the @c{inherited} value @c{(super method-id)} as arguments
and returns an extended value for the method at @c{key}:
@Code{
(define method-spec (λ (key) (λ (compute-value)
    (λ (self) (λ (super) (λ (method-id)
      (let ((inherited (super method-id)))
        (if (eqv? key method-id)
          (compute-value self inherited)
          inherited))))))))}

Note how @c{method-spec} turns an open modular extension for a value
into an open modular extension for a record (that has this value under some key).
In this case, the module context @c{self} is the same,
whereas the @c{super} value for the inner function @c{compute-value}
is the specialized @c{(super method-id)} value extracted from the record.
That’s an example for how open modular extensions themselves have a rich algebraic structure,
wherein you can combine, compose, decompose, extract, and otherwise
operate on open modular extensions to get richer open modular extensions,
and eventually build a closed modular extension that you can instantiate.

Now, where performance or space matters,
you would use an encoding of records-as-structures instead of records-as-functions.
Then, instead of calling the record as a function with an identifier,
you would invoke a dereference function with the record as first argument
and the identifier as second argument.
But with a some small overhead, records-as-functions is perfectly usable
for many simple applications@xnote["."]{
  We notably use this technique to generate all our slides in a pure functional way
  in Racket (a close cousin of Scheme). @; TODO CITE
  Interestingly, we could define a generic specification for
  slides that indicate where they are in the presentation,
  highlighting the name of each new section
  in an otherwise constant list of all sections.
  That specification uses the @c{self} context
  to extract the list of sections in the presentation,
  including sections not yet defined.
  It might seem impossible in an eager language, and without side-effects,
  to import data from slides that will only be defined later
  into a whichever slide is being defined now;
  and yet the Y combinator achieves this feat,
  and though we use the stateful Y for performance,
  a pure applicative Y also works without too much slowdown,
  because the substructures we recursively examine remain shallow.
}
Also, in a more practical implementation,
the inherited value in the @c{method-spec} would be made lazy,
or would be wrapped in a thunk, to avoid unneeded computations (that might even bottom);
or for more power, the @c{compute-value} function
would directly take @c{super} as its second argument,
and @c{(super method-id)} would only be computed in the second branch.
In a lazy context, @c{lazy-method-spec} could also directly use @c{lazy-record-cons}
to add a binding to the record without having to eagerly compute the bound value.

Whichever way simple modular extensions are defined,
they can thereafter be composed into larger modular extensions
using the @c{mix} function, and eventually instantiate a target record
from a modular extension using the @c{fix} function.
Since we will be using records a lot,
we will use the @c{fix-record} function above specialized for records.
Note that since our targets are records, our minimal object system is
closer to Prototype OO than to Class OO,
though, as we will see, it doesn’t offer “prototypes” per se,
or “objects” of any kind.

@subsubsection{Minimal Colored Point}

Let us demonstrate the classic “colored point” example in our Minimal Object System.
We can define a modular extension for a point’s coordinates as follows:
@Code{
(define coord-spec
  (mix (method-spec 'x (λ (self) (λ (inherited) 2)))
       (method-spec 'y (λ (self) (λ (inherited) 4)))))}
The modular extension defines two methods @c{x} and @c{y},
that respectively return the constant numbers @c{2} and @c{4}.

We can similarly define a modular extension for some record’s @c{color} field as follows:
@Code{
(define color-spec
  (method-spec 'color (λ (self) (λ (inherited) "blue"))))}

And we can check that indeed we can instantiate a point specified by combining
the color and coordinate modular extensions above, and verify that indeed the values
for @c{x} and @c{color} are as expected:
@Code{
(define point-p (fix-record (mix color-spec coord-spec)))}
@Code{
(point-p 'x) ;⇒ 2
(point-p 'color) ;⇒ "blue"}
When querying the composed modular extensions for the value for method @c{x},
in a call-by-value right-to-left propagation of information,
the first modular extension, @c{coord-spec}, ignores the top value @c{#f} passed to it
and returns @c{2} that is then returned unchanged by @c{color-spec} since
@c{x} is not recognized by the @c{case} in @c{color-spec}.
Similarly, the query for method @c{color} returns the string @c{"blue"}.

However, this colored point example is actually trivial:
there is no collision in method identifiers between the two modular extensions,
such that the two modular extensions commute;
and more importantly, the values defined by the modular extensions are constant
and exercise neither modularity nor extensibility:
their value-computing functions make no use of either their @c{self} and @c{super} arguments.
Let us then see more interesting examples.

@subsubsection{Minimal Extensibility and Modularity Examples}

We illustrate extensibility with this example wherein function @c{add-x-spec}
accepts an argument @c{dx}, and returns a modular extension that
overrides method @c{x} with a new value to adds @c{dx} to its @c{inherited} value:
@Code{
(define add-x-spec
  (λ (dx) (method-spec 'x (λ (self) (λ (inherited) (+ dx inherited))))))}

And we illustrate modularity with another example wherein @c{rho-spec}
specifies a new field @c{rho} bound to the euclidian distance
to the point from the origin of the coordinate system, using the pythagorean formula.
We assume two functions @c{sqrt} for the square root (builtin in Scheme)
and @c{sqr} for the square (that could be defined as @c{(λ (x) (* x x))}).
Note how the coordinates @c{x} and @c{y} are modularly extracted
from the module context @c{self}, which is the record being defined,
into which they have to be specified by other modular extensions
to be composed with @c{rho-spec} using @c{mix}:
@Code{
(define rho-spec
  (method-spec 'rho (λ (self) (λ (inherited)
    (sqrt (+ (sqr (self 'x)) (sqr (self 'y))))))))}

We can check that the above definitions work by instantiating
the composed modular extensions @c{(add-x-spec 1)}, @c{coord-spec} and @c{rho-spec},
and verifying that the @c{x} value is indeed @c{3},
i.e. first (right-to-left) specified to be @c{2} by @c{coord-spec},
then incremented by @c{1} by @c{(add-x-spec 1)},
whereas @c{rho} is @c{5}, as computed by @c{rho-spec} from the @c{x} and @c{y} coordinates:
@Code{
(define point-r (fix-record
   (mix (add-x-spec 1)
     (mix coord-spec
          rho-spec))))

(point-r 'x) ;⇒ 3
(point-r 'rho) ;⇒ 5}

This demonstrates how modular extensions do work,
and indeed implement the basic design patterns of OO.

Now, note how trying to instantiate @c{(add-x-spec 1)} or @c{rho-spec} alone would fail:
the former relies on the @c{super} record to provide a useful inherited value to extend,
whereas the latter relies on the @c{self} context to modularly provide @c{x} and @c{y} values.
Neither modular extension is meant to stand alone, but instead to be a mixin in the sense of Flavors.
That not every specification can be successfully instantiated,
is actually an essential feature of modular extensibility,
since the entire point of a specification is to contribute some @emph{partial} information
about a small aspect of an overall computation,
that in general depends on other aspects being defined by other specifications.

@subsubsection[#:tag "IME"]{Interaction of Modularity and Extensibility}

Without extensibility, a modular module specification need never access
the identifiers it specifies via the global module context,
since it can more directly access or inline their local definition
(though it may have to explicitly call a fixpoint locally
if these definitions implicitly involved recursion globally via the module context).

For instance, consider the following modular definition,
to be merged with other specifications defining disjoint sets of identifiers,
where the @c{case} special form of Scheme selects a clause to execute
based on which constant if any (constant symbol, in these clauses) matches its first argument:
@Code{
(define my-modular-spec
   (case method-id
      ((start) 42)
      ((size ((self 'length) (- (self 'end) (self 'start)))))
      ((length) (λ (l) (if (null? l) 0 (+ 1 ((self 'length) l)))))))}
Since by our disjointness hypothesis,
the global specification for @c{start}, @c{len} and @c{length}
will not be overridden, then @c{(self 'start)} and @c{(self 'length)}
will always return be bound to the values locally specified.
Therefore, the value @c{42} may be inlined into the specification for @c{size},
and a fixpoint combinator or @c{letrec} can be used to define the @c{length} function,
that can also be inlined in the specification for @c{size}:
@Code{
(define my-modular-spec-without-global-recursion
  (let ((_start 42))
    (letrec ((_length (λ (l) (if (null? l) 0 (+ 1 (_length l))))))
      (λ (self) (λ (method-id)
        (case method-id
          ((start) _start)
          ((size) (_length (- (self 'end) _start)))
          ((length) _length)))))))}

Now with extensibility, a modular extensible module specification may usefully access
the identifiers @emph{an extension to which} it specifies,
to refer to the final resolved value bound to this identifier
after all the extensions are accounted for,
information that obviously cannot be provided locally by the specification
since it involves further extensions that are not knowable in advance,
many different variants of which can be instantiated in the future.

Thus, consider the specification below, a list of parts method is defined,
with the empty list being the initial default, and a count of parts method is defined
that will count the parts in the future instantiated record,
rather than in the specification so far:
@Code{
(define base-bill-of-parts
  (λ (self) (λ (super) (λ (method-id)
    (case method-id
      ((parts) '())
      ((part-count) (length (self 'parts)))
      (else (super method-id)))))))}
You cannot inline the empty list in the call to @c{(self 'parts)}
because the method @c{parts} can be extended, and indeed
such is the very intent and entire point of this @c{base-bill-of-parts} specification.
Even future extensions cannot inline the value they reach,
unless they are guaranteed that no further extension will extend the list of parts
(a declaration known as “sealing”, after Dylan). @;{CITE Dylan}

The interaction between modularity and extensibility therefore
extends the scope of useful uses for modularity
compared to modularity without extensibility.
Programming with modularity and extensibility is more modular than modularity alone!
This makes sense when you realize that when the software is divided into
small modular extensions many of which conspire to define each bigger target,
there are more modular entities than when there is only
one modular entity for each of these bigger targets.
Modular extensibility enables modularity at a finer grain.

There is another important shift between modularity alone and modularity with extensibility,
that we quietly smuggled in so far,
because it happened naturally when modeling first-class entities using FP,
but that actually deserves to be explicitly noted,
especially since it is not quite natural in other settings
such as existed historically during the discovery of OO
or still exist today for most programmers:
Modularity alone was content with a single global module context that everyone linked into,
but the whole point of extensibility is that you will have many entities
that will be extended in multiple different ways;
therefore when you combine the two, it becomes essential that module contexts
be not a single global entity, but many local entities.
This shift from singular and global to plural and local is essential for Class OO,
and even more so for Prototype OO.

@section{Rebuilding OO from its Minimal Core}

Now that we have reconstructed a minimal OO system from first principle,
we can rebuild all the usual features from OO languages on top of that core.
This section will rebuild the most common features from popular OO languages,
those so omnipresent that most developers think they are necessary for OO,
even though they are just affordances easily added on top of the above core.
More advanced and less popular features will follow in subsequent sections.

@subsection{Rebuilding Prototype OO}

@subsubsection{What did we just do?}
In the previous section, we reconstructed
a minimal yet recognizable model of OO from first principles,
the principles being modularity, extensibility, and first-class entities.
We will see shortly extend this model to support more features
(at the cost of more lines of code).
Yet our “object system” has no classes, and indeed no objects at all:
instead, like the object system of Yale T Scheme @~cite{adams88oopscheme},
on top of which its windowing system was built,
our system is made of records and their specifications,
that enable about everything that a Prototype object system do,
but without either records or specifications being objects nor prototypes.

We defined our system in two lines of code, that can be similarly defined
in any language that has higher-order functions,
even a pure functional language without mutation;
and indeed Nix defines its “extension” system similarly@~cite{nix2015}.
But there is indeed one extra thing Nix does that our system doesn’t,
wherein Nix has prototype objects and our system (as is) has not: conflation.

@subsubsection{Conflation: Crouching Typecast, Hidden Product}

Prototype object systems have a notion of “object”,
wherein the same entity, the “prototype” or “object”,
can be used both for computing methods, as with our model’s target records,
and for composing with other objects through some form of inheritance,
as with our model’s specifications.
How can these two very different notions be unified in a single entity?
Very simply: by bundling the two together as a pair.

The type @c{Proto = Spec × Target} for a prototype is thus notionally
the product of the type @c{Spec} for a specification
and the type @c{Target} for its target.
Giving more precise types for @c{Spec} and @c{Target} may involve
types for fixpoints, subtyping, existential types, etc.
@;{Secref typing}

However, the notions of specification and target and this notional product
all remain unmentioned in the documentation of any OO system we are aware of.
Instead, the product remains implicit, hidden,
and the prototype is implicitly typecast to either of its factors
depending on context:
when calling a method on a prototype, the target is used;
when composing prototypes using inheritance, their respective specifications are used,
and then the resulting composed specification is wrapped into a pair of
the specification and its target.

Our implementation below makes this product explicit,
where we use the prefix @c{pproto} to denote a prototype implemented as a pair.
We use the @c{cons} function of Scheme to create a pair, and
the functions @c{car} and @c{cdr} to extract its respective first and second components;
in a more practical implementation, a special kind of tagged pair would be used,
so the runtime would know to implicitly dereference the target in the common case,
without developers having to painfully maintain the knowledge and
explicitly tell the program when to dereference it (most of the time).
The function @c{pproto←spec} is used to define a prototype from a specification,
and is used implicitly when composing prototypes using inheritance
with the @c{pproto-mix} function.
The function @c{spec←pproto} extracts the specification from a prototype,
so you may inherit from it.
The function @c{target←pproto} extracts the target from a prototype,
so you may call methods on it:
@Code{
(define pproto←spec (λ (spec) (cons spec (fix-record spec))))
(define spec←pproto (λ (pproto) (car pproto)))
(define target←pproto (λ (pproto) (cdr pproto)))
(define pproto-mix
  (λ (child parent) (pproto←spec (mix (spec←pproto child) (spec←pproto parent)))))}

Now, there is a subtle issue with the above implementation:
when a target recursively refers to “itself” as per its specification,
it sees the target only, and not the conflation of the target and the specification.
This is not a problem with second-class OO, or otherwise with a statically staged style
of programming where all the specifications are closed before any target is instantiated,
or at least so within each of many “stages” of evaluation.
But with a more dynamic style of programming where no such clear staging is guaranteed,
it is insufficient@xnote["."]{
  @; TODO Have a later section just on metaobjects?
  Metaobjects are a typical use case where you don’t (in general) have a clean program-wide staging
  of specification and targets: to determine the meta methods,
  you partially instantiate the meta part of the objects, based on which you can instantiate the rest.
}

@subsubsection[#:tag "RC"]{Recursive Conflation}

In a dynamic first-class OO language, the conflation of specification and target
into a single entity, the prototype, must be recursively seen by the target
when instantiating the specification.
This is achieved by having the instantiation function compose a “magic” wrapper specification
in front of the user-given specification before it takes a fixpoint;
said magic wrapper will wrap any recursive reference to the target into
an implicit conflation pair of the specification and the target.
Here is an implementation of that idea, wherein we prefix function names with @c{qproto}:

@Code{
(define qproto-wrapper (λ (spec) (λ (self) (λ (super) (cons spec super)))))
(define qproto←spec (λ (spec) (fix-record (mix qproto-wrapper spec))))}

Note how the following functions are essentially unchanged compared to @c{pproto}:
@Code{
(define spec←qproto (λ (qproto) (car qproto)))
(define target←qproto (λ (qproto) (cdr qproto)))
(define qproto-mix
  (λ (child parent) (qproto←spec (mix (spec←qproto child) (spec←qproto parent)))))}

What changed from the previous @c{pproto} variant was that the
@c{(λ (x) (cons spec x))} extension was moved from outside the fixpoint to inside:
If @c{R} is the parametric type of the reference wrapper
(e.g. @c{R Integer} is the type of a reference to an integer),
and @c{M} is the parameteric type of modular extension, also known as a @emph{recursion scheme},
then the type of @c{pproto} is @c{R (Y M)},
and that of @c{qproto} is @c{Y (R ∘ M)}, so in both cases we have
a reference to a recursive data structure that follows the recursion scheme,
but in the second case further recursive accesses also use the reference.
Note that @c{Y (R ∘ M) = R (Y (M ∘ R))} and @c{Y (M ∘ R)} is the type of
a raw record that follows the recursion scheme and uses references for recursion,
instead of the type of reference to such, i.e. we have in turn @c{Y (M ∘ R) = M (Y (R ∘ M))};
people interested in low-level memory access might want to privilege this latter @c{Y (M ∘ R)}
representation instead of @c{Y (R ∘ M)}, which indeed is a notable difference
between the OO models of C++ vs Java: C++ makes you deal with data structures,
Java with references to data structures.

Now, it is not unusual in computer science for access to records, recursive or not,
to be wrapped inside some kind of reference type:
pointer into memory, index into a table,
key into a database, cryptographic hash into a content-addressed store,
location into a file, string or identifier used in a hash-table or tree, etc.
In the case of recursive data structures implemented as data in contiguous regions of memory,
such level of indirection is inevitable, as there is no way to have a contiguous region of memory
of some size contain as a strict subset a contiguous region of memory of the same size,
as would be required for a data structure to directly
include an recursive element of the same type@xnote["."]{
  Exception: if the only element of a structure is an element of the same structure.
  At that which point it’s just an infinite loop to the same element,
  the type is isomorphic to the unit type, and can be represented as
  a trivial data structure of width zero.
  But if there is any other data element that isn’t a unit type,
  direct recursion would mean infinite copies of it, one for each recursive path,
  which can’t fit in finite memory.
}

This use of a reference wrapper can be seen as an instance of the so-called
“Fundamental theorem of software engineering” @~cite{WikiFTSE}:
@emph{We can solve any problem by introducing an extra level of indirection}.
But more meaningfully, it can also be seen as the embodiment of the fact that,
computationally, @principle{recursion is not free}:
while at some abstract level of pure logic or mathematics,
the inner object is of the same type as the outer one,
and accessing it is free or constant time,
at a more concrete level, recursion involves fetching data from another memory region.
In the best case of a simple sequence of data,
the sequence can be a contiguous array of memory and this fetching is constant time;
in general, this fetching goes through the memory caching hierarchy,
the latency of which grows as the square root of the size of the working set@~cite{MythOfRAM2014}.

Importantly, isomorphic as it might be at some abstract level,
the reference type is not equal to the type being referenced,
and is not a subtype of it.
Thus, the necessary reference wrapper extension is crucially not a strict extension
with respect to the type being wrapped.
This proves that is a bad idea to require all extensions to always be strict
(which would beg the question of which type to be strict for,
or lead to the trivial answer that of being strict for the top type,
which all extensions are trivially).
The reference wrapper, pure isomorphism at one level,
yet effectful non-isomorphism at another
(requiring access to disk, database, network, credentials, user interface, etc.),
also illustrates that one man’s purity is another man’s side effect (to channel Alan Perlis).
For instance, with merkleization, a reference uniquely identifies some pure data structure
with a cryptographically secure hash that you can compute in a pure functional way;
but dereferencing the hash is only possible if you already know the data
based on which to compute and verify the hash, that you indexed into a database
that you need some side-effect to consult.
Many OO languages have an implicit builtin reference wrapper,
as opposed to e.g. explicit pointers as in C++;
but the reference semantics doesn’t disappear for having been made implicit.

@subsubsection{Conflation for Records}

If the target type can be anything, including an atomic value such as small integers,
then there’s nowhere in it to store the specification,
and the conflation of specification and target must necessarily involve
such a wrapping as we saw earlier.
But if the target type is guaranteed to be a (subtype of) Record,
we can do better.

In the Nix extension system, a target is a record (called an attrset in Nix),
mapping string keys to arbitrary values,
and the extension instantiation function @c{fix} stores
the specification under a “magic” string @c{"__unfix__"}.
The advantage is that casting a prototype (called “extension” in Nix)
to its target is a trivial zero-cost identity no-op;
the slight disadvantage is that the target must be a record,
but that record cannot use arbitrary keys,
and must avoid the magic string as key.
As a minor effect, casting to a specification becomes slightly more expensive
(table lookup vs fixed-offset field access),
whereas casting to a target (the more common operation by far) is free.
The semantic is otherwise essentially the same as for our implementation using pairs.

Here is a Scheme implementation of the same idea,
where the prefix @c{rproto} denotes a prototype implemented as a record,
and our magic key is @c{#f}, the boolean false value,
instead of some reserved symbol,
so it doesn’t impede on the free use of arbitrary symbols as keys.
The function @c{rproto←spec} is used to define a prototype from a specification,
by prepending a special specification @c{rproto-wrapper} in front
that deals with remembering the provided specification;
this function is used implicitly when composing prototypes using inheritance
with the @c{rproto-mix} function.
The function @c{spec←rproto} extracts the specification from a prototype,
so you may inherit from it; this specifically doesn’t include the @c{rproto-wrapper},
which would notably interfere with mixing.
The function @c{target←rproto} extracts the target from a prototype,
so you may call methods on it—it is the identity function, and
you can often simplify it away.
@Code{
(define rproto-wrapper
  (λ (spec) (λ (parent) (λ (method-id) (λ (super)
    (if method-id super spec))))))
(define rproto←spec (λ (spec) (fix-record (mix (rproto-wrapper spec) spec))))
(define spec←rproto (λ (rproto) (rproto #f)))
(define target←rproto (λ (rproto) rproto))
(define rproto-mix
  (λ (child parent) (rproto←spec (mix (spec←rproto child) (spec←rproto parent)))))}

Once again, some special extension is used in front, that is not strict,
and is almost-but-not-quite an isomorphism, and specially memorizes the specification.

@subsubsection{Small-Scale Advantages of Conflation: Performance, State Sharing}

First, note how, if a specification is pure functional,
i.e. without side-effect, whether state, non-determinism, I/O or otherwise,
then indeed there is only one target, up to behavioral equality;
recomputing the target multiple times will lead to the same result in all contexts;
caching the target value next to specification can thus be seen as a performance enhancement.
However, in case of recursive access to the target, this performance enhancement
can grow exponentially with the depth of the recursion,
by using a shared computation instead of repeated recomputations
(see the related discussion on the applicative Y combinator in
@secref{DSF}).

If however, the specification has side-effects
(which of course supposes the language also has side-effects),
then multiple computations of the target value will lead to different results,
and caching a one canonical target value next to the specification is
not just a performance enhancement, but a critical semantic feature enabling
the sharing of the state and side-effects of a prototype between all its users.
And if some users explicitly want to recompute the target,
they can always clone the prototype,
i.e. create a new prototype that uses the same specification,
or inherits from it using the neutral element @c{(proto←spec idMExt)}.

Now, plenty of earlier or contemporary Prototype OO languages,
from Ani and ThingLab to SELF and JavaScript and beyond,
support mutation yet also offer objects as conflation of two aspects,
one for inheritable and instantiatable specification,
another one for the instantiated target that holds mutable state
and that methods are called against.
However, the two aspects might not be as neatly separated
in these stateful languages as in pure functional languages,
because the mutation of the internals of the specification, in languages that allow it,
may interact with the target state and behavior in weird ways.
This mutation is not usually colloquial in production code,
but may be heavily relied upon during interactive development,
or as part of implementing advanced infrastructure. @;{ TODO secref mutation }

Also note that if your choice of representation for specifications and targets
is such that instantiating a specification may itself issue side-effects such
as errors or non-termination or irreversible I/O, then it becomes essential
to wrap your target behind lazy evaluation, or, in Scheme, a @c{delay} form,
so that you may still define prototypes from incomplete erroneous specifications,
and use them through inheritance to build larger prototypes, that, when complete,
will not have undesired side-effects.
Once again, laziness proves essential to OO,
even and especially in presence of side-effects.

@subsubsection{Large-Scale Advantage of Conflation: More Modularity}

Remarkably, conflation makes prototypes more modular than the lack thereof,
because thanks to it, programmers do not have to decide in advance
when each part of their configuration is to be extended or instantiated.
Indeed, if only using unconflated specifications and targets,
one would have to choose for each definition of each identifier in each (sub)module
which of a specification or its target to use;
and when choosing to maintain a specification for the purpose of extensibility,
the programmer may still want to also explicitly bind an identifier to the target,
and another to the specification,
for the sake of state sharing and sanity and not having to remember the hard way
the “shape” of the nested extensions to replace by the values when instantiating the target.
Thus, users would end up doing by hand in an ad hoc way what conflation
gives them systematically for free.

Furthermore, a user cannot know which of his definitions he himself or another user
might later want to extend.
A simple service will often be made part of a larger service set,
in multiple copies each slightly tweaked;
its configuration, complete and directly instantiable as it may be for its original author,
will actually be extended, copied, overridden, many times, in a larger configuration,
by the maintainers of the larger service set.

The modularity of conflation is already exploited at scale
for large software distributions on one or many machines,
using GCL, Jsonnet or Nix as (pure functional) Prototype OO languages:
@itemize[
@item{
  The Google Control Language GCL@~cite{gclviewer2008} (née BCL, Borg Control Language),
  has been used to specify all of Google’s distributed software deployments
  since about 2004 (but uses dynamic rather than static scoping,
  causing dread among Google developers).}
@item{
  Jsonnet@~cite{jsonnet}, inspired by GCL but cleaned up to use static scoping,
  has been particularly used generate configurations for AWS or Kubernetes.}
@item{
  Nix@~cite{dolstra2008nixos} is used not just to configure
  entire software distributions for Linux and macOS,
  but also distributed services with NixOps or DisNix.}]
All three languages have proven the practicality of pure lazy functional prototype objects,
with mixin inheritance and conflation of specification and target,
as a paradigm to specify configuration and deployment of software on a world-wide scale,
each with hundreds of active developers, tens of thousands of active users,
and millions of end-users.
Despite its minimal semantics and confidential existence,
this mixin prototype object model has vastly outsized outreach.
It is hard to measure how much of this success is due to the feature of Conflation as such,
yet this feature is arguably essential to the ergonomics of these languages.

@subsubsection{Implicit Recognition of Conflation by OO Practitioners}

The notion of a @emph{conflation of specification and target},
that we presented, is largely unknown by OO developers, and
seems to have been made explicit in publication only as late as 2021 @~cite{poof2021}.
And yet, the knowledge of this conflation is implicit in the OO community.

Common practitionners of OO have long implicitly recognized
the conflated concepts of specification and target
by making the distinction between abstract classes and concrete classes@~cite{johnson1988designing}:
an abstract class is one that is only used for its specification,
to inherit from it;
a concrete class is one that is only used for its target type,
to use its methods to create and process class instances.
Experienced practitioners recommend keeping the two kinds of classes separate, and
frown at inheriting from a concrete class,
or trying to instantiate an abstract class.

Theorists have also long implicitly recognized the conflated concepts
when working to develop sound type systems for OO:
for instance, Fisher @~cite{Fisher1996thesis} distinguishes
@c{pro} types for objects-as-prototypes and @c{obj} types for objects-as-records;
and Bruce @~cite{bruce1996typing} complains that
“the notions of type and class are often confounded in object-oriented programming languages” and
there again distinguishes subtyping for a class’s target type (“subtyping”)
and for its open specification (“matching”).
Yet though they offer correct models for typing OO,
both authors fail to distinguish specification and target
as syntactically and semantically separate entities in their languages,
leading to much extraneous complexity in their respective type systems.

Thus, through all the confusion of class OO languages so far,
both practitioners and theorists have felt the need to distinguish specification and target,
even though no one seems to have been able to fully tease apart the concepts up until recently.

@subsection[#:tag "Rebuilding_Classes"]{Rebuilding Classes}

@subsubsection{A Class is a Prototype for a Type}

Having elucidated Prototype OO in the previous sections,
including its notion of Object as conflationg of Prototype and Instance,
we can now fully elucidate Class OO including its notion of Class:
@principle{A Class is a Prototype for a Type}.
Or, to be pedantic, a class is a prototype, the target of which is @emph{type descriptor},
i.e. a record describing a type together with methods associated to the type.

The target type of a class is usually, but not always,
a record type (indexed product, structure, struct, named tuple),
at which point the descriptor will also describe for its fields;
it may also be an enumeration type (indexed sum, enum, variant, discriminated union, tagged union),
at which point the descriptor will also describe its alternatives;
and while this is less common in Class OO, a class’s target could really describe
any kind of type: function, number, array, associative array, etc.

The target of a class may be a runtime type descriptor,
that describes how to create, recognize, and process elements of the type
in the same evaluation environment that the type exists in;
or, as often the case in second-class Class OO,
the target may be a compile-time type descriptor,
that describes how to generate code for entities of that type
to be executed in a further stage of evaluation;
or it may be both.

Whichever kind of type descriptors are used,
Class OO is but a special case of Prototype OO,
wherein a class is a prototype for a type,
i.e. the conflation of a modular extension for a type descriptor,
and the type descriptor that is the fixpoint of that specification.
Thus when we claimed in @seclink{classes_only} that
the situation of classes in OO was similar to that of types in FP,
we meant it quite literally.

@subsubsection{Simple First-Class Type Descriptors}

Not desiring to introduce a theory of compilation in this essay
in addition to a theory of OO, we will only illustrate how to build
first-class Class OO at runtime on top of Prototype OO,
using a technique described by Lieberman @~cite{Lieberman1986},
and no doubt broadly similar to how many Class OO systems were implemented on top of Javascript
before web browsers ubiquitously adopted ES6 classes@~cite{EcmaScript:15}. @;{CITE ???}

A type descriptor (which in C++ would correspond to a @c{vtable})
will typically have methods as follows:
@itemize[
@item{A method @c{instance-methods} returning a record of instance methods
      (or, as a runtime optimization that requires more compile-time bookkeeping,
      encode those object methods directly as methods of the type descritor).}
@item{For record types, a method @c{instance-fields}, a record of field descriptors,
      each of them a record with fields @c{name} and @c{type} (and possibly more).
      Also, self-describing records (the default)
      will have a special field @c{#t} (the Scheme true boolean) to hold their type descriptor
      (we could have used the string @c{"__type__"} if keys had to be strings
      using a common convention of surrounding a system-reserved identifier with underscores;
      but in Scheme we can use a different kind of entity and leave strings entirely for users;
      our choice of @c{#t} also rhymes with our previous choice of
      @c{#f} (the Scheme false boolean) to hold specification;
      plus @c{#t} also has the same letter as the initial of “type”).}
@item{In dynamic languages, or static languages with types as subsets of canonical static types,
      a method @c{element?} as predicate that is true if its argument is an element of the type.
      If the language supports error reporting, a method @c{validate} that returns
      its argument unchanged if it is an element, and otherwise raises a suitable error.
      Either method can be derived from the other using sensible defaults.
      First-class class instances are definitely subsets of whichever underlying data type
      is used for their representation, and so their type descriptors
      will usefully have such a method.}
@item{Usually some kind of method @c{make-instance} to create a new object instance
      with some extensible protocol from user-specified arguments.
      To support indexed sum types, either the @c{make-instance} method
      will take a discriminant as first argument,
      or a method @c{instance-constructors} could hold a record
      of several “constructor” functions for each case.
      Instead of constructors, or in addition to them,
      a basic @c{instance-prototype} (or @c{instance-prototypes})
      could be offered that implements the skeleton of a class instance.}
@item{For languages that support users managing dynamic object lifetime,
      a @c{destroy-instance} method could be part of the class, or among the @c{instance-methods};
      or if there can be several kinds of destructors,
      they could be held by a method @c{instance-destructors}.}]

A “class instance” (corresponding to an “object” in Class OO) is a self-describing record,
the type descriptor of can be extracted with the function @c{type-of} below;
to call an “instance method” on it, you use the function “instance-call” with it as first argument,
and it calls the method from the instance’s type’s @c{instance-methods} record,
with the instance as first argument.
@Code{
(define type-of (λ (instance) (instance #t)))
(define instance-call (λ (instance) (λ (method-id)
  ((((type-of instance) 'instance-methods) method-id) instance))))}

Note that, if we use the Nix approach of zero-cost casting to target when the target is a record,
then we can use the very same representation for type descriptors, whether they were generated
as the fixpoint target of a specification, or directly created as records without such a fixpoint.
This kind of representation is notably useful
for bootstrapping a Meta-Object Protocol@~cite{amop}.

As for “class methods” (also known as “static methods” in C++ or Java),
they can be regular methods of the type descriptor,
or there can be a method @c{class-methods} in the type descriptor containing a record of them.

@;{
TODO Discuss encodings and types?

Encoding as record of pre-methods
[AC96c] M. Abadi and L. Cardelli. A theory of primitive objects: Untyped and first-order
        systems. Information and Computation, 125(2):78-102, 1996. Earlier version appeared
        in TACS ’94 proceedings, LNCS 789.

Reppy, Rieke "Classes in ObjectML via Modules" 1996 FOOL3

BruceCardelliPierce2006
}

@subsubsection{Parametric First-Class Type Descriptors}

There are two main strategies to represent parametric types:
“function of descriptors” vs “descriptor of functions”.

In the “function of descriptors” strategy, a parametric type is represented as
a function from type descriptor to type descriptor,
taking a type parameter as input, and
returning a type descriptor specialized for those parameters as output.
As it computes the specialized descriptor, it can apply various specializations and optimizations,
pre-selecting code paths and throwing away cases that do not apply,
allowing for slightly better specialized code,
at the expense of time and space generating the specialized descriptor.
This representation allows for uniform calling conventions
for all type descriptors satisfying the resulting monomorphic interface,
regardless of whether it was obtained by specializing a parametric type or not.
This strategy is notably used during the “monomorphization” phase
used within C++ compilers when expanding templates.
It is useful when trying to statically inline away all traces of type descriptors before runtime,
but in a dynamic setting requires creation of more descriptors,
or some memoization mechanism.

In the “descriptor of functions” strategy, a parametric type is represented as
a type descriptor the methods of which may take extra parameters,
one the type descriptor of each type parameter.
Methods that return non-function values may become functions of one (or more) type parameter(s).
Thus, the type descriptor for a functor @c{F} may have a method @c{map}
that takes two type parameters @c{A} and @c{B} and transforms an element of @c{P A}
into an element of @c{P B}.
This strategy eliminates the need to heap-allocate a lot of specialized type descriptors;
but it requires more bookkeeping to remember which method of which type descriptor
takes how many extra type descriptor parameters.

Both strategies are useful, and which to prefer depends on the use case and its tradeoffs.
A given program or compiler may use both, and may very well have to:
even using the “descriptor-of-functions” strategy, you may still have to generate
specialized type descriptors, so that you may pass them to functions
that expect their parametric type descriptors to only take @c{N} type parameters,
and are unaware that these were specialized from more general parametric type descriptors
with @c{N+M} type parameters (where @c{M > 0}).
This unawareness may stem from any kind dynamic typing, or
from a choice to avoid generating many copies of the code
for each value of @c{M} as the depth of parametric constructors varies.
And even using the “function-of-descriptors” strategy,
you may want to maintain collections of functions that each take
one or many descriptors as arguments,
especially when such collections contain a large number of functions,
only one or a few of which are to be used at a time per (tuple of) descriptor argument(s),
and this (tuple of) descriptor argument(s) itself changes often.

Even fixpoints can be done differently in the two strategies, wherein
the “function-of-descriptors” strategy leads to generating descriptors that embed the fixpoints
so that clients never need to know they were even fixpoints,
whereas the “descriptor-of-functions” strategy leads to descriptors the calling convention of which
requires clients to follow a convention of passing the descriptor itself to the functions
so as to close the fixpoint loop.

@subsubsection[#:tag "Class_style_vs_Typeclass_style"]{Class-style vs Typeclass-style}

Now, there is a slight variant on using type descriptors, wherein,
instead of passing around “class instances” such that functions each time extract
must extract the type descriptor of their argument objects
to then invoke methods as extracted from this type descriptor,
you explicitly pass along the type descriptor together with values of the described type.
The type descriptor then corresponds to the dictionaries
into which Haskell desugar uses of its typeclasses @~cite{typeclasses},
or to the first-class “interfaces” of “Interface-Passing Style” @~cite{LIL2012}
The advantages are many:
@itemize[
  @item{
    When running an algorithm that involves many values of the same type,
    you can pass along one type descriptor for all these values, and not have
    to extract the same descriptor each and every time from values represented as class instances,
    for a slight performance bonus.
    Algorithms for which performance matter involve a lot of calls to methods
    on values of the same type, at which point it is always a good performance
    enhancement to compute a type descriptor and extract its methods once and only once, cache it,
    and repeatedly call the cached method—instead of going through method extraction mechanisms
    over and over again for each method call.
    A good static type system will also save you from having to check the type
    of each and every value.}
  @item{
    Because the type descriptor is being passed “out of band”,
    values can be light and not have to carry their type descriptor in a record field,
    saving a bit space. Actually, the values being described need not even be records at all:
    if the language has primitive types and type constructors such as
    numbers, tuples, vectors, strings, etc.,
    these types can also be described and have methods associated to them
    despite not being records carrying a type descriptor.}
  @item{
    There can be many different type descriptors that match any given object,
    with different methods to work with them from a different point of view,
    parameterized by administrator-configured or user-specified parameters that vary at runtime.
    For instance, a same number may be used, in different contexts,
    with type descriptor that will cause it to be interacted with as a decimal number text,
    a hexadecimal number text, a position on a slide bar, or a block of varying color intensity;
    or to be serialized according to some encoding or some other.
    In a static language like Haskell, @c{newtype} enables compile-time selection between
    multiple different points of view on what underneath is a “same” low-level data representation;
    in a language with Prototype OO,
    “typeclass”-style type descriptors enable the same kind of behavior,
    sometimes to implement at runtime
    second-class typeclasses that are known constants at compile-time,
    but sometimes for first-class typeclasses that change at runtime based on user interaction,
    even while the object representation may stay the same.}
  @item{
    Constructors are quite special in “class-style”,
    since regular methods are called by extracting them from an object’s type descriptor,
    but there is not yet an object from which to extract a type descriptor
    when what you are doing is precisely constructing the first known object of that type
    (in the given scope at least).
    Constructors are so special, that some encodings of classes identify a class with
    “the” constructor function that generates an object of that type,
    complete with an appropriate type descriptor.
    This creates an asymmetry between constructors and other methods,
    that requires special treatment when transforming type descriptors.
    By contrast, in “typeclass-style”, constructors are just regular methods;
    there can be more than one, espousing different calling conventions,
    or creating element from different subsets or subtypes of the type (disjoint or not).
    Many typeclass transformations, dualities, etc., become more uniform and simpler
    when using typeclass-style. Type descriptors in typeclass style, in which constructors
    are just normal methods, are therefore more natural and easy to use for parametric polymorphism
    than type descriptors in class style.}
  @item{
    Finally, typeclass-style can be extended more easily and more uniformly
    than traditional class-style to support APIs involving
    multiple related types being simultaneously defined in mutually recursive ways:
    data structures and their indexes, paths or zippers, containers and containees,
    bipartite graphs, grammars with multiple non-terminals, expressions and types, etc.
    Instead of distinguishing a main class under which others are nested,
    or having a hierarchy of “friend” classes indirectly recursing through a global context,
    typeclass-style treats all classes uniformly, yet can locally encapsulate
    an entire family of them, potentially infinite.
    There is however, a way to retrieve most of these advantages of typeclass-style
    while remaining in class-style, though only few languages support it:
    using multi-methods (see below). @; TODO secref
}]

There is an isomorphism between class-style and typeclass-style type descriptors,
to the point that a simple transformation can wrap objects written in one style and their methods
so they follow the calling convention of the other style.
Where a typeclass uses “naked” primitive values that may not always themselves be records,
class-style will require all these values to be wrapped in a class instance with one field
that records the value, and a type descriptor for a class whose methods
invoke the typeclass methods on the unwrapped values, and rewraps results that need be
(in a pure setting, or update the wrapped value, in a mutable setting),
based on which positions in each method’s type is of the proper self-type.
@;{
  TODO
  "encapsulation" meh. We don’t need every value in the computation to be encapsulated
  in an extra layer of records; that is emphatically not the point of OO.
  The existentially quantified record that describes the computation is already
  all the encapsulation we need and want!
  The computation that was modularly extensibly specified can directly handle
  numbers and pointers, APL tables, etc., and can deal with any paradigm,
  not just object graph of encapsulated data.
  Now, if the underlying language, like Smalltalk,
  exposes its primitive types with a uniform interface compatible with its “object” records,
  that’s great, nothing extra to do there;
  then you have “pure OO”.
  @xnote["."]{
    Even then, primitive types are not usually be extensible,
    though tricks a la CLOS MOP could be used to make them so. @~cite{AMOP}
  }
}
Simple metaprograms that enact this transformation have been written in Lisp @~cite{LIL2012}
in the simple case of functions in which the self-type only appears directly
as one of the (potentially multiple) arguments or (potentially multiple) return values of a method.
Such automation could be generalized to work with any kind of higher-order function types,
where occurrences of the self-type in arbitrary position
require suitable wrapping of arguments and returns values,
in ways similar to how higher-order contracts
wrap arguments and return values with checks@~cite{findler2002contracts}.
Note how similarly, metaprograms have been written to transform pure objects into
mutable objects that store a current pure value, or mutable objects into
linear pure objects that become invalid if a use is attempted after mutation.

Thus, programming with either classes or typeclasses,
wherein objects are either pure or mutable, is mainly a matter of style,
with some tradeoffs with respect to performance or ease of reasoning between those four styles;
in the end, these four styles yield equivalent semantics such that programs in one style
can be mechanically transformed into programs in another style, and vice versa.

Finally, note that type descriptors can be used either “class-style” or “typeclass-style”
without any OO involved to modularly and extensibly specify these descriptors.
Indeed, the dictionaries that Haskell typeclasses expand into are generated
from modular but non-extensible specifications.
The same holds for the equivalent feature called “traits” in Rust,
that do not allow for extension of inherited methods.
Modules in SML or OCaml can also offer “typeclass-style” type modular descriptors
without modular extensibility through inheritance.
Non-OO class-style type descriptors are also possible, some languages such as Gambit Scheme
allow users to define new data structures, and to declare the equivalent of methods
that specialize how values of those new types will be printed, or tested for equality,
without these methods being part of any actual object system capable of inheritance,
yet with each object carrying the equivalent of a type descriptor field
in “class style” so that the system knows which method to use.
It is possible to build an actual OO class system on top of such non-OO “class-style” mechanism,
and the @(GerbilScheme) object system is indeed built atop the Gambit Scheme non-OO structure facility.
@;{ TODO Kiselyov and Lämmel’s OOHaskell @~cite{Kiselyov2005HaskellOOS}
also enable OO on top of non-OO constructs.
TODO re-read in detail and find whether this is the best place to cite,
how it interoperate with typeclasses. }
And it is also possible to program using OO completely in typeclass-style,
without any class instance, only typeclass-style type descriptors
that are specified in modular extensible ways. @;{TODO cite LIL library?}

@subsubsection{A Class is Second-Class in Most Class OO}

In most Class OO languages,
the semantics of Class OO is fully evaluated at compile-time,
in a “type-level” evaluation stage.
The class specifications, and the type descriptors they specify,
are second-class entities:
they are not available as first-class values subject to arbitrary programming at runtime.
Class OO then is a special case of Prototype OO,
but only in a restricted second-class language—a reality
that is quite obvious when doing template metaprogramming in C++.
@;{TODO SECREF appendix demonstrating lazy prototype OO in C++}

Some dynamic languages, such as Lisp, Smalltalk, Ruby or Python,
let you programmatically use, create, inspect or modify classes at runtime,
using some reflection mechanisms.
Even though regular definition and usage of classes uses dedicated syntax,
such that restricting yourself to the language fragment with that syntax
and never using reflection would be equivalent to classes being second-class,
the reflection mechanisms ultimately make classes into first-class entities.
Indeed, the second-class semantics of classes are often implemented in terms of
those first-class mechanisms, that are thus just as powerful, or more so.

Static languages lack such full-powered runtime reflection mechanisms
(or would arguably be dynamic languages indeed).
Some lack any runtime reflection at all;
but many offer read-only runtime reflection,
whereby users can inspect classes created at compile-time, yet not create new ones:
such capabilities can notably simplify I/O, property-based testing, debugging, etc.
A few static language implementations may even offer limited ability
to modify existing classes at runtime,
but often void the compiler’s warranty of those who use them.

@subsubsection{Type-Level Language Restrictions}

The language in which second-class classes are defined and composed
in static languages is almost never the same as the “base” language
in which the programmer is specifies first-class computations (e.g. C++, Java, C#),
but instead a distinct @emph{type-level language},
deliberately restricted in expressiveness so as to enable static analysis and optimizations,
in which the types and the base-level functions operating on them
are being modularly and extensibly specified.

Restrictions on the type-level language
often deliberately attempt to keep it from being “Turing-equivalent”.
This attempt sometimes succeeds (as in OCaml), but more often than not
utterly fails, as computational power emerges from unforeseen interactions
between language features added over time (as in C++, Java, Haskell).
@TODO{cite Nada on Java type system constraints}
@TODO{cite LangSec on unforeseen emergence of Turing-equivalence through “weird machines”?}
The attempts do usually succeed, however, at making these type-level languages
require a completely different mindset from the “base language”,
and very roundabout design patterns, to do anything useful,
a task then reserved for experts.

Computationally powerful or not, the type-level language of a Class OO language
is almost always very different from the base language:
the type-level languages tend to be pure functional or logic programming languages
with pattern-matching and laziness but without any I/O support,
even though the base languages themselves tend to be
eager stateful procedural languages with lots of I/O support
and often without pattern-matching or laziness
(or limited ones as afterthoughts).

@subsubsection{More Popular yet Less Fundamental}

Class OO was historically discovered (1967)
nine years before Prototype OO (1976),
and remains overall more popular in the literature and in practice:
most popular OO languages only offer Class OO; @;CITE
and even though the arguably most popular OO language, JavaScript,
may have started with Prototype OO only (1995),
people were constantly reimplementing classes on top, and twenty years later
classes were added to the language itself@~cite{EcmaScript:15}.

And yet we will argue that Prototype OO is more fundamental than Class OO:
as we demonstrated above, Class OO can be very easily expressed in terms of Prototype OO
and implemented on top of it,
such that inheritance among classes is indeed a special case of
inheritance among the underlying prototypes;
however the opposite is not possible,
since you cannot express Prototype OO’s first-class entities and their inheritance
in terms of Class OO’s second-class entities and their inheritance.

At best, Prototype OO can be implemented on top of those dynamic languages
that offer full-powereed reflection so that prototypes can be classes;
but even then it is unclear how much these mechanisms help,
compared to directly implementing prototypes.
There could be code sharing between the two, yet trying to fit prototypes
on top of classes rather than the other way around is what Henry Baker dubbed
an @emph{abstraction inversion} @~cite{Baker1992CritiqueDKL},
i.e. putting the cart before the horse.

@subsection{Types for OO}

@subsubsection{Dynamic Typing}

One could just say that objects are of a monomorphic type @c{Record},
and that all accesses to methods are to be dynamically typed and checked at runtime,
with no static safety.
Many OO languages, like Smalltalk, Lisp, Ruby, Python, Javascript, Jsonnet, Nix, etc.,
adopt this strategy.

Advantages of dynamic typing include the ability to express programs that even the best
static typesystems cannot support, especially when state of the art typesystems are too
rigid, or not advanced enough in their OO idioms.
Programs that involve dependent types, staged computation,
metaprogramming, long-lived interactive systems,
dynamic code and data schema evolution at runtime, etc.,
can be and have been written in dynamically typed systems
that could not have been written in existing statically typed languages,
or would have required reverting to unitypes with extra verbosity.

On the other hand, system-supported static types can bring extra performance and safety,
help in refactoring, debugging programs,
some forms of type-directed metaprogramming, and more.
And even without system enforcement, thinking in terms of types can help understand
what programs do or don’t and how to write and use them.
We have already started to go deeper by describing records as indexed products.
Let’s see how we can model OO with more precise types.

@subsubsection{Partial Record Knowledge as Subtyping}

In a language with static types,
programmers writing extensible modular definitions should be able to specify types
for the entities they provide (an extension to) and require (a complete version of),
without having to know anything about the types of the many other entities
they neither provide nor require:
indeed, these other entities may not have been written yet, and by other people,
yet will be linked together with his modules into a complete program.

Now, with only modularity, or only extensibility, what more second-class only,
you could contrive a way for the typechecker to always exactly know all the types required,
by prohibiting open recursion through the module context,
and generating magic projections behind the scenes (and a magic merge during linking).
But as we saw previously in @seclink{IME},
once you combine modularity and extensibility, what more first-class,
then open recursion through the module context becomes the entire point,
and your typesystem must confront it.

Strict extensibility, which consists in monotonically contributing partial knowledge
about the computation being built,
once translated in the world of types, is subtyping.
In the common case of records,
a record type contains not just records
that exactly bind given identifiers to given types,
but also records with additional bound identifiers, and existing identifiers bound to subtypes.
Record types form a subtyping hierarchy; subtyping is a partial order among types;
and function types monotonically increase with their result types,
and decrease with their argument types.
Then, when modularly specifying a module extension, the modular type for the module context,
that only contains “negative” constraints about types for the identifiers being required,
will match the future actual module constraint, that may satisfy many more constraints;
meanwhile, the “positive” constraints about types for the identifiers being provided
may satisfy more many constraints than those actually required from other modules using it.

@subsubsection[#:tag "NNOOTT"]{The NNOOTT, Naive Non-recursive Object-Oriented Type Theory}

The simplest and most obvious theory for typing OO,
that we will dub the Naive Non-recursive Object-Oriented Type Theory (NNOOTT),
consists in considering subclassing (a relation between specifications)
as the same as subtyping (a relation between targets).
Thus, in this theory, a subclass, that extends a class with new fields,
is (supposedly) a subtype of the parent “superclass” being extended.
@;{ XXX Eiffel, Java, C#, Smalltalk
  The theory is implicit in the names of the @c{is} operator in C#.
}

To demonstrate an advanced variant of the NNOOTT,
consider extending the Simply Typed Lambda-Calculus (STLC),
or some more elaborate but still well-understood variant of the λ-calculus,
with primitives for the language’s builtin constructs and standard libraries,
and (if not already available) a minimal set of features for OO:
indexed products for records, subtyping (@c{⊂} or @c{≤} or in ASCII @c{<:})
and type intersections (@c{∩}).
In this NNOOTT variant, a NNOOTT Modular Extension could have a type of the form
@Code{
type NMExt required inherited provided =
  required → inherited → (inherited ∩ provided)}
A @c{NMExt} is a type with three parameters,
the type @c{required} of the information required by the modular extension from the module context,
the type @c{inherited} of the information inherited and to be extended,
and the type @c{provided} of the information provided to extend what is inherited.
Note that this type refines the @c{C → V → V} from @seclink{MOI}:
@c{inherited} and @c{provided} each separately refine the value @c{V} being specified;
that value can be anything: it need not be a record at all, and if it is,
it can have any shape or type, and does need not have the same as the module context.
Meanwhile, @c{required} refines the module context @c{C}, and is (almost) always some kind of record.
The two need not be the same at all, and usually are not for (open) modular extensions,
unless and until you’re ready to close the recursion, tie the loops and compute a fixpoint.

In Prototype OO, the value inherited holds the methods defined so far by the ancestors;
the value provided consists in new methods and specialization of existing methods;
the top value is an empty record.
In Class OO, that value inherited holds methods and fields defined so far by the ancestors;
the value provided consists in the new or specialized fields and methods;
the top value is a type descriptor for an empty record type.
In both cases, the context required may narrowly define a prototype or class,
but may also more broadly define an entire namespace.

The @c{fix} operator, first takes a top value as a seed,
then second takes a specification for a target with the target itself as module context
and starting with the top value as a seed, and returns the target fixed-point.
The @c{mix} operator chains two mixins, with the asymmetry that
information provided by the parent (parameter @c{p2} for the second argument)
can be used by the child (first argument), but not the other way around.
@Code{
fix : top → NMExt target top target → target
mix : NMExt r1 i1∩p2 p1 → NMExt r2 i2 p2 → NMExt r1∩r2 i1∩i2 p1∩p2
}

This model is simple and intuitive, and
has good didactic value to explain how inheritance works:
given two “mixin” specifications, you can chain them as child and parent;
the combined specification requires a context with all the information
required from either child or parent;
the inherited information must contain all information expected by the parent,
and all information expected the child that isn’t provided by the parent;
the provided information contains all information provided by either child or parent.

However, this “Naive Non-recursive OO Type Theory”, as the name indicates,
is a bit naive indeed, and only works in simple non-recursive cases.
Yet the NNOOTT is important to understand,
both for the simple cases it is good enough to cover,
and for its failure modes that tripped so many good programmers
into wrongfully trying to equate inheritance and subtyping.

@subsubsection{Limits of the NNOOTT}

The NNOOTT works well in the non-recursive case, i.e.
when the type of fields does not depend on the type of the module context;
or, more precisely, when there are circular “open” references between
types being provided by a modular extension,
and types it requires from the module context.
In his paper on objects as co-algebras,
Bart Jacobs characterizes the types for the arguments and results of his methods
as being “(constant) sets” @~cite{Jacobs1995ObjectsAC}@xnote[","]{
  Jacobs is particularly egregious in smuggling this all-important restriction
  to how his paper fails to address the general and interesting case of OO
  in a single word, what more, in parentheses, at the end of section 2,
  without any discussion whatsoever as to the momentous significance of that word.
  A discussion of that significance could in itself have turned this bad paper into a stellar one.
  Instead, the smuggling of an all-important hypothesis makes the paper misleading at best.
  His subsequent paper @~cite{Jacobs1996InheritanceAC}
  has the slightly more precise sentence we also quote,
  and its section 2.1 tries to paper over what it calls “anomalies of inheritance”
  (actually, the general case), by separating methods into a “core” part
  where fields are declared, that matter for typing inheritance,
  and for which his hypothesis applies, and “definitions” that must be reduced to the core part.
  The conference reviewing committees really dropped the ball on accepting those papers,
  though that section 2.1 was probably the result of at least one reviewer doing his job right.
  Did reviewers overall let themselves impressed by formalism beyond their ability to judge,
  or were they complicit in the sleight of hand to grant their domain of research
  a fake mantle of formal mathematical legitimacy?
  Either way, the field is ripe with bad science,
  not to mention the outright snake oil of the OO industry in its heyday:
  The 1990s were a time when IBM would hire comedians to become “evangelists”
  for their Visual Age Smalltalk technology, soon recycled into Java evangelists.
  Jacobs is not the only one, and he may even have extenuating circumstances.
  He may have been ill-inspired by Goguen, whom he cites, who also abuses
  the terminology from OO to make his own valid but loosely-related
  application of Category Theory to software specification.
  He may also have been pressured to make his work “relevant” by publishing in OO conferences,
  under pains of losing funding, and
  he may have been happy to find his work welcome even though he didn’t try hard,
  trusting reviewers to send stronger feedback if his work hadn’t been fit.
  The reviewers, unfamiliar with the formalism,
  may have missed or underestimated the critical consequences of a single word.
  In other times, researchers have been hard pressed to join the bandwagon of
  Java, Web2, Big Data, Mobile, Blockchain or AI, or whatever trendy topic of the year;
  and reviewers for the respective relevant conferences may have welcome
  newcomers with unfamiliar points of view.
  Even Barbara Liskov, future Turing Award recipient, was invited to contribute to OO conferences,
  and quickly dismissed inheritance to focus on her own expertise,
  which involves modularity without extensibility. @; CITE
  Are either those who talk and publish what turns out not to be OO at all at OO conferences,
  or those who invite them to talk and publish, being deliberately misleading?
  Probably not, yet, the public can be fooled just the same as if dishonesty were meant:
  though the expert of the day can probably make the difference,
  the next generation attending or looking through the archives
  may well get confused as to what OO is or isn’t about as they learn from example.
  At the very least, papers like that make for untrustworthy identification and labeling
  of domains of knowledge and the concepts that matter.
  The larger point here being that we should be skeptical of papers,
  even by some of the greatest scientists
  (none of Jacobs’, Goguen’s nor Liskov’s expertises are in doubt),
  even published at some of the most reputable conferences in the field (e.g. OOPSLA, ECOOP),
  because science is casually corrupted by power and money,
  and only more cheaply so for the stakes being low.
  This particular case from thirty years ago is easily corrected in retrospect;
  its underlying lie was of little consequence then and is of no consequence today;
  but the system that produced dishonest science hasn’t been reformed,
  and we can but imagine what kind of lies it produces to this day in topics
  that compared to the semantics of OO are both less objectively arguable,
  and higher-stake economically and politically.
}
which he elaborates in another paper @~cite{Jacobs1996InheritanceAC}
as meaning «not depending on the “unknown” type X (of self).»
This makes his paper inapplicable to most OO, but interestingly,
precisely identifies the subset of OO for which inheritance coincides with subtyping,
or, to speak more precisely,
subtyping of modular extensions coincides with subtyping of their targets.

Indeed, in general, specifications may contain so called “binary methods”
that take another value of the same target type as argument,
such as in very common comparison functions (e.g. equality or order)
or algebraic operations (e.g. addition, multiplication, composition), etc.;
and beyond these, they can actually contain arbitrary higher-order functions
involving the target type in zero, one or many positions,
both “negative” (as an overall argument)
or “positive” (as an overall result), @; TODO cite Felleisen???
or as parameters to type-level functions, “templates”, etc.
These methods will break the precondition for subclassing being subtyping.

And such methods are not an “advanced” or “anomalous” case, but quintessential.
The very first example in the very first paper about actual classes @~cite{Simula1967},
involves recursion data types:
it is a class @c{linkage} that defines references @c{suc} and @c{pred} to the “same” type,
that classes can inherit from so that their elements shall be part of a doubly linked list.
This example, and any data structure defined using recursion,
will defeat the NNOOTT if examined closely.
Not only is such recursion a most frequent occurrence, we saw above in @seclink{IME} that
while you can eschew support for fixpoints through the module context
when considering modularity or extensibility separately,
open recursion through module contexts becomes essential when considering them together.
In the general and common case in which a class or prototype specification
includes self-reference, subtyping and subclassing are very different,
a crucial distinction that was first elucidated in @~cite{cook1989inheritance}.

Now, the NNOOTT can be “saved” by reserving static typing to non-self-referential methods,
whereas any self-reference must dynamically typed:
wherever a recursive self-reference to the whole would happen, e.g. in the type of a field,
programmers must instead declare the value as in being of a dynamic typecheck,
or some other “base” type or class,
so that there is no self-reference in the type, and the static typechecker is happy.
Thus, when defining a list of elements of type @c{A}, you could not write the usual
@c{List(A) = Y (λ Self . (1 + A*Self))}
and would just write @c{List(A) = 1 + A*Any}.
Similarly, for trees with leaves of type @c{B}, you couldn’t write the recursive
and would instead write just the non-recursive and dynamically typed
@c{Tree(B) = B + List(Any))}.

To compensate for the imprecision of the type system
when retrieving an element of the desired self-type,
some kind of explicit dereference, type cast (downcast), or coercion
is required from the programmer;
that operation may be either safe (with a dynamic runtime check), or
unsafe (program may silently misbehave at runtime if called with the wrong argument).
In some languages, self-reference already has to go through
pointer indirection (e.g. in C++), or
boxing (e.g. in Haskell, when using a @c{newtype Fix} generic constructor for fixpoints,
while the open modular definition goes into a “recursion scheme”);
thus the NNOOTT does not so much introduce an extra indirection step for recursion
as it makes an existing indirection step obvious—and
makes it dynamically rather than statically typed.
In other words, it makes us realize once again that @emph{recursion is not free}.

@subsubsection{Why NNOOTT?}

The NNOOTT was implicit in the original OO paper @~cite{Simula1967}
as well as in Hoare’s seminal paper that inspired it @~cite{hoare1965record}@xnote["."]{
  Hoare probably intended subtyping initially indeed for his families of record types;
  yet subclassing is what he and the SIMULA authors discovered instead.
  Such is scientific discovery:
  if you knew in advance what lied ahead, it would not be a discovery at all.
  Instead, you set out to discover something, but usually discover something else,
  that, if actually new, will be surprising.
  The greater the discovery, the greater the surprise.
  And you may not realize what you have discovered until analysis is complete much later.
  The very best discoveries will then seem obvious in retrospect,
  given the new understanding of the subject matter,
  and familiarity with it due to its immense success.
}
It then proceeded to dominate the type theory of OO
until debunked in the late 1980s @~cite{cook1989inheritance}.
Even after that debunking, it has remained prevalent in popular opinion,
and still very active also in academia and industry alike,
and continually reinvented even when not explicitly transmitted.

The reasons why, despite being inconsistent, the NNOOTT was and remains so popular,
not just among the ignorant masses, but even among summities in computer science,
is well worth examining.

@itemize[
@item{
  The NNOOTT directly follows from the confusion between specification and target
  when conflating them without distinguishing them (@secref{PaC}).
  The absurdity of the theory also follows from the categorical error of equating entities,
  the specification and its target, that
  not only are not equivalent, but are not even of the same type.
  But no one @emph{intended} for “a class” to embody two very distinct semantic entities;
  quite on the contrary, Hoare, as well as the initial designers of
  SIMULA, KRL, Smalltalk, Ani, etc.,
  were trying to have a unified concept of “class” or “frame” or “actor”, etc.
  Consequently, the necessity of considering two distinct entities
  was only fully articulated in the 2020s(!).
}
@item{
  In the 1960s and 1970s, when both OO and type theory were in their infancy,
  and none of the pioneers of one were familiar with the other,
  the NNOOTT was a good enough approximation that even top language theorists were fooled.
  Though the very first example in OO could have disproven the NNOOTT,
  still it requires careful examination and familiarity with both OO and Type Theory
  to identify the error, and pioneers had more urgent problems to solve.
}
@item{
  The NNOOTT actually works quite well in the simple “non-recursive” case
  that we characterized above.
  In particular, the NNOOTT makes sense enough
  in the dynamically typed languages that (beside the isolated precursor SIMULA)
  first experimented with OO in the 1970s and 1980s,
  mostly Smalltalk, Lisp and their respective close relatives.
  In those languages, the “types” sometimes specified for record fields
  are suggestions in comments, dynamic checks at best,
  sometimes promises made by the user to the compiler,
  never static guarantees made by the language;
  the recursive case was always dynamically typed, as was any non “atomic” value.}
@item{
  Even in the 1980s and 1990s, theorists and practitioners being mostly disjoint populations,
  did not realize that they were not talking about precisely the same thing
  when talking about a “class”.
  Those trained to be careful not to make categorical errors
  might not have realized that others were doing it in ways that mattered.
  The few at the intersection may not have noticed
  the discrepancy, or understood its relevance, when scientific modeling
  must necessarily make many reasonable approximations all the time.
  Once again, more urgent issues were on their minds.
}
@item{
  Though the NNOOTT is inconsistent in the general case of OO,
  as obvious from quite common examples involving recursion,
  it will logically satisfy ivory tower theorists or charismatic industry pundits
  who never get to experience cases more complex than textbook examples,
  and pay no price for dismissing recursive cases as “anomalies” @~cite{Jacobs1996InheritanceAC}
  when confronted with them.
  Neither kind owes their success to getting a consistent theory
  that precisely matches actual practice.
}
@item{
  The false theory will also emotionally satisfy those practitioners and their managers
  who care more about feeling like they understand rather than actually understanding.
  This is especially true of the many who have trouble thinking about recursion,
  as is the case for a majority of novice programmers and vast majority of non-programmers.
  Even who can successfully @emph{use} recursion,
  might not be able to @emph{conceptualize} it, what more, enough to criticize a theory of it.
  @;{ TODO locate study that measures the recursion-ables from the unable. }
}]

@subsubsection{Beyond the NNOOTT}

The key to dispelling the
“conflation of subtyping and inheritance” @~cite{Fisher1996thesis}
or the “notions of type and class [being] often confounded” @~cite{bruce1996typing}
is indeed first to have dispelled, as we just did previously,
the conflation of specification and target.
Thereafter, OO semantics becomes simple,
for we notice that though the two are often referenced together in an implicit product,
target and specification remain distinct, therefore we must be careful
to always treat them accordingly,
as two distinct entities with very distinct types,
when others may insist to treat them as a single entity with a common type.

We then realize what most people actually mean by “subtyping” in extent literature is
@emph{subtyping for the target type of a class} (or target prototype),
which is distinct from
@emph{subtyping for the specification type of a class} (or prototype),
variants of the latter of which Kim Bruce calls “matching” @~cite{SubtypingMatch1997}.
@; TODO cite further
But most people, being confused about the conflation of specification and target,
don’t conceptualize the distinction, and either
try to treat them as if it were the same thing,
leading to logical inconsistency hence unsafety and failure;
or they build extremely complex calculi to do the right thing despite the confusion.
By having a clear concept of the distinction,
we can simplify away all the complexity without introducing inconsistency.

We can use the usual rules of subtyping @~cite{cardelli1986understanding} @; TODO cite
and apply them separately to the types of specifications and their targets,
knowing that “subtyping and fixpointing do not commute”,
or to be more mathematically precise,
@emph{fixpointing does not distribute over subtyping},
or said otherwise, @principle{the fixpoint operator is not monotonic}:
If @c{F} and @c{G} are parametric types,
i.e. type-level functions from @c{Type} to @c{Type},
and @c{F ⊂ G} (where @c{⊂}, sometimes written @c{<:}, is the standard notation for “is a subtype of”,
and for elements of @c{Type → Type} means @c{∀ t, F t ⊂ G t}),
it does not follow that @c{Y F ⊂ Y G} where @c{Y} is the fixpoint operator for types@xnote["."]{
  The widening rules for the types of specification
  and their fixpoint targets are different;
  in other words, forgetting a field in a target record, or its some of its precise type information,
  is not at all the same as forgetting that field or its precise type in its specification
  (which introduces incompatible behavior with respect to inheritance,
  since extra fields may be involved as intermediary step in the specification,
  that must be neither forgotten, nor overridden with fields of incompatible types).

  If the two entities are treated as a single one syntactically and semantically,
  as all OO languages so far have done, @; ALL??
  then their type system will have to encode in a weird way a pair of subtly different types
  for each such entity, and the complexity will have to be passed on to the user,
  with the object, and each of its field having two related but different declared types,
  and potentially different visibility settings.
  Doing this right involves a lot of complexity, both for the implementers and for the users,
  at every place that object types are involved, either specified by the user, or display to him.
  Then again, some languages may do it wrong by trying to have the specification fit the rules
  of the target (or vice versa), leading to inconsistent rules and consistently annoying errors.

  A typical way to record specification and target together is to annotate fields with visibility:
  @c{public} (visible in the target)
  yet possibly with a more specific type in the target
  than in the specification (to allow for further extensions that diverge from the current target);
  fields marked @c{protected} (visible only to extensions of the specification, not in the target);
  and fields marked @c{private} (not visible to extensions of the specification,
  even less so to the target; redundant with just defining a variable in a surrounding @c{let} scope).
  We retrieve these familiar notions from C++ and Java just by reasoning from first principles
  and thinking about distinct but related types for a specification and its target.

  Now, our opinion is that it is actually better to fully decouple the types
  of the target and the specification, even in an “implicit pair” conflating the two:
  Indeed, not only does that means that types are much simpler, that also mean that
  intermediate computations, special cases to bootstrap a class hierarchy,
  transformations done to a record after it was computed as a fixpoint, and
  records where target and specification are out of sync
  because of effects somewhere else in the system, etc.,
  can be safely represented and typed,
  without having to fight the typesystem or the runtime.
}

A more precise view of a modular extension is thus as
an entity parameterized by the varying type @c{self} of the module context
(that Bruce calls @c{MyType} @~cite{bruce1996typing SubtypingMatch1997}). @; TODO cite further
As compared to the previous parametric type @c{NMExt} that is parametrized by types @c{r i p},
this parametric type @c{MExt} is itself parametrized by parametric types @c{r i p}
that each take the module context type @c{self} as parameter@xnote[":"]{
  The letters @c{r i p}, by contrast to the @c{s t a b} commonly used for generalized lenses,
  suggest the mnemonic slogan: “Generalized lenses can stab, but modular extensions can rip!”
}
@Code{
type MExt required inherited provided =
  ∀ self, super : Type
    self ⊂ required self, super ⊂ inherited self ⇒
        self → super → provided self ∩ super}

Notice how the type @c{self} of the module context
is @emph{recursively} constrained by @c{self ⊂ referenced self}),
whereas the type @c{super} of the value in focus being extended
is constrained by @c{super ⊂ inherited self},
and the returning a value is of type @c{provided self ∩ super},
and there is no direct recursion there
(but there can be indirectly if the focus is itself referenced via self somehow).
Those familiar with universal quantifiers may also notice how
the quantification of @c{self} and @c{super},
in absence of reflective capabilities, pretty much forces those functions
to be well-behaved with respect to gracefully passing through
any call to a method they do not define, override or otherwise handle.
Finally, notice how, as with the simpler NNOOTT variant above,
the types @c{self} and @c{referenced self} refer to the module context,
whereas the types @c{super} and @c{provided self} refer to some value in focus,
that isn’t at all the same as the module context
for open modular extensions in general.

Our two OO primitives then have the following type:
@Code{
fix : ∀ required, inherited, provided : Type → Type, ∀ self, top : Type,
      self = inherited self ∩ provided self,
      self ⊂ required self,
      top ⊂ inherited self ⇒
        top → MExt referenced inherited provided → self
mix : MExt r1 i1∩d2 p1 → MExt r2 i2 p2 → MExt r1∩r2 i1∩i2 p1∩p2}

In the @c{fix} function, we implicitly define a fixpoint @c{self}
via suitable recursive subtyping constraints.
We could instead make the last constraint a definition
@c{self = Y (inherited ∩ provided)}
and check the two subtyping constraints about @c{top} and @c{referenced}.
As for the type of @c{mix}, though it looks identical with @c{MExt}
as the NNOOTT type previously defined with @c{NMExt},
there is an important but subtle difference:
with @c{MExt}, the arguments being intersected
are not of kind @c{Type} as with @c{NMExt},
but @c{Type → Type}, where
given two parametric types @c{f} and @c{g},
the intersection @c{f∩g} is defined by @c{(f∩g)(x) = f(x)∩g(x)}.
Indeed, the intersection operation is defined polymorphically, and
in a mutually recursive way for types, functions over types, etc.

@subsubsection{Typing Advantages of OO as Modular Extensions}

By defining OO in terms of the λ-calculus, indeed in two definitions @c{mix} and @c{fix},
we can do away with the vast complexity of “object calculi” of the 1990s,
@; TODO cite Cardelli, Fisher, Bruce, Pierce, etc.
and use regular, familiar and well-understood concepts of functional programming such as
subtyping, bounded parametric types, fixpoints, existential types, etc.
No more @c{self} or @c{MyType} “pseudo-variables” with complex “matching” rules,
just regular variables @c{self} or @c{MyType} or however the user wants to name them,
that follow regular semantics, as part of regular λ-terms@xnote["."]{
  Syntactic sugar may of course be provided for optional use,
  that can automatically be macro-expanded away through local expansion only;
  but the ability to think directly in terms of the expanded term,
  with simple familiar universal logic constructs that are not ad hoc but from first principles,
  is most invaluable.
}
OO can be defined and studied without the need for ad hoc OO-specific magic,
making explanations readily accessible to the public.
Indeed, defining OO types in term of LaTeX deduction rules for ad hoc OO primitives
is just programming in an informal, bug-ridden metalanguage that few are familiar with,
with no tooling, no documentation, no tests,
no actual implementation,
and indeed no agreed upon syntax much less semantics@xnote["…"]{
  See Guy Steele’s keynote “It’s Time for a New Old Language”
  at the Principles and Practice of Parallel Programming 2017 conference
  about the “Computer Science Metanotation” found in scientific publications.
}
the very opposite of the formality the authors affect.

Not having OO-specific magic also means that when we add features to OO,
as we will demonstrate in the rest of this essay, such as single or multiple inheritance,
method combinations, multiple dispatch, etc.,
we don’t have to update the language
to use increasingly more complex primitives for declaration and use of prototypes or classes.
By contrast, the “ad hoc logic” approach grows in complexity so fast
that authors soon may have to stop adding features
or find themselves incapable of reasoning about the result
because the rules for those “primitives” boggle the mind@xnote["."]{
  Authors of ad hoc OO logic primitives also soon find themselves
  incapable of fitting a complete specification within the limits of a conference paper,
  what more with intelligible explanations, what more in a way that anyone will read.
  The approach is too limited to deal even with the features 1979 OO @~cite{Cannon1979},
  much less those of more modern systems.
  Meanwhile, readers and users (if any) of systems described with ad hoc primitives
  have to completely retool their in-brain model at every small change of feature,
  or introduce misunderstandings and bugs,
  instead of being able to follow a solidly known logic that doesn’t vary with features.
}
Instead, we can let logic and typing rules be as simple as possible,
yet construct our object features to be as sophisticated as we want,
without a gap in reasoning ability, or inconsistency in the primitives.

Our encoding of OO in terms of “modular extension”, functions of the form
@c{mySpec (self : Context, super : Focus) : Focus}, where in the general “open” case,
the value under @c{Focus} is different from the @c{Context}, is also very versatile
by comparison to other encodings, that are typically quite rigid, specialized for classes,
and unable to deal with OO features and extensions.
Beyond closed specifications for classes, or for more general prototypes,
our @c{MExt} type can scale down to open specifications for individual methods,
or for submethods that partake in method combination;
it can scale up to open specifications for groups of mutually defined or nested classes or prototypes,
all the way to open or closed specifications for entire ecosystems.

More importantly, our general notion of “modular extension”
opens an entire universe of algebraically well-behaved composability
in the spectrum from method to ecosystem;
the way that submethods are grouped into methods, methods into prototypes,
prototypes into classes, classes into libraries, libraries into ecosystems, etc.,
can follow arbitrary organizational patterns largely orthogonal to OO,
that will be shaped the evolving needs of the programmers,
yet will at all times benefit from the modularity and extensibility of OO.

OO can be one simple feature orthogonal to plenty of other features
(products and sums, scoping, etc.) to achieve in a @emph{reasonable} manner @; TODO cite
what too many languages achieve my making “classes” a be-all, end-all ball of mud of
more features than can fit in anyone’s head, interacting in sometimes unpredictable ways,
as is the case in languages like C++, Java, C#, etc.

@subsubsection{Typing First-Class OO}

We are aiming at understanding OO as @emph{first-class} modular extensibility,
so we need to identify what kind of types are suitable for that.
The hard part is to type @emph{classes}, and more generally specifications
wherein the type of the target recursively refers to itself
through the open recursion on the module context.

Happily, our construction neatly factors the problem of OO
into two related but mostly independent parts:
first, understanding the target, and second, understanding their instantiation via fixpoint.

We already discussed in @secref{Rebuilding_Classes}
how a class is a prototype for a type descriptor:
the target is a record that describes one type and a set of associated functions.
The type is described as a table of field descriptors
(assuming it’s a record type;
or a list of variants for a tagged union, if such things are supported; etc.),
a table of static methods, a table of object methods,
possibly a table of constructors separate from static methods, etc.
A type descriptor enables client software to be coded against its interface,
i.e. being able to use the underlying data structures and algorithms
without having to know the details and internals.

A first-class type descriptor is a record whose type is existentially quantified:
@~cite{cardelli1986understanding mitchell1988abstract PT1993STTFOOP}
@; TODO cite harper1994modules remy1994mlart
as per the Curry–Howard correspondence, it is a witness of the proposition according to which
“there is a type @c{T} that has this interface”, where the interface may include field getters
(functions of type @c{T → F} for some field value @c{F}),
some field setters (functions of type @c{T → F → T} for a pure linear representation,
or @c{T → F @(⇝) 1} if we denote by @c{@(⇝)} a “function” with side-effects),
binary tests (functions of type @c{T → T → 2}),
binary operations (functions of type @c{T → T → T}),
constructors for @c{T} (functions that create a new value of type @c{T},
at least some of which without access to a previous value of type @c{T}),
but more generally any number of functions, including higher-order functions,
that may include the type @c{T} zero, one or arbitrarily many times
in any position “positive” or “negative”, etc.
So far, this is the kind of things you could write with first-class ML modules, @; TODO cite
which embodies first-class modularity, but not modular extensibility.

Now, if the type system includes subtypes, extensible records, and
fixpoints involving open recursion,
e.g. based on recursively constrained types @~cite{isoop1995 iloop1995}, then
those first-class module values can be the targets of modular extensions.
@;{TODO @~cite{remy1994mlart} ?}
And there we have first-class OO capable of expressing classes.

Regarding subtyping, however, note that when modeling a class as a type descriptor,
not only is it absolutely not required that a subclass’s target should be
a subtype of its superclass’s target (which would be the NNOOTT above),
but it is not required either that a subclass’s specification should be
a subtype of its superclass’s specification.
Indeed, adding new variants to a sum type, which makes the extended type a supertype of the previous,
is just as important as adding fields to a product type (or specializing its fields),
which makes the extended type a subtype of the previous.
Typical uses include extending a language grammar (as in @~cite{garrigue2000code}),
defining new error cases, specializing the API of some reified protocol, etc.
In most statically typed OO languages, that historically mandate the subclass specification type
to be a subtype of its superclass specification types, programmers work around this limitation
by defining many subclasses of each class, one for each of the actual cases of an implicit variant;
but this coping strategy requires defining a lot of subclasses,
makes it hard to track whether all cases have been processed;
essentially, the case analysis of the sum type is being dynamically rather than statically typed.

@subsubsub*section{Note on Types for Second-Class Class OO}

Types for second-class classes can be easily deduced
from types for first-class classes:
A second-class class is “just” a first-class class that happens
to be statically known as a compile-time constant, rather than a runtime variable.
The existential quantifications of first-class OO and their variable runtime witnesses
become unique constant compile-time witnesses,
whether global or, for nested classes, scoped.
This enables many simplifications and optimizations,
such as lambda-lifting (making all classes global objects, modulo class parameters),
and monomorphization (statically inlining each among the finite number of cases
of compile-time constant parameters to the class),
and inlining globally constant classes away.

However, you cannot at all deduce types for first-class classes from
types for second-class classes:
you cannot uninline constants, cannot unmake simplifying assumptions,
cannot generalize from a compile-time constant to a runtime variable.
The variable behavior essentially requires generating code
that you wouldn’t have to generate for a constant.
That is why typing first-class prototypes is more general and more useful
than only typing second-class classes;
and though it may be harder in a way, involving more elaborate logic,
it can also be simpler in other ways, involving more uniform concepts.

@subsubsection{First-Class OO Beyond Classes}

Our approach to OO can vastly simplify types for it, because it explicitly decouples
concepts that previously people implicitly conflated:
not only specifications and their targets,
but also modularity and extensibility,
fixpoints and types.

By decoupling specifications and targets, we can type them separately, subtype them separately,
not have to deal with the extreme complexity of
the vain quest of trying to type and subtype them together.

By decoupling modularity and extensibility, we can type not just closed specifications,
but also open specifications, which makes everything so much simpler,
more composable and decomposable.
Individual class, object, method, sub-method specifications, etc.,
can be typed with some variant of the @c{C → V → V} pattern,
composed, assembled in products or co-products, etc.,
with no coupling making the unit of specification the same as the unit of fixpointing.

Finally, with typeclass-style (as in @secref{Class_style_vs_Typeclass_style}),
we find that the unit of fixpointing need not one type descriptor;
it could be a value without an existential type, or a descriptor for multiple existential types,
that could be a finite set, or even an infinite family of types, etc.
In an extreme case, it can even be the entire ecosystem — a pattern actively used
in Jsonnet or Nix (though without formal types).

To build such records, one may in turn make them the targets of modular extensions,
such that first-class OO can express much more than mere “classes”,
especially so than second-class classes of traditional Class OO.
First-class OO can directly express sets of cooperating values, types and algorithms
parameterized by other values, types and algorithms.

@;{TODO
@subsubsection{More Static Typing for OO}
XXX TODO integrate citations to the below and more

Pierce @~cite{PT1993STTFOOP Pierce2002TAPL} at times manages
to push the difficulty with fixpoints and recursion
into a small corner wherein they let the user manually tie a simple knot,
and their automation can take it from there;
it’s brilliant and even insightful, yet in the end they are dodging
the hard problem behind OO, rather than solving it—as in the classic joke:
“Tell me everything you need, and I'll show you how to do without it.”

Oliveira @~cite{MonadsMixins}
shows that there is enough subtyping in Haskell typeclasses
for cool applications of Mixins, though he stays short of the type astronautics
needed to enable the structural subtyping of extensible records of colloquial OO.

Special mention for the magical Oleg @~cite{Kiselyov2005HaskellOOS}
who shows that, actually, there is enough structural subtyping hidden in the Haskell typesystem
to do precisely the kind of things people usually do with OO,
with lists of types at the type-level to handle extensible record types.
Though Oleg does not bother to implement multiple inheritance,
some have implemented Quicksort at the type-level in Haskell,
so topological sort with C3 is probably just some more type astronautics away.
As compared to most OO papers that discuss theory only, or push their own ad hoc system,
Oleg discusses pragmatics, and reuses someone else’s language, without having to change it,
all by programming at the type-level.
On the other hand, Oleg is well versed in the theory precisely because he does his practical work
decades after the theorists wrote their papers.

Wegner and Cardelli 1985 @~cite{cardelli1986understanding}. NNOOTT.

Cardelli 1988. NNOOTT.

OCaml 199x. @~cite{remy1994mlart}

Scala DOT 200x

Fortress 2011

Typescript
https://www.typescriptlang.org/docs/handbook/utility-types.html

Type-Safe Prototype-Based Component Evolution" (2002)
https://www.cs.cornell.edu/andru/cs711/2002fa/reading/zenger02typesafe.pdf

Ego
https://www.cs.cmu.edu/~aldrich/ego/

Bad(?): NOOP Robert Cartwright & Moez Abdelgawad
https://www.semanticscholar.org/reader/3855d0beac44b1623731bf581f80ec4d348eb4ba

https://counterexamples.org/subtyping-vs-inheritance.html

Andrew K. Wright & Robert Cartwright
"A practical soft type system for Scheme"
1997

TODO

Why do "unary methods" work in class OO, but not e.g. binary methods?
Because you moved construction / destruction out of the way,
so all you’re doing is consuming data,
in a way that (as far as types are concerned) is extensible.
(if considered not trivially returning unit, but effectful with linear resources to forcibly manage).
Also, when an object of same type is linearly returned,
there is one obvious place from which to copy the extended rest of the object;
when multiple objects are returned... that is still a possible interpretation
(and though that’s seldom the useful one, that’s enough for the type theorist).

Kim Bruce, @;CITE 1993 1994 1995
thereafter gave sounder types to OO,
saving subtyping where it could be saved because the NNOOTT applies, @; CITE PolyTOIL
but otherwise abandoning subtyping as a goal. @; CITE LOOM 1997
@; Kathleen Fisher @;CITE ...

Meanwhile, the relationship between a module context and a focused value being
modularly and extensibly specified within it is characterized by
a @emph{lens} @~cite{Foster2007CombinatorsFB},
generalizing a path of identifiers to some arbitrary way of accessing a subcomputation.

Classes as object-generator see Cook87, A self-ish model of inheritance
or Cook89a A Denotational Semantics of Inheritance, thesis
}

@subsection{Stateful OO}

@subsubsection{Mutability of Fields as Orthogonal to OO}

We saw how OO is best explained in terms of pure lazy functional programming,
and how mutable state is therefore wholly unnecessary for OO.
There have been plenty of pure functional object libraries since at least the 1990s,
even for languages that support mutable objects;
OO languages that do not support any mutation at all have also existed since at least the 1990s,
and practical such languages with wide adoption exist since at least the early 2000s.
@;{TODO cite}

Yet, historical OO languages (LISP, SIMULA, Smalltalk),
just like historical OO-less languages of the same time (FORTRAN, ALGOL, Pascal),
were stateful, heavily relying on mutation of variables and record fields.
So are the more popular OO and OO-less languages of today, still,
though there are now plenty of less-popular “pure (functional)” options.
How then does mutation fit in our function OO paradigm?
The very same way it does on top of Functional Programming in general, with or without OO:
by adding an implicit (or then again explicit) “store” argument to all (or select) functions,
that gets linearly (or “monadically”) modified and passed along the semantics of those functions
(the linearity, uniqueness or monadicity ensuring that there is one single shared state
at a time for all parts of the program).
Then, a mutable variable or record field is just
a constant pointer into a mutable cell in that store,
a “(mutable) reference”.

This approach perfectly models the mutability of object fields, as found in most OO languages.
It has the advantages of keeping this concern orthogonal to others,
so that indexed products, fixpoints, mutation, visibility rules and subtyping constraints
(separately before and after fixpointing), etc.,
can remain simple independent constructs each with simple reasoning rules,
logically separable from the each other yet harmonously combinable together.
By contrast, the “solution” found in popular languages like C++ or Java
is all too often to introduce a single mother-of-all syntactic and semantic construct
of immense complexity, the “class”, that frankly not a single person in the world fully understands,
and of which scientific papers only dare study but simplified (yet still very complex) models.

Actually, when we remember that in most OO languages, OO is only ever relevant but at compile-time,
of course mutation is orthogonal to OO,
even in these languages, nay, especially so in these languages:
since OO fragments are wholly evaluated at a time before there is any mutation whatsoever,
mutation cannot possibly be part of OO, even though it is otherwise part of these languages.
Indeed the compile-time programming model of these languages, if any, is pure lazy functional.
Thus, whether fields are mutable or immutable is of precious little concern
to the compiler fragment that processes OO:
it’s just a flag passed to the type checker and code generator after OO is processed away.

@subsubsection{Mutability of Inheritance as Code Upgrade}

Keeping mutability orthogonal to OO as above works great as long as the fields are mutable,
but the inheritance structure of specifications is immutable.
Happily, this covers every language with second-class classes (which is most OO languages),
but also all every day uses of OO even in languages with first-class prototypes and classes.
Still, there are use cases in which changes to class or prototype hierarchies
is actively used by some dynamic OO systems such as Smalltalk or Lisp:
to support interactive development, or schema upgrade in long-lived persistent systems.
How then to model mutability of the inheritance structure itself,
when the specification and targets of prototypes and classes are being updated?

First, we must note that such events are relatively rare,
because they involve programmers not only typing, but thinking,
which happens millions of times slower than computers process data.
Most evaluation of most programs, especially where performance matters,
happens in-between two such code upgrades,
in large spans of time during which the code is constant.
Therefore, the usual semantics that consider inheritance structures as constant
still apply for an overwhelming fraction of the time and an overwhelming fraction of objects,
even in presence of such mutability.
There are indeed critical times when these usual semantics are insufficient,
and the actual semantics must be explained;
but these usual semantics are not rendered irrelevant
by the possibility of dynamic changes to object inheritance.

Second, updates to the inheritance structure of OO specifications
can be seen as a special case of code upgrade in a dynamic system.
Code upgrade, whether it involves changes to inheritance structure or not,
raises many issues such as the atomicity of groups of upgrades
with respect to the current thread and concurrent threads,
or what happens to calls to updated functions from previous function bodies in
frames of the current thread’s call stack,
how the code upgrades do or do not interfere with optimizations such as inlining or reordering
of function calls,
how processor caches are invalidated, page permissions are updated,
how coherency is achieved across multiple processors on a shared memory system, etc.
These issues are not specific to inheritance mutability, and
while they deserve a study of their own,
the present paper is not the right place for such a discussion.
The only popular programming language that fully addresses all these code upgrade issues
in its defined semantics is Erlang @~cite{DBLP:phd/basesearch/Armstrong03};
however it does not have any OO support, and its approach
is not always transposable to other languages@xnote["."]{
  Erlang will notably kill processes that still use obsolete code
  from before the current version now being upgraded to the next one.
  This is possible because Erlang has only very restricted sharing of state between processes,
  so it can ensure PCLSRing @~cite{PCLSRing} without requiring user cooperation;
  this is useful because Erlang and its ecosystem have a deep-seated “let it fail” philosophy
  wherein processes randomly dying is expected as a fact of life,
  and much infrastructure is provided for restarting failed processes,
  that developers are expected to use.
}
Lacking such deep language support, user support is required to ensure upgrades only happen
when the system is “quiescent” (i.e. at rest,
so there are no issues with outdated code frames upstack or in concurrent threads)
for “Dynamic Software Updating” @~cite{DSU2001};
at that point, the compiler need only guarantee that calls to the upgradable entry points
will not have been inlined.
Happily, since code upgrade events happen at a much larger timescale than regular evaluation,
it is also generally quite acceptable for systems to wait until the right moment
that the system is indeed quiescent, after possibly telling its activities to temporarily shutdown,
before to apply such code upgrades.

Third, we must note how languages such as Smalltalk and Common Lisp include a lot of support
for updating class definitions, including well-defined behavior with respect to how objects
are updated when their classes change:
see for instance the protocol around @c{update-instance-for-redefined-class}
in CLOS, the Common Lisp Object System @~cite{bobrow88clos}.
These facilities allow continuous concurrent processing of data elements
with some identity preserved as the code evolves, even as
the data associated to these identities evolves with the code.
Even then, these languages do not provide precise and portable semantics
for how such code upgrade upgrades interfere with code running in other threads,
or in frames up the stack from the upgrade, so once again,
users must ensure quiescence or may have to deal with spurious incoherence issues,
up to possible system corruption.

Lastly, as to providing a semantics for update in inheritance structure,
language designers and/or programmers will have to face the question of what to do
with previously computed targets when a specification is updated:
Should a target once computed be left forever unchanged,
now out-of-synch with the (possibly conflated) specification?
Should a target be wholly invalidated, losing any local state updates since it was instantiated?
Should a target have “direct” properties that override any computation involving inheritance,
while “indirect” properties are recomputed from scratch just in case the inheritance structure changed?
Should some “indirect” properties be cached, and if so how is this cache invalidated when
there are changes?
Should a protocol such as @c{update-instance-for-redefined-class} be invoked to update this state?
Should this protocol be invoked in an eager or lazy way
(i.e. for all objects right after code update, or on a need basis for each object)?
Should a class maintain at all times and at great cost a collection of all its instances,
just so this protocol can be eagerly updated once in a rare while?
Should some real-time system process such as the garbage collector
ensure timely updates across the entire heap
even in absence of such explicitly maintained collection?
Are children responsible for “deep” validity checks at every use,
or do parents make “deep and wide” invalidations at rare modifications,
or must parents and children somehow deal with incoherence?
There is no one-size-fits-all answer to these questions.

If anything, thinking in terms of objects with identity and mutable state
both forces software designers to face these issues,
inevitable in interactive or long-lived persistent systems,
and provides them with a framework to give coherent answers to these questions.
Languages that assume “purity” or lack of code upgrade, thereby deny these issues,
and leave their users helpless, forced to reinvent entire frameworks of mutation
so they may then live in systems they build on top of these frameworks,
rather than directly in the language that denies the issues.

@section{Inheritance: Mixin, Single or Multiple}

@subsection{Mixin Inheritance}
@subsubsection{The Last Shall Be First}

What we implemented in the sections above is mixin inheritance
(@seclink{MIXIO}):
the last discovered and least well-known variant of inheritance.
And yet, we already saw above that object prototypes with mixin inheritance
are used to specify software configurations at scale. @;TODO secref
We further claim that it is the most fundamental variant of inheritance,
since we built it in two lines of code, and
will proceed to build the other variants on top of it.

@subsubsection{Mixin Semantics}
We saw above (@seclink{MFCME})
that mixin inheritance involves just
one type constructor @c{MExt} and two functions @c{fix} and @c{mix},
repeated here more concisely from above:
@Code{
type MExt r i p = ∀ s, t : Type . s ⊂ r s, t ⊂ i s ⇒ s → t → (p s)∩t
fixt : ∀ r i p : Type → Type, ∀ s, t : Type .
       s = i s ∩ p s, s ⊂ r s, t ⊂ i s ⇒
       t → MExt r i p → s
mix : MExt r1 i1∩p2 p1 → MExt r2 i2 p2 → MExt r1∩r2 i1∩i2 p1∩p2

(define fixt (λ (m) (Y (λ (s) ((m s) top)))))
(define mix (λ (c p) (λ (r) (compose (c r) (p r)))))}

@subsection{Single inheritance}

@subsubsection{Semantics of Single Inheritance}

@principle{In single inheritance, the specifications at stake are open modular definitions},
as studied in @seclink{MFCM},
simpler than the modular extensions of mixin inheritance from @seclink{MFCME}@xnote["."]{
  In @~cite{Cook1989 bracha1990mixin}, Cook calls “generator” what we call “modular definition”,
  and “wrapper” what we call “modular extension”.
  But those terms are a bit too general, while Cook’s limitation to records
  makes his term assignment a bit not general enough (specialized for record).
  Even in the confines of our exploration of OO,
  we already used the term “wrapper” in a related yet more specific way
  when discussing wrapping references for recursive conflation in @seclink{RC};
  and a decade before Cook, Cannon @~cite{Cannon1979} also used a notion of wrapper
  closer to what we use, in Flavor’s predecessor to CLOS @c{:around} methods @~cite{cltl2},
  or in the more general case, to CLOS declarative method combinations.
  @; TODO seclink
  The term “generator” is also too generic, and could describe many concepts in this paper,
  while being overused in other contexts, too.
  We will thus stick with our expressions “modular definition” and “modular extension”
  that are not currently in widespread use in computer science, that are harder to confuse,
  and that we semantically justified by reconstructing their meaning from first principle.
}
Modular definitions take a @c{self} as open recursion argument
and return a record using @c{self} for self-reference.
Unlike modular extensions, they do not take a @c{super} argument,
since they only are inherited from, but don’t themselves inherit, at least not anymore:
what superclass they did inherit from is a closed choice made in the past,
not an open choice to make in the future; it is baked into the modular definition already.
The semantics can then be reduced to the following types and functions:
@; TODO CITE Cook
@Code{
type MDef r p = ∀ s : Type . s ⊂ r s ⇒ s → p s
fixMDef : MDef p p → Y p
extendMDef : MExt r1 p2 p1 → MDef r2 p2 → MDef r1∩r2 p1∩p2
baseMDef : MDef (λ (_) Top) (λ (_) Top)

(define fixMDef Y)
(define extendMDef (λ (mext) (λ (parent) (λ (self)
  (mext self (parent self))))))
(define baseMDef (λ (_) top))}

Note how the type for an open modular definition has two parameters @c{r} (required)
and @c{p} (provided), but a closed modular definition
has the same value for those two parameters.
There is no parameter @c{i} (inherited), just like there was no argument @c{super}.

We already saw how the instantiation function for a closed modular definition was simply
the fixpoint combinator @c{Y}.
The case of extending a modular definition is more interesting.
First, let us simply remark that
since extending works on open modular definitions, not just on closed ones like instantiating,
the value under focus needs not be the same as the module context.
But more remarkably, extension in single inheritance requires
you use a modular @emph{extension} in addition to an existing modular definition.

When building a modular definition through successive extensions,
an initial known existing modular definition is needed as a base case to those extensions;
this role is easily filled by the @c{baseMDef}.
Now the recursive case involves a different kind of entities, modular extensions.
But we saw that modular extensions were already sufficient by themselves
to define mixin inheritance.
Why then use single inheritance,
since it still requires the entities of mixin inheritance in addition to its own?
Might we not as well directly adopt the simpler and more expressive mixin inheritance?

@subsubsection[#:tag "CMSI"]{Comparing Mixin- and Single- Inheritance}

@subsubsub*section{Mixin Inheritance is Simpler, @emph{assuming FP}}

Assuming knowledge of Functional Programming (FP),
the definitions of single inheritance above
are slightly more complex than those of mixin inheritance,
and noticeably more awkward.
And indeed, @emph{if} you have already paid the price of living
in a world of functional programming, with higher-order functions
and sufficiently expressive types and subtypes and fixpoints,
and of thinking in terms of programming language semantics,
you might not need single inheritance at all.

But while Functional Programming and its basic concepts including
lexical scoping and higher-order functions
may be boringly obvious to the average programmer of 2025,
they were only fully adopted by mainstream OO programming languages like
C++, Java in the 2010s, and slightly earlier for C#,
after JavaScript became popular in application development
and made FP popular with it, in the 2000s.
Back when single inheritance was invented in the 1960s,
these were extremely advanced concepts that very few mastered
even among language designers.
Unlike Mixin inheritance, single inheritance does not require any FP,
and many languages have or had single inheritance and no FP, including
Simula, many Pascal variants, early versions of Ada or Java or Visual Basic.
@;{TODO cite}

Neither lexical scoping nor higher-order functions are required for single inheritance
because the “modular extension” conceptually present in the extension of a modular definition
need never be explicitly realized as a first-class entity:
literally using our above recipe to implement a class or prototype definition with single inheritance
would involve building a modular extension, then immediately applying it with @c{extendMDef},
only to forget it right afterwards;
but instead, most OO languages would support some special purpose syntax for the definition,
and process it by applying the extension to its super specification as it is being parsed,
without actually building any independent first-class entity embodying this extension.
The semantics of this special purpose syntax
extremely complex to explain without introducing FP concepts,
but neither implementors nor users need actually conceptualize that semantics
to implement or use it.
@; As for those clever enough to figure out that semantics,
@; they tend to be clever enough not to need it to be simplified for them,
@; and not to care enough to simplify it for others.

@subsubsub*section{Mixin Inheritance is More Expressive}

Single inheritance can be trivially expressed in terms of Mixin inheritance
by tagging some modular extensions as only to be used as second argument of the @c{mix} function,
never the first, and only considering them as specifications;
meanwhile, those extensions used as the first argument of the @c{mix} function
must be constant, defined on the spot, and not reused afterwards.
Thus, single inheritance can be seen as just a restrictive style in which to use mixin inheritance,
and is no more expressive than mixin inheritance.

Conversely, given a language with FP and dynamic types or sufficiently advanced types,
you can implement first-class mixin inheritance on top of first-class single inheritance by
writing a function that abstracts over which parent specification
a specification will inheritance from, as in Racket née PLT Scheme @~cite{Mixins1998 Flatt2006Mixins}.
In terms of complexity, this construct puts the cart before the horse,
but it is possible, and may allow to cheaply leverage and extend existing infrastructure
in which single inheritance was already implemented and widely used.

Just like in mixin inheritance, a @emph{target} can thus still be seen as
the fixed point of the composition of a list of elementary modular extensions
as applied to a top value.
However, since modular definitions, not modular extensions, are the specifications,
the “native” view of single inheritance is more to see the parent specified in @c{extend}
as a direct super specification, and the transitive supers-of-supers as indirect super specifications;
each specification is considered as not just the modular extension it directly contributes,
but as the list of all modular extensions directly and indirectly contributed.

Now what if you only have second-class class OO, and
your compile-time language lacks sufficiently expressive functions
to build mixin inheritance atop single inheritance?
Then, mixin inheritance is strictly more expressive@~cite{eppl91}
than single inheritance:
Single inheritance only allows you to build a specification as a list of modular extensions
to which you can add one more modular extension at a time (as in @c{cons}),
when mixin inheritance allows you to build a specification as a list of modular extensions
that you can concatenate with other lists of modular extensions (as in @c{append}).
If you have already defined a list of modular extensions, and want to append it in front of another,
single inheritance will instead force you to duplicate the definition of each and every
of those modular extensions in front of the new base list.
See next section for the issues this creates.

Finally, since the two are equivalent in the context of first-class OO with higher-order functions,
but different in the more common context of second-class OO
without higher-order second-class functions,
it makes sense to only speak of single inheritance in a context where
the language syntax, static type system, dynamic semantics,
or socially-enforced coding conventions, or development costs
somehow disallow or strongly discourage modular extensions as first-class entities.

@subsubsub*section{Mixin Inheritance is More Modular}

In second-class class OO with single inheritance,
each modular extension can only be used once, at the site that it is defined,
extending one modular definition’s implicit list of modular extensions.
By contrast, with mixin inheritance, a modular extension can be defined once
and used many times, to extend many different lists of modular extensions.

Thus, should a @c{WeightedColoredPoint} inherit from @c{ColoredPoint} and then
have to duplicate the functionality from @c{WeightedPoint},
or should it be the other way around?
Single inheritance forces you not only to duplicate the functionality of a class,
but also to make a choice each time of only one which will be inherited from to reuse code.
Multiply this problem by the number of times you combine duplicated functionality.
This limitation can cause a maintenance nightmare:
bug fixes and added features must also be duplicated;
changes must be carefully propagated everywhere;
subtle discrepancies creep in, that cause their own issues, sometimes critical.

When there are many such independent features that are each duplicated onto many classes,
the number of duplicated definitions can grow quadratically with the number of desired features,
while the potential combinations grows exponentially, requiring users to maintain some
arbitrary order in that combination space.
Important symmetries are broken, arbitrary choices must be made all over the codebase,
and the code is more complex not just to write, but also to think about.
Every instance of this duplication is external modularity through copy/paste
rather than internal modularity through better language semantics.
Overall, single inheritance is much less modular than mixin inheritance,
and in that respect, it fails to fulfill the very purpose of inheritance.

@subsubsub*section{Single Inheritance is More Performant}

If single inheritance is more complex, less expressive and less modular than mixin inheritance,
is there any reason to ever use it? Yes:
the very semantic limitations of single inheritance are what enables
a series of performance optimizations.

Using single inheritance, the system can walk the method declarations
from base to most specific extension, assign an index number to each declared method,
and be confident that every extension to a specification will assign
the same index to each and every inherited methods.
Similarly for the fields of a class.
Method and field lookup with single inheritance can then be as fast as memory access
at a fixed offset from the object header or its class descriptor (or “vtable”).

By contrast, when using mixin inheritance,
because the code for a method cannot predict in advance what other modular extensions
will have been mixed in before or after the current one, and thus cannot assume
any common indexes between the many instances of the prototype or class being specified;
in the general case, a hash-table lookup will be necessary to locate
any method of element field provided by an instance of the current specification,
which is typically ten to a hundred times slower than fixed offset access.
Some caching can speed up the common case somewhat, but it will remain noticeably slower
than fixed offset access, and caching cannot wholly avoid the general case.

The simplicity of implementation and improvement superiority of single inheritance
makes it an attractive feature to provide even on OO systems that otherwise support
mixin inheritance or multiple inheritance (that has the same performance issues as mixin inheritance).
Thus, Racket’s default object system has both single and mixin inheritance,
and Common Lisp, Ruby and Scala have both single and multiple inheritance.
Users can selectively single inheritance when they want more performance
across all the subclasses of a given class.


@subsection[#:tag "MI"]{Multiple inheritance}

@subsubsection{Correct and Incorrect Semantics for Multiple Inheritance}

With multiple inheritance (see @seclink{MULIO}), a specification can declare
a list of parent specifications that it inherits from.
Each specification may then contribute methods to the overall definition of the target.
The list can be empty in which case the specification is a base specification
(though many systems add a special system base specification
as an implicit last element to any user-specified list),
or can be a singleton in which case the inheritance is effectively the same as single inheritance,
or it can have many elements in which case the inheritance is actually multiple inheritance.

Now, early OO systems with multiple inheritance (and sadly many later ones still)
didn’t have a good theory for how to resolve methods when a specification
inherited different methods from multiple parents,
and didn’t provide its own overriding definition @~cite{Kahn1976 Borning1977 Smalltalk78 Traits}.
This situation was deemed a “conflict” between inherited methods,
which would result in an error, at compile-time in the more static systems.
@; ??? Early Lisp systems would let users resolve things themselves ???
@; TODO triple check how KRL, Ani did it
Flavors @~cite{Cannon1979} identified the correct solution,
that involves cooperation and harmony rather than conflict and chaos.
Failing to learn from Flavors, C++ @~cite{stroustrup1989multiple}
and after it Ada not only issue an error like older systems,
they also and try to force the ancestry DAG into a tree
like CommonObjects @~cite{Snyder1986Encapsulation}.
Self initially tried a weird resolution method along a “sender path”
that dives depth first into the first available branch of the inheritance DAG
without backtracking @~cite{parentsSharedParts1991},
but the authors eventually recognized how wrongheaded that was,
and reverted to, sadly, the conflict paradigm @~cite{self2007hopl}@xnote["."]{
  Like the “visitor pattern” approach to multiple dispatch, the
  Self's once “sender path” approach to multiple inheritance
  fails to capture semantics contributed by concurrent branches of a partial order,
  by eagerly taking the first available branch without backtracking.
  In the end, like the “conflict” approach to method resolution though in a different way,
  it violates of the “linearity” property we describe section @seclink{CiMR},
  which explains why it cannot be satisfying.
}

We will mainly focus on explaining the correct,
@emph{flavorful} semantics for multiple inheritance, discovered by Flavors,
and since then widely but sadly not universally accepted;
but we will have to introduce several concepts before we can offer a formalization,
and along the way, we will explain where the @emph{flavorless} dead end of
“conflict” stems from.

@subsubsection{Specifications as DAGs of Modular Extensions}

Let us call “inheritance hierarchy”, or when the context is clear, “ancestry”,
the transitive closure of the parent relation, and “ancestor” is an element of this ancestry.
With single inheritance, this ancestry was a list.
With multiple inheritance, where each specification may inherit from multiple parents,
the ancestry of a specification is not a list as it was with single inheritance.
It is not a tree, either, because and a given ancestor can be reached through many paths.
Instead, the ancestry of a specification is a Directed Acyclic Graph (DAG).
And the union of all ancestries of all specifications is also a DAG,
or which each specification’s ancestry is a “suffix” sub-DAG
(i.e. closed to further transitive parents, but not to further transitive children),
of which the specification is the initial element.

Note that in this essay, we will reserve the word “parent” for a specification
another (“child”) specification depends on, and the word “super” to the partial target
the value which is inherited as argument passed to the child’s modular extension.
This is consistent with our naming the second argument to our modular extensions @c{super}
(sometimes shortened to @c{t}, since @c{s} is taken for the first @c{self} argument)
and the second argument to our @c{mix} function @c{parent} (sometimes shortened to @c{p}).
Extent literature tends to confuse specification and target
as the same entity “class” or “prototype” without being aware of a conflation,
and so confusing “parent” and “super” is par for the course in that literature.
Our nomenclature also yields distinct terms for “parent” and “ancestor”
where the prevailing nomenclature has the slightly confusing “direct super” and “super”
(or “direct superclass” and “superclass”, in a literature dominated by Class OO).

The ancestor relation can also be viewed as a partial order on specifications
(and so can the opposite descendent relation).
In the single inheritance case, this relation is a total order over a given specification’s ancestry,
and the union of all ancestries is a tree, which is a partial order but more restricted than a DAG.
We can also try to contrast these structures with that of mixin inheritance, where
each mixin’s inheritance hierarchy can be viewed as a composition tree,
that since it is associative can also be viewed flattened as a list,
and the overall hierarchy is a multitree… except that an ancestor specification
(and its own ancestors) can appear multiple times in a specification’s tree.

@subsubsection{Representing Specifications as DAGs Nodes}

To represent specification in multiple inheritance,
we will need not just a modular extension, but a record of:
@itemize[#:style enumparenalph
@item{a modular extension, as in mixin inheritance,
  that contributes an increment to the specification,}
@item{an ordered list of parent specifications it inherits from,
  that specify increments of information on which it depends, and}
@item{a tag (unique name, fully qualified path, string, symbol, identifier, number, etc.)
  to uniquely identify each specification as a node in the inheritance DAG.}]

Most languages support generating without side-effect some kind of tag
for which some builtin a comparison operator will test identity with said entity.
Many languages also support such identity comparison directly on the specification record,
making the tag redundant with the specification’s identity.
To check specification identity, we will thus use @c{eq?} in Scheme, but
we could use @c{==} in Java, @c{===} in JavaScript, address equality in C or C++, etc.
Implementation of multiple inheritance will also be significantly sped up if
records can be sorted by tag, or by stable address or hash,
so that looking up a record entry, merging records, etc., can be done efficiently;
but we will leave that as an exercise to the reader.
Side-effects could also be used to generate unique identifying numbers for each specification;
but note that in the case of second-class OO, those effects would need be available at compile-time.
If the language lacks any of the above features, then users can still implement multiple inheritance
by manually providing unique names for specifications; but maintaining those unique names
is a burden on users that decreases the modularity of the object system, and
can become particularly troublesome in nested and computed specifications.
Interestingly, the λ-calculus itself crucially lacks the features needed
for DAG node identity; a tag must be externally provided,
or a side-effects (or a monad encoding) are required for a counter.

The type for a multiple inheritance specification would thus look like the following,
where @c{Nat} is the type of natural numbers,
@c{Iota} introduces a finite dependent type of given size,
@c{DependentList} introduces a dependent list,
@c{Tag} is a type of tags giving the specifications an identity as nodes in a DAG,
and the @c{{...}} syntax introduces some kind of record type.

@Code{
type MISpec r i p =
  ∀ r i p : Type → Type .
  ∀ l : Nat .
  ∀ pr pi pp : Iota l → Type → Type .
  r ⊂ Intersection pr,
  i ∩ Intersection pp ⊂ Intersection pi ⇒
  { getMExt : MExt r i p ;
    parents : DependentList j: (MExt (pr j) (pi j) (pp j)) ;
    tag : Tag }}

@subsubsection[#:tag "DMRMI"]{Difficulty of Method Resolution in Multiple Inheritance}

Then comes the question of how to instantiate a multiple inheritance specification into a target.
It seems obvious enough that the inheritance DAG of modular extensions
should be reduced somehow into a single “effective” modular definition:
only then can specifications of large objects and ecosystems
be composed from specifications of many smaller objects, methods, etc.,
such that the effective modular definition for a record of methods
is the record of effective modular definitions for the individual methods.
What then should be the “super” argument passed to a each modular extension,
given the place of its specification in the ancestry DAG?

@subsubsub*section{The Diamond Problem}

One naive approach could be to view the inheritance DAG as some kind of attribute grammar,
and compute the (open modular definition for) the super at each node of the DAG
as a synthetic attribute@xnote[","]{
  Beware that what is typically called “child” and “parent” in an attribute grammar
  is inverted in this case relative to what is “child” and “parent” in the inheritance DAG.
  For this reason, computing effective modular extensions from ancestor to descendent
  along the inheritance DAG makes that a synthesized attribute rather than an inherited attribute
  along the attribute grammar. This can be slightly confusing.
}
by somehow combining the modular definitions at each of the supers.
After all, that’s what people used to do with single inheritance:
synthesize the modular definition of the child from that of the parent
and the child’s modular extension.
Unhappily, we saw earlier @seclink{MFCM} that there is no general way to combine
multiple modular definitions into one, except to keep one and drop the others.
Modular extensions can be composed left and right,
but modular definitions can only be on the right and on the left must be a modular extension.

The difficulty of synthesizing a modular definition is known as
the “diamond problem” @~cite{bracha1992jigsaw inheritance1996}@xnote[":"]{
  Bracha quotes Bertrand Meyer who talks of “repeated inheritance”.
  He says he didn’t invent the term “diamond problem”,
  that must have already circulated in the C++ community.
}
Consider a specification C with two parents B1 and B2 that both have a common parent A.
The contribution from A has already been baked into the modular definitions of each of B1 and B2;
therefore trying to keep the modular definitions both of B1 and of B2
leads to duplication of what A contributed to each,
which can cause too many side-effects, resource explosion,
yet possibly still the loss of what the B2 contributed,
when the copy of A within B1 reinitializes the method
(assuming B2 is computed before B1).
Keeping only one of either B1 or B2 loses information from the other.
There is no good answer;
any data loss increases linearly as diamonds get wider or more numerous;
meanwhile any duplication get exponentially worse as diamonds stack,
e.g. with E having parents D1 and D2 sharing parent C, and so on.
That is why Mesa, Self, C++, Ada, PHP, etc.,
view multiple distinct methods as a “conflict”, and issue an error
if an attempt is made to rely on a method definition from specification C’s parents;
C has to provide a method override,
to specify one of its parents to effectively inherit the method from,
or to signal an error if the method is called.

The “conflict” approach is internally consistent;
but it is probably the single least useful among all possible consistent behaviors:
@itemize[
@item{The approach drops all available information in case of conflict;
      users are then forced to otherwise reimplement the functionality of all but at most one
      of the methods that could have been combined,
      thereby failing in large part the Criterion for Extensibility (see @seclink{CfE}).}
@item{Even this reimplementation in general is impractical or at times impossible;
      the whole point of modularity is that
      the person doing the extensions is not an expert in the code being extended
      and vice versa (division of labor);
      the source code for the module being extended might not even be available for legal reasons,
      and not be understandable even when it is;
      even when the code is available, understandable and legally copyable,
      it may not be affordable to keep up with changes in code from a different project,
      with people moving at a speed and in a direction incompatible with one’s own schedule.
      This is a big failure for Modularity (see @seclink{CfM}).}
@item{Users trying to circumvent the broken inheritance mechanism still have to invent their
      own system to avoid the same exponential duplication problem that the
      implementers of the OO system have punted on.
      They are in a worse position to do it because their are mere users;
      and their solution will involve non-standard coding conventions
      that will not work across team boundaries.
      This is another big failure for Modularity (again see @seclink{CfM}).}]

Now, if computing a modular definition from parent modular definitions,
conflict detection and picking a winner are the only consistent solutions,
and the latter is not much better than the former, less symmetrical,
and more prone to wasting hours of programmer time by silently doing the wrong thing.
Which means, better behavior has to @emph{not} be simply based on
synthesizing a child’s modular definition from its parents’ modular definition.

@; TODO See also Malayeri & Aldrich’s 2009 "CZ: Multiple Inheritance without Diamonds" and its citations 43, 46.
@; TODO Discuss Snyder’s idiotic "make ancestry a tree" idea reprised by C++ and Ada.

@subsubsub*section{Cooperation not Conflict}

To find a better consistent behavior than conflict requires a reassessment
of what better designed attribute than a modular definition
should be synthesized from the inheritance DAG if any.
Step back@xnote["."]{
  As Alan Kay said, “Perspective is worth 80 IQ points”.
}
From what can we extract a modular definition, that isn’t bothered by diamonds?
How about a modular extension?
Well, we have individual modular extensions we can compose.
Ah, but we can’t just compose everything with exponential repetitions.
How then do we find an ordered list of modular extensions to compose without repetition,
and how can we maximize modularity as we determine that list?
And, if we step back further—what are all the consistency constraints
that this ordered list should satisfy, and how do we know?

@subsubsection[#:tag "CiMR"]{Consistency in Method Resolution}

Here are important consistency properties for method resolution to follow,
also known as constraints on the method resolution algorithm.
There is sadly no consistent naming for those properties across literature,
so we will propose our own while recalling the names previously used.

@subsubsub*section{Inheritance Order: Consistency with Inheritance}
A specification’s modular extension shall always be composed
“to the left” of any of its ancestors’,
where sequential effects and computation results flow right to left.
Thus children may have as preconditions the postconditions of their parents.

Thus, if @c{method-spec} declares @c{record-spec} as a parent,
every method defined or overridden by the former can safely assume
that indeed there will be a properly initialized record
into a specific field of which to define or override the value.
Overrides will happen after initialization, and
will not be cancelled by a duplicate of the initialization.
Similarly, if specification adds a part to a design,
it can depend on @c{base-bill-of-parts} as a declared parent,
it can be confident that when it registers a part,
the part database will already be a initialized, and will not be overwritten.
database of parts to which to register it to (at least if the latter wasn’t ignored due to conflict).

This property is so fundamental it is respected by all OO languages since SIMULA @~cite{Simula1967},
and may not have been explicitly named before as distinct from inheritance itself.

@subsubsub*section{Linearity: Conservation of Information}
The information contributed by each ancestor’s modular extension
shall be taken into account once and only once.
User-specified extensions may drop or duplicate information,
but the system-provided algorithms that combine those extensions
and are shared by all methods must not.

Thanks to this property, a specification for a part as above can declare
the base-bill-of-parts as a parent then safely assume that the part database
will be initialized before it is used (no ignoring the initialization),
and won’t be reinitialized again after registration, cancelling the registration
(no duplicating the initialization). Each part registered by its respective extension
will be counted once and only once, even and especially when contributed by independent
specifications that are in no mutual ancestry relation.

The linearity property is not respected by the languages that see “conflict”
in independent method specifications as above;
instead this linearity property replaces conflict with @emph{cooperation}.
Instead of distrust and negative sum games
where developers have to fight over which extension will prevail,
contributions from others extensions are dropped and must be reimplemented,
there can be trust and positive sum games,
where developers of each specification contribute their extension to the final result,
and all these contributions combine harmonously into the whole.
This property also was not respected by the once “sender path” approach of Self
@~cite{parentsSharedParts1991 self2007hopl}.
This property would be expressible but not modular and hard to enforce in
hypothetical languages that would require users to manually synthesize attributes
from the inheritance DAG so as to extract semantics of methods.
We are naming this property after the similar notion from “linear logic”, @; TODO cite
wherein the preservation of computational resources corresponds to
some operator being “linear” in the mathematical sense of linear algebra.

This property was the groundbreaking innovation of Flavors @~cite{Cannon1979}.
Flavors’ flavor of multiple inheritance, that includes many more innovations,
was a vast improvement in paradigm over all its precedessors,
and sadly, also over most of its successors.

@subsubsub*section{Linearization: Consistency across Methods}
Any sequential effects from the ancestor’s modular extension should be run
in a consistent “Method Resolution Order@xnote["”"]{
   The term and its abbreviation MRO were introduced by Python 2.3 circa 2003,
   @; TODO cite Michele Simionato https://docs.python.org/3/howto/mro.html
   and subsequently adopted by various popular languages including Perl 5.10 circa 2007.
}
across all methods of a given specification that may have such effects.
This property, that extends and subsumes the previous two, implies that
this order is a @emph{linearization} of the inheritance DAG,
i.e. a total (“linear”) order that has the partial order of the DAG as a subset@xnote["."]{
  Note how the word “linear” means something very different in the two constraints
  “linearity” and “linearization”:
  In the first case, the word comes from Linear Logic,
  and means conservation of information or other resources.
  In the second case, the word comes from Set Theory and Order Theory,
  and means a total order where any two elements are comparable,
  as opposed to a partial order where some elements are incomparable.
  Ultimately, the word “linear” in Linear Logic is inspired by Linear Algebra,
  that is connected to Order Theory via Boolean Algebras.
  And we’ll see the two are related in that a way to combine arbitrary
  black box sequential computations by executing each of them once and only once (linearity)
  necessarily implies finding a total order (linearization) in which to compose them.
  Still the same word has very different meanings in the two contexts.
}
Since CommonLoops @~cite{bobrow86commonloops}, it has been customary to call it the
class (or object, for Prototype OO) @emph{precedence list}, a term we will use@xnote["."]{
  The original Flavors paper just mentions that
  “the lattice structure is @emph{flattened} into a linear one”,
  and the original source code caches the list in a field called @c{FLAVOR-DEPENDS-ON-ALL}.
  The LOOPS manual talks of precedence but not yet of precedence list.
  The SIMULA manual has a “prefix sequence” but it only involves single inheritance.
}

Thanks to this property, methods that marshal (“serialize”) and unmarshal (“deserialize”)
the fields of a class can follow matching orders and actually work together.
Methods that acquire and release resources can do it correctly,
and avoid deadlock when these resources include holding a mutual exclusion lock.
Inconsistency can lead to resource leak, use-before-initialization, use-after-free, deadlock,
data corruption, security vulnerability, and other catastrophic failures.

This property was also one of the major innovations of Flavors @~cite{Cannon1979}.
As we will see, it implies that the semantics of multiple inheritance
can be reduced to those of mixin inheritance
(though mixin inheritance would only be formalized a decade later).
It is the first of the constraints after which C3 @~cite{Barrett96amonotonic} is named.
Inheritance order and linearity together imply linearization,
especially since some methods involve sequential computations,
and a uniform behavior is mandated over all methods.

Interestingly, all Class OO languages, even the “flavorless” ones,
necessarily have some variant of this property:
when they allocate field indexes and initialize instance fields,
they too must walk the inheritance DAG in some total order preserving
the linearity of slots, initialized in inheritance order.
Unhappily, they do not expose this order to the user,
and so pay the costs without providing the benefits@xnote["."]{
  A clever C++ programmer might recover the linearization implicit in object initialization
  by having all classes in his code base follow the design pattern of constructors
  computing the effective methods for the class as Flavors would do.
  Unhappily, “static” member initialization does not rely on linearization,
  only instance member initialization does; thus object constructors would have to do it
  the first time an object of the class is instantiated;
  but the test for this first time would slow down every instantiated a little bit,
  which defeats the “need for speed” that often motivates the choice of C++.
  Also, since this design pattern requires active programmer cooperation,
  it will not work well when extending classes from existing libraries,
  though this can be worked around in ugly ways
  if those classes didn’t keep crucial functionality “private”.
}

Now, a valid objection to linearization is that when two extensions ignore their super argument,
and the system puts one in front of the other, the second and everything after the first one
is actually ignored, and it might not be obvious which,
and there probably should be at least some warning@~cite{Snyder1986Encapsulation}.
However, if that were actually a problem practically worth addressing (which is arguable),
then just like some languages like Java or C++ have you annotate some methods
with a keyword @c{override} to signify that they modify a previous method,
an OO language could have users annotate their methods with a keyword @c{base}
(or deduce it from the method body ignoring the @c{super} argument),
and issue a warning or error if one inherits two different @c{base} definitions for a method,
and tries to call the super method (either through an override, or through the lack thereof).

@subsubsub*section{Local Order: Consistency with User-Provided Order}
The “local (precedence) order” in which users list parents in each specification must be respected:
if a parent appears before another in the list of parents local to some specification,
then the two will appear in the same relative order (though not necessarily consecutively)
in the precedence list.

This property enables users to control the precedence list, and
to specify ordering dependencies or tie-breaks that the system might not otherwise detect or choose,
including but not limited to compatibility with other systems or previous versions of the code.
If users really want to relax ordering dependencies,
they can introduce intermediate shim specifications with pass-thru behavior,
so that the ordering constraint only concerns the irrelevant shims,
while the actual parents are not constrained.
This is burdensome, though, and users may prefer to simply adjust
the local order of their parents to whichever global order of specifications
is mandated by constraints from other parts of the code,
despite a very slight decrease in modularity when the ordering is partly an arbitrary choice
heuristically made by the linearization algorithm.

This property was first used in New Flavors @~cite{Moon1986Flavors},
speaks of “local ordering”.
CommonLoops @~cite{Bobrow86CommonLoops} adopted it as
“local precedence”, “local ordering”, and “local precedence list”.
CLOS @~cite{Bobrow88CLOS cltl2} adopts it as “local precedence order”.
Ducournau et al. speak of “local ordering” or “local precedence order”
@~cite{Ducournau1992Monotonic Ducournau1994Monotonic}.
C3 says “local precedence order”.
It is the second of the three eponymous constraints of C3 @~cite{Barrett96amonotonic}.
Among popular “flavorful” languages,
Python, Perl, Lisp and Solidity notably respect this constraint,
but Ruby and Scala fail to.

@subsubsub*section{Monotonicity: Consistency across Ancestry}
The “method resolution order” for a child specification should be consistent
with the orders from each of its parents:
if the precedence list for a parent places one extension before another,
it will keep doing so in every child.

This property allows extensions to partake in the same protocols as the specifications
being extended. Indeed, lack of this consistency property when the order of the extensions
drives the acquisition and release of resources including but not limited to
heap space, locks, file descriptors, stack space, time slots, network bandwidth, etc.,
can cause memory leaks, deadlocks, kernel space leak, memory corruption,
or security vulnerabilities instead of deadlocks.
By contrast, with this consistency property, developers may not even have to care
what kind of resources their parents may be allocating, if any, much less in what order.

This property was first described @~cite{Ducournau1992Monotonic}
then implemented @~cite{Ducournau1994Monotonic} by Ducournau & al.,
and is the third of the three constraints after which C3 is named @~cite{Barrett96amonotonic}.
Among popular “flavorful” languages, Python, Perl and Solidity respect this constraint,
but Ruby, Scala and Lisp fail to.
(Though at least in Common Lisp you can use metaclasses to fix the issue in your code.)


@subsubsub*section{Shape Determinism: Consistency across Equivalent Ancestries}
Two specifications with equivalent inheritance DAGs
(with an isomorphism between them, bijection preserving partial order both ways)
will yield equivalent precedence lists, up to the same isomorphism.
Renaming methods or specifications, moving code around, fixing typos,
updating method bodies, adding or removing methods,
changing filenames and line numbers, etc., will not change the precedence list.

This property enables users to predict the “method resolution order” for a specification,
based on the “shape” of its inheritance DAG alone.
Unrelated changes to the code will not cause a change in the precedence list,
thereby potentially triggering bugs or incompatibilities between code versions.
This property can also be seen as generalizing linearization, in that
linearization guarantees the same precedence list for all methods within a given closed specification,
whereas shape determinism guarantees the same precedence list for all open specifications
with equivalent inheritance DAG, which subsumes the previous case,
since the methods of a class or prototype are “just” open specifications
that have been assembled together into a closed one, with a shared ancestry.
Thanks to Shape Determinism, changes made while debugging won’t suddenly hide bad behavior, and
changes made while refactoring or adding features won’t introduce unrelated bad or unexpected behavior.

This property was first described @~cite{Ducournau1992Monotonic}
under the nondescript name “acceptability”.
It received little attention, maybe because most (all?) popular OO systems
already respect it implicitly. The C3 algorithm respects it,
but not enough to name it and count it among the constraints
it purports to implement @~cite{Barrett96amonotonic}@xnote["."]{
  There are thus effectively four constraints enforced by C3,
  just like there are effectively four musketeers as main protagonists in
  The Three Musketeers @~cite{Dumas1844}.
}

As an alternative to Shape Determinism, you could establish a global ordering
all defined classes across a program,
e.g. lexicographically by their name or full path,
or by assigning a number in some traversal order,
or from a hash of their names or definitions, etc.
This ordering could then be used by a linearization algorithm
as a tie-breaking heuristic to choose which superclass to pick next
while computing a precedence list,
whenever the constraints otherwise allow multiple solutions.
But the instability of such a heuristic when the code changes
would lead to many @emph{heisenbugs}.

@subsubsection{Computing the Precedence List}

Let us consider a function @c{compute-precedence-list}
that takes a specification featuring multiple inheritance (and possibly more features)
and returns a list of specifications, that is
a linearization of the specification’s ancestry DAG,
as per the linearization property above.
Further assume that the above returned precedence-list
starts with specification itself, followed by its ancestors
from most specific to most generic (left to right).
This is the convention established both by Flavors’ “class precedence list”
and, maybe surprisingly, also by SIMULA’s “prefix sequence”,
though in the case of SIMULA this convention is contravariant with
to the order in which the bodies of the “prefix classes” are concatenated into
the effective class definition.
@; TODO secref appendix
Most (all?) OO systems seem to have adopted this convention.
@;{ TODO cite CLOS, Scala. Ruby? Python? C++? }

Now, the precedence list can be computed simply by walking the DAG depth-first, left-to-right.
The original Flavors used a variant of such an algorithm,
@;{ TODO cite the CADR code https://www.heeltoe.com/retro/mit/mit_cadr_lmss.html
    See MAP-OVER-COMPONENT-FLAVORS in src/lispm2/flavor.160
    https://github.com/mietek/mit-cadr-system-software }
and Ruby still does to this day. @; Also NewtonScript, and probably more
Unhappily, this approaches fails at respecting either Local Order or Monotonicity.

Another approach is to consider the precedence list a synthesized attribute,
and compute a child’s precedence list from those of its parents.
That’s the only reasonable way to ensure monotonicity.
However, the naive way to do it, by concatenating the lists then removing duplicates,
like LOOPS @~cite{Bobrow1983Loops}
or after it (though removing from the other end) Scala @~cite{scalableComponentAbstractions2005},
preserves neither Local Order nor Monotonicity.
The slightly more careful algorithm used by CommonLoops @~cite{Bobrow86CommonLoops}
and after it by CLOS (with minor changes) @; TODO check what those changes are
preserves Local Order, but not monotonicity.
The slightly complex algorithm by Ducournau et al. @~cite{Ducournau1994Monotonic},
and the latter somewhat simpler C3 algorithm @~cite{Barrett96amonotonic WikiC3},
synthesize the precedence list while preserving all desired properties.
C3 was notably adopted by OpenDylan, Python, Raku (Perl), Parrot, Solidity, PGF/TikZ.

We will provide a section @seclink{C4} below an informal description of
our extension to the C3 algorithm, and in appendix the full code.

@subsubsection{Mixin Inheritance plus Precedence List}

How then can we use this precedence list to extract and instantiate a modular definition
from the modular extensions of a specification and its ancestors?
By extracting the list of these modular extensions in that order, and
composing them as per mixin inheritance:

@Code{
compute-precedence-list : MISpec ? ? ? → DependentList ? (MISpec ? ? ?)
effectiveMExt : MISpec r i p → MExt r i p
fixMISpec : top → MISpec p top p → p

(define effectiveMExt (λ (mispec)
  (foldr mix idMExt (map getMExt (compute-precedence-list mispec)))))
(define fixMISpec (λ (top) (λ (mispec)
  (fix top (effectiveMExt mispec)))))}

The @c{map} function is the standard Scheme function to map a function over a list.
The @c{foldr} function is the standard Scheme function to fold a list with a function
(also known as @emph{reduce} in Lisp and many languages after it).
The type parameters to @c{MISpec} in @c{compute-precedence-list} were left as wildcards above:
the precise dependent type, involving existentials for @c{l pr pi pp} such that
every specification in the list can be composed with the reduced composition
of the specifications to its right, is left as an exercise to the reader.

We have thus reduced the semantics of multiple inheritance
to mixin inheritance (or, in this case equivalently, single inheritance)
by way of computing a precedence list.

Complete implementations of prototypes using multiple inheritance
in a few tens of lines of code are given
in our previous paper using Scheme@~cite{poof2021},
or in a proof of concept in Nix@~cite{POP2021}.
Our production-quality implementation in @(GerbilScheme)@~cite{GerbilPOO}
including many features and optimizations fits in about a thousand lines of code.

@subsubsection{Notes on Types for Multiple Inheritance}

As usual, @c{effectiveMExt} works on open specifications,
whereas @c{fixMISpec} only works on closed specifications.
The present formalization’s ability to deal with open specification
and not just closed ones crucially enables finer granularity for modular code.

Now, note how multiple inheritance relies on subtyping of specifications in a way that
single inheritance and mixin inheritance don’t:
In those simpler variants of inheritance, the programmer controls precisely
what are the next modular extensions to be composed with, and so does not need to rely on subtyping;
indeed, we saw when introducing wrappers for conflation that sometimes we really want
to use modular extensions that do not follow the usual subtyping constraints
(in that case in section @seclink{RC}, @c{qproto-wrapper} that wraps the value into a pair).
By contrast, with multiple inheritance, a specification only controls the relative order of its
ancestors in the precedence list that will be composed, but its modular extension must remain
correct when used as part of a descendent specification, in which case other modular extensions
may be interleaved with its ancestors as part of a larger precedence list.
That is why a multiple inheritance specification’s modular extension
must always be a strict modular extension
(though there can be non-strict wrapper extensions around the precedence list),
whereas single inheritance and mixin inheritance can use any kind of modular extension.

@;{TODO: Remove this rant before publication, or substantiate it
with citations each accompanied by detailed notes on what goes wrong}
Sadly, multiple inheritance often remains
unjustly overlooked, summarily dismissed,
or left as an exercise to the reader in books that discuss the formalization
of programming languages in general and/or OO in particular
@~cite{AbadiCardelli1997ToO Pierce2002TAPL eopl3 plai}. @TODO{more?}
The wider academic literature is also lacking in proper treatment of
types for multiple inheritance, with some notable exceptions like@~cite{allen2011type}.
@TODO{cite more: Jonathan Aldrich ? Odersky ?}
Much of the focus of the literature is on subtyping,
with a deemphasis or outright avoidance of fixpoints and self-recursion,
leading many authors to confuse subtyping of specification and target.
Subtyping is then often studied in the context of single inheritance,
even though subtyping isn’t quite as important without multiple inheritance.

More generally, computer science researchers seem largely uninterested in the nature
of modularity or extensibility, at best assuming they are purely technical
aspects of a language with a fixed formal expression, or else someone else’s problem;
they have no consideration for how programming language features do or do not affect
the social dynamics of interactions between programmers,
how much coordination they require or eschew across development teams,
or within one programmer’s mind.
Consequently, they lack any criterion for modularity,
and how to compare no inheritance, single inheritance, mixin inheritance and multiple inheritance.
Finally, a lot of language designers, industrial or academic,
invent some primitives that embodies all the features of a small model of OO;
they fail to enlighten in any way by introducing their own ad hoc logic,
and still crumble under the complexity of the features they combined
despite being way short of what an advanced OO system can provide.
Meanwhile, truly groundbreaking work, such as Flavors, is routinely rejected as obscure,
left uncited, or is only cited to quickly dismiss it with no attempt
to take its contents seriously.

And yet languages that care more about expressiveness, modularity and incrementality
than about ease of writing performant implementations with simpler type systems,
will choose multiple inheritance over the less expressive and less modular alternatives:
see for instance Common Lisp, C++, Python, Scala, Rust.
@TODO{cite Scala OO model. What else? Kathleen Fisher’s thesis?}

@subsubsection{Comparing Multiple- to Mixin- Inheritance}

Since all variants of OO can be expressed simply as first-class concepts in FP,
comparing the variants of OO actually requires assuming second-class OO,
with a compile-time language significantly weaker than the λ-calculus.
The comparison can still inform us about first-class OO in that it tells us
how much one can enjoy OO as if it were second-class,
versus how much one has to painfully escape the illusion,
when using this or that variants.
The expressiveness comparison informs us about what some variants automate
that has to be done manually under other variants.
The modularity comparison informs us about what requires synchronization
with other programmers under some variants but not others.
Together, they also tell us that some features could be automated in theory
yet cannot be so in practice due to lack of synchronization between programmers,
due to lack of a better OO variant.

@subsubsub*section{Same Expressiveness}

Mixin inheritance is clearly as expressive as multiple inheritance,
since every entity that can be written using multiple inheritance
can just as well be written using mixin inheritance,
by computing the precedence list manually.

Multiple inheritance is as expressive as mixin inheritance,
if you restrict yourself to the subset of mixin inheritance
where you don’t repeat any modular extension in a list you compose:
define one multiple inheritance specification without parents per modular extension,
and one specification with the list of the previous as parents to combine them.
If for some reason you do want to repeat a modular extension, then you may have
to duplicate the repeated definitions, though you may factor most code
out of the definition into a helper (part of the specification containing the modular extension),
and only have duplicate calls to the helper.
Strictly speaking, this is still slightly less expressive than mixin inheritance,
but this case never seems to happen in the wild@xnote["."]{
  If you ever see this repeated usage pattern appear in useful code,
  or if you have a good argument why it is never actually useful,
  you should definitely publish a paper about it, and send me a copy.
}
Also, this slight decrease in expressiveness, if any, does not impact modularity,
since the same module that exported a modular extension to use multiple times,
would instead export a multiple inheritance specification to use once, and
per-method helpers to use multiple times.
Therefore we can say that multiple inheritance is as expressive as mixin inheritance in practice.

@subsubsub*section{Superior Modularity}

Let us now compare the two variants of inheritance from the point of view of modularity.
Multiple inheritance requires somewhat more sophistication than mixin inheritance,
adding the cognitive burden of a few more concepts,
which at first glance can be seen as detrimental to modularity.
Yet, we will argue that inasmuch as modularity matters,
these concepts are already relevant,
and that multiple inheritance only internalizes modularity issues
that would otherwise still be relevant if left as external concepts.
Moreover, multiple inheritance automates away a crucial cross-module task
that mixin inheritance requires users to handle manually,
thereby reducing modularity.

Thus, consider the issue of dependencies between modular extensions.
We saw that in practice, the common modular extension @c{method-spec} depends on @c{record-spec},
while part specifications in our notional example depend on @c{base-bill-of-parts}.
More generally, a specification may depend on a method having been implemented in an ancestor
so its inherited value may be modified in a wrapper (in this case, the “database” of parts),
or, in some languages, just declared so it may be used (though with a proper type system
this might not have to be in a parent).
Dependency constraints between modular extensions are an ubiquitous phenomenon in mixin inheritance,
and these constraints exist even when they are not represented within the language
as internal notions of “parent” and “ancestor”.

Some clever chaps might suggest to pre-compose each modular extension with all its dependencies,
such that when modular extension @c{B1} depends on @c{A},
you’d export @c{B1precomposed = (mix B1 A)} instead of @c{B1},
and that’s what your users would use.
Unhappily, that means that if another module @c{B2} also depends on @c{A}
and exports @c{B2precomposed = (mix B2 A)},
then users who want to define a class @c{C} that uses both @c{B1} and @c{B2},
will experience the very same diamond problem as when trying to synthesize
a modular definition from an attribute grammar view of of multiple inheritance in @seclink{DMRMI}:
the pre-composed dependencies (@c{A} in this case) would be duplicated in the mix of
@c{(mix B1precomposed B2precomposed) = (mix (mix B1 A) (mix B2 A))};
these copies would badly interfere, in addition to leading to an exponential resource explosion
as you keep pre-composing deeper graphs.
Therefore, pre-composing modular extensions is the same non-solution
that led to the “conflict” view of multiple inheritance,
based on the naive conceptualization of how to generalize single-inheritance.
Indeed, precomposed modular extensions are essentially the same as modular definitions.

In the end, composing modular extensions is subject to dependency constraints.
And these dependency constraints only get more complex and subtle
if you want your linearization to respect not just the inheritance order,
but also the local precedence order, monotonicity of precedence lists.
Yet, automatically or manually, the constraints @emph{will} be enforced in the end,
or programs @emph{will} fail to run correctly.
Multiple inheritance enforces these constraints intra-linguistically,
and requires no further communication between programmers.
Mixin inheritance requires the programmer to enforce them extra-linguistically,
and thus care about, and communicate about, which modular extension depends on which,
in which order, including subtler details of local order and monotonicity
if their specifications ever drive order-dependent resource management such as with locking.
In other words,
with multiple inheritance, a specification’s dependencies are part of its implementation, but
@principle{with mixin inheritance, a specification’s dependencies become part of its interface}.

In practice, that means that with mixin inheritance, programmers must not just
document the current direct dependencies of their specifications;
they must keep it up to date with every indirect dependencies
from libraries they transitively depend on.
And when a library makes a change to the dependencies of one of its specification,
then every single library or program that directly or indirectly depends on that specification,
must be suitably updated.
Sensitivity to change in transitive dependencies more generally means
much @emph{tighter coupling} between the versions of the many software libraries, and
fragility of the entire ecosystem, as incompatibilities ripple out,
and it becomes hard to find matching sets of libraries that have all the features one needs.
Tight coupling is the antithesis of modularity@xnote["."]{
  If you want to make the change easy on your transitive users,
  you may have write and send patches to lots of different libraries and programs
  that depend on your software.
  This is actually the kind of activity we engaged in for years,
  as maintainer of Common Lisp’s build system ASDF.
  This was greatly facilitated by the existence of Quicklisp,
  a database of all free software repositories using ASDF (thousands of them),
  and of @c{cl-test-grid}, a program to automatically test all those repositories,
  as well as by the fact that most (but by no means all) of these software repositories
  were on github or similar git hosting webserver.
  Making a breaking change in ASDF was painful enough as it is,
  having to slightly fix up to tens of libraries each time,
  but was overall affordable.
  If every refactoring of the class hierarchy within ASDF, of which there were several,
  had broken every repository that uses ASDF, due to
  every user having to update their precedence list,
  then these changes might have been one or two orders of magnitude more expensive.
  This would have been prohibitive, whether the cost is born solely by the maintainer,
  or distributed over the entire community.
  By contrast, our experience with the OCaml and Haskell ecosystems is that
  their strong static types without lenient subtyping creates
  very tight coupling between specific versions of libraries,
  with costly rippling of changes.
  What results is then “DLL hell”, as it becomes hard to find coherent sets
  of inter-compatible libraries that cover all the needs of your program,
  and to keep them up-to-date when one library requires a bug fix,
  even though there might be a library to cover each of your needs.
}

One thing that @emph{could} actually help deal with dependencies without multiple inheritance
would be a rich enough strong static type system such that
the @c{r}, @c{i} and @c{p} parameters (for required, inherited and provided)
of our parameterized type @c{MExt r i p} can identify whether modular extensions
are composed in ways that make sense.
This strategy can indeed greatly help in dealing with dependencies of modular extensions.
However, it does not fully solve the problem:
yes it helps users @emph{check} that their manual solutions are a valid ordering
that will eliminate certain classes of runtime errors;
and yes it helps struggling users search for such a valid solution faster
than random attempts unguided by types, especially if error messages
can pinpoint what elements fail to be provided, or are provided in ways incompatible
with what other extensions expect.
But the user still has to come up with the solution manually to begin with;
and the user still has to enforce all the consistency constraints that his application needs
between the solutions he comes up with for each and every specification;
and the user still has to synchronize with authors of related other packages
so they too do all those tasks correctly, and fight those who don’t see the point
because they don’t personally need the same level of consistency.

Interestingly, single inheritance doesn’t have the above modularity issue of mixin inheritance,
since every specification already comes with all its ancestors,
so that users of a specification don’t have to worry about changes in its dependencies.
However, single inheritance avoids this issue only by eschewing all the benefits
of mixin and multiple inheritance. The above issue is only an obstacle
to kinds of modularity that single inheritance can never provide to begin with,
and not to uses of OO wherein programmers export specifications
that should only be used as the pre-composed rightmost base to further extensions.
Therefore not having this modularity issue is actually a symptom of single inheritance
being less modular rather than more modular than mixin inheritance.

All in all, @principle{multiple inheritance is more modular than mixin inheritance},
that is more modular than single inheritance, that is more modular than no inheritance;
the modularity issues one experiences with one kind of inheritance are still superior
to the lack of modularity issues one experiences with the kinds of inheritance
lacking the modularity about which to have issues to begin with.

@subsection[#:tag "OISMIT"]{Optimal Inheritance: Single and Multiple Inheritance Together}

@subsubsection{State of the Art in Mixing Single and Multiple Inheritance}

With all these variants of Inheritance and their tradeoffs
naturally comes a question:
Is there some Optimal Inheritance, that is
equally or more expressive, modular and performant than any other variant?
It would have to be at least as expressive as mixin inheritance and multiple inheritance,
as modular as multiple inheritance,
and as performant as single inheritance.

Now, as far back as 1979, Lisp offered both single-inheritance with its @c{struct}s,
and multiple-inheritance with its @c{class}es (nées Flavors).
Since 1988 or so, the Common Lisp Object System (a.k.a. CLOS) @~cite{bobrow88clos}
even offered a way to interface uniformly with either structs or classes,
using generic functions and metaclasses.
Programmers could develop software with the flexibility of classes,
then when their design was stable, declare their classes to be structs underneath,
for an extra boost in performance.
However, CLOS has this limitation, that structs and classes constitute disjoint hierarchies:
a class cannot extend a struct, and a struct cannot extend a class.
Thus, before you can declare a class to actually be a struct underneath,
you must make sure to eliminate any trace of multiple inheritance in all its ancestry,
both the classes that it extends, and those that extend it,
and declare them as structs, too, thereby forfeiting use of multiple inheritance
anywhere in that hierarchy, and losing any modularity benefit you might have enjoyed@xnote["."]{
  Another limitation of structs and classes in Common Lisp is that for historical reasons,
  the default syntax to define and use structs is very different (and much simpler)
  from the CLOS syntax to use and define objects. You can use the explicitly use the CLOS
  syntax to define structs by specifying an appropriate metaclass @c{structure-class}
  as opposed to @c{standard-class} for the standard objects of CLOS;
  however, the resulting syntax is more burdensome than either plain struct or plain CLOS syntax.
  This syntax discrepancy creates another barrier to refactoring of code
  between structs and classes. Yet this syntactic barrier remains minor compared to
  the semantic barrier of having to forfeit multiple inheritance in an entire class hierarchy.
}

Since then, Ruby and Scala have done better, wherein classes can extend structs,
and structs can extend classes—except that
Ruby respectively calls “classes” and “modules”
the respective single- and multiple- inheritance entities,
whereas Scala, following the Smalltalk tradition in that,
respectively calls them “classes” and “traits” @~cite{scalableComponentAbstractions2005}.
Ruby and Scala combine the two forms of inheritance in ways broadly similar to each other
and to the Optimal Inheritance we propose below
(Ruby did about 10 years earlier that Scala,
but without an academic publication to cite, and also without static types).
However, Ruby uses a variant of the Flavors algorithm, and Scala a variant of the LOOPS algorithm,
and neither respects either Local Order or Monotonicity, making them less modular than they could,
which is sub-optimal.

Since 2024, our own @(GerbilScheme),
allows “structs” and “classes” (named after the Lisp tradition)
to extend each other like Ruby and Scala do,
but, in doing so, respects all the consistency constraints of the C3 algorithm,
that it further extends to support structs.
In the rest of this paper, we will keep naming structs and classes
after the Lisp tradition—although we will also accept
specifications that are not classes (not being for type descriptors),
but may be prototypes, or neither.

@subsubsection{The Key to Single Inheritance Performance}

As seen in @secref{CMSI}, what enables the optimizations of single inheritance is
that the indexes to the fields and methods of a specification’s target
are also the indexes of the same fields and methods in the targets of its extensions.
These indexes are computed by walk the specification’s ancestry
from least specific to most specific ancestor.
In the terminology of multiple inheritance,
this is walk along the reverse of the precedence list.
And for the walks to yield the same results,
after putting those lists in the usual order,
can be stated as the following property, that we will call the @emph{suffix property}:
@principle{the precedence list of a struct must be a suffix of that of its descendents}.

Now this semantic constraint can be very well be expressed and enforced
in a system that has multiple inheritance.
Thus it turns out that indeed, a struct can inherit from a class, and a class from a struct;
as long as this property holds, the optimizations of single inheritance are still valid,
even though structs partake in multiple inheritance!
Interestingly, the ancestry of a struct is not a linear (total) order anymore:
a few classes may be interspersed between structs in the precedence list.
However, the subset of this ancestry restricted to structs, is a linear order,
as every struct in a given specification’s ancestry is either ancestor or descendent
of every other struct in that same ancestry.
Thus structs are still in single inheritance with respect to each other,
even though they are part of multiple inheritance in their relationship with classes.

Since the suffix property is the thing that matters,
we will name “suffix” the generalization of structs
(in Lisp lingo, or classes in Smalltalk or Scala lingo)
from classes to prototypes and arbitrary specifications.
By contrast we will call “infix” the specifications
that are explicitly not declared as suffix by the programmer,
and just say specification (or prototype if conflated with target,
or class if furthermore the target is a type descriptor)
when it can be either infix or suffix.
Thus, in Lisp lingo, a “struct” is a suffix specification,
a “class” is a specification and a “mixin” is an infix specification.
In Smalltalk or Scala lingo, a “class” is a suffix specification
(though Scala documentation sometimes use it for any specification),
and a “trait” is an infix specification@xnote["."]{
  Interestingly, our “suffix” is the same as the “prefix” of SIMULA.
  SIMULA calls “prefix” a superclass, precisely because its single inherited behavior
  comes before that of the class in left to right evaluation order
  of its code concatenation semantics.
  But in multiple inheritance, we compose modular extensions right-to-left,
  and in a tradition that goes back to Flavors (and maybe beyond),
  we also keep the precedence list in that order.
  And so our “suffix” actually means the same as SIMULA’s “prefix”.
  Now, since SIMULA only has single inheritance, all its classes are “prefix”
  (i.e. “suffix”, for us).
  by contrast, in a multiple inheritance system, regular classes are infix, and
  their precedence list, while an ordered sublist of an extension’s precedence list,
  is not necessarily at the end of it, and necessarily contiguously embedded.
  It can also be confusing that SIMULA calls “prefix sequence” the list of superclasses
  that it keeps in the same order as the precedence list of Flavors and its successors,
  from most specific to least specific, which is opposite to the order of “prefixing”.
  Finally, a “final” class in Java or C++ could be called “prefix” by symmetry,
  because its precedence list is the prefix of that of any transitive subclass
  (of which there is only one, itself); but that would be only introduce more
  terminological confusion, without bringing any useful insight,
  for this prefix property, while true, is not actionable.
  It is better to leave suffix and prefix as twisted synonyms.
}

@subsubsection[#:tag "C4"]{C4, or C3 Extended}

The authors of C3 @~cite{Barrett96amonotonic WikiC3},
after Ducournau et al. @~cite{Ducournau1992Monotonic Ducournau1994Monotonic},
crucially frame the problem of ancestry linearization in terms of
constraints between the precedence list of a specification and those of its ancestors:
notably, the “monotonicity” constraint states that
the precedence list of an ancestor must be an ordered subset
of that of the specification, though its elements need not be consecutive.
We define our own algorithm C4 as an extension of C3,
that in addition to the constraints of C3, also respects the @emph{suffix property}
for specifications that are declared as suffixes.
This means that an Optimal Inheritance Specification, or @c{OISpec}
by extending the @c{MISpec} of Multiple Inheritance
with a new field @c{suffix?} of type @c{Boolean},
that tells whether the specification follows the @emph{suffix property} or not.
(In a practical implementation, we could add more flags, for instance
to determine if the specification is sealed, i.e. allows no further extensions.)
@;{TODO also cite Dylan for that, and Scala}

We give a complete Scheme implementation of C4 in the appendix,
but informally, the algorithm is as follows,
where the steps tagged with (C4) are those added to the C3 algorithm
(remove them for plain C3):
@itemize[#:style'ordered
  @item{As in C3, we first extract the precedence lists of each parent,
        in the declared Local Order.}
  @item{(C4) Before proceeding to the regular C3 algorithm, we split each precedence list
        into a prefix containing only infix specifications,
        and a suffix that starts with the first suffix ancestor.
        (Plain C3: put everything in the prefixes; suffixes are empty.)}
  @item{(C4) We merge those suffixes into a merged suffix;
        The suffix property requires that these tails must be in total order, such that
        given any two tails, one must be a suffix of the other,
        or else there is an error due to incompatible ancestries.}
  @item{(C4) Then, in each precedence list prefix, remove from its end the infix specifications
        that are already in the suffix in the same order;
        stop if you reach one that is in the suffix but in the wrong order,
        in which case it’s an error due to incompatible ancestries;
        stop if you reach one that isn’t in the merged suffix,
        at which point you check that no specification remains in the prefix is in the merged suffix
        (another incompatibility).
        If you didn’t find any incompatibility, you have cleaned your prefix.}
  @item{Proceed with C3 on the cleaned up prefixes:
        first, add the list of parents to the list of precedence lists,
        to enforce the local order;
        then, and until all the lists are empty,
        pick out as next element of the merged precedence list,
        from the front elements of what remains of the lists,
        the first (in local order) that isn’t in any of the lists
        excluding each of their front element;
        if none fits, that’s an incompatibility error.
        If you eventually exhaust the lists, you’ve got your merged prefix.}
  @item{(C4) Append the merged suffix to the end of the merged prefix.}
  @item{Return the merged prefix.}]

@;{TODO examples of the working algorithm, of incompatibility cases,
        of discrepancies with C3.
   this example bug: direct supers (A) (S A) (B A S)).
}

As a slight optimization, you could keep those precedence lists pre-split
between a prefix of infix-only ancestors, and
a suffix of the most specific suffix ancestor (followed by its infix and suffix ancestors).

Importantly, note that the constraints of C3
do not in general suffice to uniquely determine how to merge precedence lists.
There are cases with multiple solutions satisfying all the constraints,
at which point the linearization algorithm must use some heuristic
to pick which candidate linearization to use.
The C3 algorithm (and after it C4) adopts the heuristics that when faced with a choice,
pick for next leftmost element the candidate that appears leftmost
in the concatenation of precedence lists;
we @emph{believe} (but haven’t proved) that this is also equivalent to
picking for next rightmost element the candidate that appears rightmost in that concatenation.
Importantly, we also believe (but again, haven’t proved) this heuristic maximizes
the opportunity for a specification’s precedence list to share a longer suffix with its parents,
thereby maximally enabling in practice the optimizations of single inheritance
even when specifications are not explicitly declared “suffix”.

This is the algorithm used in @(GerbilScheme).
Interestingly, Ruby and Scala 3 essentially behave the same way with respect to merging suffixes,
but they both fail to use C3 or otherwise respect either local order or monotonicity
when handling the prefixes.
Note that Scala 2 behaves largely like Scala 3, but a further requires the user
to explicitly include the most specific suffix ancestor
as the semantically last parent of a specification@xnote["."]{
  We say semantically last, as Scala, per its documentation,
  also keeps precedence lists in the usual most-specific-first order.
  However, syntactically, Scala requires users to specify parents in the opposite
  most-specific-last order, so your suffix parent (a “class” in Scala)
  must be syntactically specified @emph{first} in Scala 2.
  It also doesn’t help that Scala documentation ambiguously uses “class”
  at times to mean suffix specifications only,
  but at other times to mean all specifications including infix ones
  (usually called “traits” in Scala).
  @;TODO cite
}

@section{Advanced Topics in OO}

@subsection{Focused modular extension}

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX HERE XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

@(generate-bibliography)
