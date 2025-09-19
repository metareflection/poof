#lang scribble/acmart @manuscript @anonymous @review @nonacm
@; -*- Scheme -*-
@; Default already: @10pt @natbib @screen @acmsmall
@; @anonymous @authordraft @authorversion @timestamp @review @nonacm

@; TODO: a ltuo.bib with per-entry summary and commentary tailored to the relevance of the cited piece
@; to the current one.

@; @title{C4: The best of single and multiple inheritance}
@title{Lambda, the Ultimate Object
@; @linebreak[] @;
} @subtitle{
       A theory of Object-Orientation, and a modest contribution on optimal inheritance}

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
  what OO is and isn't, what is its purpose, what is inheritance, what are known variants of it,
  what makes each variant desirable or not, what are the relevant challenges with
  using, implementing or combining them.
  In particular, we reconstruct from first principles OO
  as a mechanism for reified (in-language) Extensible Modularity,
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
          (only-in scribble/manual racket racketblock code codeblock litchar itemize item)
          (only-in scribble/example examples make-base-eval)
          (only-in scriblib/footnote note)
          (only-in scribble-abbrevs appendix)
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
Object-Oriented (“OO”) language are characterized by their use of
@emph{inheritance} @~cite{inheritance1996},
a mechanism within a programming language to
modularly and extensibly combine partial specifications of programs@xnote["."]{
  A notable dissident to this characterization is William Cook,
  a respected academic who made key contributions to understanding the semantics of inheritance
  @~cite{Cook1989 cook1989inheritance bracha1990mixin Cook1994}
  yet later argued that inheritance was orthogonal to OO @; Also Cook1989?
  and that OO is about “classes” of “objects” that can only be accessed through “interfaces”
  @~cite{Cook2009 Cook2012}.

  However, coding against an SML “module” would count as OO by Cook's criteria,
  and indeed Cook explicitly calls the untyped λ-calculus “the first object-oriented language”,
  while dismissing Smalltalk as not OO enough because its integers are not pure objects@~cite{Cook2009}.
  Cook’s definition, that embraces the modular aspect of OO while rejecting
  its extensible or dynamic aspect, runs contrary to all practice,
  and brings no insight whatsoever on what people commonly call OO,
  the many languages that provide it,
  the common idioms, libraries and patterns on top of those languages,
  as opposed to languages that are not commonly considered OO.
  It brings no light on any of the OO languages cursed by Cook as not actually being OO,
  no light on any of the Functional Programming (FP) languages blessed by Cook as actually being OO
  to the surprise of their users, and no light on the difference between the two.

  Cook's many works on OO over the years also systematically paper over important concepts
  in OO, such as prototypes, multiple inheritance, method combination or multiple dispatch.
  In the end, while Cook's PhD and subsequent academic career grew out of
  brilliantly modeling the key mechanism of OO (inheritance)
  from the foreign point of view of FP,
  his wilful ignorance and deep misunderstanding of the OO tradition, indeed missing the point,
  were such that they have become proverbial: immortalized in Gabriel's essay
  “The Structure of a Programming Language Revolution” @~cite{gabriel2012}
  as a prototypical failure to understand a phenomenon when viewed
  through a scientific paradigm incommensurable with the one that produced it.

  In the end, Cook is well worth mentioning precisely to illustrate the lack of
  common vocabulary, common concepts, and common paradigms among those
  who practice and study OO, even or especially
  among notable academics with deep expertise in the field.
  And yet, there are undeniably common practices, common phenomena, common concepts,
  worth understanding, conceptualizing, defining and naming
  in the rich (and sometimes mutually conflicting) traditions
  that grew around OO.
  There is thus a need to elucidate the key concepts of OO behind the hype and confusion;
  such is the main purpose of this paper.
}
Now language designers have faced a dilemma since almost the very beginning,
between several variants of inheritance:
some languages choose single inheritance for its simplicity and performance
@~cite{Simula1967 kay1996early},
whereas others prefer multiple inheritance
for its greater expressiveness and modularity @~cite{Bobrow1976 Cannon1979}.
A few outliers use mixin inheritance, a variant between the two
that can also be seen as more fundamental and composable @~cite{bracha1990mixin}.
@; XXX Reverse Inheritance as trivially expressible in terms of mixin or multiple inheritance.

@subsubsection{Problem: Optimal Inheritance?}
Is there a form of inheritance that is objectively better than the others,
with respect to expressiveness, modularity, extensibility, and runtime performance?
Is one of the usual variants superior to the others in every way?
If not, is there a combination of them, or a superset of them, that is?
Some languages notably combine both single inheritance and multiple inheritance,
though with some constraints @~cite{cltl2 scalableComponentAbstractions2005};
would the best way to do inheritance subsume these combinations?
Importantly, multiple inheritance usually involves a computation called
the @emph{linearization} of the inheritance graph.
Is that linearization necessary?
What becomes of it in this optimal inheritance?

@subsubsection{Claim: A Theory of OO}
So that we may offer a solution to the problem of optimal inheritance,
we will first quickly recapitulate a theory of OO sufficient to state the problem
and the criteria that may make a solution better than others.
In a way, we will only be stating lore that has been known for decades.
Yet no element of that lore is uniformly known to all;
some may never have been told explicitly in academic literature;
a few elements actually go radically against prevalent opinions in academic literature
without being necessarily wholly original;
and they have never been formulated together.
What more, our theory does not merely restate the obvious,
but shapes it into a coherent whole that is @emph{productive}:
capable of generating and justifying new knowledge,
and not merely explaining existing one.

@subsubsection{Claim: Constructive OO}
Our Theory of OO is constructive:
we include code in Scheme, using as few features of Scheme as possible,
so that it may be easily adapted to any programming language or dialect.
We explain which features we need beyond the mere applicative λ-calculus, why,
and how to typically implement them in existing programming languages.
Remarkably, the main feature we need is lazy evaluation, as
OO is most naturally defined in a pure lazy functional setting,
and eager evaluation of OO without side-effects leads to exponential recomputations.
One aspect for which we do not provide a construction, however, is static typing;
others have provided good theories of typing for Class OO@~cite{allen2011type},
but we don’t know of a good typesystem capable of dealing with Prototypes,
especially so if it must also support the colloquial use of classes as prototypes for type descriptors:
in addition to subtyping, the typesystem would need to support staged programming if not dependent types.
@; TODO evaluate Scala DOT in this context. Ask nada.

@subsubsection{Claim: C4 is Optimal}
We claim that (a) indeed there is a best way to combine single and multiple inheritance,
that (b) indeed it involves linearization of the inheritance graph,
that (c) there are enough constraints on linearization for the optimal algorithm
to be well-defined up to some heuristic, and
that (d) even then there are good reasons to prefer a specific heuristic.
We call C4 the resulting algorithm, that we implemented, and that
is included in the next release of @(GerbilScheme)
as part of its builtin object system@xnote["."]{
  Scheme @; XXX CITE lambda-the-ultimate r4rs r7rs-small
  is a language with a rather minimalist definition.
  There are dozens of implementations of the language
  (with various degrees of compliance to the various standards),
  and each implementation provides its own extensions on top of this common core,
  to offer a usable programming environment.
  The extensions are usually mutually incompatible,
  though there are trends, similarities, variants of shared code between the many implementation,
  and even standards for some of these extensions.
  In particular though, there is no standard object system,
  instead plenty of different object systems that span the entire design space for OO—except for
  generally lacking static types.
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
As we do, we introduce some simple formal models of Modularity, Extensibility and OO
in terms of pure Functional Programming (FP), and derive from first principles
a minimal Object-Oriented system, using Mixin Inheritance,
on top of the λ-calculus, using Scheme syntax, with example for how to use it.a
The system, remarkably, has neither objects nor prototypes, even less classes,
only Specifications and Targets.

In section 5, we dispel a lot of common confusions about OO by
presenting a few insights that almost everyone misses about OO:
the too easy confusion between subtyping and subclassing;
the common myth associating OO with imperative programming
when surprisingly it is actually naturally pure functional lazy;
the generally ignored fact that inheritance works on any type, not just records,
yet the reason why it is more useful with records of some sort.

In section 6, we discuss inheritance in detail:
single inheritance, multiple inheritance and mixin inheritance,
and issues surrounding method conflict and resolution, or harmonious combination.
We examine the known consistency constraints that matter
for linearization algorithms in the context of multiple inheritance,
and state of the art in satisfying them, the C3 algorithm.
We touch on the topics of
method combinations and multimethods (multiple dispatch).

In section 7, we discuss how to combine multiple and single inheritance.
We discuss existing solutions solutions respectively adopted by
Common Lisp @~cite{cltl2}, Ruby @; TODO cite
and Scala @~cite{scalableComponentAbstractions2005}.
We then propose our solution, a linearization algorithm we call C4,
that satisfies all the constraints we discussed for a good linearization algorithm,
and for combining single and multiple inheritance.
We explain why the residual heuristic we also adopt from C3 is arguably the best one.

In section 8, we discuss more advanced topics such as
Mutation, Multiple Dispatch, Monotonicity, Typeclasses, Global

Finally, in section 9 we conclude by recapitulating our findings.

@; TODO: Describe Appendices


@subsection{Note on Nomenclature}

As we restate well-known and lesser-known lore of Object Orientation,
we will endeavour to precisely define the terms we use in the rest of the article.
Defining terms is especially important since various authors
from the communities around each of many OO language
each use slightly different intersecting terminologies
that often use different words for the same concepts,
or, much worse, the same words for different concepts.
This tower of Babel can cause much confusion
when trying to communicate ideas across communities,
as people ascribe opposite presuppositions and connotations to the words
used by other people, and talk past each other
while incorrectly believing they understood what the other said.

Thus, when multiple nomenclatures conflict, we will give precedence to
the @emph{least ambiguous} word for a concept,
even if neither the most popular word for the concept, nor the oldest,
even if we sometimes make one up just for this article.
The words we choose will hopefully cause readers to pause and reflect,
rather than unwittingly misunderstand the sometimes subtle points we make
due to a treacherously familiar word.

In particular, we will conspicuously avoid using the unqualified words
“object” and “class” because they connote for each reader,
depending on the traditions he has adopted, a different set of assumptions,
that are parasitic to the theory we are laying out.
We will also reject the word “class” to mean the most general kind
of entity subject to inheritance,
since a class is but a quite limited special case of a @emph{prototype},
that itself can be seen as a special case of what we’ll call a @emph{specification}.

@; TODO for submission, move to appendix?
@section{What Object-Orientation is @emph{not}}

Before we explain in detail what OO @emph{is},
let us cast aside a lot of things it @emph{isn’t}
that too many people (both proponents and opponents)
too often falsely identified with OO.
This is important, because attempts to explain a theory of OO often fail
due to the reader having wrong expectations about what OO is supposed to be.

@subsection{Whatever C++ is}

At one point the most popular OO language,
C++ indeed enables programming in OO style to a large degree.
But C++ is a rich language with many dimensions completely independent of OO
(consider e.g. bit-banging, RAII, template metaprogramming or pointer aliasing and the memory model),
whereas the OO dimension that it undoubtly is very different from how OO works in other languages,
and colloquial C++ often goes against the principles of OO@xnote["."]{
  Alan Kay famously declared at OOPSLA ’97, near peak C++ popularity:
  “I made up the term ‘object-oriented’, and I can tell you I didn’t have C++ in mind.”}
Therefore, C++ is in no way representative of OO, and
if what you know of “Object Orientation” comes from C++,
please put it aside, at least while reading this article, and come with a fresh mind.

This is especially true with regard to multiple inheritance,
that will be an important topic later in this paper.
C++ boasts support for multiple inheritance, and many people,
when thinking of multiple inheritance, think of what C++ offers.
Yet what C++ calls “multiple inheritance” is not at all the same as
what everyone else calls “multiple inheritance”:
while C++ supports single inheritance well, what it calls “multiple inheritance”
is actually a modified kind of mixin inheritance with automatic renaming (for non-@r[virtual] classes),
and a subset of multiple inheritance (for @r[virtual] classes and members).
Moreover, C++ crucially lacks the proper method resolution
that enables a lot of the modularity of multiple inheritance in other languages.

Now, you can use C++'s powerful template language to reconstitute actual mixin inheritance
on top of C++'s weird variant of inheritance@~cite{smaragdakis2000mixin},
then implement proper multiple inheritance on top.
But this technique is quite uncolloquial, syntactically heavy, slower than the colloquial ersatz,
and you have to manually compute and chain each class's class precedence list,
which cancels some of the modularity benefits of multiple inheritance
versus single and mixin inheritance.

Finally, and at the very least, consider that
unless you explicitly tag your classes and their members @r[virtual],
C++ will deliberately eschew the “dynamic dispatch” of OO
and use “static dispatch” instead for the sake of “going fast”.
In the end, C++ is many great and not-so-great things, but only few of those things are OO,
and even most of those that look like OO are often different enough that
they do not reliably inform about OO in general@xnote["."]{
  The situation is similar for ADA, that adopted multiple inheritance in 2005
  by seemingly copying the general design of C++.
  Even when C++ got multiple inheritance wrong@~cite{stroustrup1989multiple},
  ignorance was no valid excuse,
  since Lisp got it right ten years earlier@~cite{Cannon1979}.
  Ignorance is even less forgivable in the case of ADA
  copying C++ “multiple inheritance” yet 14 years later.
}

@subsection{Classes Only}

Many claim that classes, as introduced by Simula 67@~cite{Simula1967}
(though implementing a concept previously named by Hoare@~cite{hoare1965record}),
are essential to OO, and only ever care to implement, use, formalize,
study, teach, propagandize, or criticize class-based OO (a.k.a. Class OO).
Books from summities in Programming Languages @~cite{tapl plai eopl3}
seldom even mention any other kind of OO in their chapter about OO, much less study it.

Yet KRL@~cite{Winograd1975},
the second recognizably OO language,
whose authors first applied the words “inheritance” and “prototypes” to describe it,
has prototype-based OO (a.k.a. Prototype OO).
Certainly, the modern concept of OO
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
with neither prototypes nor classes as in T @~cite{adams88oopscheme}.

It is therefore just wrong to dismiss Prototype OO as not being part and parcel
of the OO tradition, historically, conceptually, and popularly.

Now of course, classes, while not @emph{essential} to OO,
are still @emph{important} in its tradition.
The situation is similar to that of types in Functional Programming (“FP”):
the historical preexistence and continued use of the untyped λ-calculus
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
That question that would only be slowly answered positively, in theory in the early 1990s
and in practice in the mid 2000s to mid 2010s, as Haskell grew up to become a practical language.
@; darcs 2003, cabal 2005, bytestring 2005, “cabal hell” 2006, ghc6 2006, pandoc 2006, xmonad 2007,
@; “Real World Haskell” 2008. Stack 2015 “made non-trivial haskell programs & scripts repeatable”
@; There’s obviously a lot of subjectivity there—but I expect an S curve such that
@; whichever arbitrary threshhold criteria you choose the answer would be at about the same time.

Yet, there are (a) pure models of OO such as those of
Kamin, Reddy, Cook and Bracha@~cite{Kamin1988 ObjectsAsClosures Cook1989 bracha1990mixin},
(b) pure lazy dynamic OO languages such as Jsonnet or Nix@~cite{jsonnet dolstra2008nixos},
and pure lazy OO systems for Scheme@~cite{poof2021}
@; TODO maybe mention foreshadowing by Oleg Kiselyov ?
and (c) languages happily combining OO and FP such as Common Lisp or Scala
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
pure lazy functional programming is the natural setting for OO. @;{TODO secref}

@subsection{Encapsulation}

Many OO pundits claim that an essential concept in OO
is “encapsulation” or “information hiding”@~cite{DeRemerKron1975},
though there is no consensus as to what this or these concepts mean,
and no clear definition. @; TODO{CITE}

Inasmuch as “encapsulation” informally denotes but part or all of modularity,
the ability to code against an interface,
with code on either side not caring which way the other side implements its part of the interface
(or not even being able to distinguish between multiple such implementations),
then yes, this is half of the essence of OO, as per our definition
(the other half being extensibility).
Some may also call this concept “data abstraction” or some other kind of “abstraction”.
@; XXX cite Liskov???

However, inasmuch as some people identify encapsulation as the presence
of specific visibility mechanisms such as found in C++ or Java
(with some attributes or methods being @r[public], @r[private] or something in–between,
on the precise semantics of which designers of different languages cannot agree),
we’ll easily dismiss such mechanisms as not actually essential to OO,
since many quintessential OO languages like Smalltalk or Common Lisp
lack any such specific mechanism,
whereas many non-OO languages possess mechanisms to achieve the same effect,
in the form of modules defining but not exporting identifiers
(e.g. not declaring them @r[extern] in C),
or simply lexical scoping@~cite{rees1995}.
@; TODO{cite Simula? JS?}

Now, these mechanisms themselves can be very useful,
worthy features to add to an OO language, to use and study, etc.
They are just not essential to OO and not specific to it,
though of course their adaptation to OO languages will follow
the specific shape of OO constructs not found in non-OO languages.
And misidentifying OO as being about these mechanisms rather
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
most modern OO languages contain FP as a subset,
and Lisp has harmonously combined OO and FP together since both their emergences in the 1970s,
decades before anyone had the idea to invent a conflict between the two.

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
  you may model a @r{car} as a @r{lengthy} object with a @r{length} slot,
  and a @r{chassis} too. Now if your program will only ever be interested
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
  yet be actually distinct: ultimately, both @r{car} and @r{chassis} @emph{are} @r{lengthy},
  but a @r{car} @emph{has} a @r{chassis} and @emph{is not} a @r{chassis}.

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
  which will quickly lead to an obvious showstopper issue if it doesn't work,
  whereas picking is-a where has-a was the better choice can lead to a lot of complications
  before it is realized that it won't work right.
  Yet it is always preferable to understand the difference between “is” and “has”,
  and to use the correct one based on understanding of the domain being modeled,
  rather than on vague heuristics that substitute for lack of understanding.
  At any rate, this slogan, though oft quoted out of context in online debates,
  actually has nothing to do with the OO vs FP debate, it is about using OO effectively.
}

@subsection{Message Passing}
Alan Kay, who invented Smalltalk and coined the term “Object-Oriented Programming” in 1967
notably explained@~cite{Kay2020} that by that he originally meant
a metaphor of computation through independent (concurrent, isolated) processes
communicating by passing asynchronous messages.
This metaphor also guided the modifications originally
brought by Simula to Algol@~cite{Simula1966}.
It is also present in notable early object systems such as
Director @~cite{Kahn1976 Kahn1979} and
ThingLab @~cite{Borning1977 Borning1979 Borning1981}.

However, neither Simula, nor Smalltalk nor any claimed OO language
actually fits that metaphor, though Simula comes closer than its successors.
Instead, the only popular language ever to fit this metaphor
is Erlang@~cite{OOP2010};
yet Erlang is not part of the OO tradition,
and its authors have instead described its paradigm as “Concurrency-Oriented Programming”.
Meanwhile the theory of computation through message-passing processes
was studied with various “process calculi”, @; TODO cite pi calculus, join calculus, rho calculus, etc.
that are also foreign to the OO tradition,
and largely unembraced by the OO community.
Indeed Erlang crucially lacks inheritance, or support for the “late binding”
that Alan Kay also once mentioned was essential for OO@xnote["."]{
  In Erlang, each process is a dynamic pure applicative functional language
  enriched with the ability to send and receive messages to and from other processes.
  Now, as we’ll see, you need fixed-points to express the semantics of OO;
  but in a pure applicative context, you cannot directly express sharing the results of a computation,
  so the pure fixed-point combinators lead to exponential recomputations as the fixed-point
  involves deeper self-references. OO therefore cannot be supported directly within Erlang.
  It could be achieved indirectly, by restricting it to compile-time (as in most static class OO),
  or by using a global cache (as a table of multiple shared lazy computations in each process, or
  with one or multiple processes being spawned for each shared lazy computation),
  which would also require some reimplementation of garbage collection for lazy computation caches.
  Neither solution would qualify as supporting OO any the more than
  assembly language “supports” OO or any Turing-universal language “supports” any paradigm, though.
  In the end, the essence of OO, which is Prototype OO,
  directly fits in the pure lazy functional paradigm,
  but only fits indirectly in other paradigms.
}

Moreover, many OO languages generalize and extend their method dispatch mechanism
from “single dispatch” to “multiple dispatch”@~cite{
  bobrow86commonloops bobrow88clos CecilMultimethods allen2011type}.
Their “multimethods” are attached to tuples of prototypes or classes,
and there is no single prototype, class, or single independent entity of any kind
capable of either “receiving” or “sending” a message.
Instead, they are attached to a “generic function”
that handles the dispatch based on the types of its arguments@xnote["."]{
  The “generic function” functionality from in the Common Lisp Object System (CLOS) @; TODO cite
  can be viewed as isomorphic to the “protocols” functionality of Clojure.
  They would in turn be isomorphic to the “typeclasses” of Haskell or the “traits” of Rust, @; TODO cite
  if only these latter two suppored inheritance.}
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
where one object sends a “message” to a “group” of objects (rather than each member of the target group)
@; TODO cite ABCL group messaging
or to a “chemical” paradigm where a “chemical reaction” may involve
multiple entities in and multiple entities out, with “message” entities
conveying the changes in intermediary steps. @; TODO cite CHAM
But even with these extensions to the paradigm,
you would still have to also specifically shoe-horn extensibility and method resolution
into the paradigm to fit OO and its method inheritance,
whether with single dispatch or multiple dispatch.

In conclusion, whatever historical role the paradigm of message-passing processes
may have had in inspiring the discovery of OO,
it remains a wholly distinct paradigm,
with its own mostly disjoint tradition and very different concerns,
that describes a different set of languages and usage patterns.

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
Interestingly, Bart Jacobs's categorical theory of classes as co-algebras is equivalent to UML.
But at least he explicitly embraces early on the limitation whereby
self-reference or recursion is prohibited from field definitions. @; TODO cite
Just like UML, his co-algebra utterly fails to model OO;
but at least his theory is internally consistent if not externally.

UML, co-algebras and other similar methodologies
are actually relational data modeling @; TODO cite
disguised as OO.
As we’ll see later, their “classes” are extensible indeed,
but in a trivial way that fails to support modularity@xnote["."]{
  Note that there is nothing wrong at all with with relational data modeling as such:
  it is a fine technique for many purposes,
  despite being deliberately limited in abstraction (and, therefore, modularity)—and
  sometimes @emph{thanks to it}.
  Restrictions to expressiveness can be very useful,
  in the necessarily restricted or imprecise cases that they apply.
  Indeed, in some cases, relational data modeling, not OO,
  is what you need to organize your data and your code.
  However, what is very wrong, and intellectually dishonest,
  was to sell relational data modeling as OO back when OO was trendy, based on
  a superficial and at times deliberate misunderstanding of OO
  by either or both sellers and buyers, resulting in more confusion.
}.
UML and co-algebras describe the “easy case” of OO, where objects are just a convenient way
of merging records of elementary data types
(or “constant” data types, for co-algebras)
— an easy case without recursion, where subclassing indeed coincides with subtyping.
But these methodologies avoid crucial features of OO programming,
where records can recursively refer to other records,
where the operations of interest are higher-level than getting or setting fields,
where there are “binary methods” that involve two objects at once (and more),
where you incrementally extend not just data types but also algorithms, etc.
More broadly, these methodologies lack any effective semantics of inheritance,
of method resolution in computing properties of objects along their class hierarchies,
or of anything that has the precision required to specify code
that can actually run and be reasoned about.
But specifying code is exactly where the conceptual difficulties and gains of OO
are both to be found with respect to software construction.
In fact, these handwaving methodologies are specifically designed to make
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
And if you picked an OO-capable language like Scala, Java, C# or C++
(or, with manually enforced dynamic types, Python, Ruby or Lisp),
you can actually use OO as you do it.

@section{What Object-Orientation @emph{is} — Informal Overview}

@subsection{Extensible Modular Specifications}
Object-Orientation (“OO”) is a technique that enables the specification of programs
through extensible and modular @emph{partial} specifications,
embodied as entities @emph{within} a programming language:
@subsubsection{Partial specifications}
A program is made of many parts that can be written independently,
@; and need not be complete or well-founded by themselves,
enabling division of labor,
as opposed to all logic being expressed in a single big monolithic loop.
@subsubsection{Modularity}
A programmer can write or modify one part (or “module”)
while knowing very little information about the contents of other parts,
enabling specialization of tasks. Modularity is achieved by having modules
interact with each other through well-defined “interfaces” only,
as opposed to having to understand in detail the much larger contents
of the other modules so as to interact with them.
@subsubsection{Extensibility}
A programmer can start from the existing specification and only need contribute
as little incremental information as possible when specifying a part
that modifies, extends or specializes other parts,
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
started with Prototype OO, with Class OO added on top twenty years later.

@subsubsection{Classes as Prototypes for Types}
A class is a compile-time prototype for a type,
or more precisely for a type descriptor:
a record of a type and accompanying type-specific methods,
or some reflective representation thereof across stages of evaluation.
@; TODO see appendix XXX

Class OO is therefore a special case of Prototype OO,
which is therefore the more general form of OO @~cite{poof2021}.
And indeed within Prototype OO, you can readily express
prototypes for runtime type descriptors for your language,
or prototypes for type descriptors for some other language you are processing as a meta-language.

In this article, we will thus use the terminology of the more general case, Prototype OO,
and will avoid talking about “class”, since it’s a particular case of “prototype”
with very little worth discussing about it with respect to the principles of OO,
apart from too many programming language designers’ incapacity to see beyond them@xnote["."]{
  There would be plenty to discuss about types from the point of view of library design
  and ecosystem growth, the patterns that OO enables and antipatterns to avoid.
  But that’s a topic for another essay. There are plenty of existing books on OO
  and OO software libraries to get inspiration from, both positive and negative.
  Their many practical lessons apply even if they don’t get the theory of OO just right.
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

A notable exception is C++, which has a full
Pure Functional Lazy Dynamically-Typed Prototype OO language at compile-time: templates.
Java and after it Scala are also Turing-universal at compile-time,
but not in an intentional way that enables easy metaprogramming,
just an unintentional way that defeats guarantees of analysis termination
@~cite{grigore2016javagenericsturingcomplete}.

@subsection{More Fundamental than Prototypes and Classes}
@subsubsection{Specifications and Targets}
As we reconstruct the semantics of OO from first principles,
we will see that the fundamental notion of OO,
more fundamental than prototype, class or object,
is that of (extensible and modular partial)
@emph{specification} for some @emph{target} computation.

In the case of Prototype OO, the target is usually a record
but it could be of any type, though records can keep things modular.
In the case of Class OO, the target is usually a record type,
or rather a record of a record type and associated methods,
or of their compile-time representation.

It is a function from referenced target (self) and inherited partial target (super)
to defined and/or extended partial target
(if specifying a type, it’s a function from type to type to type;
or type descriptors instead of types if you want to be more precise),
possibly together with additional data (as we’ll find out when we study inheritance).

These specifications can be modeled simply using the λ-calculus
as functions of two parameters (considered together with other metadata):
@r[self] for modularity, for each specification to access finished aspects
as computed from (their and) other specifications, and
@r[super] for incremental extensibility,
for each specification to be able to contribute
its modifications to the computation as partially specified thus far.
The specified computation is then the fixed point for the @r[self] parameter
given a trivial @r[base] value for the initial super.
Composing specifications is just chaining them through the @r[super] argument
while sharing the same @r[self] argument. @; See section XXX. Cite bracha

The usage pattern of a @r[self] argument intended for fixed-point
has been dubbed “open recursion”. @; cite Pierce
It is what allows the “late binding” touted by OO proponents,
and makes the design modular: authors of one partial specification can reference
and use aspects of the total computation handled by other partial specifications.
The usage pattern of the @r[super] argument is how inheritance brings about
incrementality, extensibility and compositionality within this modular context.

@subsubsection{Prototypes as Conflation}
A specification is not a prototype, not a class, not a type, not an object:
it is not even a value of the target domain.
It is not a record and does not have fields, attributes or any such thing.

The target value is not an object either, it’s just an arbitrary value of that arbitrary domain,
that may be neither a record nor a record type, and that need not have been computed
as the fixed-point of a specification.
It cannot be incrementally extended or composed
according to any of the design patterns that define OO.

Rather, a prototype (a.k.a. “object” in Prototype OO, and “class” but not “object” in Class OO)
is a @emph{conflation} of a specification and its target, i.e. an entity that depending on
context at various times refers to either the specification or its target.

Formally speaking, a conflation can be seen as a cartesian product
with implicit casts to either component factor depending on context.
In the case of a prototype, you’ll implicitly refer to its specification
when speaking of extending it or inheriting from it;
and you’ll implicitly refer to its target when speaking of calling a method,
looking at its attributes, reading or writing a field, etc.

@subsubsection{Modularity of Conflation}
Conflation is quite practical and increases modularity in many contexts
wherein developers are not forced to decide in advance and in utter ignorance
which parts of a system they or someone else may want to extend in the future.
Having to choose between specification and target at every potential extension point
would lead to an exponential explosion of bad choices to make:
choosing specification over target for the sake of extensibility
but without the shared computation cache afforded by conflation
would lead to an exponential explosion of runtime re-evaluations;
and choosing target over specification everywhere would of course defeat extensibility.

But conflation without distinction causes utter confusion.
Those who fail to distinguish between the two very different concepts being conflated
will make categorical errors of using one concept or its properties
when the other concept needs to be used, leading to inconsistent language design,
subtle and persistent application bugs, bad tooling, and doomed research,
and nonsensical publications. @; TODO cite
Indeed, the confusion between classes and types, or between subclassing and subtyping,
are common failures among OO practitioners, even the most advanced ones.
@;{TODO cite Meyer OOSC, see other section}

Conflation was only first explicitly discussed in @~cite{poof2021} even though
(a) the concept is implicitly older than OO, going at least as far back as @~cite{hoare1965record},
and (b) the implementtion of various Prototype OO systems has to explicitly accommodate for it
(see e.g. the @c{__unfix__} attribute in @~cite{nix2015})
even when the documentation is silent about it.
However, in the case of Class OO in static languages, you can always squint and pretend
conflation is only a figure of speech used when describing the system from the outside
but not a feature of the system itself
(and yet usually still implicitly present in the documentation
using the same word “class” to mean either a type or the extensible specification of a type
depending on context).

In Class OO, all OO is resolved at compile-time in a usually restricted language:
only the type descriptors, targets of the specifications,
may still exist at runtime (if not inlined away);
the specifications as such are gone and cannot be composed or extended anymore
(except in some reflective systems
like Lisp or Smalltalk where compilation can keep happening at runtime).
In a static setting, conflation of specification and target is just a notation shorthand
with little to no semantic benefit since it is always clear at the use site which is meant,
and at runtime it is always the target.
The cost however, is dear, in terms of confusion, even among experts,
who find themselves unaware of the semantic ambiguity, and
therefore incapable of explaining the semantics of OO in a simple way,
or of reasoning about the correctness of OO programs.

Now in Prototype OO, the partial specifications for these type descriptors
can be built, extended, composed at runtime, as well as partial specifications
containing involving zero, two or more types. Conflation of specification and target
is then a great asset in terms of modularity, since the programmers who define a prototype
do not have to decide which parts of a program are left as extensible specifications,
and which are computed as non-extensible targets. They can have both, all the time,
and so can the next wave of developers (including themselves) who will keep extending
previous prototypes into further extensible prototypes.
In this context, conflation of specification and target makes for objectively more and better OO:
more modularity, more extensibility, inside the language—and at runtime.

@; TODO example Nix specification?

@subsection{Objects}
@subsubsection{An Ambiguous Word}
In Prototype OO, a prototype, conflation of a specification and its target,
is also called an “object”, especially if the target is a record.
Note that laziness in computing the target record is essential, since most specifications,
being partial, do not specify a complete computation that terminates without error in finite time.

In Class OO, a prototype, conflation of a specification and its target,
is instead called a “class”,
and the target is specifically a type descriptor rather than an arbitrary record or non-record value.
What in Class OO is called an “object” is an element of a target type as described.
Regular prototype fields and methods are called “class fields” or “class methods”
(or “static” fields and methods, after the keyword used in C++ and Java)—but
remember they only involve the target type, not the specification.
“Object methods” and fields are semantically regular methods and fields
that take one implicit argument in front, the object (i.e. element of the target type).

Finally, many languages, systems, databases, articles or books call “object” some or all
of the regular runtime values they manipulate, that may or may not be records,
and that are in no way part of an actual OO system extensible with inheritance,
at least none available to the user, whether Prototype OO or Class OO.
The authors may not claim that these objects are part of an OO framework
actual or imagined, or then again sometimes they may.

@subsubsection{OO without Objects}

The word “object” is therefore quite ambiguous, and practically useless absent the context
or a specific language, system, article, etc.
Furthermore, the fundamental patterns of OO can exist and be usefully leveraged in a language
that lacks any notion of object, merely with the notions of specification and target:
Indeed, Yale T Scheme has a class-less “object system” @~cite{adams88oopscheme},
wherein the authors call “objects” any language value,
“instance” those records of multiple function entry points used as the non-extensible targets
of their extensible specifications, themselves called “components”,
that use mixin inheritance.

To avoid confusion, we will be careful in this article to only speak of
“specification”, “target”, “prototype”, and (type) “element”
and to avoid the word “object”—both a uselessly ambiguous word and a non-necessary notion.
As already mentioned above, we will also be avoiding “class” for a different reason,
because it is a special case of the more general prototypes we are studying.

This is all particularly ironic when the field we are studying is called “object orientation”,
in which the most popular variant involves classes.
But fields are usually named as soon as the need is felt
to distinguish them from other fields,
long before they are well-understood,
thus based on misunderstandings,
so this is par for the course.

@subsection{Inheritance Overview}
@subsubsection{Inheritance as Modular Extension of Specifications}
Inheritance is the mechanism by which partial modular specifications are
incrementally extended into larger modular specifications,
until the point where a complete specification is obtained
from which the specified target computation can be extracted.

There have historically been three main kinds of inheritance,
with each object system using a variation on one or two of them:
single inheritance, multiple inheritance and mixin inheritance.
For historical reasons, we will speak of classes, subclasses and superclasses
in this subsection, since the debate was originally in those terms;
but the discussion applies as well to prototypes, subprototypes and superprototypes,
or even just to specifications, subspecifications and superspecifications.

@subsubsection{Single Inheritance Overview}

Historically, the first inheritance mechanism discovered was @emph{single inheritance},
though it was not known by that name until a decade later.
In an influential 1965 paper@~cite{hoare1965record},
Hoare introduced the notions of “class” and “subclass” of records
(as well as, infamously, the @c{null} pointer).
Subclasses though were first implemented in Simula 67 @~cite{Simula1967};
Alan Kay later dubbed this mechanism “single inheritance” and adopted it for Smalltalk 76
as a compromise instead of the more general but then less well understood multiple inheritance
@~cite{kay1996early}.
Many other languages adopted it after Smalltalk,
including Java that made it especially popular circa 1995. @;{@~cite{}. @TODO{or C#}}

In Simula, a class is defined starting from a previous class as a “prefix”.
The effective text of a class (hence its semantics) is then the “concatenation”
of the direct text of all its transitive @emph{prefix classes},
including all the field definitions, method functions and initialization actions,
in order from least specific superclass to most specific.
In modern terms, we call the prefix a superclass. @; TODO CITE
The @emph{inheritance hierarchy} of a class, set of its direct and transitive superclasses,
then constitutes a list, and the (transitive) subclass relation is a total order.

Single inheritance is easy to implement without higher-order functions:
method lookup can be compiled into a simple and efficient array lookup at a fixed index
— as opposed to some variant of hash-table lookup in the general case
for mixin inheritance or multiple inheritance.
In olden days, when resources were scarce and before FP was mature,
these features made single inheritance more popular
than the more expressive but costlier and less understood alternatives.

Even today, most languages that support OO only support single inheritance, for its simplicity.
@;{TODO cite}

@subsubsection{Multiple Inheritance Overview}

Discovered a few years later, and initially just called @emph{inheritance},
in what in retrospect was prototype OO, in KRL @~cite{Winograd1975 Bobrow1976},
multiple inheritance allows a class to have multiple direct superclasses.
The notion of (multiple) inheritance thus predates Smalltalk 1976
adopting the term, retroactively applying it to SIMULA,
and inventing the terms “single” and “multiple” inheritance
to distinguish the two approaches as well as recognize their commonality.
Although some more early systems @~cite{Kahn1976 Borning1977 Traits}
used multiple inheritance, it is only with Flavors in 1979 @~cite{Cannon1979}
that it became really understood and usable and started gaining popularity.

The structure of a class and its transitive superclasses is
a Directed Acyclic Graph (“DAG”).
The set of all classes is also a DAG, the subclass relation is a partial order.
Most OO systems include a common system-wide base class
at the end of their DAG; but it is possible to do without one.

Computing the effective method for a class when many of its superclasses,
not necessarily in total order, provide definitions for that method,
is trickier than for single inheritance, and it took many years and many systems
to get the answer just right, from seeing it as a matter of conflict between methods you override
to seeing it as a matter of cooperation between methods you combine.
Multiple inheritance is harder to understand, to implement and to use correctly.
For this reason, despite it being more expressive and more modular than single inheritance,
it still isn’t widely adopted@xnote["."]{
  Out of the top 50 most popular languages in the TIOBE index 2025, @;{TODO cite}
  6 fully support multiple inheritance (Python, Perl, Ruby, Lisp, Scala, Solidity),
  5 have partial or non-colloquial support for it (C++, JavaScript, ADA, PHP, Lua),
  17 support single inheritance only (Java, C#, VB, Delphi, R, MATLAB, Rust, COBOL, Kotlin, Swift, SAS, Dart, Julia, TypeScript, ObjC, ABAP, D)
  and the rest don’t support inheritance at all (C, Go, Fortran, SQL, Assembly, Scratch, Prolog, Haskell, FoxPro, GAMC, PL/SQL, V, Bash, PowerShell, ML, Elixir, Awk, X++, LabView, Erlang).
}

@subsubsection{Mixin Inheritance Overview}

Mixin inheritance was discovered last @~cite{bracha1990mixin},
probably because it relies on a more abstract pure functional view of OO;
yet it is perhaps more fundamental than the other two for the same reason.
It is the simplest kind of inheritance to formalize @emph{given the basis of FP},
in a couple of higher-order functions:
specifications are simple functions, inheritance is just chaining them, and
extracting the target computation is just computing their fixed-point.
Mixin inheritance also maps directly to the concepts
of Modularity and Extensibility we are discussing,
and for these reasons we will study first when presenting a formal semantics of OO.
@;{TODO ref section}

Chaining classes is a semi-group (associative with neutral element),
and the inheritance structure of a class is just the list of elementary classes chained into it.
Mixin inheritance works better at runtime, either with Prototype OO,
or for Class OO in a dynamic and somewhat reflective system.

Mixin inheritance is in some way simpler than single inheritance
(but only if you understand FP yet are not bound by limitations of today’s FP type systems),
and as expressive as multiple inheritance
(actually more, unless multiple inheritance is complemented with a renaming mechanism),
but is less modular than multiple inheritance because it doesn’t automatically handle
transitive dependencies but forces developers to handle them manually,
effectively making those transitive dependencies part of a class’s interface.

For all these reasons adoption of Mixin Inheritance remains relatively limited,
to languages like
Racket @~cite{Mixins1998 Flatt06schemewith},
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
Lieberman explores at length the philosophical underpinnings of these two idea-complexes,
which is quite interesting from a historical and psychological point of view,
but is irrelevant to a technical discussion on the semantics of OO,
its proper use or implementation decades later.
}
also explains in detail how classes and their “inheritance” can be expressed
as a special case of prototypes and their “delegation”,
while classes cannot express prototypes, with multiple examples of behaviors
expressible with prototypes but not with classes,
that can be reduced to prototypes enabling dynamic extension
of individual “objects” (prototypes) at runtime,
while classes only allow extension at compile-time,
only for an entire type (“class”) of “objects” (elements of the type).
But while it is extremely important indeed to understand the distinction and the relationship
between the general notion of prototypes from the special case of classes,
it only begets confusion to treat inheritance and delegation
as separate concepts when they have identical concerns of semantics, implementation or performance,
whether used for prototypes versus classes.

One confounding factor is that of mutable state.
Early prototype systems were usually part of systems with ubiquitous mutable state,
and their delegation algorithms include specific strategies for interaction with such state,
including mutation and sharing or non-sharing of per-object variables,
and especially tricky, mutation of the inheritance (or “delegation”) structure.
By contrast, class systems usually had all their inheritance semantics resolved at compile-time,
during which there is no interaction with user-visible side-effects, and
it doesn’t matter whether the compiler does or doesn’t itself use mutable state:
from the user point of view it is as if it were pure functional and there is no mutation
in the inheritance structure or state-sharing structure of objects.
One may have been tempted then to see prototype delegation as intrinsically stateful,
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
@r[update-instance-for-redefined-class] to control how data is preserved, dropped or transformed
when a class is redefined. @;{TODO XXX @~cite{}}
Mutable state and mutable inheritance structure in particular are therefore
clearly an independent issue from prototypes vs classes,
though it might not have appeared so at the time.
As we introduce formal models of OO, we will start with pure functional models, and
will only discuss the confounding matter of side-effects much later.@note{
  It might be interesting to explain @emph{why} previous authors failed so hard to
  identify delegation and inheritance, when the similarities are frankly obvious,
  and the relationship between classes and prototypes is well-known
  to anyone who implemented classes atop prototypes.
  But lacking direct access to those authors’ brains, our explanations must remain speculative.

  First, pioneers are eager to conceptualize and present their experiments as original
  and not just the same concept in a different context.
  They necessarily have to sell their ideas as historical package deals,
  before the underlying concepts are clearly identified and separated from each other.
  They are too close to the matter to tell which features they built would be immortalized
  through the ages as fundamental concepts vs just contingent implementation details to be soon forgotten.
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
  when they could clearly conceptualize it away as an implementation details when happening at compile-time.
  Systems paradigm people (including the old Lisp, Smalltalk and SELF communities)
  who freely mix or interleave runtime and compile-time in the very same language,
  might have had no trouble unifying the two across evaluation times,
  but they tend not to publish articles about PL semantics,
  and not to be read and understood by PL semanticians when they do.

  Revisiting these topics several decades after they were in vogue,
  and finding their then-treatment lacking, with errors from the time still uncorrected to this day,
  makes me wonder about what other false ideas I like most people assume are true
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
(1) Is it correct?
(2) What does it even mean for a definition to be correct?
Aren’t definitions “just” arbitrary conventions?
(3) Is there an authority on those words?
(4) Should we dig into the words of Alan Kay,
who invented the expression “Object Oriented Programming”,
to find a correct definition?

@itemlist[#:style'ordered
@item{Yes our definition is correct:
it accurately identifies what people mean usually by those words,
as distinguished from situations to which the words do not apply.}
@item{No, the phenomena people care to name, discuss, think about and act on
are not arbitrary, and so the important part of definitions isn’t convention at all:
the structure and understanding of these phenomena, rather than the labels used for them.}
@item{No there is authority on software vocabulary, person or committee,
that can decree different words for others to use,
even less so different phenomena for others to care about.
And people care about OO even if you try to change the name for it,
or denature the name “OO” not to identify the same phenomenon anymore.}
@item{Yes we should listen to Alan Kay@note{
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
  which means not only dynamic dispatch of method invocation depending on the runtime class of an object,
  but also the ability to dynamically define, incrementally extend, refine or combine in the language,
  and those units are first-class prototypes,
  or if they are second-class classes, there is a reflection mechanism to define and modify them.
  When this extensibility is only available at compile-time,
  as in the object system of many static languages, then
  the OOP only happens in the meta-language (as in e.g. C++ templates),
  or the language lacks complete support for OOP.

  Note that Kay didn't immediately adopt SIMULA's inheritance mechanism in Smalltalk
  (it wasn't called that yet, either);
  but he did adopt it eventually, and this adoption is what launched OO as a phenomenon.
  Kay stated adopting single inheritance over multiple inheritance was a compromise @~cite{kay1996early},
  but didn't adopt multiple inheritance later.
  More broadly, Kay didn’t endorse any specific inheritance mechanism,
  and never focused on that part of the design. To Kay it was only a means to an end,
  which is what Kay called “extreme late binding”: the fact that behavior definition
  happens and takes effect dynamically up to the last moment based on values computed at runtime.
  Inheritance, the practical means behind the late behavior definition that is late bound,
  and the precise form it takes, is secondary to Kay;
  what matters to Kay is the role it plays in enabling dynamic code specialization.
  But inheritance becomes a primary concern to us as we formalize the concepts behind OO,
  and refine the intuitions of a pioneer into codified knowledge after decades of practice.
  And if other means are found to satisfy Kay’s “extreme late binding”,
  then we’ll have to give them a name that distinguishes them from what is now called OO.
} and other experts and pioneers,
but then look at what they do and not (just) what they say,
especially since you could recursively apply
the same skepticism to the words they use in their definitions.
Also, be wary that pioneers provide inspiration, insight and discoveries,
but seldom well rounded theories, that only come after lots of
experience, filtering, and reformulation.}]

So what phenomena count as OO?
The design patterns used by programmers when they write code in an OO language.
The interactions they have with computers and with each other.
The decision trees that are enabled or disabled when evolving a program into another.
In the case of OO, these phenomena are what is captured by
the intralinguistic extensible modularity as defined above:
(a) the ability to “code against an interface” and pass any object of any type that satisfies the interface
(modularity, be it following structural or nominative rules),
(b) the ability to extend and specialize existing code by creating a new entity
that “inherits” the properties of existing entities and only needs specify
additions and overrides in their behavior rather than repeat their specifications, and
(c) the fact that these entities and the primitives to define, use and specialize them
exist @emph{within} the programming language rather than as an external preprocessing layer.

We contend that the above is what usually meant by OO,
that matches the variety of OO languages and systems
without including systems that are decidedly not OO like Erlang, SML or UML.
Whatever clear or murky correspondance between names and concepts others may use,
this paradigm is what we will call OO and discuss in this article,
systematically reducing it to elementary concepts.

@section{OO as Internal Extensible Modularity}
@subsection[#:tag "modularity"]{Modularity}

@subsubsection{Division of Labor}

Modularity@~cite{Parnas1972 Dennis1975} is the organization of software source code
in order to support division of labor, dividing it into “modules” that can each be
understood and worked on mostly independently from other modules.




@subsubsection{First- to Fourth-class, Internal or External}

A few languages offer a builtin notion of modules as @emph{first-class} entities,
that can be manipulated as values at runtime.
But popular modern programming languages usually only offer
@emph{some} builtin notion of modules as @emph{second-class} entities,
entities that exist at compile-time but are not available as regular runtime values.@note{
  In between the two, some languages offer a “reflection” API that gives some often limited
  runtime access to representations of the module entities.
  This API is often limited to introspection only or mostly;
  for instance, it won't normally let you call the compiler
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
in size and complexity generally bother.@note{
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
a preprocessor, an object linker, editor macros, or “wizards”.
And even if they somehow don’t because they can’t or can’t afford to use such automation,
developers may achieve modules as @emph{fourth-class} entities,
ones that they handle manually, with design patterns, editors, copy-paste, and lots of debugging.
Either third-class or fourth-class entities are considered @emph{external} to the language,
not part of its semantics, not handled by its processors,
yet conceptually present in the minds of the programmers.@note{
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
@item{copy and paste sections of code as poor man’s modules;}
@item{preprocess files to concatenate, transform and generate code fragments;}
@item{transclude “include” files to serve as libraries or interfaces to later-linked libraries;}
@item{divide code in files they independently compile then “link” or “load” together;}
@item{orchestrate incremental (re)building of software with utilities such as “make”;}
@item{bundle software into “packages” they exchange and distribute online;}
@item{use “package managers” to manage those bundles.}
@item{“containerize” consistent package installations into images.}]

When for the sake of “simplicity”, “elegance”, or ease of development or maintenance,
support for modularity is lacking within a language,
or within the ecosystem around one or multiple languages,
this language or ecosystem becomes but
the weak kernel of a haphazard collection of tools cobbled together
to palliate its lack of internal modularity with external modularity.
The result inevitably ends up growing increasingly
complex, ugly, and hard to use, develop and maintain.

@subsubsection{Criterion for Modularity}
@principle{A design is more modular if it enables developers to cooperate more while coordinating less}
compared to alternative designs that enable less cooperation or require more coordination,
given some goals for developers, a space of changes they may be expected to enact in the future, etc.
More ability to code one’s part, while requiring less knowledge about other people’s parts.

For instance, the object-oriented design of ASDF@~cite{ASDF2}
made it simple to configure, to extend, and
to refactor to use algorithms in @emph{O(n)} rather than @emph{O(n³)} or worse,
all of it without any of the clients having to change their code.
This makes it arguably more modular than its predecessor MK-DEFSYSTEM@~cite{kantrowitz1991}
that shunned use of objects (possibly for portability reasons at the time),
was notably hard to configure, and resisted several attempts to extend or refactor it.

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
files embody modularity internal to programming languages.@note{
  Note that while a file may be a unit of modularity, this is not always the case:
  often, actual modules span multiple files;
  other times, there may be multiple modules in a single file.
  For instance, compiling a C program typically requires each file to be registered
  into some “build” file (e.g. a @c{Makefile}) to be considered as part of the software;
  a C library also comes with an accompanying “header” file, though there need not be
  a 1-to-1 correspondence between C source files and header files.
  A C “module” therefore typically spans multiple files,
  and some files (headers and build files) can have bits from multiple modules.
  Then again, the C language also recently adopted some notion of namespace,
  which, though it doesn’t seem used that much in practice,
  can constitute yet another notion of modules, several of which can be present in a file.
  At a bigger scale, groups of files in one or multiple directories may constitute a library,
  and a “package” may include one or several libraries, etc.
  Even in a language, there can be many notions of modularity at several different scales,
  enabling division of labor across time for a single person, for a small team, for a large team,
  between several teams, etc., each time with different non 1-to-1 correspondences to files.
}

Now, while files go back to the 1950s and hierarchical filesystems to the 1960s,
@; First filesystem??? IBM disk???
@; Semi-Hierarchical: ERMA Mark 1 1958 https://dl.acm.org/doi/10.1145/1458043.1458058 - but 100 fixed categories
@; Hierarchical: Multics 1965 https://www.multicians.org/fjcc4.html OS/360, Burroughs B5000
there was an even earlier breakthrough in modularity,
so pervasive that programmers use it without even thinking about using it:
subroutines,
already conceptualized by Ada Lovelace as “operations”, @; that could be “subsidiary”,
@;{Scientific Memoirs, Selected from the Transactions of Foreign Academies of Science and Learned Societies, and from Foreign Journals, R. and J. E. Taylor , Vol III, 1843, L. F. Menabrea on Babbage's Analytical Engine. Notes of the translator (= Augusta Ada (King, Countess of) Lovelace, A.A.L. not A.L.L.)
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
}
formalized early on by Wilkes et al. in the 1940s @; TODO cite
used by the earliest programmers in manually assembled code,
part of the first programming language Plankalkül, @; TODO cite
and of FORTRAN II. @; TODO cite
Subroutines allow programmers to divide bigger problems into smaller ones
at a fine grain, that can each be solved by a different programmer,
while other programmers only have to know its calling convention.

No less important than subroutines as such yet also taken for granted
was the concept of a call stack of return addresses,
which while anticipated by Turing in 1945 @; TODO cite
only became common in the 1960s, after LISP and ALGOL. @; TODO cite
Call stacks enable reentrancy of subroutines, such that a subroutine
can be recursively called by a subroutine it itself called.
Thus, programmers do not have to care that their subroutines should not be reentered while being executed
due to a direct or indirect subroutine call.
Indeed, subroutines can now be reentered, not just accidentally, but intentionally,
to express simpler recursive solutions to problems.
The ability to do more with less coordination with programmers from other parts of the software:
that’s the very definition of modularity.

Sadly, the generalization of call stacks to first-class continuations in the 1970s, @; TODO cite
and to delimited control structures in the 1990s are still not available @; TODO cite
in most programming languages, eschewing another progress in modularity,
wherein programmers could otherwise abstract over the execution of some fragment of code
without having to worry about transformations to their control structure,
e.g. for the sake of non-deterministic search, @; TODO cite
robust replaying of code in case of failure, @; TODO cite
security checks of code behavior, @; TODO cite
dynamic discovery and management of side-effects, etc. @; TODO cite

OO prototypes and classes, modules a la ML (both second class and later first-class),
typeclasses a la Haskell, interfaces a la Java, and much more @; TODO cite
are also entities providing modularity internal to their respective programming languages.

@subsubsection[#:tag "modularity_and_complexity"]{Modularity and Complexity}

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
duplication of entities on both sides of each module's interface,
extra work to cover the “impedance mismatch” between
the representation used by the common solution
and that used by the actual problems, etc.

Modularity used incorrectly, with too many module boundaries,
module boundaries drawn badly,
resolved at the wrong evaluation stage,
can increase complexity rather than reduce it:
more parts and more crossings between parts cause more overhead,
whereas the factoring fails to bring simplification,
and instead require programmers to manage large interfaces,
subtle underdocumented interactions between features,
little code reuse of modules between different situations,
more implicit state with many cases needed to mentally juggle with
while reasoning about calls to exposed APIs.@note{
  A notable trend in wrongheaded “modularity” is @emph{myopic} designs,
  that achieve modest savings in a few modules under focus
  by vastly increasing the development costs left out of focus,
  in extra boilerplate and friction, in other modules having to adapt,
  inter-module glue being made harder, or module namespace curation getting more contentious.

  For instance, the once vastly overrated and overfunded notions “microkernels” or “microservices”
  consist in dividing a system in a lot of modules, “servers” or “services”
  each doing a relatively small task within a larger system,
  separated by extremely expensive barriers wherein data is marshalled and unmarshalled
  across processes or across machines.
  Each service in isolation, that you may focus on,
  looks and is indeed simpler than the overall software combining those services;
  but this overall software kept out of focus
  was made significantly more complex by dividing it into those services.
  Another myopic design that is much rarer these days is to rewrite a program
  from a big monolithic heap of code in a higher level language
  into a lot of subroutines in assembly language,
  boasting how vastly simpler each of these subroutines is to the previous program;
  yet obviously the sum total of those routines is much more complex to write and maintain
  than a similarly factored program written in an appropriate higher level language
  (that indeed the original language might not be).

  While myopic designs might offer some small performance improvement in terms of
  pipelining, locality and load-balancing for very large worksets,
  they can offer no improvement whatsoever in terms of modularity, quite the contrary:
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
  and by compilers at compile-time, and runtime barrier crossings
  can do nothing to help about that, only hinder.
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
  to the point of replacing many routine names by common idioms, known sequences of combinators.
  @;{TODO cite}
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
For local or first-class modules, compilers usually generate some kind of “dispatch table”
for each modularly defined entity, that, if it cannot resolve statically,
will exist as such at runtime.
Haskell typeclasses become by “dictionaries” that may or may not be fully inlined.
The case of OO, prototypes are indeed typically represented
by such “virtual dispatch table” at runtime—which in Class OO
would be the type descriptor for each object, carried at runtime for dynamic dispatch.

In a low-level computation with pointers into mutable memory,
the module context is often being accessed through a global variable,
or if not global, passed to functions implementing each module as a special context argument,
and each field or subfield is initialized through side-effects,
often with some static protocol to ensure that they are (hopefully, often all) initialized
before they are used.
In a high-level computation without mutation,
each module is implemented as a function taking the module context as argument,
the fields are implemented functional lenses@;{TODO cite},
and the mutual recursion is achieved using a fixed-point operator@;{TODO cite},
whereas lazy evaluation can be used as a dynamic protocol to ensure that
each field is initialized before it is used.@note{
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
  but at least the symptoms will be visible and you'll have a vague idea
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
  However, we’ll see that open recursion is ubiquitous in OO,
  wherein partial specifications define methods that use other methods that are yet to be defined
  in other partial specifications, that may or may not follow any particular protocol
  for initialization order.
  Thus we see that the simplest, most natural and safest usable setting for OO is:
  lazy evaluation.

  This may come at a surprise to many who mistakenly believe the essence of OO
  is in the domain it is commonly applied to
  (defining data structures and accompanying functions as “classes”),
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
that stage all the complex computations before the actual initialization of objects of the desired class.

A pure functional language without side-effects is the setting for the simplest and
probably clearest model of modularity:
(a) all modular definitions are regular λ-terms that take as first argument
a module context argument @r[m];
(b) the same value will be provided to all definitions at instantiation time;
(c) the context @r[m] makes each defined value accessible through some field or lens
as identified by a name or path of names from the root;
(d) the effective value of @r[m] to be passed simultaneously to every definition
is the fixed-point of the computation wherein the value bound to each field
is the result from computing the given definition with the effective value.

This model is notably used as is in NixOS’s package repository @c{nixpkgs},
as configured with the pure lazy dynamic functional language Nix:
every module definition is a function that conventionally takes an argument @c{pkgs}
that encompasses the entire ecosystem, a namespace hierarchy that includes
not only all packages being defined, but also the standard library of functions @c{pkgs.lib}
(though some like to redundantly pass it as an additional argument @c{lib})
and all kind of intermediary data structures.
As we will soon see, when implementing OO, this modular context is typically called @r[self],
to access (the rest of) the modularly (and extensibly) defined entity.

@subsubsection{The Importance of Laziness}

Note that in Nix, lazy evaluation crucially enables sharing of sub-computations
along a common structure of values; this is especially important when in defining fixed-points,
and thus, when using modular definitions.
By contrast, a pure applicative language without side-effects can only express such fixed-points
as indefinitely recomputed functions, with no sharing and instead with potentially hyper-exponential
recomputation as the definitions contains deeply nested recursive self-references or mutual references.
Therefore it is a practical necessity in an applicative language to use some extension for
lazy evaluation, such as the Scheme primitives @r[delay] and @r[force],
even though small programs without deep nesting and branching in recursion can do without.

Some might object that most OO languages are not functional and especially not lazy;
but that misses the point: most OO languages use Class OO,
where all the OO actually only happens but at compile-time.
What more, classes are defined in an extremely restricted compile-time language,
that has a very simple functional model, that has a somewhat simpler description
in terms of lazy evaluation, yet that doesn’t matter much because the language is so restricted.
And of course, it matters none at all what meta-language is used to implement
whatever compile-time language, or whether it is pure functional or imperative,
unless maybe that meta-language is exposed to the user via reflection.

For evidence of whether lazy evaluation does or doesn’t offer a better model of OO,
in addition to the Prototype OO languages we will discuss,
one has to look at those few Class OO languages that do not have such restrictions in handling prototypes.
At that point, a notable case is C++, that offers a Turing complete language at compile-time,
template metaprogramming;
and at least since C++11, that compile-time language indeed is
a @emph{pure functional, lazy, dynamically typed} Prototype OO language,
with the @c{using} keyword introducing lazy let-bindings,
the @c{constexpr} keyword enabling arithmetics and other primitive computations,
and the entity known as statically typed classes at runtime
being actually dynamically typed prototypes at compile-time.
@; TODO maybe C++14 for some of the semantics??
@; TODO see appendix for examples

@subsubsection{Types for Modularity}

A simple type for a modular definition is @r[M ⟶ E] where
@r[M] is the type of the common module context shared between
all modular definitions in a given program fragment,
and @r[E] is the type of the entity defined by the modular definition at hand.

The big issue with assigning static types to modular definitions is then
that the type @r[M] of the common module context value
is shared between all definitions within that program fragment:
this module context must therefore include all the information from all
the existing modular definitions currently in use,
but also from all the yet-undefined other partial specifications
that can or will ever be combined with the current one.
Yet to keep things modular, no module author should be required to know, much less specify,
all the constraints demanded by each and every other module,
including modules yet to be used, yet to be written,
as part of an assemblage of modules not yet anticipated.
To preserve modularity in this setting without reverting to some kind of dynamic typing as in Nix,
some kind of subtyping with extensible types is therefore required,
such that each module can specify the bindings it requires and those it provides
without the need for global coordination.@note{
  In a language like Haskell, that does not have any mechanism of subtyping or extensible types,
  module programmers can be creative by having modular typeclass constraints on a type parameter,
  then depend on each application programmer making some gigantic non-modular definition
  of the common type that will be fed as parameter to all the modular definitions of his entire application,
  with the potentially thousands of typeclass instances this definition satisfies,
  and suitable initial defaults for each field (hopefully, a lazy bottom computation will do).
  The non-modularity hasn’t been completely eliminated,
  but moved and concentrated onto application developers,
  while library developers can enjoy more modularity.
  Some Haskellers notably do that to modularly define a type for errors,
  lacking an extensible exception type like in OCaml.

  However, if there is more than one modularly-defined entity, especially local ones
  (and in OO, each and every prototype definition is modular),
  then each user of modularity (and not just application programmer) must similarly
  create his non-modular types to feed to each modular computation fixed-point.
  One could invent a way to share a single mother-of-all modular context object for all fixed-point,
  inside of which each individual modular definitions each have an access path;
  but then one might have to make sure that each modular definition has its own set of typeclasses
  so as to avoid clashes.

  While possible, these strategies are quite onerous, and still require module developers
  to follow a lot of extra-linguistic conventions — thereby sharply decreasing modularity
  compared to language-supported extensibility.
}

Note that we haven’t started talking about objects yet.
We find that some form of subtyping for modules
is a necessity for modularity in general, even without OO.
Though indeed, without OO and its in-language extensibility,
you can have a somewhat useful notion of modules with
a quite limited and inexpressive notion of subtyping
compared to what is needed to support classes, and even more so to support prototypes.

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
When done automatically, it's third-class extensibility.
When done manually, it's fourth-class extensibility.
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

@subsubsection{A Criterion for Extensibility}

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
in Java it involves some changes all over though straightforward and preserving the local program structure;
in Lisp requires minimal local changes, and Haskell requires one local change only.
Thus with respect to this and similar kinds of change, if expected,
Haskell is more extensible than Lisp that is more extensible than Java that is more extensible than C.
@;{TODO examples for C, Java, Lisp, Haskell}

@subsubsection{Historical Extensibility Breakthroughs}

While any software is externally extensible by patching the executable binary,
the invention of assembly code made it much easier to extend programs,
especially so with automatic offset calculations from labels.
Compilers and interpreters that process source code meant for human consumption and production,
preprocessors, interactive editors, and all kinds of software tooling,
also much facilitated external extensibility.
And software that requires configuration through many files that must be carefully kept in synch manually
is less extensible than one that can be configured through a single file
from which all required changes are automatically propagated in a coherent manner
(whether that configuration is internal or external, first-class to fourth-class);
if multiple files must be modified together (such as interface and implementation files in many languages),
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
while good for distributing software, are totally infeasible for humans to directly create.
Instead, humans use modular forms of extensibility, such as editing source code
as part of a edit-evaluate-debug development loop, until they reach a new version
of the software they want to release, after which point they use automated tools
to extract from the modularly-achieved new version some non-modular compressed patches.

As for internal extensibility without modularity,
UML, co-Algebras or relational modeling, as previously discussed in @secref{modeling_the_world},
fail to model OO precisely because the “classes” they specify are mere types:
they lack the modularity context that enables self-reference in actual OO classes.
As in-language entities, they can be extended by adding new attributes,
but those types have to be constants.

@subsubsection[#:tag "extensibility_and_complexity"]{Extensibility and Complexity}

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
  of multiple inheritance in C++ or ADA may see it does not bring benefits commensurable with its costs;
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
those that are at the level of abstraction, from the correct point of view,
so that programmers can express precisely the changes they want
with the least effort, and the least semantic discrepancy.
However, in the case of second-class internal extensibility,
or of first-class internal extensibility in a less-than-universal language,
representations are no longer equivalent,
as only a subset of computable extensions are possible.
Thus viewed, extensibility is not uncommon in computing systems,
and is a matter of perceiving the benefits enough to justify the costs of automation,
when undaring programmers would get used to manual external extension at best.

In a pure functional model of extensibility,
where entities of type @r[E] are being specified extensibly,
then new extended entity specified from a previous one of type @r[E] is also of type @r[E],
and the “extension” if first class would be a function of type @r[E ⟶ E]
(though we will later propose more refined types for that).
In terms of λ-calculus, a programmer can take an opaque value of type @r[E],
without having to see inside the term for that value, or being able to access it,
and write an “extension” that takes that previous term as an argument,
and operate on it to return a new term.
When implementing OO, the variable for this previous term is conventionally called @r[super],
after Simula. @; cite @~cite{Simula1967}

In cases that an entity is extended “in place” and the old version no longer used,
as in editing a file, or redefining a class in Smalltalk or Lisp,
an extension is a function that side-effects a mutable reference of type @r[E],
or something equivalent.

@;{TODO examples}

@subsection[#:tag "extensible_modularity"]{Extensible Modularity}
@subsubsection{A Dynamic Duo}

Modularity and Extensibility work hand in hand:
@principle{Modularity means you only need to know
a small amount of old information to make software progress.
Extensibility means you only need to contribute
a small amount of new information to make software progress.}
Together they mean that finite-brained developers can better divide work between each other
and make more software progress with a modular and extensible design,
tackling larger problems while using less mental power each,
compared with using less-modular and less-extensible programming language designs.

@subsubsection{Modular Extensions}

Let us consider implementing modular extensibility in a pure functional setting.
If we modularly define extensions, our terms will take an argument @r[self], the modularity context,
and return an extension, which takes an argument @r[super],
the previous “inherited” (record of) definitions,
and returns some extended (record of) definitions.
A simple type for a modular extension is then @r[M ⟶ E ⟶ E],
wherein terms are typically of the form @linebreak[] @r{(λ (self super) ...extended_super)}.

If we instead define extensions to modular definitions, our terms will take
an argument @r[super], the previous “inherited” modular definition,
of type @r[M ⟶ E], and return an extended modular definition also of type @r[M ⟶ E],
and therefore is of type @r[(M ⟶ E) ⟶ M ⟶ E].
But since the point of modularity is to plug
the same element of @r[M] at the end through a fixed-point,
the construct contains the same useful information as @r[E ⟶ M ⟶ E], or as
@r[M ⟶ E ⟶ E] above, just with extra complexity in the composition.
We will therefore prefer the simpler “modular extension” point of view.

In the general case, the type @r[E] or @r[super] and the return value
will be that of a @emph{method} of a prototype, or sub-entity being incrementally defined,
whereas @r[M] will be some language-wide namespace, registering all
known (and yet unknown) computations, prototypes and library functions in the language ecosystem.
(This is notably the case with @c{nixpkgs}, wherein the role @r[M] is taken
by the argument @r[pkgs], top of the global namespace of packages
and other entities within the ecosystem (including library functions, etc.)).
In the simplest case, @r[E] and @r[M] will both be the same type,
that of a single target being modularly and extensibly specified, typically a record.

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX HERE XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

@subsubsection{Extensible Modularity, Interactively}
@subsection{Internal vs External}
@subsubsection{Internal Modularity and Complexity}
Recombine modules at runtime depending on user input.
Or just at compile-time depending on program configuration.
Reduce complexity.
Compare with FP, which is pure modularity without extensibility:
Well-understood complete problem vs growing ecosystem.

@section{Minimal OO}
@subsection[#:tag "internal_extensible_modularity"]{Internal Extensible Modularity}
@subsubsection{Internalized Feature}
@subsubsection{Embodying Specification}
@subsubsection{Embodying Modularity}
@subsubsection{Embodying Extensibility}
@subsubsection{Prototype Primitives}
@subsection[#:tag "simplest_prototypes"]{Simplest prototypes}
@subsubsection[#:tag "mixin_functions"]{Mixin Functions}
@subsubsection{Elucidating Mixin Instantiation}
@subsubsection{Elucidating Mixin Inheritance}
@subsubsection[#:tag "stricter_types"]{Stricter, More Modular Types}
@subsubsection[#:tag "minimal_design_maximal_outreach"]{Minimal Design, Maximal Outreach}
@subsection{Working with Records}
@subsubsection{Records, Methods, Instances}
@subsubsection[#:tag "encoding_records"]{Encoding Records}
@subsubsection{Mixins and Helpers for Records}
@subsubsection{Example Records built from Mixins}
@subsubsection{Mixin Caveats}

@section{Missing Insights into OO}
@subsection[#:tag "laziness"]{Pure Laziness}
@subsubsection{Lazy makes OO Easy}
@subsubsection{Computations vs Values}
@subsubsection{Method Initialization Order}
@subsubsection{If it’s so good…}
@subsection[#:tag "instances_beyond_records"]{Instances Beyond Records}
@subsubsection{Prototypes for Numeric Functions}
@subsubsection{Conflating Records and Functions}
@subsubsection{Freedom of and from Representation}
@subsubsection{OO without Objects 2}
@subsection[#:tag "objects"]{Objects: The Power of Conflation}
@subsubsection[#:tag "conflating_prototype_and_instance"]{Conflating Prototype and Instance}
@subsubsection[#:tag "keeping_extensibility_modular"]{Keeping Extensibility Modular}
@subsubsection{Conflating More Features}
@subsubsection{Distinction and Conflation}
@section[#:tag "classes"]{Classes}
@subsection{Class OO as Type-Level Prototype OO}
@subsubsection{Type Prototypes}
@subsubsection{Class OO makes classes Second-Class}
@subsubsection{More Popular yet Less Fundamental}
@subsection{Typing Records}

@section[#:tag "inheritance"]{Mixin, Single, and Multiple Inheritance}
@subsection{Mixin Inheritance}
@subsubsection{The Last Shall Be First}
@subsubsection{Mixin Semantics}
@subsection[#:tag "single_inheritance"]{Single inheritance}
@subsubsection{Simple and Efficient}
@subsubsection{Semantics of Single Inheritance}
@subsubsection{Single Inheritance with Second-Class Mixins}
@subsubsection{Lack of expressiveness and modularity}
@subsection[#:tag "multiple_inheritance"]{Multiple inheritance}
@subsubsection{More Sophisticated}
@subsubsection{Prototypes as a DAG of mixins}
@subsubsection{Precedence Lists}
@subsubsection{More Expressive than Mixin Inheritance}
@subsubsection{More Modular than Mixin Inheritance}
@subsubsection[#:tag "single_and_multiple_inheritance_together"]{Single and Multiple Inheritance Together}
@subsubsection{Under-Formalized}

@section{Combining Single and Multiple Inheritance}
@subsection{State of the Art}
@subsubsection{Previous Art}
@subsubsection{Terminology}
@subsection{Common Lisp}
@subsubsection{Common Lisp classes}
@subsubsection{Common Lisp structs}
@subsubsection{Separate Class and Struct Hierarchies}
@subsubsection{User-defined Hierarchies}
@subsection{Ruby}
@subsubsection{Ruby Modules}
@subsubsection{Ruby superclasses}
@subsubsection{Ruby linearization}
@subsection{Scala}
@subsubsection{Scala Traits}
@subsubsection{Scala superclasses}
@subsubsection{Scala linearization}
@subsection{Our C4 Algorithm}
@subsubsection{Best Combining Single and Multiple Inheritance}
@subsubsection{Unifying Classes and Structs}
@subsubsection{Adding a Fifth Constraint}
@subsection{The Suffix Constraint}
@subsubsection{Constraint}
@subsubsection{Why the Suffix Constraint}
@subsubsection{Special Treatment of Suffix}
@subsection{Advantages of C4}
@subsubsection{Struct declarations optional}
@subsubsection{Coherent Naming}
@subsection{Single-Inheritance Yet Not Quite}
@subsubsection{Single-Inheritance among Structs}
@subsubsection{No Single-Inheritance among Structs plus Classes}
@subsubsection{Preserving the Property that Matters}

@section{Advanced Topics in OO}

@subsection{Method Combination}
Idiots have methods conflict. Clever people have methods combine harmonously.
Optics.
@subsection{Multiple Dispatch}
In OO, “late binding” enables modular definition and use of an interface
with objects of unknown future type that satisfy this interface.
To achieve this, Class OO systems customarily use “dynamic dispatch” of methods,
wherein every object (element of a record type defined using a class)
remembers its class (or rather, the target type of the class) in an implicit field,
from which object methods can be recalled when the object's type is not known statically.
Most Class OO languages have special syntax for this dynamic method dispatch on a single object
based on its recorded type.
This special syntax then seems to make “single dispatch” of methods with exactly one object
something special in OO.
The early “message passing” metaphor also seemed to make this “single dispatch” something special.

But from the more general point of view of Prototype OO,
this “single dispatch” is not special at all:
@itemize[
@item{First the notion of “object” as type element doesn’t even exist in Prototype OO.
(as a primitive; you can obviously still construct it on top);
and even when a prototype does describe a single type, elements of that type need not
have a special type field to dynamically dispatch on (though once again it is allowed).}
@item{Second a prototype can describe not just one type, but two or more, or none,
and each of the methods defined by the prototype may take any number of elements
of those types as arguments (including none at all), and in turn
use the types of the arguments to do multiple dispatch, or not.}
@item{Third there are many common cases of functions of two or more arguments
(e.g. comparisons, algebraic operations, composition, recursive constructions),
where the correct and/or efficient behavior varies with arguments of multiple,
and trying to implement this behavior through a series of “single dispatch”
calls to intermediate leads to non-modular code that is hard to maintain,
and further does not support method combination.}]

Thus, many languages support “multiple dispatch” or “multimethods”, @; TODO cite. LOOPS?
a technique wherein the semantics of calling some “generic function”
is specified through methods that depend on the types multiple of its arguments,
and their type hierarchy: Lisp, Clojure, Julia support it natively,
as well as less popular languages; many more popular languages support it using libraries.

@subsection{Static Typing}

@subsection[#:tag "mutation"]{Mutation}
@subsection[#:tag "multiple_dispatch"]{Multiple Dispatch}
Simple methods, Binary methods, Multimethods, Constructors —
Number object inputs being 1, 2, N, 0.
Big big problem in the naive view of class OO.
Not at all a problem with prototypes / typeclasses.
@subsection{Type Monotonicity}
Makes no sense at all as a general constraint when you realize anytime there's recursion of any kind,
your methods won't all be simple methods. Deeply idiotic idea.
@subsection[#:tag "typeclasses"]{Typeclasses}
Essentially equivalent to multiple dispatch.
Would gain by having a notion of inheritance.
@subsubsection[#:tag "global"]{Global Open Recursion}
Orphaned Typeclasses.
Open Mutual Recursion between multiple classes.
How to do it in a pure functional way?
Solution: global namespace hierarchy. You grow not just a language, but its library ecosystem with it.
@subsection[#:tag "optics"]{Optics}
Fields vs Optics for method combination wrapping vs Generalized optics.
@subsubsection{Meta-Object Protocols}
@subsubsection{Runtime Reflection}

@section{Conclusion}
@section{Conclusion: Best of Both Worlds}
@subsection{Findings}
@subsubsection{Restating the Obvious and Not-So-Obvious}
@subsection{Suffix Property}
@subsubsection{C4 Algorithm}
@subsubsection{Implementation}
@subsubsection{Our Scheme}
@subsubsection{Code Size}
@subsubsection{Open Source}
@subsection{Related Work}
@subsection{Parting Words}

@section{Data-Availability Statement} @appendix

@section{BLAH START (RE)WRITING FROM HERE}
@subsection{FOOOOOOOOOOOO}

@subsection{Extensible Specification}
@subsubsection{Records}
@subsubsection{xxx}
@subsubsection{yyy}
@subsection{Conflation}
@subsubsection{Prototypes as Conflation 2}
@subsubsection{Classes as Conflation 2}
@subsubsection{Conflation vs Confusion}
@subsubsection{Specifications}
@subsubsection{Methods}
@section{What OO isn’t}
@subsection{Information Hiding}
@subsection{OO without Conflation}
@subsubsection{OO without Records}
@subsubsection{OO without Messages}
@subsubsection{Inheritance}
@subsection{Single Inheritance}
@subsubsection{Direct superclasses and subclasses}
@subsubsection{Global structure of single inheritance}
@subsubsection{Prefix}
@subsubsection{Suffix}
@subsubsection{Method Resolution}
@subsubsection{Simplicity}
@subsection{Multiple Inheritance}
@subsubsection{Early History}
@subsubsection{Global Structure of Multiple Inheritance}
@subsubsection{Method Resolution in Multiple Inheritance}
@subsubsection{Class linearization}
@subsubsection{Method Combinations}
@subsection{Comparison between single and multiple inheritance}
@subsubsection{Modularity Comparison}
@subsubsection{Expressiveness Comparison}
@subsubsection{Performance Comparison}
@subsection{Mixin Inheritance 2}
@subsubsection{Last but not least}
@subsubsection{Composing Mixins}
@subsubsection{Comparative Expressiveness}
@subsubsection{Comparative Modularity}
@subsubsection{Popularity}
@subsubsection{No Further Comment}
@section{Constraints on Linearization}
@subsection{Consistency Matters}
@subsubsection{Consistency Constraints}
@subsubsection{Matching Methods}
@subsection{Ordering Consistency}
@subsubsection{Linearization}
@subsubsection{Local Ordering}
@subsubsection{Monotonicity}
@subsection{Shape Determinism}
@subsubsection{Only Shape Matters}
@subsubsection{Original Name}
@subsubsection{Rationale}
@subsubsection{Alternatives to Shape Determinism}
@subsection{Constraint-Respecting Algorithms}
@subsubsection{Inconsistent Algorithms}
@subsubsection{First Solution}
@subsubsection{C3}
@subsubsection{Depth-First Traversal}
@subsubsection{Naming}
@subsubsection{Adoption}

@section{Inheritance Examples}
@subsection{Example 1}
@subsection{Example 2}

@(generate-bibliography)
