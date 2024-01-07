#lang scribble/lncs
@; -*- Scribble -*-

@title{The Essence of Object-Orientation: @linebreak[]
           Modularity and Incrementality, @linebreak[]
       or: @linebreak[]
           Lambda, the Ultimate Object Prototype}

@abstract{
We herein argue that the essence of Object-Orientation (OO)
is a mechanism for Incremental Modularity,
and discuss how some features of OO languages
can facilitate or hinder this Incremental Modularity,
from inheritance to mutation to classes themselves.
We do so by building upon our previous Functional Pearl
that reduced Object-Orientation (OO) to a few lines of Functional Programming (FP),
and re-constructed the fundamental notions of OO from first principles.
Our exploration yields answers that sometimes coincide with
prevalent academic discourse or industrial practice,
but sometimes goes against one or both.
Along the way, we offer a semi-formal theory of modularity and incrementality.
}

@authors[
@author[#:inst "1"]{François-René Rideau}
]
@institutes[
  @institute{@emph{Mutual Knowledge Systems, Inc.}@linebreak[]
             @email|{fare@mukn.com}|}
]

@(require scriblib/bibtex
          (only-in scribble/core make-paragraph make-style)
          (only-in scribble/manual racket racketblock code codeblock litchar itemize item)
          (only-in scribble/example examples make-base-eval)
          (only-in scriblib/footnote note)
          (only-in scribble-abbrevs appendix)
          (only-in scribble-math/dollar $)
@;          scribble/minted
          syntax/parse/define
          "util/examples-module.rkt"
          "util/enumitem.rkt"
          (for-label racket))

@(define-simple-macro (c a ...) (elem #:style 'tt a ...))
@(define-simple-macro (Code a ...) (verbatim a ...))
@(define-simple-macro (r a ...) (racket a ...))
@(define-simple-macro (Definitions a ...) (examples/module poof #:no-result a ...))
@(define-simple-macro (Examples a ...) (examples #:eval poof #:no-result a ...))
@(define-simple-macro (Checks a ...) (examples #:eval poof #:label #f a ...))
@(define-simple-macro (λ formals body ...) (lambda formals body ...))
@(define-simple-macro (TODO body ...) '())

@(declare-examples/module poof racket
   (provide (all-defined-out))
   (require syntax/parse/define))
@examples/module[poof #:hidden
   (define (Y f) ;; fixed-point operator
      (define (self x) (f self x))
      self)
]
@(define (principle . x) (bold (emph x)))

@(define-bibtex-cite "poof.bib" ~cite citet generate-bibliography)

@section{The Essence of OO}

@subsection{OO in 2 lines of FP}

Our previous paper @; “Prototypes: Object-Orientation, Functionally”
@~cite{poof2021} showed
how to reduce Object-Orientation (OO) to a few lines of Functional Programming (FP).
Its kernel consists of just two functions involving the @c{Y} combinator:
@Code{fix = λ t m ↦ Y (λ s ↦ m s t)
mix = λ c p ↦ λ s d ↦ c s (p s d)
}

These two definitions, for mixin instantiation and mixin composition respectively,
slightly generalize formulas known in theory for many decades @~cite{bracha1990mixin},
and actually used as the basis of practical implementations for many years @~cite{nix2015}.

Our paper shows how this technique can be used to implement
several complete Object Systems in a few tens of lines of code each
in any language that can express higher-order functions.

@subsection{OO as Incremental Modularity}

In that paper, we briefly mention how
@principle{OO is a mechanism to specify computations in modular increments},
and how modularity justifies using multiple inheritance over mixin inheritance,
or conflating of prototypes and instances (or classes and types) over keeping them separate.

In this present paper, we will elaborate on this relationship
between OO and Incremental Modularity.
Without presenting a complete theory of Modularity (sketched in @citet{ngnghm9})
we introduce some semi-formal criteria for what Modularity and Incrementality mean.
We can then make our previous claims about OO and Modularity more explicit and less informal.

@subsection{Claims}

As original contributions, the present paper does the following:

@TODO{see defsection in poof.scrbl, use that here and everywhere.}

@itemize[
@;#:style enumparenalph
@item{Dispel common misconceptions as to what OO is about (1.4).}
@item{Propose criteria for Modularity (2.1) and Incrementality (2.2)
  in terms of information needed to make software modifications.}
@item{Elucidate how Incrementality and Modularity go together (2.3).}
@item{Map the basic concepts of OO to modularity and incrementality (3.1, 3.2).}
@item{Explain how single, mixin and multiple inheritance relate,
  how the first is less expressive than the others, while the second is less modular (4).}
@item{Clarify the relationship between Prototype OO and Class OO,
  and why Prototypes, being first-class, enable more modularity (5.1).}
@item{Expose the conflation between prototypes and instances (or classes and types)
  at the heart of most OO, and why it contributes to modularity (5.2).}
@item{Discuss how purity and laziness make OO more modular,
  and solve difficult initialization order issues (5.3).}
@; ^ and are actually used in practice in most OO languages—but only at the metalevel.
@item{Generalize OO methods from fixed fields to functional lenses,
  very simply enable modular features like method combinations (6.1).}
@item{Show how the “typeclass” approach can be more composable and thus
  more modular than the “class” approach (6.2).}
@item{Provide a pure functional modular solution to issues with
  multiple dispatch vs single dispatch, friend classes or mutually recursive classes,
  by making library namespace management an explicit part of the language (6.3).}]

Many of the concepts and relationships we tackle have long been part of OO practice and lore,
yet have been largely neglected in scientific literature and formalization attempts.

@subsection{What OO is @emph{not} about}

We make the bold claim that the essence of OO is Incremental Modularity.
Yet, many other slogans or concepts have been claimed to be essential to OO in the past.
We can summarily dismiss those claims as follows:

@subsubsection{Classes}
Many think that classes are essential to OO, as introduced by Simula 67@~cite{Simula1968}
and only ever care to implement, use, formalize, teach or propagandize
class-based OO (a.k.a. Class OO).

The existence since 1976@~cite{Kahn1976 Borning1977 Kahn1979 Borning1979 Borning1981 Rees82t:a adams88oopscheme}
of languages using class-less
prototype-based OO (a.k.a. Prototype OO)@~cite{Lieberman1986 Borning1986 chambers1989efficient Lawall89SelfInScheme},
and the fact that the most used OO language in the world, JavaScript@~cite{TopPL2022},
uses prototypes@~cite{EcmaScript:15}, provide clear counter-evidence to this claim.
The original inventors of OO also later unified classes, procedures and more
into a more general notion of “patterns”@~cite{kristensen1987beta},
which also voids any appeal to their authority in declaring classes essential to OO.

Of course, classes @emph{are} an important concept in OO,
though they are not essential.
The situation is similar that of types and FP:
types are an important concept in FP,
yet they are not essential,
as evidenced by the historical preexistence of the untyped λ-calculus
and the wide use of dynamically typed functional languages like Scheme or Nix.
Actually, we'll demonstrate below in section 5 @;TODO FIX REF
how classes are a indeed special case of prototypes, and
how they precisely relate to types.

@subsubsection{Imperative Programming}

Many people assume that OO requires that
all fields of all objects should be mutable, or be so by default,
and that OO requires mutation deep in its object initialization protocols.
Furthermore, they assume the same eager evaluation model
for function calls and variable definitions as in every common imperative language.
@TODO{CITE? C++ Perl5 Python Java JavaScript Scala Ruby Go (see GitHub)}
Meanwhile, many have of late claimed that purity (the lack of side-effects including mutable state)
is essential to FP and not just its pure functional core, and is thus incompatible with OO.
Some purists also argue that call-by-name or call-by-need is also essential for true FP,
making it even more incompatible with OO.

Now there are many good historical reasons,
having to do with speed and memory limitations at runtime as well as compile-time
for which the first OO languages, as well as most languages until recently,
were using state and side-effects everywhere, and an eager evaluation model, at least by default.
With early 1980s slogans like “objects are a poor man’s closures” and
“closures are a poor man’s objects”@~cite{adams88oopscheme},
the problem back then was clearly not whether OO could be done purely with functions,
but whether it made practical sense to program purely with functions in general.
That question that would only be slowly answered positively, in theory in the early 1990s
and in practice in the mid 2000s to mid 2010s, as Haskell grew up to become a practical language.
@; darcs 2003, cabal 2005, bytestring 2005, "cabal hell" 2006, ghc6 2006, pandoc 2006, xmonad 2007, "Real World Haskell" 2008. Stack 2015 "made non-trivial haskell programs & scripts repeatable"
@; <Fare> there's obviously a lot of subjectivity there—but I expect an S curve such that whichever arbitrary threshhold criteria you choose the answer would be at about the same time.

Yet, the existence of pure models of OO such as those of
Kamin, Reddy, Cook and Bracha@~cite{Kamin1988 ObjectsAsClosures Cook1989 bracha1990mixin} @;TODO CHECK
of pure lazy OO languages such as Jsonnet or Nix@~cite{jsonnet dolstra2008nixos},
and of pure OO systems such as presented in this paper and its predecessors@~cite{poof2021}
@;TODO cite ScalaZ, etc.
@; TODO maybe mention foreshadowing by Oleg Kiselyov ?
and languages happily combining OO and FP such as Scala
provide ample evidence that OO does not at all require mutation,
but can be done in a pure setting, and is very compatible with FP, purity and even with laziness.
We could even argue that Haskell embodies OO@~cite{typeclasses},
though its designers might not wholly embrace the OO tradition. @TODO{CITE}

@subsubsection{Inheritance as opposed to Composition}
Some argue that the essence of OO is to model every possible domain in terms of inheritance,
especially so where it can be preferred compared to alternatives not involving it,
and even more so when such alternative involves FP and composition.
But OO and FP are just distinct concepts neither of which subsumes the other,
that fit distinct sets of situations.
@;Each distinct concept has its set of situations that it fits,
@;distinct from that of any other concept (or else they are actually the same concept);
@;a concept that fits all situations has no content and is useless;
@;and two concepts like OO and FP neither of which subsumes the other,
@;cover sets of situations neither of which is a subset of the other.

It makes no sense to oppose them, especially not when we see that
OO can be expressed in a few lines of FP, whereas
most modern OO languages contain FP as a subset.
@;Even back in the days of those flamewars in the mid 1990s,
@;Yale T Scheme from 1981 had long shown that the two could be unified.
@; TODO see section XXX
As to which whether to use inheritance or not in the modelling of a particular phenomenon,
the entire point of OO being incrementality, the answer crucially depends on
the size, evolution and uncertainty of the phenomenon.
If the phenomenon is small, unchanging and well-understood,
it doesn't really matter which technique you use to model it.
But as it grows, evolves and boggles the mind,
a more modular and incremental approach is more likely to enable adapting the software
to a moving situation, at which point OO and inheritance can help a lot.

@subsubsection{Encapsulation}
Many OO pundits claim that an essential concept in OO
is “encapsulation” also sometimes called “information hiding”,
though there is no consensus as to what this concept means,
and no clear definition. @TODO{CITE}

Inasmuch as some people identify encapsulation as the presence
of specific visibility mechanisms
(with some methods being public, private or something in–between),
we'll easily dismiss the claim that it is an essential aspect of OO
by showing that many quintessential OO languages like Smalltalk or Common Lisp
lack any such specific mechanism,
whereas many non-OO languages possess mechanisms to achieve the same effect,
in the form of modules defining but not exporting identifiers (e.g. declaring them @c{extern} in C),
or simply lexical scoping as present in FP. @TODO{cite W7 Simula? JS?}

On the other hand, inasmuch as this “encapsulation” informally denotes
an aspect of modularity, @; TODO cite
we'll argue that the claim of encapsulation being essential to OO
partakes in our better formalized argument
according to which OO is about modularity (and incrementality).

@section{Modularity and Incrementality}

@subsection{Modularity}

@subsubsection{Division of Labor}

Modularity@~cite{Parnas1972 Dennis1975} is the organization of software source code
in order to support division of labor, dividing it into “modules” that can each be
understood and worked on mostly independently from other modules.

@subsubsection{A Meta-linguistic Feature}

Most modern programming languages offer
@emph{some} builtin notion of modules as “second-class” (meta-level) entities.
A few languages even offer a notion of modules as “first-class” values in the language.
We'll argue that if nothing else, “classes” or “objects”
are such second-class or first-class notions, in languages that support them.

Yet modularity is foremost a @emph{meta-linguistic} concept:
even in a language that by itself provides no support whatsoever for modules
(such as C), programmers will find manual and automated means
to achieve and support modularity outside the language:
@itemize[
@item{They will copy and paste sections of code as poor man’s modules;}
@item{automate organized concatenation of code snippets with preprocessors;}
@item{divide code in files they can “link” or “load” together;}
@item{transclude “include” files in lieu of interfaces;}
@item{orchestrate the build with “make” and rival utilities;}
@item{bundle files into “packages” they exchange and distribute online;}
@item{create “package managers” to handle those bundles.}]

A language from which (better) modularity support may have been excluded
for the sake of “simplicity”, “elegance”, or ease of development or maintenance,
then becomes but the kernel of a haphazard collection of tools cobbled together
to palliate the weakness of this kernel, that ends up being extremely
complex, ugly, and hard to develop and maintain.

@subsubsection{Criterion for Modularity}
@principle{A design is modular if it enables developers to cooperate without having to coordinate},
compared to alternative designs that enable less cooperation or require more coordination,
given some goals for developers, a space of changes they may be expected to enact in the future, etc.

For instance, the object-oriented design of ASDF@~cite{ASDF2}
made it simple to configure, to extend, and
to refactor to use algorithms in @emph{O(n)} rather than @emph{O(n³)} or worse,
all of it without any of the clients having to change their code.
This makes it arguably more modular than its predecessor MK-DEFSYSTEM@~cite{kantrowitz1991}
that shunned use of objects (possibly for portability reasons at the time),
was notably hard to configure, and resisted several attempts to extend or refactor it.

@subsection{Incrementality}
@subsubsection{Small Changes}
Developers quickly lose direction, motivation, support from management
and buy-in from investors and customers when they do not have tangible results
to show for their work.
Incrementality is the ability for a system to
deliver more rewards for fewer efforts, compared to alternatives.
In other words, incrementality supports a short feedback loop in software development.

@subsubsection{A Developer-Interface Feature}
Incrementality should be understood within a framework of what changes
are or aren’t “small” for a human (or AI?) developer, rather than
for a fast and mindless algorithm. Otherwise, the most “incremental” design
would be to have code produced by @c{gunzip} or some similar decompressor,
that can expand a few bits of incremental change into a large amount of code.

Thus, for instance, changing some arithmetic calculations to use
bignums (large variable-size integers) instead of fixnums (builtin fixed-size integers)
in C demands a whole-program rewrite with a different program structure;
in Java involves some changes all over though straightforward and preserving the program structure;
in Lisp or Haskell requires no changes, or minimal and local.
Thus with respect to this and similar kinds of change, if expected,
Java has a more incremental design than C, but less than Lisp or Haskell.

There again, incrementality is usually a meta-linguistic notion, wherein
changes happen as pre-syntactic operations on the source code, rather than
semantic operations within the language itself. And yet, using reflection
and/or considering the entire “live” interactive development environment
as “the system” rather than a “dead” program in a programming language,
these pre-syntactic operations can be internalized.

@subsubsection{A Criterion for Incrementality}
@principle{A design is incremental if it enables developers
to enact change through small local modifications}
compared to alternative designs that require larger (costlier) rewrites
or more global modifications (or prohibit change, same as making its cost infinite).

@subsection{Incremental Modularity}

@subsubsection{A Dynamic Duo}
Modularity and Incrementality work hand in hand:
@principle{Modularity means you only need to know
a small amount of old information to make software progress.
Incrementality means you only need to contribute
a small amount of new information to make software progress.}
Together they mean that a finite-brained developer
can make more software progress with a modular and incremental design
than with a less-modular and less-incremental design.

@subsubsection{Reducing Costs vs Moving them Around}
Beware that many designs have been wrongfully argued as
more modular and/or incremental based on moving code around:
these myopic designs achieve modest development savings in a few modules under focus
by vastly increasing the development costs left out of focus,
in extra boilerplate and friction, in other modules having to adapt,
inter-module glue being made harder, or module namespace curation getting more contentious.

For instance microkernels or microservices may make each "service" look smaller,
but only inasmuch as the overall code has been butchered into parts
between which artificial runtime barriers were added;
yet each barrier added involves extra code, actually increasing the incidental
complexity of the code in direct proportion to the alleged benefits,
without doing anything whatsoever to address its intrinsic complexity.
These misguided designs stem from the inability to think about meta-levels
and distinguish between compile-time and runtime organization of code.

@subsubsection{Incremental Modularity, Interactively}
Incrementality does not necessarily mean that a complex addition or refactoring
can be done in a single small change;
rather, code evolution can be achieved in many small changes, wherein
the system can assist the developer into only having to care
about a small change at a time, while the system tracks down what are all
the small remaining changes necessary.

For instance, a rich static type system can often be used as a tool
to guide large refactorings by dividing them in manageably small changes,
making the typechecker happy one redefinition at a time after a type modification.
This example also illustrates how
@principle{Incrementality and Modularity usually happen through
meta-linguistic mechanisms rather than linguistic mechanisms},
i.e. through tooling outside the language rather than
expressions inside the language.

@section{Prototypes}
@subsection{Internal Incremental Modularity}
@subsubsection{Internalized Feature}
Now what if modular increments of computational specifications could be
embodied as linguistic expressions @emph{within} a programming language,
that could be manipulated at runtime and studied formally,
rather than just as semi-formal meta-linguistic interactions?

Let's dub @emph{prototype} such an embodiment.
And to narrow the discussion down to a formal context,
let's consider programming languages with a functional programming core,
i.e. that contain some variant of the lambda-calculus as a fragment,
either untyped or with suitably expressive types (to be determined later).

@subsubsection{Embodying Modularity}
Each prototype should be able to
contribute information that other modules can use while
using information from other modules it depends on.
In functional terms, it will be or contain a function with
the former among its outputs and the latter among its input.

Now to maximize the expressiveness of this Modularity in a functional setting,
a prototype specifying one aspect of a computation should be able to make
(forward) references to the complete computation being specified itself,
so as to pass it as argument to higher-order functions extracting information
about arbitrary aspects of it.
This means the prototype should be or contain a function
with the computation @c{self} as input,
returning the enriched computation @c{self} as output,
in an @emph{open recursion}; that function then specifies (part of)
a larger specification function of which the complete computation will be
a @emph{fixed-point}.

@subsubsection{Embodying Incrementality}
Each prototype should be able to refer not only to
the complete computation with all available information, but also to
the partial computation with only the information specified @emph{so far}.
Thus, it may examine so-far specified aspects and use them to
contribute small modifications to these existing aspects as well as
new aspects based on this information.

In functional terms, the prototype function will take
an additional input @c{super} based on which to specify a computation.
Thus, to embody incremental modularity, a prototype will be or contain
a prototype function of @c{self} and @c{super} returning an enriched @c{self}.

@TODO{
  Mixins: The simplest of OO models, pure functional prototypes using mixin inheritance,
    and how its (λ (super self) ...) pattern directly maps to Incrementality and Modularity,
    or to Ad Hoc polymorphism and Open Recursion.}

@subsubsection{Prototype Primitives}

Prototypes of course depend on whichever primitive operations support
the type of computation being specified;
but those are not specific to prototypes as such.
The minimal set of prototype-specific primitives follows:
@itemize[
@item{A function that given a prototype specifying a computation
  (and possibly some context) returns a complete computation
  exactly as specified, closing the open recursion;
  this function we call @c{instantiate}, or @c{fix}
  (for reasons that will soon be obvious).
}
@item{A function to compose, chain, juxtapose and/or cross-reference
  multiple smaller prototypes (at least two, maybe more)
  each specifying some aspects of a computation,
  return a larger prototype that contains all these combined aspects,
  yet ready to be further composed, keeping the recursion open;
  this function we call @c{mix}, or @c{inherit}
  (for reasons that will also soon be obvious)}]

@subsection{Simplest prototypes}
@subsubsection{Naked Mixin Functions}
The very simplest possible design for @emph{prototypes}
is thus as “mixin” functions with the following minimal type:
@Code{Mixin self super = self → super → self}
where @c{self} is the type of the computation being modularly specified,
and @c{super} is a supertype of the computation being incrementally specified,
and @c{Mixin} is the name introduced by Cannon@~cite{Cannon82}
and reprised by Cook & Bracha@~cite{bracha1990mixin}.

The mixin instantiation and inheritance primitives are as follows:
@Code{instantiate : top → Mixin self top → self
instantiate = λ base mixin ↦ Y (λ instance ↦ mixin instance base)

inherit : Mixin self super → Mixin super duper → Mixin self duper
inherit = λ child parent ↦ λ instance inherited ↦
              child instance (parent instance inherited)}

@subsubsection{Elucidating Mixin Instantiation}
The @c{instantiate} function above computes a fixed-point @c{instance}
for a @c{mixin} given as extra argument a type-appropriate @c{base} value
that serves as seed of the computation being instantiated:
an empty record @c|{{}}|, a function that always fails @c{⊤ = λ _ ↦ ⊥}, etc.
The type @c{top} of @c{base} is thus a base type for the specified computation:
a supertype of the type @c{self} of the @c{instance} being computed.
In a monomorphic setting, @c{top} is just @c{self} itself;
with a rich-enough type system, it can be a “top” type
for many distinct types of computations, carrying no information.

The @c{Y} combinator is the usual fixed-point combinator, chosen to match
the variant of λ-calculus being used (e.g. using eager or lazy evaluation).
The definition matches the @c{fix} function from the introduction
modulo α-renaming, well-named since its essence is to extract a fixed-point.

@subsubsection{Elucidating Mixin Inheritance}
Mixin inheritance combines two mixins by passing
the complete computation @c{instance} to each of them as @c{self} argument,
and passing @c{(parent instance inherited)} as the @c{super} argument to @c{child}.

By the time the complete @c{instance} and @c{inherited} value so far
are provided (if ever), the combined mixin may itself be but part of
a wider combination, with further mixins both to the right and to the left.
The provided @c{instance} will then be the fixed-point of
the entire wider combination (further children to the left,
@c{child} and @c{parent} here, and further parents to the right).
Meanwhile, the @c{inherited} value will only contain the information from
applying the further parent mixins to the right to the provided base object.
The @c{parent} will be able to extend (enrich or override)
any method definition from the @c{inherited} computation;
the @c{child} may further extend it, and further mixins to the left yet more.

The function matches the @c{mix} function from the introduction
modulo α-renaming, well-named since its essence is to compose or “mix” mixins.
The function is associative, with identity mixin @c{idm = λ s t ↦ t}.@note{
As usual, a change of representation from @c{p} to @c{cp = inherit p}
would enable use regular function composition for @c{mix},
whereas @c{fix} would retrieve @c{p} as @c{cp idm}.}

@subsubsection{Minimal Design, Maximal Outreach}
We have just derived from first principles a minimal design
for embodying modular increments of software specification
inside a functional programming language.
And the two functions we obtained closely reproduce and generalize
(a) the earliest theoretical models of OO@~cite{bracha1990mixin};
(b) the formal semantics of the prototype object language Jsonnet@~cite{jsonnet}
(though not its implementation), itself a conceptual cleanup of GCL@~cite{gclviewer2008}
which has been used to specify all of Google's deployments since about 2004
(but uses dynamic rather than lexical scoping);
(c) the actual implementation of “extensions” as heavily used by the
NixOS@~cite{nix2015} software distribution for Linux and macOS.

This is not a mere coincidence:
OO was indeed invented and developed to facilitate
the modular and incremental specification of software,
though there was and still is neither clear nor widespread
conceptual vocabulary to discuss the fact.
As we'll see, prototypes embody the primitive core of OO,
to which other forms of OO can be reduced.
In the end, we can rightfully claim that the essence of OO is
incremental modularity embodied as language entities,
and that prototypes are the most direct form of this embodiment.
@TODO{cite to substantiate}

@subsection{Working with Records}

@subsubsection{Records, Methods, Instances}
Most OO tradition, including the precedents cited above, follows
the historical restriction of only enabling modular and incremental specification of
@emph{“records”} mapping names to values@~cite{Cook1989}.
The names, the values they are bound to, and/or the bindings,
are at times called @emph{“methods”}, “slots”, “fields”, “attributes” or otherwise,
depending on the specific sub-tradition.

The records themselves will be suitably wrapped into a proper computation result @emph{instance}:
a class (in class-based OO, a.k.a. Class OO),
an object (in prototype-based OO, a.k.a. Prototype OO),
a typeclass (in functional languages with typeclasses, though they may deny the OO tradition),
wherein the record will embody the “method dispatch table”,
“attribute set”, “dictionary” or whatchamacallit of the aforementioned entity.

Note that this meaning of the word @emph{instance} itself comes from the Prototype OO tradition,
and does not match what the meaning of the word in the class OO tradition;
in the latter tradition, “instance” instead refers to an element of the class seen as a type,
whereas that type would be the instance in the prototype OO tradition.
For now we will focus on the simplest and most primitive kind of OO, Prototype OO,
in its simplest form where the instances are the records themselves.
We will extend our point of view in sections 4 and later. @TODO{FIX REF}

@subsubsection{Encoding Records}
We will assume that, either with some language primitives,
some “builtin modules” to import from,
or some variant of Church encoding, our Functional Language
is suitably extended with the usual essential data structures:
numbers, booleans, strings, tuples, lists.
Record keys can be of a language-appropriate type with a decidable equality predicate:
integers (sometimes as named constants),
strings, or optionally symbols (interned strings) or
identifiers (source code tracking entities).

The empty record @c{rtop} and a constructor @c{rcons k v r}
that given a key @c{k}, a value @c{v} and a previous record @c{r} returns a new
record that extends @c{r} with a new or overriding binding of @c{k} to @c{v}.
The three simplest encodings of records would then be
as function, as @emph{alist} (association list, list of key-value pairs),
or as (pure) mapping table:
@Code|{ftop = ⊤ = λ _ ↦ ⊥
fcons = λ k v r m ↦ if m == k then v else r m
atop = []
acons = λ k v r ↦ [(k,v), ...r]
mtop = {}
mcons = λ k v r ↦ {k: v, ...r}}|

Functions are the simplest, but overriding and deletion will leak memory and access time;
also they don't support iteration over method bindings
(an introspection operation that is desired in some contexts).
Alists solve these issues but like functions are inefficient with linear-time operations.
Tables can provide logarithmic-time operations, but can be more complex if not provided
as language builtin. (Binding accessor, binding presence test, binding deletion, etc.,
are left as an exercise to the reader.)

In our previous article@~cite{poof2021} we showed how you could start with
a simple of records as function, use OO style to incrementally and modularly specify
a more elaborate mapping table data structure, and thereafter use that data structure
in the definition of more efficient further records.
That's our first case of a “meta-object protocol”@~cite{amop}, one that illustrates
how to @emph{bootstrap} more elaborate variants of OO from simpler variants.

@subsubsection{Mixins and Helpers for Records}
Abstracting over the specific encoding for records,
the primitive way to define a mixin that adds a method to a record being specified is with:
@Code{methodG = λ rcons k f s t ↦ rcons k (f s t) t}
wherein the argument @c{k} is a key naming the method,
@c{f} is a function that takes the instance @c{s} of type @c{self} and
a inherited record @c{t} of type @c{super} and returns a value @c{v}
to which to bind the method in a record that extends the inherited record,
according to the record encoding defined by @c{rcons}.

In practice, OO language implementations provide a fixed builtin encoding for records,
with specialized instantiation function @c{fixR} and method-addition mixin @c{methodR}:
@Code{fixR = fix rtop
methodR = methodG rcons}

For a mixin that binds a method to a constant value @c{v}, you can then use
@Code{methodK k v = methodR k (λ _ _ ↦ v)}
Common helpers could similarly be defined for mixins that bind a method to a value
that only depends on the instance @c{s} of type @c{self}
and not the inherited value @c{t} of type @c{super}, or vice versa.

Further helpers could help define more than one method at once
e.g. by somehow appending record contents rather than consing bindings one at a time.
Furthermore, given macros in the base language,
specialized syntax could help make such definitions concise.

With or without macros, we will assume a syntax @c{a.b}
for calling an appropriate record accessor with record @c{a}
and method name @c{b} suitably encoded as a key.
For simplification purposes, we will hereafter assume method names are strings.

Meanwhile, we will assume the following helpers to handle lists of mixins
without having to awkwardly nest lots of applications of the @c{mix} function,
assuming bracketed and comma-delimited lists, with @c{[head, ...tails]} patterns:
@Code{mix* [] = idm
      mix* [h, ...t] = mix h (mix* t)
      fix* base l = fix base (mix* l)
      fixR* = fix* rtop)}
Giving polymorphic types to these list helpers may require not only subtyping
but also some form of type indexing for those lists.
Doing it without requiring full dependent types is left as an exercise to the reader.

@subsubsection{Example Records built from Mixins}
We can now define the usual point and colored-point example as follows,
where @c{$point} is the @emph{prototype} for the point
(in our simplest prototypes-as-mixin model),
and @c{point} its @emph{instance}:
@Code{$point = mix (methodK "x" 3.0) (methodK "y" 4.0)
      point = fixR $point
      $blue = (methodK "color" "blue")
      coloredPoint = fixR* [$blue, $point]}

Assuming a primitive @c{assert} that checks that a boolean value is true,
and an equality predicate that behaves properly for records,
we can then assert:
@Code|{assert (point == {x: 3.0, y: 4.0})
       assert (coloredPoint == {x: 3.0, y: 4.0, color: "blue"})}|

We can further define and use a radius-defining mixin,
assuming functions @c{sqr} and @c{sqrt} for square and square roots of numbers respectively:
@verbatim|{$radius == methodR "radius" λ s _ ↦ sqrt ((sqr s.x) + (sqr s.y))
pointWithRadius = fixR* [$radius, $point]
assert (pointWithRadius == {x: 3.0, y: 4.0, radius: 5.0})}|

@subsubsection{Mixin Caveats}
Note that in the above examples,
all the mixins commute, and we could have changed the order in which we define those methods
— because they never use inheritance nor overrode any method, and
instead pairwise define disjoint sets of methods.
Such merge of disjoint commuting mixins embodies modularity, but not incrementality:
incrementality can still be achieved in an extralinguistic way by
rebuilding modules in different ways from smaller modules,
but to achieve it intralinguistic, you need a way to operate on existing modules,
and that by definition is not commutative.

As a counterpoint, the mixins below do override or inherit previous method bindings,
and therefore do not commute, and instead yield different results when mixed in different orders:
@Code|{$v1 = methodK "v" 1
       $v2 = methodK "v" 2
       $v10 = methodR "v" λ _ t ↦ t.v * 10
       assert (fixR* [$v1,$v2] == {v: 1})
       assert (fixR* [$v2,$v1] == {v: 2})
       assert (fixR* [$v1,$v10] == {v: 1})
       assert (fixR* [$v10,$v1] == {v: 10})}|

Finally note that trying to instantiate @c{$v10} alone would fail:
it would try to multiply by 10 the inherited value of @c{v},
but the base record @c{rtop} has no such value and this would result in an error.
Even without inheritance, the prototype @c{$radius} above would also fail to instantiate alone,
because it will try to access undefined methods @c{x} and @c{y}.
This illustrates how not every prototype can be successfully instantiated,
which is actually an essential feature of prototypes
(whether implemented as simple mixins or not),
since the entire point of a prototype is to provide a @emph{partial} specification
of a small aspect of an overall computation,
that in general depends on aspects defined by other prototypes.

@section{Mixin, Single and Multiple Inheritance}

@subsection{Flavors of Inheritance}
What we have described above is called @emph{mixin inheritance},
and is arguably the simplest kind of inheritance to formalize@~cite{bracha1990mixin}.
Beside GCL, Jsonnet or Nix, it is notably used in
the OO system used by Racket's GUI library@~cite{Flatt06schemewith}.

Historically however, the first kind of inheritance that appeared was
@emph{single inheritance}, introduced in 1967 by Simula @;@~cite{Simula}, @;TODO FIX ME
and made popular circa 1971 by Smalltalk @;@~cite{Smalltalk} @;TODO FIX ME
and later circa 1995 by Java@~cite{EcmaScript:15}.
While it is less advanced, less expressive and less simple to formalize given FP,
it was simpler to conceive in the world before FP was mature,
and simpler to implement efficiently.
For these reasons, it remained a popular favorite form of OO
until technological progress made these constraints obsolete.

Finally, a third kind of inheritance is @emph{multiple inheritance},
that historically appeared before mixin inheritance was formalized@~cite{Cannon82},
and that is more sophisticated than the two above.
It was popularized by CLOS@~cite{bobrow88clos} and C++ @; TODO cite
and these days is notably used by Scala or Rust. @; where is C# ?
While often unjustly overlooked or summarily dismissed by academic literature
without adequate formalization,@; TODO cite
we will argue that it is actually more expressive and more modular than the above.

@subsection{Single inheritance}

Less expressive than mixin inheritance.
Therefore less modular.
Cases where a class may inherit from more than one super class.
Copy/paste vs share.
More code to repeat.
Repetition is bad factoring when the source code must change for each variant.

Yes mixin inheritance also less modular in that you must remember all the inheritance DAG,
not just the single parent.

@subsection{Multiple inheritance}

More expressive than single inheritance.
More modular than both single- and mixin- inheritance.

@section{BLAH START (RE)WRITING FROM HERE}

@subsection{Instances Beyond Records}

@subsubsection{Callable Objects}

In 1981, Yale T Scheme@~cite{Rees82t:a} included a mature variant of such an object system
as part of a general-purpose programming environment.
A paper was later published describing the object system@~cite{adams88oopscheme}.
Importantly, T lives by the slogan that “closures are a poor man's objects”
and “objects are a poor man's closures”:
CLOS has funcallable-instance, Java has Functional Interfaces, Scala apply method.

@subsubsection{Typing Records}
Now, a type system with suitable indexed types and subtyping
is required to use rich records. With a less-expressive type system,
each use of mixins will be monomorphic;
at the very least, methods will have to be options
to support prototypes that say nothing about them;
dynamic typing may have to be reimplemented on top of static typing
to support more advanced cases;
and users will have to do a lot of wrapping and unwrapping to use mixins,
adding a lot of overhead to the cost of incremental specification.
This may explain why using the above implementation kernel for OO in FP
has so far only been found non-trivial use but in dynamically typed languages.



without expressive-enough subtyping
will also constrain record mixins to be very monomorphic,
and require its users to resort to awkwardly emulate dynamic types
on top of static types to achieve desired results.

This also makes them hard to type without subtypes

@subsubsection{Mixins Beyond Records}
Now nothing mandates records of any kind in the two above functions.
You could use them to incrementally specify computations
of any type, shape or form whatsoever@~cite{poof2021}.
For instance, a triangle wave function from real to real could be specified as follows
by combining three prototypes, wherein the first handles 2-periodicity,
the second handles parity, and the third the shape of the function on interval @c{[0,1]}:
@verbatim{twf = (λ p q r ↦ fix (mix p (mix q r)) λ x ↦ ⊥)
                (λ self super x ↦ if x > 1 then self (x - 2) else super x)
                (λ self super x ↦ if x < 0 then self (- x)  else super x)
                (λ self super x ↦ x)}
@;           |
@; \  /\  /\ | /\  /\  /
@;  \/  \/  \|/  \/  \/
@; ----------|----------
@;           |
@;           |
The prototypes are reusable and can be combined in other ways;
for instance, by keeping the first and third prototypes, but
changing the second prototype to specify an odd rather than even function
(having the @c{then} case be @c{- self (- x)} instead of @c{self (- x)}),
we can change the function from a triangle wave function to a sawtooth wave function.

@;@verbatim{swf = (λ p q r ↦ fix (mix p (mix q r)) λ x ↦ ⊥)
@;                (λ self super x ↦ if x > 1 then self (x - 2) else super x)
@;                (λ self super x ↦ if x < 0 then - self (- x) else super x)
@;                (λ self super x ↦ x)}
@;
@;           |
@;    /   /  | /   /   /
@;   /   /   |/   /   /
@; ----------|----------
@;  /   /   /|   /   /
@; /   /   / |  /   /

However these real functions
are very constraining by their monomorphic type:
every element of incremental specification has to be part of the function.
One could conceivably encode extra information as fragments of the real function
to escape this stricture, but that would be very awkward.@note{
For instance, one could use the image of floating-point @c{NaN}s
or the indefinitedigits of the image of a special magic number as stores of data.}

Records, by contrast, are a better suited target
for general-purpose incremental modular specification, since
they facilitate the indefinite further specification of ever new aspects
each described by one or many methods each with its own type,
usually distinct and independently specialized, modified or overridden.

@section{BLAH RANDOM STUFF STARTS HERE}

Incrementality, when understood in terms of human changes,
well complements Modularity. Because humans can only comprehend
small enough sections of code at a time, changes need to be local,
and modularity gives a framework for what they can be local to:
a module, an assemblage of modules (considered as a module at another level),
a frontwave of changes from one module to the next, etc.
Conversely, Incrementality enriches Modularity with a notion of changes smaller
than rewriting a complete module from scratch—changes that can be
infra-linguistic and monotonic (adding new definitions that use old ones),
or extra-linguistic and non-monotonic (making small modifications to existing definitions).
In most systems, the former is a small subset of the latter, though
some systems might provide reflection mechanisms that close the gap between the two.

     @; whether knowledge of some service's implementation is needed to make a change in clients
     @; based on how much additional information is required to define one program given another

the informal practical notions touted by proponents of OO, and
the formal theoretical concepts used by proponents of FP.
We thus aim at bringing mutual understanding
between people assuming mutually unintelligible paradigms,
who often talk past each other and miss key notions proposed by the other “camp”.

Here are the concepts we try to elucidate:

@itemize[

@item{
  Combination: specializing inheritance with respect to how increments are combined;
    generalizing precedence lists with DAG attribute grammars; metaobject-compatibility.}
@item{
  Commutativity: The special case of Mixin commutativity,
    the advantages and limits of sticking to it in terms of Modularity and Incrementality.
    Manual linearization of meta-state is not modular.}
@item{
  Computations: The duality between Computation and Value, and
    why purity and lazy evaluation are a great fit for OO,
    more so than the conventionally accepted state mutability and eager evaluation,
    and notably make instance initialization more modular vs “factories” and “builders”.}
@item{
  Representation: The freedom of representation for objects that follows the above duality,
    the extra features it enables such as default values or type constraints,
    and the generalization of OO techniques to arbitrary objects, not just records.}
@item{
  Objects: Introducing Objects as such, and how they crucially enables modular extensibility.
    The key concept of Conflation of Prototype and Instance.}
@item{
  Distinction: How both Distinction between Prototype and Instance and Conflation of the two
    are essential to understanding OO, yet have been missed by the theoretical literature so far
    (but not by practical implementations).
    Explicit “Functors” vs their fixed points.}
@item{
  Classes: The relationship between Prototype OO and Class OO, and how the latter is a special
    case of the former—classes being meta-level prototypes for types.}
@item{
  Mutability: The performance optimizations and semantic issues related to mutability in OO.}
@item{
  Monotonicity: Why Subclassing is rarely Subtyping, and other questions of monotonicity,
    (co-, contra- and in-) variance in Functor Mixins and Fixed-Point Operators.}
@item{
  Typeclasses: The relationship between Classes and Typeclasses.
    How typeclasses make object creation less ad hoc and more modular.}
@item{
  Autowrapping: The relationship between Mutable or Immutable objects, linear typing and subtyping.}
@item{
  Optics: The generalization of OO from overriding methods in records
    to overriding arbitrary aspects of arbitrary computations using functional lenses or zippers,
    and how this generalization can accommodate advanced OO practices like method combinations.}
@item{
  Global Open Recursion: A pure functional solution, already widely used in practice, yet neglected
    in the literature, to the problem of “multimethods”, “friend classes” or “orphan typeclasses”,
    and the according implications on designing and growing a language.}
@item{
  Meta-Object Protocols: tying together all the bells and whistles in defining
    bindings, representations, objects, classes, methods, combinations, etc.
    We can adapt and generalize the techniques from AMOP in a pure functional setting.}
@item{
  Runtime Reflection: Controlling Meta-Objects,
    from Synchronous Message-Passing Proxies to Fully Abstract Asynchronous Containers.
    We can only briefly survey this topic, maybe reusing the Collapsing Towers of Interpreters.}
@item{
  Conclusion: OO and FP are best friends.
    My concepts come with constructive implementations in terms of the λ-calculus
    either normal or applicative or both, pure or mutable,
    with or without (sub)types, with or without staging.
    Any λ-capable language can now be equipped with an Object System à la carte
    in a few tens of lines of code,
    the formal semantics of which can be nicely decomposed in a few orthogonal concepts.
    Say “No” to languages with missing or badly designed Object Systems —
    use our principled approach to build your own OO above or underneath them.
}]

As far as our bibliographical search goes, all these concepts have been largely or completely
neglected by previous literature trying to provide formal semantics to OO
(the kind that can be used for logical reasoning about objects):
the underlying knowledge undoubtedly has existed for a long time, at least among implementers,
yet it remained implicit or ad hoc rather than explicit and systematic.
We are grateful to any reviewer, pre- or post- publication,
who can pin-point previous works that did make these concepts explicit, named them,
and explained the relationship between the human factors and the formal model,
and we will issue according addenda to our bibliography.
We also welcome pointers to more informal literature that may have discussed these concepts
without an attempt at formal semantics,
though such works were unlikely to build a bridge between the two paradigms.
In the end, the two paradigms OO and FP are as complementary as sums and products.

Note how Objects themselves appear only half way through the exposition,
while Classes and Mutability appear even further.
These are obviously all essential concepts to fully understand OO, yet
they are not as @emph{primitive} as the concepts introduced before them
in a suitable theory of OO.
Indeed, they are much more elaborate constructions,
the semantics of which can be simple, clear and general
when decomposing it into the preceding concepts, but
hopelessly complex, confusing and ad hoc when failing to.

PS: This is way too much work, and some of those chapters may have
to be split into several chapters (or then again merged).
In the end, this is a treatise on OO, and even with the head start of my work so far,
it would take a month working full time in complete focus for a first draft,
another month full time for a publishable version,
yet another month for reviews and complementary bibliographical work.
Probably all that times three in practice to account for slack and family life.
But I don't presently have any funding or independent wealth allowing me
to work on that topic full time for even one month.
On the other hand, the whole thing done thoroughly over an entire year
might constitute a decent PhD thesis
(though not the one of which I already completed a first draft).
Would it be possible to find funding for such PhD-level work?

PPS: Instead of writing that entire treatise, I could "merely" present a select few concepts.
With levels going from 0 (wholly skip), 1 (gloss over), 2 (summarize), 3 (discuss),
to 4 (be detailed), I could do the following, which is already a lot of unpaid work.
A quick and dirty job is possible in a short time frame, but even then will be hard to self-fund.

@(generate-bibliography)
