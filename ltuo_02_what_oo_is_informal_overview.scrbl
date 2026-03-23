#lang scribble/base
@; -*- Scheme -*-
@(require "util/ltuo_lib.rkt")
@(set-chapter-number 2)

@title[#:tag "WOOiIO"]{What Object Orientation @emph{is} — Informal Overview}
@epigraph{Ce qui se conçoit bien s'énonce clairement, @linebreak[]
  Et les mots pour le dire arrivent aisément. @linebreak[]
  @~ @~ (What is clearly conceived is clearly expressed, @linebreak[]
  @~ @~ And the words to say it flow with ease.)
  @|#:- "Nicolas Boileau"|
}
In this chapter, I map out the important concepts of OO,
that I will develop in the rest of this book.
This chapter will contain only what explanation is strictly necessary
to relate concepts to one another and prevent their misinterpretation.
More details, and justifications, will follow in subsequent chapters.

@section{Extensible Modular Specifications}
@epigraph{Say what you mean and mean what you say.
  @|#:- "Lewis Carroll"|
}
@principle{Object Orientation (“OO”) is a technique that enables the specification of programs
through extensible and modular @emph{partial} specifications,
embodied as entities @emph{within} a programming language}
(see @secref{OOaIEM}, @secref{MOO}).

@subsection{Partial specifications}
A program is made of many parts that can be written independently,
enabling division of labor,
as opposed to all logic being expressed in a single monolithic loop@xnote["."]{
  The entire point of partial specifications is that they are not complete,
  and you want to be able to manipulate those incomplete specifications,
  even though, of course, trying to “instantiate” them
  before all the information has been assembled should fail.
  Typesystems and semantic frameworks incapable of dealing with such incomplete information
  are thereby incapable of apprehending OO.
}

@subsection[#:tag "MO"]{Modularity (Overview)}
A programmer can write or modify one part (or “module”)
while knowing very little information about the contents of other parts,
enabling specialization of tasks. Modularity is achieved by having modules
interact with each other through well-defined “interfaces” only,
as opposed to having to understand in detail the much larger contents
of the other modules so as to interact with them.

@subsection[#:tag "EO"]{Extensibility (Overview)}
A programmer can start from the existing specification and only need contribute
as little incremental information as possible when specifying a part
that modifies, extends, specializes or refines other parts,
as opposed to having to know, understand and repeat existing code nearly in full
to make modifications to it.

@subsection{Internality}
Those partial programs and their incremental extensions
are entities @emph{inside} the language,
as opposed to merely files edited, preprocessed or generated @emph{outside} the language itself,
which can be done for any language.

Note that Modularity and Extensibility are properties of the process of writing software,
rather so than of the software itself.
Internality crucially embeds part of this process inside the software.
But the software development process remains larger than the software itself.

@; TODO: examples, secref, etc.
@section[#:tag "P&C"]{Prototypes and Classes}
@epigraph{
  Sir, I admit your gen'ral Rule @linebreak[]
  That every Poet is a Fool: @linebreak[]
  But you yourself may serve to show it, @linebreak[]
  That every Fool is not a Poet.
  @|#:- "Alexander Pope (or Jonathan Swift), “Epigram from the French”"|
}
@subsection{Prototype OO vs Class OO}
These in-language entities are called @emph{prototypes} if first-class
(manipulated at runtime, and specifying values, most usually records),
and @emph{classes} if second-class
(manipulated at compile-time only, and specifying types, most usually record types).
A language offers prototype-based object orientation (“Prototype OO”) if it has prototypes,
and class-based object orientation (“Class OO”) if it only has classes.

The first arguably OO language used classes @~cite{Simula1967},
but the second arguably and first definitely OO language used prototypes @~cite{Bobrow1976};
and some languages provide both @~cite{Hewitt1979 EcmaScript2015}.
Class OO is the more popular form of OO,
but the most popular OO language, JavaScript,
started with just Prototype OO, with Class OO only added on top twenty years later.

@subsection[#:tag "CaPfT"]{Classes as Prototypes for Types}
@principle{A class is a compile-time prototype for a type descriptor}:
a record of a type and accompanying type-specific methods,
or some meta-level representation thereof across stages of evaluation.
@;{ TODO see appendix XXX }

Class OO is therefore a special case of Prototype OO,
which is therefore the more general form of OO @~cite{Lieberman1986 poof2021}.
And indeed within Prototype OO, you can readily express
prototypes for runtime type descriptors for your language,
or prototypes for type descriptors for some other language you are processing as a meta-language.

In this book, I will thus discuss the general case of OO,
and thus will seldom mention classes, that are a special case of prototypes.
Exceptions include when I elucidate the relationship between Class OO and Prototype OO,
or mention existing systems that are somehow restricted in expressiveness
so they only support classes@xnote["."]{
  There would be plenty to discuss about classes and types from the point of view of
  library design and ecosystem growth, patterns that OO enables and antipatterns to avoid,
  tradeoffs between expressiveness and ease of static analysis, etc.
  There are plenty of existing books and papers on Class OO,
  OO software libraries, techniques to type and compile OO, to get inspiration from,
  both positive and negative.
  While some of the extant OO lore indeed only applies to defining classes,
  a great deal of it easily generalizes to any use of inheritance,
  even without classes, even without prototypes at all,
  even when the authors are unaware that any OO exists beyond Class OO.
}
Still my discussion of OO, and my exploration of inheritance in particular,
directly applies just as well to the special case that is Class OO.
See @secref{RCOO}.

@subsection{Classes in Dynamic and Static Languages}
Dynamic languages with reflection, such as Lisp, Smalltalk or JavaScript,
can blur the distinction between runtime and compile-time,
and thus between Prototype OO and Class OO.
Indeed, many languages implement Class OO on top of Prototype OO
exactly that way, with classes being prototypes for type descriptors. @;{ TODO cite JavaScript ?}

Static languages, on the other hand, tend to have very restricted sublanguages at compile-time
with termination guarantees, such that their Class OO is vastly less expressive than Prototype OO,
in exchange for which it is amenable to somewhat easier static analysis.
A notable exception is C++, that has a full-fledged
Pure Functional Lazy Dynamically-Typed Prototype OO language at compile-time: templates.
On the other hand, Java and after it Scala are also Turing-universal at compile-time,
but not in an intentional way that enables practical metaprogramming,
only an unintentional way that defeats guarantees of termination
@~cite{Grigore2017}.


@section[#:tag "MFtPaC"]{More Fundamental than Prototypes and Classes}
@epigraph{Simplicity does not precede complexity, but follows it.
  @|#:- "Alan Perlis"|
}
@subsection[#:tag "SaT"]{Specifications and Targets}
As I reconstruct the semantics of OO from first principles,
I will see that more so than prototype, class, object, or method,
@principle{the fundamental notions of OO are @emph{specification} and @emph{target}}:
@emph{target} computations are being specified by
extensible and modular, partial @emph{specifications} (see @secref{MOO}).

The target can be any kind of computation, returning any type of value.
In Prototype OO, the target most usually computes a record,
which offers simpler opportunities for modularity than other types.
In Class OO, the target is instead a record @emph{type},
or rather a type descriptor, record of a record type and its associated methods,
or of their compile-time or runtime representation.

A (partial or complete) specification is a piece of information that
(partially or completely) describes a computation.
Multiple specifications can be combined into a larger specification.
From a complete specification, a target computation is specified that can be extracted;
but most specifications are incomplete and you can’t extract any meaningful target from them.
A specification is modular inasmuch as it can define certain aspects of the target
while referring to other aspects to be defined in other specifications.
A specification is extensible inasmuch as it can refer to some aspects of the target
as partially defined so far by previous specifications, then amend or extend them.
In Prototype OO, access to this modular context and to the previous value being extended
are usually referred to through variables often named @c{self} and @c{super} respectively.
In Class OO, such variables may also exist, but there is an extra layer of indirection
as they refer to an element of the target type rather than to the target itself.

@subsection[#:tag "PaC"]{Prototypes as Conflation}
A specification is not a prototype, not a class, not a type, not an object—it is not even
a value of the target domain.
It is not a record and does not have methods, fields, attributes or any such thing.

A target value in general is not an object, prototype or class either;
it’s just an arbitrary value of that arbitrary domain that is targeted.
It needs not be any kind of record nor record type;
it needs not have been computed as the fixpoint of a specification;
indeed it is not tied to any specific way to compute it.
It cannot be inherited from or otherwise extended
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
See @secref{CCTHP}.

@subsection{Modularity of Conflation}

In first-class OO without conflation,
developers of reusable libraries are forced to decide in advance and in utter ignorance
which parts of a system they or someone else may want to either extend or query in the future.
Having to choose between specification and target at every potential extension point
leads to an exponential explosion of bad decisions to make:
Choosing target over specification everywhere would of course defeat extensibility;
but choosing specification over target for the sake of extensibility,
especially so without the shared computation cache afforded by conflation,
would lead to an exponential explosion of runtime reevaluations.
By letting programmers defer a decision they lack information to make,
the conflation of specification and target is an essential pragmatic feature of Prototype OO:
it increases intertemporal cooperation between programmers
and their future collaborators (including their future selves)
without the need of communication between the two (that would require time-travel).
Thus, in first-class OO, @principle{Conflation Increases Modularity}.

This modularity advantage, however, is largely lost in
static languages with second-class Class OO@xnote["."]{
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
Yet its major cost remains: the confusion it induces,
not just among novice developers, but also experts, and even language authors.

@subsection{Conflation and Confusion}
@; TODO include historical examples

@principle{Conflation without Distinction is Confusion}.
Those who fail to distinguish between two very different concepts being conflated
will make categorical errors of using one concept or its properties
when the other should be used.
Inconsistent theories will lead to bad choices.
Developers will pursue doomed designs they think are sound,
and eschew actual solutions their theories reject.
@; { TODO examples, citations }
At times, clever papers will offer overly simple solutions that deal with only one entity,
not noticing an actual solution needed to address two.
In rare cases, heroic efforts will lead to overly complicated solutions
that correctly deal with both conflated entities at all times.
Either way, incapable of reasoning correctly about OO programs,
developers will amend bad theories with myriad exceptions and “practical” recipes,
rather than adopt a good theory that no one is offering.
Murky concepts will lead to bad tooling.
Confused developers will write subtle and persistent application bugs.
Researchers will waste years in absurd quests and publish nonsense along the way,
while fertile fields lay unexplored.
See the NNOOTT (@secref{NNOOTT}) regarding decades of confusion
between subtyping and subclassing due to
confusing target (subtyping) and specification (subclassing).
@;{TODO cite Meyer OOSC, see other section}

Still, when you clearly tease the two notions apart,
and are aware of when they are being conflated for practical purposes,
so you can distinguish which of the two aspects should be invoked in which context,
then the semantics of OO becomes quite simple.
Shockingly, conflation was first explicitly discussed only in @citet{poof2021} even though
(a) the concept is implicitly older than OO, going at least as far back as @citet{Hoare1965},
and (b) the implementation of various Prototype OO systems has to explicitly accommodate for it
(see e.g. the @c{__unfix__} attribute in @citet{nix2015})
even when the documentation is silent about it.

@; TODO example Nix specification?

@section{Inheritance Overview}
@epigraph{Discovery consists of seeing what everybody has seen and thinking what nobody has thought.
  @|#:- "Albert Szent-Györgyi"|
}
@subsection{Inheritance as Modular Extension of Specifications}
Inheritance is the mechanism by which partial modular specifications are
incrementally extended into larger modular specifications,
until the point where a complete specification is obtained
from which the specified target computation can be extracted.

There have historically been three main variants of inheritance,
with each object system using a variation on one or two of them:
single inheritance, multiple inheritance and mixin inheritance.
For historical reasons, I may speak of classes, subclasses and superclasses
in this section, especially when discussing previous systems using those terms;
but when discussing the general case of prototypes and specifications,
I will prefer speaking of specifications and their
parents, ancestors, children and descendants—also specifications.

@subsection{Single Inheritance Overview}

Historically, the first inheritance mechanism discovered
was @emph{single inheritance} @~cite{Simula1967},
though it was not known by that name until a decade later.
In an influential paper@~cite{Hoare1965},
Hoare introduced the notions of “class” and “subclass” of records
(as well as, infamously, the @c{null} pointer).
The first implementation of the concept appeared in Simula 67 @~cite{Simula1967}.
Alan Kay later adopted this mechanism for Smalltalk 76 @~cite{Ingalls1978},
as a compromise instead of the more general but then less well understood multiple inheritance
@~cite{Kay1993EHoS}.
Kay took the word “inheritance” from KRL @~cite{Winograd1975 Bobrow1976},
a “Knowledge Representation Language” written Lisp around Minsky’s notion of Frames.
KRL had “inheritance of properties”,
which was what we would now call “multiple inheritance”.
The expressions “single inheritance” and “multiple inheritance”
can be first be found in print in @citet{Stansfield1977COMEX},
another Lisp-based frame system.
Many other languages adopted “inheritance” after Smalltalk,
including Java that made it especially popular circa 1995. @;{TODO @~cite{}. @TODO{or C#}}

In Simula, a class is defined starting from a previous class as a “prefix”.
The effective text of a class (hence its semantics) is then the “concatenation”
of the direct text of all its transitive @emph{prefix classes},
including all the field definitions, method functions and initialization actions,
in order from least specific superclass to most specific@xnote["."]{
  Simula later on also allows each class or procedure
  to define a “suffix” as well as a “prefix”,
  wherein the body of each subclass or subprocedure
  is the “inner” part between this prefix and suffix,
  marked by the @c{inner} keyword as a placeholder.
  Lack of explicit @c{inner} keyword is same as before, as if the keyword was at the end.
  This approach by Simula and its successor Beta @~cite{Kristensen1987Beta}
  (that generalized classes to “patterns” that also covered method definitions the same way;
  except that lack of “inner” means the “do” block cannot be extended anymore,
  like “final” in Java or C++),
  is in sharp contrast with how inheritance is done in almost all other languages,
  that copy Smalltalk.
  The “prefix” makes sense to initialize variables, and to allow procedure definitions
  to be overridden by later more specialized definitions;
  the “suffix” is sufficient to do cleanups and post-processing,
  especially when all communication of information between concatenated code fragments
  happens through side-effects to shared instance variables
  (including, in Algol style, a special variable with the same name as the procedure being defined,
  to store the working return value).
  The entire “inner” setup also makes sense in the context of spaghetti code with GOTOs,
  before Dijkstra made everyone consider them harmful in 1968;
  the reliance on side-effects everywhere also made more sense before Lisp,
  Functional Programming, and eventually concurrency and large distributed systems,
  made people realize side-effects can be more confusing than pure information flow
  through function calls, return values and immutable let bindings.
  But this concatenation semantics is both limited and horribly complex to use
  in the post-1968 context of structured code blocks,
  not to mention post-1970s higher-order functions, etc.
  You could express the modern approach in a roundabout way in Beta,
  by explicitly building a list or nesting of higher-order functions as your only side-effect,
  that re-invert control in a pure way back the way everyone else does it,
  that you call at the end; but that would be an awkward design pattern.
  And so, while Simula was definitely a breakthrough, its particular form of inheritance
  was also a dead-end.
  No one but the Simula inventors wants anything resembling @c{inner}
  for the language they build or use.
  After Smalltalk, languages instead let subclass methods control the context
  for possible call of superclass methods, rather than the other way around).
  Beta behavior is easily expressible with user-defined method combinations
  in CLOS @~cite{Bobrow1988CLOS},
  or can also be retrieved by having methods
  explicitly build an effective method chained the other way around.
  Thus, I can rightfully say that inheritance, and OO,
  were only invented and named through the interaction of
  Bobrow’s Interlisp team and Kay’s Smalltalk team at PARC circa 1976,
  both informed by ideas from Simula, and Minsky’s frames,
  and able to integrate these ideas in their respective
  AI and teachable computing experiments thanks to their dynamic environments,
  considerably more flexible than the static Algol context of Simula.
  In the end, Simula should count as a precursor to OO, or at best an early draft of it—but
  either way, not the real, fully-formed concept.
  Dahl and Nygaard never invented, implemented, used or studied OO as most of us know it:
  not then with Simula, not later with Beta, and never later in their life either.
  Rather it was discovered and identified 9 years later by Bobrow and Winograd.
  Just like Columbus never set foot on the continent of America,
  which was rather discovered and identified 9 years later by Amerigo Vespucci.
  Yet they made the single key contribution thanks to which
  the later greater discovery of OO became not just possible, but necessary.
  They rightfully deserve to be gently mocked for getting so close to a vast continent they sought
  yet failing to ever set foot on it.
  But this only makes me admire them more for having crossed an uncharted ocean
  the vast extent of which no one suspected, beyond horizons past which no one dared venture,
  to find a domain no one else dreamed existed.
}
In modern terms, most authors call the prefix a superclass in general,
or direct superclass when it is syntactically specified as a superclass by the user.
I will be more precise, and after @citet{Snyder1986},
I will speak of a parent for what users explicitly specify,
or, when considering transitively reachable parents of parents, an ancestor;
and in the other direction, I will speak of child and descendant.
The words parent and ancestor, in addition to being less ambiguous,
also do not presume Class OO.
I will call @emph{inheritance hierarchy} of a specification,
the set of its ancestors, ordered by the transitive parent relation.
When using single inheritance, this hierarchy then constitutes a list,
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

@subsection[#:tag "MULIO"]{Multiple Inheritance Overview}

Discovered a few years later, and initially just called @emph{inheritance},
in what, in retrospect, was prototype OO, in KRL @~cite{Winograd1975 Bobrow1976},
Multiple inheritance allows a specification (frame, class, prototype, etc.)
to have multiple direct parents.
The notion of (multiple) inheritance thus predates Smalltalk-76 @~cite{Ingalls1978}
adopting the term, retroactively applying it to Simula.
The terms “single” and “multiple” inheritance were subsequently invented
to distinguish the two approaches as well as recognize their commonality @~cite{Stansfield1977COMEX}.

Although some more early systems
@~cite{Borning1977 Traits Goldstein1980Extending Borning1982Multiple Bobrow1983LOOPS}
used multiple inheritance,
@principle{multiple inheritance only became usable with the epochal system Flavors}
@~cite{Cannon1979 Weinreb1981Chinual3},
refined and improved by successor Lisp object systems
New Flavors@~cite{Moon1986Flavors}, CommonLoops@~cite{Bobrow1986CommonLoops}
and CLOS@~cite{Bobrow1988CLOS CLtL2}.
Since then, many languages including Ruby, Perl, Python and Scala
correctly adopted the basic design of Flavors (though none of its more advanced features)—I
will call them @emph{flavorful}@xnote["."]{
  To be fair, these languages all include the capability for a method to call a super-method,
  that was not @emph{directly} possible in Flavors (1979) without writing your own method-combination,
  but only introduced by CommonLoops (1986) with its run-super function,
  known as call-next-method in CLOS (1988).
}
On the other hand, influential or popular languages including Smalltalk, Self, C++ and Ada
failed to learn from Flavors and got multiple inheritance largely wrong—I
will call them @emph{flavorless}.

With multiple inheritance, the structure of a specification and its ancestors is
a Directed Acyclic Graph (“DAG”).
The set of all classes is also a DAG, the subclass relation is a partial order.
Most OO systems include a common system-wide base class
at the end of their DAG; but it is possible to do without one@xnote["."]{
  Indeed, Flavors allowed you not to include the system-provided @c{vanilla-flavor}
  at the end of its precedence list, so you could write your own replacement, or do without.
}

The proper semantics for a specification inheriting multiple different methods
from a non-linear ancestry proved tricky to get just right.
The older (1976) “flavorless” viewpoint sees it as a conflict between methods you must override.
The newer (1979) “flavorful” viewpoint sees it as a cooperation between methods you can combine.
Unhappily, too many in both academia and industry are stuck in 1976,
and haven’t even heard of the flavorful viewpoint, or, when they have, still don’t understand it.
For this reason, despite its being more expressive and more modular than single inheritance,
flavorful multiple inheritance still isn’t as widely adopted as of 2026@xnote["."]{
  Out of the top 50 most popular languages in the TIOBE index, 2025, @;{TODO cite}
  6 support flavorful multiple inheritance (Python, Perl, Ruby, Lisp, Scala, Solidity),
  3 only support flavorless multiple inheritance (C++, Ada, PHP),
  2 require a non-standard library to support multiple inheritance (JavaScript, Lua),
  17 only support single inheritance (Java, C#, VB, Delphi, R, MATLAB, Rust, COBOL, Kotlin, Swift, SAS, Dart, Julia, TypeScript, ObjC, ABAP, D),
  and the rest don’t support inheritance at all (C, Go, Fortran, SQL, Assembly, Scratch, Prolog, Haskell, FoxPro, GAMC, PL/SQL, V, Bash, PowerShell, ML, Elixir, Awk, X++, LabView, Erlang).
}

@subsection[#:tag "MIXIO"]{Mixin Inheritance Overview}

Mixin inheritance was discovered last @~cite{bracha1990mixin},
probably because it relies on a more abstract pure functional view of OO—maybe
also because it was one of the first successful attempts at elucidating inheritance
in the paradigm of programming language semantics,
when the concept had previously been developed in paradigm of computing systems @~cite{Gabriel2012}.
Yet, for the same reasons,
@principle{Mixin Inheritance is more fundamental than the other two variants}.
It is the simplest kind of inheritance to formalize @emph{given the basis of FP},
in a couple of higher-order functions.
Specifications are simple functions, inheritance is just chaining them, and
extracting their target computation is just computing their fixpoint.
Mixin inheritance also maps directly to the concepts
of Modularity and Extensibility I am discussing,
and for these reasons I will study it first when presenting a formal semantics of OO
(see @secref{MOO}).

Mixins equipped with a binary inheritance operator constitute a monoid
(associative with neutral element),
and the inheritance structure of a specification is just
the flattened list of elementary specifications chained into it,
as simple as single inheritance.
Mixin inheritance works better at runtime, either with Prototype OO,
or, in a dynamic and somewhat reflective system, with Class OO.

Mixin inheritance is in some way simpler than single inheritance
(but only if you understand FP yet are not bound by limitations of most of today’s FP typesystems),
and as expressive as multiple inheritance
(arguably slightly more, though not in a practically meaningful way),
but is less modular than multiple inheritance because it doesn’t automatically handle
transitive dependencies but forces developers to handle them manually,
effectively making those transitive dependencies part of a specification’s interface.

For all these reasons adoption of mixin inheritance remains relatively limited,
to languages like
StrongTalk @~cite{Bracha1993Strongtalk Bak2002Mixins},
Racket @~cite{Mixins1998 Flatt2006Mixins},
Newspeak @~cite{bracha2008},
GCL @~cite{gclviewer2008},
Jsonnet @~cite{jsonnet},
and Nix @~cite{nix2015}. @; TODO: cite gBeta ?
Yet it still has outsized outreach, for just the use of GCL at Google means
a large part of the world computing infrastructure
is built upon configurations written using mixin inheritance@xnote["."]{
  My understanding is that GCL as such only had single inheritance,
  but that users would define their own mixins by abstracting over
  the base class being extended using single inheritance, e.g.
  @c{lambda base: base { a = 1 + super.a; b = c + d;}}
  Semantically, this is just the same trick as used by Racket to implement mixins
  on top of single inheritance.
}
One may also construe the way C++ handles non-“virtual” repeated superclasses
as a form of mixin inheritance with automatic renaming,
at which point mixin inheritance is actually very popular, just not well-understood.


@exercise[#:difficulty "Easy"]{
  Identify one to three concepts from this chapter that you were not familiar with.
  For each, write a one-sentence definition in your own words.
}

@exercise[#:difficulty "Easy"]{
  For each concept above you feel comfortable with, and among the programs you know,
  identify a program that illustrates the use of the concept, if you know any.
  If you don’t know any program that illustrates the concept,
  you might know instead a language that offers a builtin construct for it.
}

@exercise[#:difficulty "Easy"]{
  Make a mindmap of the concepts I presented, and
  establish correspondences with OO languages you know.
  Include at least: modularity, extensibility, internality;
  specification, target, prototype, class, and conflation;
  single, multiple and mixin inheritance.
  Do you now have a clearer understanding of these languages and how their concepts relate?
  Write a couple of sentences describing how your understanding has changed.
}

@exercise[#:difficulty "Medium"]{
  Make a note of cases where you were confused in the past
  by people using the same word for what turned out to be
  very different concepts (though possibly related)
  or by people using different words for what turned out to be the same concept.
  The words “object”, “inheritance”, “delegation” are obvious candidates;
  but there are probably more such words, beyond the field of OO,
  for which you know or suspect this might be the case.
}

@exercise[#:difficulty "Medium"]{
  Identify an OO language in a point of the OO design space you didn’t suspect existed.
  Read the tutorial for that language,
  look at examples exercising idioms not directly expressible in languages you know of.
  Play with it. Use it to extend programs or configurations that you or other people wrote.
}

@exercise[#:difficulty "Medium"]{
  Consider among programs you are familiar with.
  For each, ask:
  Does it have multiple variants or configurations?
  Do different parts need to be modified independently?
  Are there places the code could be extended, specialized or modified in different ways
  to satisfy different goals?
  Would there be a benefit to users if a shared core were maintained
  on top of which users each have their own custom variant?
  See if you can devise a simple criterion (1-2 sentences) for what separates
  programs that would benefit from modular extensibility from those that wouldn’t.
}

@exercise[#:difficulty "Medium"]{
  In the first footnote of the chapter, I claim that
  “typesystems and semantic frameworks incapable of dealing with such incomplete information
  are thereby incapable of apprehending OO.”
  Find a type system you know (Java, Haskell, TypeScript, etc.) and explain:
  Can it represent incomplete specifications? If so, how?
  If not, what would need to be added?
  Describe the transition from incomplete specification to complete.
  Hint: Many typesystems can deal with incomplete specifications at the level of types,
  but not directly of values. You might however a different “partial” record type
  with options on each field as a feature outside the language if not inside it.
}

@exercise[#:difficulty "Medium, Recommended"]{
  If you did exercise @exercise-ref{01to02},
  compare your previous answers to mine.
  See what surprised you—what you agreed and disagreed with before you read this chapter,
  and how your understanding has evolved already.
}

@exercise[#:difficulty "Hard, Recommended" #:tag "02to03"]{
  If you are familiar with propaganda about OO, then,
  in advance of reading the next chapter,
  try to make a list of things many people claim are OO, but that aren’t really,
  at least not what programmers mean when they think of an OO language.
  You can later compare your list to mine after reading @secref{WOOin}.
}

@exercise[#:difficulty "Hard"]{
  Identify another phenomenon named with a word the meaning of which
  has been diluted in the public at large,
  yet a precise definition of which can be given;
  see how people misled by the dilution of meaning may hold wrong beliefs
  and make bad decisions based on the misidentification of concepts.
}

@exercise[#:difficulty "Research"]{
  Find a controversy on the Internet where two sides have a fundamental disagreement—if
  possible in a technical topic with verifiable facts, though possibly not.
  (The exercise is much easier if you’re on neither side, but can be more useful if you are.)
  Talk to people on both sides, to drill down to what words they may be using
  with different meanings or hidden connotations and assumptions.
  Identify fundamental underlying concepts they actually disagree about,
  which may include such hidden assumptions.
  Try to “strongman” the theories on both sides, keeping only their very best arguments
  (though also recording their worst);
  use neutral parties or verifiable facts to assess the strength of arguments.
  Then, use criteria about good theories from chapter 1 to determine whether the better
  theories on either or both sides are failing to satisfy the properties of a good theory.
  After clarifying words and concepts and theories, make your own theory,
  possibly unifying concepts and arguments from the previous two, about the topic.
  Bonus: study the kind of bias that makes each party blind
  to the strength of some arguments from the other side,
  and to the weakness of some arguments from their own side.
}
