#lang scribble/base
@; -*- Scheme -*-
@(require "util/ltuo_lib.rkt")
@(set-chapter-number 3)

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
Object Orientation (“OO”) is a technique that enables the specification of programs
through extensible and modular @emph{partial} specifications,
embodied as entities @emph{within} a programming language
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
but the second one used prototypes @~cite{Winograd1975},
and some provide both @~cite{EcmaScript2015}.
Class OO is the more popular form of OO,
but the most popular OO language, JavaScript,
started with Prototype OO, with Class OO only added on top twenty years later.

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
@~cite{Grigore2016}.


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

@section{Objects}
@epigraph{
  Computer Science is no more about computers than astronomy is about telescopes.
  @|#:-"E. W. Dijkstra"|
}
@subsection{An Ambiguous Word}
In Prototype OO, a prototype, conflation of a specification and its target,
is also called an “object” or at times an “instance”, especially if the target is a record.
Note that laziness is essential in computing the target record or its entries,
since most specifications, being partial, do not specify
a complete computation that terminates without error in finite time,
yet this expected non-termination should not prevent the use
of the conflated entity to extract and extend its specification
(see @secref{RPOO}).

In Class OO, a prototype, conflation of a specification and its target,
is instead called a “class”,
and the target is specifically a type descriptor
rather than an arbitrary record, or than a non-record value.
What, in Class OO, is called an “object” or an “instance” is
an element of a target type as described.
A class being a prototype, its regular prototype fields and methods
are called “class fields” or “class methods”
(or “static” fields and methods, after the keyword used in C++ and Java)—but
be mindful that they only involve the target type, not the specification.
“Object methods” are semantically regular methods that take one implicit argument in front,
the object (i.e. element of the target type).
“Object fields” are regular fields of the object as a record
(see @secref{RCOO}).

Finally, many languages, systems, databases, articles or books call “object” some or all
of the regular runtime values they manipulate@xnote[":"]{
  Alan Kay, in his Turing Award lecture, remarks:
  “By the way, I should mention that, you know, the name,
  the term object predates object-oriented programming.
  Object, in the early 60s, was a general term that was used to describe
  compound data structures, especially if they had pointers in them.”
}
these “objects” may or may not be records, and are in no way part of
an actual OO system extensible with inheritance.
The authors will not usually claim that these objects are part
of an OO framework actual or imagined, but then again sometimes they may@xnote["."]{
  This situation can be muddled by layers of language:
  Consider a language without OO itself implemented in an OO language.
  The word “object” might then be validly denote OO
  from the point of view of the implementer using the OO meta-language,
  yet not from the point of view of the user using the non-OO language.
  Conversely, when an OO language is implemented using a non-OO language,
  calling some values “objects” may validly denote OO for the user
  yet not for the implementer.
}.

@subsection[#:tag "OOwoO"]{OO without Objects}

Furthermore, the fundamental patterns of OO can exist and be usefully leveraged in a language
that lacks any notion of object, merely with the notions of specification and target,
as we will show in @secref{MOO}.
Meanwhile, Yale T Scheme has a class-less object system @~cite{Rees1982T Adams1988oopscheme},
wherein the authors call “object” any language value,
and “instance” the prototypes in their object system.

Therefore @principle{the word “object” is worse than useless when discussing OO in general}.
It is actively misleading.
It should never be used without a qualifier or outside the context of a specific
document, program, system, language, ecosystem or at least variant of OO,
that narrows down the many ambiguities
around its many possible mutually incompatible meanings@xnote["."]{
  It’s a bit as if you had to discuss Linear Algebra without being able to talk about lines,
  or had to discuss Imperative Programming without being able to talk about the Emperor.
  Ridiculous.
  Or perhaps just an artifact of etymology.
}
Meanwhile, the word “class” is also practically useless,
denoting a rather uninteresting special case of a prototype.
Even the word “prototype”, while meaningful, is uncommon to use when discussing OO in general.
If discussing inheritance, one will only speak of “specifications”.
And if discussing instantiation, one will speak of “specification” and “target”.
Prototypes only arise when specifically discussing conflation.
To avoid confusion, I will be careful in this book to only speak of
“specification”, “target”, “prototype”, and (target type) “element”
and to avoid the words “object” or “class” unless necessary, and then
only in narrowly defined contexts@xnote["."]{
  I am, however, under no illusion that my chosen words would remain unambiguous very long
  if my works were to find any success. They would soon be rallying targets
  not just for honest people to use, but also for ignoramus, spammers, cranks, and frauds
  to subvert—and hopefully for pioneers to creatively misuse
  as they make some unforeseen discovery.
}

This is all particularly ironic when the field I am studying is called “Object Orientation”,
in which the most popular variant involves classes.
But fields of knowledge are usually named as soon as the need is felt
to distinguish them from other fields,
long before they are well-understood, and thus based on misunderstandings;
this misnomer is thus par for the course@xnote["."]{
  The wider field of study is similarly misnamed.
  E. W. Dijkstra famously said that Computer Science is not about computers.
  Hal Abelson completed that it is not a science, either.
}

On the other hand, this book is rare in trying to study OO in its most general form.
Most people instead try to @emph{use} OO,
at which point they must soon enough go from the general to the particular:
before a programmer may even write any code, they have to pick
a specific OO language or system in which to write their software.
At that point, the context of the language and its ecosystem
as wide as it may be, is plenty narrow enough to disambiguate the meanings of all those words.
And likely, “object”, and either or both of “prototype” or “class”
will become both well-defined and very relevant.
Suddenly, the programmer becomes able to utter their thought and communicate
with everyone else within the same ecosystem...
and at the same time becomes more likely to misunderstand programmers from
other ecosystems who use the same words with different meanings.
Hence the tribal turn of many online “debates”.


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
  This approach by Simula and its successor Beta @~cite{kristensen1987beta}
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
  Just like Columbus never set foot on the continent of America.
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
I will be more precise, and after @citet{Snyder1986Encapsulation},
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
  that was not directly possible in Flavors (1979) without writing your own method-combination,
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

@subsection{False dichotomy between Inheritance and Delegation}
Many authors have called “delegation” the mechanism used by Prototype OO
@; TODO CITE Self, Castagna Cardelli 1996, …
as distinct from the “inheritance” mechanism of Class OO.
This wrongheaded distinction started with @citet{Hewitt1979Security},
in whose ACT1 language the two concepts were both implemented,
but through separate implementation paths.
The distinction was further popularized by @citet{Lieberman1986},
who contrasts the two joined concepts of prototype-delegation vs class-inheritance.

Yet, identifying inheritance with classes to the exclusion of prototypes
is historically counterfactual:
the words “inheritance” and “prototype” were both simultaneously introduced
by KRL @~cite{Winograd1975 Bobrow1976},
a system with (multiple) inheritance and prototype OO—from before
the word Object Oriented was popular.
Indeed, KRL was instrumental as an inspiration to Smalltalk-76,
the system that made OO popular.

Opposing inheritance and delegation is also logically counterfactual:
@citet{Lieberman1986} itself explains how “inheritance” (i.e. classes)
can be expressed as a special use of “delegation” (i.e. prototypes).
On the other hand, paper also explains you cannot go the other way around
and express prototypes in terms of classes:
prototypes enable dynamic extension of individual “objects” (prototypes) at runtime,
while classes only allow extension at compile-time, and only
for an entire type (“class”) of “objects” (elements of the type).

In the end, the inheritance mechanism is indeed the same, and it is very wrong to
give it two different names depending on whether it is used for prototypes or for classes.
Even Self, that became the most popular language with “delegation” in academia,
uses the word “inheritance” in its papers @~cite{Ungar1987 Chambers1989 parentsSharedParts1991}.
And @citet{Stein1987} argues that delegation and inheritance are the same concept,
and notes that prototypes map to classes, not class instances
(though strictly speaking she gets the mathematical direction of the map wrong).
The real distinction and comparison that should have been made was between the relative
expressiveness of prototypes and classes, especially if considered as second-class entities
and in absence of reflection (or refraint from using it).
But that is the conclusion that none of the authors who wrote on the topic made explicit,
even though it is implicit in both.
And so the authors focus on arguing about different ways to name the same concept in two contexts
while failing to argue on the different contextual concepts that do matter@xnote["."]{
  If irrelevant changes in the context are a valid excuse to give an existing concept a new name
  and get a publication with hundreds of citations based on such a great original discovery,
  I here dub “ainheritance” the concept of “inheritance”
  when the name of the entity inheriting from others starts with “a”,
  “binheritance” the concept of “inheritance” when the programmer’s console is blue,
  “cinheritance” the concept of “inheritance” when the programmer is in China,
  and “sinheritance” the concept of “inheritance”
  when the specification is not conflated with its target,
  therefore neither a class nor a prototype.
  Also “ninheritance” when there is no actual inheritance, and
  “tinheritance” when it looks like inheritance, but is not real inheritance,
  just target extension without open recursion through a module context.
  I am also reserving the namespace for variants of the name starting
  with a heretofore unused letter, unicode character, or prefix of any kind,
  and launching the Interplanetary Xinheritance Foundation to auction the namespace away,
  as well as the related Intergalactic Zelegation Alliance.
  I am impatiently awaiting my Turing Award, or at least Dahl Nygaard prize,
  for all these never discussed before original inventions related to OO.

  The Lieberman paper deserves its thousands of citations because it is a great paper.
  However, a lot of citers seem to fixate only on the unfortunate choice
  of concept delineation and naming by Lieberman,
  who probably did not anticipate that he would set a bad trend with it.
  The delineation made sense in the historical context of the Actor team
  separately implementing prototypes and classes with related yet distinct mechanisms
  in their ACT1 language @~cite{Hewitt1979Security}, way before they or anyone understood
  how classes were a special case of prototypes.
  But too many readers took this historical artifact as an essential distinction,
  and thereafter focused on studying or tweaking low-level “message passing” mechanisms
  on a wild goose chase for tricks and features,
  instead of looking at the big picture of the semantics of inheritance,
  what it actually is or should be and why,
  what is or isn’t relevant to its semantics.
  Concept delineation and naming is tremendously important;
  it can bring clarity, or it can mislead hundreds of researchers into a dead end.
}

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
One may then have been tempted then to see Prototype Delegation as intrinsically stateful,
and class inheritance as intrinsically pure (though at compile-time).

Yet, recent pure functional Prototype OO systems @~cite{jsonnet nix2015 poof2021}
prove constructively that prototypes can be pure, and that they use
the very same inheritance mechanisms as classes,
indeed with classes as a particular case of prototypes with the usual construction.
Meanwhile, old reflective Class OO systems like Lisp and Smalltalk
@~cite{Kahn1976 Kay1993EHoS Gabriel1991CLOS AMOP}
also support mutable state to modify the inheritance structure at runtime,
for the sake of dynamic redefinition of classes at runtime,
in what remains semantically a pure functional model once when the structure is set.
See how in CLOS you can define methods on generic function
@c{update-instance-for-redefined-class} to control how data is preserved, dropped or transformed
when a class is redefined. @;{TODO XXX @~cite{}}
Mutable state and mutable inheritance structure in particular are therefore
clearly an independent issue from prototypes vs classes,
though it might not have been obvious at the time.
As I introduce formal models of OO,
I will start with pure functional models (see @secref{MOO}), and
will only discuss the confounding matter of side-effects much later
(see @secref{SOO})@xnote["."]{
  It might be interesting to explain @emph{why} many authors failed so systematically to
  identify delegation and inheritance, when the similarities are frankly obvious,
  and the relationship between classes and prototypes is well-known
  to anyone who implemented classes atop prototypes.
  But lacking direct access to those authors’ brains, my explanations must remain speculative.

  First, pioneers are eager to conceptualize and present their experiments as original
  and not just the same concept in a different context.
  They necessarily have to sell their ideas as historical package deals,
  before the underlying concepts are clearly identified and separated from each other.
  They are too close to the matter to tell which of the features they built would be immortalized
  through the ages as fundamental concepts
  vs just contingent implementation details soon to be forgotten.
  In the brief time that publishing about Prototypes was trendy,
  scientists studying pioneering works may have focused too much
  on the specifics of Actors, Self, or other successful Prototype language du jour,
  and failed to properly conceptualize a general notion of Prototype.
  Unlike the pioneers themselves, they deserve blame for their myopia,
  and so do the followers who cite and repeat their “findings” without criticism.
  However this explanation is not specific to the topic at hand,
  and is valid for every field of knowledge.

  Second, and with more specificity to Prototypes,
  Computer Scientists following the Programming Language (PL) paradigm@~cite{Gabriel2012}
  might have been unable to unify Prototypes and Classes
  when delegation happens at runtime while inheritance happens at compile-time:
  not only does the machinery look very different to users and somewhat different as implementers,
  written in different languages with different formalisms,
  but PL people tend to deeply compartmentalize the two.
  They may have looked at low-level mutable state
  (omnipresent in any practical language until the mid-2000s)
  as essential when happening at runtime,
  when they could clearly conceptualize it away as an implementation detail
  when happening at compile-time.
  Systems paradigm people (including the old Lisp, Smalltalk and Self communities)
  who freely mix or interleave runtime and compile-time in the very same language,
  might have had no trouble unifying the two across evaluation times,
  but they tend not to publish articles about PL semantics,
  and not to be read by most PL semanticians, or
  not understood by those that do read the articles.

  Revisiting these topics several decades after they were in vogue,
  and finding their then-treatment lacking, with errors from the time still uncorrected to this day,
  makes me wonder about what other false ideas I, like most people, assume are true
  in all the other topics I haven’t revisited, whether in Computer Science or not,
  where I just blindly assume the “experts” to be correct due to Gell-Mann amnesia.
}

As for which words to keep, the word “inheritance” was used first for the general concept,
in a language with “prototypes”.
The word “delegation” stems from the Actor message-passing model,
and is both later and less general,
from after the words “inheritance” and “prototypes” were better established,
and is strongly connoted to specific implementations using the message-passing paradigm.
It also fell out of fashion some time in the 1990s,
after JavaScript became a worldwide phenomenon, and (correctly) used the term “inheritance”
rather than delegation (as it isn’t particularly “message passing”, just calling functions).
@~cite{ecmascript1997}


@section{Epistemological Digression}
@epigraph{
  Knowledge is something which you can use.
  Belief is something which uses you.
  @|#:- "Idries Shah"|
}
Many people will inevitably quibble about my definition or characterization of OO.
Though a treatise of epistemology is beyond the scope of this book, @;{TODO cite}
I can briefly answer the most frequent epistemological questions as follows.

This section is not essential to the formalization of OO in the chapters that follow,
and can be skipped.
I am aware that my answers may shock and turn off some of my readers.
Nevertheless, I believe this section is very relevant to the debate at hand,
and worth publishing as is.

If a philosophical disagreement with this section
will turn you off from reading subsequent technical chapters,
maybe you should skip this section, or only return to it
after you read those more technical chapters.
If so, you should also be careful never to ask about the philosophical opinions of
authors, inventors, colleagues, etc., in your technical field.


@subsection[#:tag "Imdc"]{Is my definition correct?}
@epigraph{
  The truth or falsehood of all of man’s conclusions, inferences, thought and knowledge
  rests on the truth or falsehood of his definitions.
  @|#:- "Ayn Rand"|
}
Yes, my definition is correct:
it accurately identifies what people usually mean by those words,
and distinguishes situations where they apply from situations where they do not,
in the contexts that people care about.
People using my definition will be able to make good decisions,
whereas those using other definitions will make bad decisions where their definitions differ.

@subsection{What does it even mean for a definition to be correct?}
@epigraph{
  “When I use a word,” Humpty Dumpty said, in rather a scornful tone,
  “it means just what I choose it to mean—neither more nor less.”
  @linebreak[]
  “The question is,” said Alice, “whether you can make words mean so many different things.”
  @linebreak[]
  “The question is,” said Humpty Dumpty, “which is to be master—that’s all.”
  @|#:-"Lewis Carroll"|
}
Some people will argue that definitions are “just” arbitrary conventions,
and that there is therefore no rational criterion of correctness,
only arbitrary political power of the strong over the weak,
to determine what the definitions of words are or should be.

But no, such a point of view is worse than wrong—it is outright evil.
The phenomena that effectively affect people,
that they care to name, discuss, think about and act on, are not arbitrary.
Thus the important part of definitions isn’t convention at all:
it is the structure and understanding of these phenomena, rather than the labels used for them.
A correct definition precisely identifies the concepts that are relevant to people’s concerns,
that help them make better decisions that improve their lives,
whereas an incorrect definition misleads them into counterproductive choices.
Specifically overriding your reason with power is an act of war against you,
and generally overriding all reason with power is the very definition of evil.

@subsection{Is there an authority on those words?}
@epigraph{Those who need leaders aren’t qualified to choose them.
  @|#:- "Michael Malice"|
}
No, there is no authority on software vocabulary, person or committee,
that can decree different words for others to use,
or different phenomena for others to care about.
People care about a phenomenon currently identified under the moniker OO,
and even if some “authority” manages to change the name for it,
or to denature the name “OO” not to identify the same phenomenon anymore,
then people will keep caring about what they now call OO under a different name,
rather than care about whatever those who corrupt the name may want them to.

@subsection{Shouldn’t I just use the same definition as Alan Kay?}
@epigraph{OOP to me means only messaging,
local retention and protection and hiding of state-process,
and extreme late-binding of all things. @|#:- "Alan Kay"|
}
No, that isn’t possible, nor would it be appropriate if it were.
Alan Kay coined the expression “Object Oriented Programming” in 1967.
Originalists might say everyone must take it to mean whatever He defined It to mean,
and sometimes cite him as in the epigraph above.

But neither the above @~cite{Kay2003} nor any of Kay’s pronouncement on OO constitutes
a precise definition with an objective criteria,
if a definition at all@xnote["."]{
  My interpretation is that the first part of this definition (until the last comma)
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

  Note that Kay didn’t immediately adopt Simula’s inheritance mechanism in Smalltalk-72
  (it wasn’t called that yet in Simula, either);
  but he did adopt it eventually in Smalltalk-76,
  notably under the push of Larry Tesler
  (who previously used “slot inheritance” on early desktop publishing applications),
  and this adoption is what launched OO as a phenomenon.
  Kay stated adopting single inheritance over multiple inheritance
  was a compromise @~cite{Kay1993EHoS};
  his team later added multiple inheritance to Smalltalk @~cite{Goldstein1980Extending}, but
  it is unclear that Kay had much to do with that addition, that never became standard.
  More broadly, Kay didn’t endorse any specific inheritance mechanism,
  and never focused on that part of the design. To Kay it was only a means to an end,
  which is what Kay called “extreme late binding”: the fact that behavior definition
  happens and takes effect dynamically up to the last moment based on values computed at runtime.
  Inheritance, the practical means behind the late behavior definition that is late bound,
  and the precise form it takes, is secondary to Kay;
  what matters to Kay is the role it plays in enabling dynamic code specialization.
  But inheritance becomes a primary concern to whoever wants to formalize the concepts behind OO,
  and must refine the intuitions of a pioneer into codified knowledge after decades of practice.
  And if other means are found to arguably satisfy Kay’s “extreme late binding”,
  then they’ll have to be given a name that distinguishes them from what is now called OO.
}
And even if he had at some point given a definition,
one still should remain skeptical of what Kay, and other pioneers, said,
if only to recursively apply the same semantic attention to the definition of the words
they used in their definitions.
Now, one should certainly pay close attention to what pioneers say,
but one should pay even closer attention to what they @emph{do}.
The pioneer’s authority lies not in precise words, but in inspiring or insightful ones;
not in well-rounded neatly-conceptualized theories,
but in the discovery of successful new practices that are not yet well understood.
Solid theories arise only after lots of experience, filtering, and reformulation.

@subsection{Shouldn’t I just let others define “OO” however they want?}
@epigraph{The opinion of 10,000 men is of no value
  if none of them know anything about the subject. @|#:- "Marcus Aurelius"|
}
Not at all.
Some people are reluctant to fight over the meaning of words,
and are ready to cave to popular opinion or spurious authorities
when they define and redefine “OO” or any word to have whatever precise or murky meaning.
Instead they propose that I should stick to “inheritance”
when discussing the field characterized by the use of inheritance.

But it is no good to let an ignorant majority “define” the term “Object Orientation”
to mean what little they know of it—for instance, to pick the most popular elements:
Class OO only, always mutable records,
only single inheritance or C++ style flavorless “multiple inheritance”,
only single dispatch, no method combination, etc.
Letting those who don’t know and don’t care define technical words
would be knowledge bowing to ignorance;
it would be for those who know and care to abdicate their responsibility
and follow the masses when they should instead lead them;
it would be ceding terrain to the Enemy—snake oil salesmen, chaosmongers,
corrupters of language, manipulators, proud spreaders of ignorance, etc.—who if let loose
would endlessly destroy the value of language and make clear meaning incommunicable.
Beside, if you retreat to “inheritance” in the hope that at least for that term
you can get people to agree on a clear unambiguous meaning@xnote[","]{
  The term “inheritance” is already corrupted,
  since Goguen uses it at times to mean refinement @~cite{Goguen1992Sheaf}
  while claiming to do OO,
  and others use it to mean the (non-modular) extension of database tables or equivalent.
  Moreover, the term “inheritance”, that originated in KRL,
  in parallel to the adoption and evolution it saw in the field of OO,
  also had its evolution in the field of
  Knowledge Representation, Description Logics, Semantic Web, etc.
  And there are plenty of further legitimate non-OO uses of the word “inherit”, to
  mean that some entity derives some property from a historical origin, an enclosing context, etc.
}
you’ll find that if you
have any success defining a useful term that way, the agents of entropy will rush
to try to defile it in direct proportion to your success;
you will have given up precious lexical real estate for no gain whatsoever,
only terrible loss@xnote["."]{
  Indeed, if you don’t know to stand your ground, you will constantly retreat,
  and be made to use ever more flowery “politically correct” vocabulary
  as a humiliation ritual before those who will wantonly take your words away
  to abuse you and thereby assert their dominance over you.
}

@subsection{So what phenomena count as OO?}
@epigraph{The medium is the message.
  @|#:- "Marshall McLuhan"|
}
What defines OO is not the metaphors of those who invent, implement, or comment about it
as much as the design patterns used by programmers when they write code in an OO language;
the interactions they have with computers and with each other;
the decision trees that are enabled or disabled when evolving a program into another—these
phenomena are what OO is.
What programmers do, not what programmers say.

And these phenomena are what is captured by
the intra-linguistic extensible modularity as defined above:
(a) the ability to “code against an interface” and
pass any value of any type that satisfies the interface
(modularity, whether following structural or nominative rules),
(b) the ability to extend and specialize existing code by creating a new entity
that “inherits” the properties of existing entities and only needs specify
additions and overrides in their behavior rather than repeat their specifications,
wherein each extension can modularly refer to functionality defined
in other yet-unapplied extensions; and
(c) the fact that these entities and the primitives to define, use and specialize them
exist @emph{within} the programming language rather than in an external preprocessing layer.

I contend that the above is what is usually meant by OO,
that matches the variety of OO languages and systems
without including systems that are decidedly not OO, like Erlang, SML or UML.
Whatever clear or murky correspondence between names and concepts others may use,
this paradigm is what matters, and is what I will call OO—it is what I will discuss in this book,
and will systematically reduce to elementary concepts.


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
  Look back at the previous chapter on what OO is not.
  Explain how the concepts of this chapter justifies the previous judgements.
  Consider sections of the previous chapter in a random order, so that,
  if you stop before the end, you don’t just do the first few sections like everyone else.
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
  If you did exercise @exercise-ref{02to03}, compare your previous answers to mine.
  See what surprised you—what you agreed and disagreed with before you read this chapter,
  and how your understanding has evolved already.
}

@exercise[#:difficulty "Hard, Recommended" #:tag "03to04"]{
  Based on this informal overview, and before you read the next chapter,
  try to write down your own short theory of what the main concepts like “modularity”,
  “extensibility” and “internality” might mean, and what formalizing them might look like.
  Bonus if you can then explain how the three together
  can mean something more than the same three apart.
  Save your answer to compare with the treatment in @secref{OOaIEM}.
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
