@subsubsection{Method Initialization Order}
Traditional imperative OO languages often have a problem
with the order of slot initialization.
They require slots must be initialized in a fixed order,
usually from most specific mixin to least specific, or the other way around.
But subprototypes may disagree on the order of initialization of their common variables.
This leads to awkward initialization protocols that are
(a) inexpressive, forcing developers to make early choices before they have the right information,
and/or (b) verbose, requiring developers to explicitly call super constructors
in repetitive boilerplate, sometimes passing around a lot of arguments, sometimes unable to do so.
Often, slots end up undefined or initialized with nulls, with later side-effects
to fix them up after the fact;
or a separate cumbersome protocol involves “factories” and “builders”
to accumulate all the initialization data and process it before to initialize a prototype.

By contrast, lazy evaluation enables modular initialization of prototype slots:
Slots are bound to lazy formulas to compute their values,
and these formulas may access other slots as well as inherited values.
Each prototype may override some formulas, and
the order of evaluation of slots will be appropriately updated.
Regular inheritance with further prototypes,
is thus the regular way to further specify how to initialize
what slots are not yet fully specified yet.

Pure lazy prototypes offer many advantages over effectful eager object initialization protocols:
@itemize[
@;#:style enumparenalph
@item{The slot initialization order needs not be the same across an entire prototype hierarchy:
      new prototypes can modify or override the order set by previous prototypes.}
@item{When the order doesn’t require modification, no repetitive boilerplate is required
      to follow the previous protocol.}
@item{There are no null values that become ticking bombs at runtime,
      no unbound slots that at least explode immediately but are still inflexible.}
@item{There are no side-effects that complicate reasoning,
      no computation yielding the wrong value because
      it uses a slot before it is fully initialized,
      no hard-to-reproduce race condition in slot initialization.}
@item{At worst, there is a circular definition,
      which can always be detected at runtime if not compile-time,
      and cause an error to be raised immediately and deterministically,
      with useful context information for debugging purposes.}
@item{There is seldom the need for the “builder pattern”,
      and when builders are desired they require less code.}]

@subsubsection{If it’s so good...}
Some may wonder why OO languages don’t use pure lazy functional programming
for OO, if the two are meant for each other.

Well, they do: as we’ll see in @seclink{classes}, class-based OO
is prototype-based OO at the type-level for type descriptors;
and the type-level meta-programming language with which to define and use
those prototypes at compile-time, thus where OO actually takes place,
is invariably pure functional:
languages with static classes have have no provision
for modifying a class after it is defined at compile-time, and
disclaim all guarantees if reflection facilities are used to modify them at runtime.
The compile-time languages in which classes are defined is often quite limited;
but a few languages have a powerful such compile-time language,
famously including C++ and its “templates”.
Templates support lazy evaluation with @c{typedef}, or, since C++11,
with @c{using … = …}. Even when eagerly evaluated,
multiple occurrences of a same type-level template expression
share their computed values, similar to lazy evaluation.

As for prototype OO, while early languages with prototypes, like T or Self,
or later popular ones like JavaScript, were applicative and stateful,
we already discussed in @seclink{minimal_design_maximal_outreach}
how in the last ten years, Jsonnet and Nix have brought out
the happy combination of pure lazy functional programming and prototypes.
We have also been using in production a lazy functional prototype object system
as implemented in a few hundred lines of Gerbil Scheme@~cite{GerbilPOO}.

Thus, we see that contrary to what many may assume from common historical usage,
not only OO does not require the usual imperative programming paradigm
of eager procedures and mutable state —
OO is more easily expressed in a pure lazy functional setting.
Indeed, we could argue that OO @emph{as such} is almost never practiced
in a mutable setting, but rather as a pure functional static metaprogramming
technique to define algorithms that often use mutation (but don’t need to).

Of course, it is also possible to embrace imperative style and stateful side-effects
when either using or implementing OO as such.
For instance, many Lisp and Scheme object systems have allowed dynamic redefinition
of classes or prototypes and their inheritance hierarchy, while
the language Self had mutable @emph{parent slots} to specify prototype inheritance,
and JavaScript objects have a mutable @c{__proto__} slot.
Often, mutation makes for much more complex semantics, with uglier edge cases, or
expensive invalidation when the inheritance hierarchy changes,
but faster execution in the common case thanks to various optimizations.
See @seclink{mutation}.


@subsection[#:tag "instances_beyond_records"]{Instances Beyond Records}
@subsubsection{Prototypes for Numeric Functions}
Looking back at the definitions for @c{Mixin}, @c{fix} and @c{mix},
we see that they specify nothing about records.
Not only can they be used with arbitrary representations of records,
they can also be used with arbitrary instance types beyond records,
thereby allowing the incremental and modular specification of computations
of any type, shape or form whatsoever@~cite{poof2021}.

For instance, a triangle wave function from real to real could be specified
by combining three prototypes, wherein the first handles 2-periodicity,
the second handles parity, and the third the shape of the function on interval @c{[0,1]}:
@verbatim{twf = (λ p q r ↦ fix (mix p (mix q r)) λ x ↦ ⊥)
                (λ self super x ↦ if x > 1 then self (x - 2) else super x)
                (λ self super x ↦ if x < 0 then self (- x) else super x)
                (λ self super x ↦ x)}
@TODO{Insert figure! -- with the graph in black, and explanations of the prototypes in red?}
@;           |
@; \  /\  /\ | /\  /\  /
@;  \/  \/  \|/  \/  \/
@; ----------|----------
@;           |
@;           |
The prototypes are reusable and can be combined in other ways:
for instance, by keeping the first and third prototypes, but
changing the second prototype to specify an odd rather than even function
(having the @c{then} case be @c{- self (- x)} instead of @c{self (- x)}),
we can change the function from a triangle wave function to a sawtooth wave function.

@TODO{Insert figure!}
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

Now these real functions
are very constraining by their monomorphic type:
every element of incremental specification has to be part of the function.
There cannot be a prototype defining some score as MIDI sequence,
another prototype defining sound fonts, and
a third producing sound waves from the previous.
Actually, one could conceivably encode extra information
as fragments of the real function to escape this stricture,
but that would be very awkward:
For instance, one could use the image of floating-point @c{NaN}s
or the indefinite digits of the image of a special magic number as stores of data.
But it’s much simpler to incrementally define a record,
then extract from the record a slot bound to a numeric function—in,
in what can be seen as a use of the “builder pattern”. @TODO{cite}

Records are thus a better suited target
for general-purpose incremental modular specification, since
they allow the indefinite further specification of new aspects,
each involving slots and methods of arbitrary types,
that can be independently specialized, modified or overridden.
Still, the kernel of OO is agnostic with respect to instance types
and can be used with arbitrarily refined types
that may or may not be records, may or may not be functions, and
may or may not generalize or specialize them in interesting ways.

@subsubsection{Conflating Records and Functions}
Many languages solve the above issue by allowing an instance to be simultaneously
both a record and a function. Thus, prototype definitions can use extra record slots
to store ancillary data (such as MIDI sequence and sound font in the example above),
yet simultaneously specify a the behavior of a function.

Thus, back in 1981, Yale T Scheme@~cite{Rees82t:a} was a general-purpose programming environment
with a graphical interface written using a prototype object system@~cite{adams88oopscheme}.
It lived by the dual slogans that “closures are a poor man’s objects”
and “objects are a poor man’s closures”;
its functions could have extra entry points,
which provided the basic mechanism on top of which methods and records were built.

Many later OO languages offer similar functionality,
though they build it on top of OO rather than build OO on top of it:
CLOS has @c{funcallable-instance},
C++ lets you override @c{operator ()},
Java has @emph{Functional Interfaces}, @; since Java 8 (2014)
Scala has @c{apply} methods,
JavaScript has the @c{Function} prototype,
etc.
Interestingly, in Smalltalk,
a “function” is just an object that can reply to the message @c{value:},
and an object can similarly be not just a function, but an array, a dictionary,
or a stand-in for any of the “builtin” primitive data types,
“just” by defining the methods that comprise the interface for each data type.

Such functionality does not change the expressiveness of a language,
since it is equivalent to having records everywhere,
with a specially named method instead of direct function calls.
Yet, it does improve the ergonomics of the language, by reducing the number
of extra-linguistic concepts, distinctions and syntactic changes required
for all kinds of refactorings.
It also opens new ways for programmers to shoot themselves in the foot,
but programmers already have plenty of them, and
these record-function instances don’t make that particularly easier.

Now, to a mathematician,
this may mean that those instances aren’t functions strictly speaking,
but an implicit product of a record and a function, and maybe more things.
The mathematical notion of “function” isn’t
directly represented in the programming language,
only somehow implemented or expressed in it.
Programmers may retort that such is the reality in any programming language anyway,
and some languages are more honest about it than others,
and won’t let a lie stop them from building more ergonomic features.
Mathematicians might insist that sometimes they really want to represent
just a function, with no other hidden capabilities, and more generally,
to maximally restrict what a program can do, so as more feasibly to reason about it.
Programmers may retort that they still can in such a language, if they insist.

Our purpose is not to repeat the debate whether or not
making objects callable is a good or bad idea, @TODO{cite}
even less to take sides in it—but instead
to notice and make explicit this important and useful notion
of implicit product of several things,
whether record, function, or more,
whether resolved syntactically at compile-time (when possible)
or dynamically at runtime (otherwise).
We will call this implicit product a @emph{conflation}.

@subsubsection{Freedom of and from Representation}
We already saw in @seclink{encoding_records} that were many ways to represent records,
that affect performance, memory usage, the ability to introspect values, etc.
There are even more ways to represent them in conflation with functions, arrays, and more.
And a language implementer may find themselves with an embarrassment of choices
for what exact specific underlying data type to use
to represent the instances of their specifications.

However, having neatly separated the core concepts of prototype, inheritance and instantiation,
as tools of incremental specification of software,
from any specific type of instance being specified,
we now find we are not only free to choose the instance type,
but also free @emph{not} to choose:
we can keep the concepts of prototypes, inheritance, etc.,
as abstract entities that can work on any instance type a programmer may want to apply them to,
instead of only supporting a single privileged instance type.
This makes prototypes a more general and more modular notion
that can be used in multiple ways in a same language ecosystem.

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX BLAH XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

@subsubsection[#:tag "keeping_extensibility_modular"]{Keeping Extensibility Modular}
Specifying software with prototypes yet without objects works great,
as long as it’s clear at all times which entities are prototypes and which are instances.
This is simple enough when the specification all happens in a single phase,
and everything is a big prototype with a big fixed point operation around it,
and plenty of explicit fixed point operations within, one for every sub-prototype.
But what if the specification involves multiple phases, where the “same” entity
is sometimes used as an instance, sometimes as a prototype, what more
without it always being used as a prototype before it is used as an instance?
What if some entity, complete and useful in itself, is later extended by another programmer,
overriding parts that the original programmer didn’t anticipate would be overridden?
What if unextended and extended variants of it are used in a same program?

The conflation of prototype and instance into an object
enables future phasing and extensions without the original programmers
having to anticipate how their code will be used and to factor it accordingly.
Programs can be written that can refer to previous or other programs
without having to track and distinguish which parts are instantiated at which point.
No need to decide at every potential extension point whether and when to either
compute a fixed-point or defer its computation, in what leads to a combinatorial explosion
of potential interfaces. No need to defer everything until the last minute,
and make it expensive to use any intermediary value to make a decision before that last minute,
while contaminating the entire computation to turn everything into explicit prototypes.
No need to construct and remember access paths or lenses that you’ll have to use
in two different contexts to access both aspects of the “same” object.

In the end, conflation of prototype and instance allows programmers
to write and refer to objects with less mutual coordination with respect
to when an object is being used or extended.
By the criteria in @seclink{modularity_and_incrementality},
this conflation indeed makes OOP more modular.

@subsubsection{Conflating More Features}
For mixin inheritance, we wanted a prototypes to be just a mixin function.
For multiple inheritance, we wanted a prototype to @emph{also} have a list of direct supers;
and for good measure, we wanted to cache rather than expensively recompute every time
the prototype’s precedence list of transitive supers.
Further features can be added by conflating further aspects into the notion of prototypes.

For instance, we can add a “default values” feature,
by conflating an additional map from slot to value
that is only consulted when no override is provided.
Compile-time type restrictions or runtime assertions on slots,
slot visibility information,
debugging information,
online documentation,
examples and test cases,
generators and minimizers for property-based testing,
introspectable method definitions, etc.,
can be added as in the same way:
as additional conflated aspects of a prototype,
factors in the prototype as (implicit) product,
or equivalently slots in the prototype seen itself as a record instance.

@subsubsection{Distinction and Conflation}
Conflating many aspects of prototypes, instances and together objects
in an implicit product brings better ergonomics and extensibility.
But doing it without having explicit notions of these aspects as distinct and separate entities
leads to a hell of ununderstandably complex semantics
as all the aspects are inextricably weaved together:
@principle{Conflation without Distinction is Confusion}.

Previous presentations of OO,
whether in programming language documentation, teaching materials or academic literature,
have largely or wholly omitted both
the implicit conflation of prototypes and instances
in objects (for Prototype OO) or classes (for Class OO),
and the explicit distinction between the two notions,
with indeed much confusion as both cause and consequence.
And yet by necessity those who implement compilers and interpreters
by necessity abide by this conflation.

By insisting on both conflation and distinguish of the two concepts of instance and prototype,
we aim at dispeling the confusion often reigns in even the most experienced OO practitioners
when trying to reason about the fine behavior of OO programs.

@section[#:tag "classes"]{Classes}

@subsection{Typing Records}
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

Record classes were initially identified with record types
and subclassing with subtyping@~cite{Hoare1965}.
However, the assumption soon proved to be false;
many attempts were made to find designs that made it true or ignored its falsity,
but it was soon enough clear to be an impossible mirage. @; TODO CITE

Without expressive-enough subtyping,
prototypes are still possible, but their types will be very monomorphic.
Users can still use them to store arbitrary data,
by awkwardly emulating dynamic types on top of static types to achieve desired results.

This also makes them hard to type without subtypes.

Type descriptors are themselves often a monomorphic type that does not require subtyping,
at least not unless the type system accommodates dependent types, or at least staging.

@subsubsection{Monotonicity}
Why Subclassing is rarely Subtyping, and other questions of monotonicity,
    (co-, contra- and in-) variance in Functor Mixins and Fixed-Point Operators.

@subsubsection{Autowrapping}
The relationship between Mutable or Immutable objects, linear typing and subtyping.


@subsubsection{Method Combination, Instance Combination}
Specializing inheritance with respect to how increments are combined.
generalizing precedence lists with DAG attribute grammars.
Metaobject-compatibility.

@; TODO: subsubsection about using the notion of defaults hiding complexity behind a simple interface,
@; and enabling, e.g. method combination with a primary method and other methods,
@; with the effective method being more than the plainly named main method.

@subsubsection[#:tag "GOR"]{Global Open Recursion}
A pure functional solution, already widely used in practice, yet neglected
    in the literature, to the problem of “multimethods”, “friend classes” or “orphan typeclasses”,
    and the according implications on designing and growing a language.

Multimethods (multiple dispatch) can enable more modular extension,
but require constraints on the definition and use of methods after the fact.
@; cite Millstein? Allen&al?
In particular, the ability to add methods retroactively
change the shape of the method DAG,
and thus make previous naive manual DAG joins ineffectual;
more careful DAG joins (that explicitly take all directly inherited methods as parameters)
can become tedious and costly to write and run.
Automated DAG joins through reduction to a method monoid via a precedence list
make a lot of sense in this context; no manual joins needed.

@subsubsection{Meta-Object Protocols}
tying together all the bells and whistles in defining
    bindings, representations, objects, classes, methods, combinations, etc.
    We can adapt and generalize the techniques from AMOP in a pure functional setting.

@subsubsection{Runtime Reflection}
Controlling Meta-Objects,
    from Synchronous Message-Passing Proxies to Fully Abstract Asynchronous Containers.
    We can only briefly survey this topic, maybe reusing the Collapsing Towers of Interpreters.

@section{Conclusion}

@subsection{Related Work}

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

@subsection{Parting Words}

OO and FP are best friends.
My concepts come with constructive implementations in terms of the λ-calculus
either normal or applicative or both, pure or mutable,
with or without (sub)types, with or without staging.
Any λ-capable language can now be equipped with an Object System à la carte
in a few tens of lines of code,
the formal semantics of which can be nicely decomposed in a few orthogonal concepts.
Say “No” to languages with missing or badly designed Object Systems —
use our principled approach to build your own OO above or underneath them.

We hope that our explanations will convince some FP practitioners
that OO both has sound meaning and practical value,
despite many of them having only seen heaps of inarticulate nonsense
in much of the OO literature.
OO can be simply expressed on top of FP,
and should be a natural part of the FP ecosystem.

Conversely, we hope that our explanations will convince some OO practitioners
that OO can be given a simple formal meaning using solid general principles
based on which they can safely reason about their programs,
and not just vague informal principles and arbitrary language-specific rules.
FP provides a robust foundation for OO,
and should be a natural part of the OO ecosystem.


XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX BLEH XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

Lack of awareness of this conflation
can cause much confusion among students of OO.
So can, to a lesser extent,
ignorance of the relationship between prototypes and classes,
or of the distinct meanings of “object” in Prototype OO vs Class OO,
or of the pure functional model through which OO as such combines incremental modules vs
the stateful imperative model used within those modules in a lot of traditional OO languages,
or more generally of the essence of OO vs features of popular OO languages
that may not actually be essential to OO.
@note{
  Also confusing is OO often being touted opposite to Functional Programming (“FP”)
  rather than complementary with it.
  While FP emphasizes reducing problems into neatly separable, simpler subproblems
  that can be solved independently and then composed together,
  Object Orientation enables the chipping away of irreducible problems
  that cannot be thus reduced into aspects that while connected
  can be addressed in many successive layers.
  FP is simpler if you can fit those problems in your mind,
  and understand the abstractions required to decompose them.
  OO is easier if you cannot.
  People very bright yet not ambitious enough may never understand the appeal of OO.
  People who lack the smarts may never understand the appeal of FP.
  Those of us who, bright as we may be, aspire to greater software than fits in our brains,
  can appreciate both, and how they nicely complement each other.
}


XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX BLOH XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

@section{Inheritance Examples}
@subsection{Example 1}

@(noindent) @image[#:scale 0.67]{C3_linearization_example.eps}


@subsection{Example 2}

@(noindent) @image[#:scale 0.066]{C3_linearization_example_2.png}


XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX BLUH XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

@item{Discuss how purity and laziness make OO more modular,
  and solve difficult initialization order issues (@seclink{laziness}).}
@; ^ and are actually used in practice in most OO languages—but only at the metalevel.

@item{Clarify the relationship between Prototype OO and Class OO,
  and why Prototypes, being first-class, enable more modularity (@seclink{classes}).}

@item{Generalize OO methods from fixed slots to functional lenses,
  very simply enable modular features like method combinations (@seclink{optics}).}

@item{Show how the “typeclass” approach can be more composable and thus
  more modular than the “class” approach (@seclink{typeclasses}).}

@item{Provide a pure functional modular solution to issues with
  multiple dispatch vs single dispatch, friend classes or mutually recursive classes,
  by making library namespace management an explicit part of the language
  (@seclink{global}).}]

Many of the concepts and relationships we tackle have long been part of OO practice and lore,
yet have been largely neglected in scientific literature and formalization attempts.

@@@


https://x.com/Ngnghm/status/1976680711345299533

And believe, I've tried a lot to write code as OCaml modules before I gave up. I had to play a lot of games with module scaffolding, composing things eagerly as much as possible and concentrating the open recursion and its resolution to a few special records.

sam pocino >> What does the failure mode look like there?
A nice and simple inheritance graph of size N becomes a set of N² modules reimplementing the same things over and over, or you try to chop things up into higher-order bits and reassemble the N² variants manually managing all the dependencies and open recursion.
Those higher-order bits aren't pretty to see, and grow more horrible as the set of methods under mutual open recursion increases in size.
So instead of N prototypes, total size N, in the worst case, you get N² modules, of size also increasing like N², yielding complexity N⁴. Or soon enough you punt on factoring things to share common code, and just redundantly implement N² similar but different enough variants.
Reminds me of that unrelated time I changed ASDF from using bad data structures leading to O(N³) practical evaluation time (and O(N⁴) theoretical worst case) down to simple O(N) time. Build planning time slashed by factor 1M.
Here we're talking development time, though.
The Scheme code for my (merkle) tries, using Prototype OO, is 2.5 times smaller, simpler and less head-achy than the OCaml code using modules, with more functionality and better testing, because of all the module scaffolding I did not have to do (also 2.5x fewer entities).
Single inheritance also gives you N² instead of N, but at least not N³ or N⁴.

@@@@

In the notable case of data records that may be later extended,
which is most relevant to OO,
access to named fields must go through some “associative array” (e.g. hash-table)
mapping identifiers to field index in the record.
These indexes may be cached between modifications, or wholly resolved at compile-time
if the extension is external or second-class.

@@@@

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
one has to look at those few Class OO languages
that do not have such restrictions in handling prototypes.
At that point, a notable case is C++, that offers a Turing complete language at compile-time,
template metaprogramming;
and at least since C++11, that compile-time language indeed is
a @emph{pure functional, lazy, dynamically typed} Prototype OO language,
with the @c{using} or @c{typedef} keywords introducing lazy let-bindings,
the @c{constexpr} keyword enabling arithmetics and other primitive computations,
and the entities known as statically typed classes at runtime
being actually dynamically typed prototypes at compile-time.
@; TODO maybe C++14 for some of the semantics??
@; TODO see appendix for examples

In cases that an entity is extended “in place” and the old version no longer used,
as in editing a file, or redefining a class in Smalltalk or Lisp,
an extension is a function that side-effects a mutable reference of type @r[E],
or something equivalent.

@@@@

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
As we will soon see, when implementing OO, this modular context is typically called @c{self},
to access (the rest of) the modularly (and extensibly) defined entity.

@@@@

"Code reuse".
Hated by detractors to OO.
Yet entire reason for OO, versus external extensibility.
Requires good factoring indeed.
Maintainership burden that is not as thoughtless as duplicating code,
yet ultimately more efficient since it doesn't require duplicating design and fixes.

@@@@

The issue then is that establishing set of indexed types for a closed modular definition
requires knowledge of all modules, whereas, to preserve the principle of modularity,
each open modular definition may only define or reference a small subset of the index,
and its type must accordingly only include the narrow subset of indexed types
as being either defined or referenced by the open modular definition.
An open modular definition shall not be required to know about and mention indexes and types
from other open modular definitions that have not been written or amended yet,
still that will be combined with it in the future.
To support modularity, a static type system thus needs to support
subtyping between sets of indexed types, as well as computation of fixed-points.

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
without the need for global coordination@xnote["."]{
  In a language like Haskell, that does not have any mechanism of subtyping or extensible types,
  module programmers can be creative by having modular typeclass constraints on a type parameter,
  then depend on each application programmer making some gigantic non-modular definition
  of the common type that will be fed as parameter to
  all the modular definitions of his entire application,
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

There is no perfect solution for either issue, there are only tradeoffs,
and each developer must make his choice. Your Mileage May Vary.
Just make sure you agree with anyone else that you will be directly sharing code with.


@; Flavors combination vs C++ conflict https://x.com/Ngnghm/status/1980509375232885161
@; https://cs.pomona.edu/~kim/FOOPL/prelim.pdf

@; Discussion with James Noble and David Barbour: https://x.com/Ngnghm/status/1988891187340615763
@; multiple inheritance


I see a potential for confusion:
- *specifications* need identity (e.g. pointer equality) as DAG nodes when using multiple inheritance or dispatch.
- *targets* and elements of target types as such need no identity, but depending on what precisely you're specifying, may have one.


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

@subsection[#:tag "multiple_dispatch"]{Multiple Dispatch}
Simple methods, Binary methods, Multimethods, Constructors —
Number object inputs being 1, 2, N, 0.
Big big problem in the naive view of class OO.
Not at all a problem with prototypes / typeclasses.

@subsection{Type Monotonicity}
Makes no sense at all as a general constraint when you realize anytime there's recursion of any kind,
your methods won't all be simple methods. Deeply idiotic idea.

@subsubsection[#:tag "global"]{Global Open Recursion}
Orphaned Typeclasses.
Open Mutual Recursion between multiple classes.
How to do it in a pure functional way?
Solution: global namespace hierarchy. You grow not just a language, but its library ecosystem with it.
@subsection[#:tag "optics"]{Optics}
Fields vs Optics for method combination wrapping vs Generalized optics.
@subsubsection{Meta-Object Protocols}
@subsubsection{Runtime Reflection}

@;{TODO: Examine Ruby linearization algorithm, compare it to LOOPS, Scala, etc.
Find concrete examples of where it does the Wrong Thing(tm).
https://norswap.com/ruby-module-linearization/
}

More extension to the multiple inheritance algorithm:
imagine each specification having a sort, with a DAG of sorts,
such that linearization of specs must respect the partial order of sorts,
or, which is stronger, the linearization of specs must respect the linearization of their sort.

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX BLYH XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

Double or "Comb" inheritance in NewtonScript: very similar to our prototypes + meta^n classes,
with single inheritance each.
https://www.newted.org/download/manuals/NewtonScriptProgramLanguage.pdf
https://thenewobjective.com/types-and-programming-languages/comb-inheritance/


@@@

As usual, you can extract the value of the module context from the lens and the modular extension
by using a fixpoint operator.
In a closed focused module extension, the value inherited is of the top type,
and the value provided is the one for the entity being defined:
@Code{
type CFMExt r p = Lens' r p × MExt r top p
}

A monomorphic open focused modular extension
where @c{r i p} are the required, inherited, provided type parameters for a modular extension,
and @c{d} is a type parameter for the entity being defined.
@Code{
type FMExt d r i p = Lens' r d × MExt r i p
}


sub-definition: a is the user-visible object or method, b is what the extension contributes
sub-context: s is the narrow context for the object definition, t is wider context for the ecosystem.

Also, I'd make the footnote number a superscript both in the text and at the beginning of the footnote, I suppose by printing then adding some unicode magic constant (- #⁰ #\0)) to each digit's code.


------>8------>8------>8------>8------>8------>8------>8------>8------>8------

But in Flavors methods do not have to be combined with the usual @c{mix} function.
Instead of being viewed as (modular) extensions that you compose,
they could be (modular) numbers that you add or multiply or get the maximum or minimum of;
they could be lists that you concatenate, or sets that you merge;
Any monoidal operation will do: associative, and with a neutral element:



But, what is more, there could be several kinds of methods cooperating:
“primary” methods that you combine as above, but also
additional routines that you execute in one order (or the opposite)
@c{before} or @c{after} the “primary” methods;
wrapper routines that run @c{around} those methods so they can setup and tear down
some environment, grab locks, transform arguments and results.
CLOS, the polished successor of Flavors and LOOPS,

There is a rich
and, really, anything the programmer may choose to implement

to be  not just to compete
over which will survive or be delete


itself inspired by Teitelman’s ADVISE facility @~cite{Teitelman1966}:


Method Combinations kept evolving in New Flavors @~cite{Moon1986Flavors},
and were adopted by CommonLOOPS @~cite{Bobrow86CommonLoops}, CLOS @~cite{Bobrow88CLOS},
and a whole lot of object systems, mainly in languages of the Lisp and Scheme family.
Sadly, the feature seems not to have been adopted in other ecosystems, except that
a lot of the “advice” part lives in Aspect Oriented Programming @; TODO cite
frameworks, for Java and C#, and less popularly, for other languages.

The most basic method combination is the one used by default by methods
when I have been defining them so far: after the list of available methods
XXXX

Generic functions for declarations. (New Flavors)

Multimethods: (not LOOPS), CommonLOOPS, CLOS. Cecil. Dylan. Fortress. Julia.

Generic functions for declarations. (CommonLOOPS?) CLOS.

The visitor pattern, even after you go through all the pain of it,
doesn't fully capture the expressiveness of multiple dispatch with method combination,
because it finds only one method, and like Self’s ill-fated sender path inheritance,
can’t back out of narrowing decision. Breaks “linearity” (conservation of information).

Purity vs Orphan Typeclasses in Haskell.

@emph{Global} fixpoint.
Other theories of OO, being focused on closed specifications,
cannot only deal with one class at a time, separate from the rest.
But that is never, ever, the right level at which to think software.
In the simple cases where a single class works,
you don’t need a class, you don’t even need OO at all.
In the complex cases, putting all the semantics a single class will boggle the mind,
and trying to put it in multiple classes will cause lots of friction,
and fail to take advantage of the mutual recursion.
With open specifications,
I can literally use functional optics to zoom into the semantics of single method declarations,
and zoom out to the semantics of entire ecosystems of mutually recursive
classes and configuration prototypes, with everything in between.
I decoupled the fixpoints and the extensions, fulfilling the promise of modularity of OO,
that was systematically ignored or destroyed by the programming language researchers
focused on coupling them.


Mutable State in Smalltalk and CL is thin 2nd class encapsulation of 2 1st-class concepts,
the getter and the setter. (though can be seen as 1st class, with reflection)

### Intrinsic Complexity

      from which the emerging meaning is always explainable.
      Certainly, there is a time when the emerging meaning of a large enough assembly
      has a complexity of its own that cannot be reduced,
      but this is @emph{intrinsic} complexity.
      Classes made into humongously complex language primitives
      introduce @emph{extrinsic} complexity, omnipresent unnecessary parasitic interactions—which
      utterly defeats the very purpose for which classes were invented:
      modularity, the ability to clearly think in terms
      of entities that minimally interact with each other.
      If a large utility function or macro must exist to define classes, each use of it
      should clearly reduce to an entity that could have been directly written without the utility,
      though perhaps in a less concise way, but still can be described in terms of simple primitives,
      with no more exhibited affordances than needed.
      Now, at a certain scale, the smaller entities disappear behind the abstraction, and
      those that were large become the building blocks;
      but if they were built with bad primitives, the protruding affordances will generate
      lots of unwanted interactions that break the abstraction,
      causing the semantic plumbing to leak everywhere,
      and offering attack vectors for active enemies to enter between the cracks.


### OO and Types

Typescript
https://www.typescriptlang.org/docs/handbook/utility-types.html

Type-Safe Prototype-Based Component Evolution" (2002)
https://www.cs.cornell.edu/andru/cs711/2002fa/reading/zenger02typesafe.pdf

https://www.cs.cmu.edu/~aldrich/ego/

https://counterexamples.org/subtyping-vs-inheritance.html

Andrew K. Wright & Robert Cartwright
"A practical soft type system for Scheme"
1997

Why do "unary methods" work in class OO, but not e.g. binary methods?
Because you moved construction / destruction out of the way,
so all you’re doing is consuming data,
in a way that (as far as types are concerned) is extensible.
(if considered not trivially returning unit, but effectful with linear resources to forcibly manage).
Also, when an object of same type is linearly returned,
there is one obvious place from which to copy the extended rest of the object;
when multiple objects are returned... that is still a possible interpretation
(and though that’s seldom the useful one, that’s enough for the type theorist).

Meanwhile, the relationship between a module context and a focused value being
modularly and extensibly specified within it is characterized by
a @emph{lens} @~cite{Foster2007CombinatorsFB},
generalizing a path of identifiers to some arbitrary way of accessing a subcomputation.

What makes Typing OO so complex is the confusion of specification and target.
People are trying to give types to an entity that is the fruit of a fixpoint,
and also retroactively undo the fixpoint to somehow type what was before.
Some superbright people manage to juggle the immense complexity of the endeavor
(by actually remembering the operator before fixpoint, of course), and
proudly show their superdupercomplex calculi as if they’ve solved the problem of semantics for OO.
The real solution is to reject complexity, just unbundled specification and target,
and it all becomes the trivial matter of lots simple regular algebraic operations
before a well-known general-purpose fixpoint.


  Once you forsake the NNOOTT, you stop trying to force subclass pegs into subtype holes,
  and all these problems evaporate.
