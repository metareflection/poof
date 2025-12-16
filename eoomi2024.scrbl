#lang scribble/acmart @manuscript @anonymous @review @nonacm
@; -*- Scribble -*-
@title{The Essence of Object-Orientation: Modularity and Incrementality}
@(require scriblib/bibtex
          (only-in scribble/core make-paragraph make-style)
          (only-in scribble/manual racket racketblock code codeblock litchar itemize item)
          (only-in scribble/example examples make-base-eval)
          (only-in scriblib/footnote note)
          (only-in scribble-abbrevs appendix)
          (only-in scribble-math/dollar $)
          syntax/parse/define "util/enumitem.rkt" "util/util.rkt" (for-label racket))
@(define-simple-macro (c a ...) (elem #:style 'tt a ...))
@(define-simple-macro (Code a ...) (verbatim a ...))
@(define-simple-macro (r a ...) (racket a ...))
@(define-simple-macro (TODO body ...) '())
@(define (principle . x) (bold (emph x))) @(define (anonymize x . y) x)
@(define (GerbilScheme) @anonymize["our Scheme implementation"]{Gerbil Scheme})
@(define-bibtex-cite "ltuo.bib" ~cite citet generate-bibliography)
@section[#:tag "foo"]{FOO} @subsection[#:tag "bar"]{BAR} @subsubsection[#:tag "quux"]{QUUX}

@; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX HERE XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

@section{Missing Insights into OO}
Here are some topics that are largely neglected by
both academic literature and public discourse about OO,
even more so than multiple inheritance,
yet that can yield essential insights about it.
Some of these insights may already be known,
but often only implicitly so, and only by a few experts or implementers.

@subsection[#:tag "laziness"]{Pure Laziness}
@subsubsection{Lazy makes OO Easy}
In a lazy functional language such as Nix,
you can use the above definitions
for @c{fix}, @c{mix}, @c{methodG} and @c{methodR} as is and obtain
a reasonably efficient object system;
indeed this is about how “extensions” are defined
in the Nix standard library@~cite{nix2015}.

Now, in an eager functional language such as Scheme,
using these definitions as-is will also yield correct answers,
modulo a slightly different @c{Y} combinator.
However applicative order evaluation may cause an explosion
in redundant recomputations of methods, and sometimes infinite loops.
Moreover, the applicative @c{Y} combinator itself requires
one extra layer of eta-expansion, such that only functions (including thunks)
can be directly used as the type for fixed-points.
Unneeded computations and infinite loops can be averted
by putting computations in thunks, protected by a λ;
but computations needed multiple times will lead to
an exponential duplication of efforts as computations are nested deeper,
because eager evaluation provides no way to share the results
between multiple calls to a same thunk,
especially those from the @c{Y} combinator.
The entire experience is syntactically heavy and semantically awkward.

Happily, Scheme has @c{delay} and @c{force} special forms that allow for
both lazy computation of thunks and sharing of thusly computed values.
Other applicative functional languages usually have similar primitives.
When they don’t, they usually support stateful side-effects
based on which the lazy computation primitives can be implemented.
Indeed, an applicative functional language isn’t very useful
without such extensions, precisely because it is condemned to endlessly
recompute expressions without possibility of sharing results
across branches of evaluation — except by writing everything
in continuation-passing style with some kind of state monad
to store such data, which would involve quite a non-modular
cumbersome global code transformation.

@subsubsection{Computations vs Values}
To reprise the Call-By-Push-Value paradigm@~cite{conf/tlca/Levy99},
prototypes incrementally specify @emph{computations} rather than @emph{values}:
instructions for recursive computing processes that may or may not terminate
(which may involve a suitable monad)
rather than well-founded data that always terminates in time proportional to its size
(that only involve evaluating pure total functions).
Others may say that the fixed-point operation that instantiates prototypes
is coinductive rather than inductive. @TODO{cite Cook?}

And indeed, laziness (call-by-need)
is the best good way to reify a computation as a value,
bridging between the universes of computations and values.
Compared to mere thunking (call-by-name) that can also bridge between these universes,
laziness enables sharing, with advantages both in terms of performance and semantic expressiveness,
without requiring any stateful side-effect to be observable in the language,
thus preserving equational reasoning.
Thunking can still be expressed on top of a lazy language,
but laziness cannot be expressed on top of a language with thunks only,
without using side-effects.

@; Note again that there is no guarantee of convergence of a fixed-point
@; for arbitrary prototypes, and that indeed, inasmuch as
@; most prototypes are meant as incomplete specifications,
@; their fixed-points won’t converge, or not to anything useful.

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

@subsubsection{OO without Objects}
At this point, we may realize we have been explaining and implementing
all key concepts of “Object Orientation” without ever introducing
any notion of object, much less of class.

There are prototypes, and there are instances; but neither is an object.
Prototypes are uninstantiated specifications, often incomplete therefore uninstantiable;
you can’t call methods on them, or do anything that you can expect to do on an object.
Instances are plain values of any type whatsoever, sometimes just simple real functions;
you can’t combine them with inheritance, or do any OO-related operation on them.
If either is an “object”, then the word “object” is utterly empty of meaning.

Indeed, we wrote code exactly in this object-less “OO” style to generate
presentation slides for this work@~cite{poof2021}.
We could express without objects everything that is usually done with objects,
but for one caveat discussed below in @seclink{keeping_extensibility_modular}.

Thus maybe “Object Orientation” was always a misnomer, born from the original confusion
of a time before science identified and clarified the relevant concepts.
Maybe the field should be named after Inheritance, or Prototypes,
or Incremental Modularity, and banish the word “Object” forevermore from its name.

Yet misnamed as OO may be, objects are possible and a useful concept in it.

@subsection[#:tag "objects"]{Objects: The Power of Conflation}
@subsubsection[#:tag "conflating_prototype_and_instance"]{Conflating Prototype and Instance}
While neither a prototype nor an instance is an object,
the @emph{conflation} of the two, is.
This is exactly what objects are in pure prototype OO languages like Jsonnet and Nix, and
a slight simplification of what they are in stateful prototype OO languages:
every object can be seen as either an instance, when querying the values of its slots,
or as a prototype, when combining it with other objects using inheritance.

Indeed in a pure functional language, without side-effects,
there is a unique instance associated to any prototype, up to observable equality:
its fixed-point.
Thus, it always makes sense to consider “the” instance for a prototype,
and to see it as but another aspect of it.
Evaluating the fixed-point may or may not converge, but thanks to lazy evaluation,
you don’t have to care about whether that is the case to refer to the two together,
and once computed once the result can be cached for performance.

If the language has side-effects, there may be multiple distinct instances to a prototype,
and a @c{clone} construct will generate a new object from an existing object,
and still keep instance and prototype together.
Even in such a language, a laziness construct can help build a simpler and nicer object system.

Note that these prototype objects correspond to @emph{classes at compile-time}
in class OO languages, that use the word “object” differently.
See @seclink{classes}.

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
and subclassing with subtyping@~cite{hoare1965record}.
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

@TODO{
cite DOT
cite fortress
cite Cook 2009
}


@section{BLAH START (RE)WRITING FROM HERE}

@subsection{FOOOOOOOOOOOO}

@subsubsection{Monotonicity}
Why Subclassing is rarely Subtyping, and other questions of monotonicity,
    (co-, contra- and in-) variance in Functor Mixins and Fixed-Point Operators.

@subsubsection{Autowrapping}
The relationship between Mutable or Immutable objects, linear typing and subtyping.

@subsubsection[#:tag "optics"]{Optics}
The generalization of OO from overriding methods in records
    to overriding arbitrary aspects of arbitrary computations using functional lenses or zippers,
    and how this generalization can accommodate advanced OO practices like method combinations.

@; build :: (partial → target) → top → (top → target → partial) → target
@; build wrap base mixin = Y (wrap . base mixin)
@; inherit is the same!
@; inherit child parent super self = child (parent super self) self



@subsubsection{Method Combination, Instance Combination}
Specializing inheritance with respect to how increments are combined.
generalizing precedence lists with DAG attribute grammars.
Metaobject-compatibility.

@; TODO: subsubsection about using the notion of defaults hiding complexity behind a simple interface,
@; and enabling, e.g. method combination with a primary method and other methods,
@; with the effective method being more than the plainly named main method.

@subsubsection[#:tag "global"]{Global Open Recursion}
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


XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX BLOH XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

@subsection{Conflation}

For instance, the wrapper function for a prototype that extends the
@c{super} record value with a field @r[rho] computed as the hypothenuse
of a right triangle of sides @r[x] and @r[y]
(being fields of the final value @c{self} to be defined by other composed prototypes)
might look like:
@codeblock{
  (define radius-from-rectangular
     (lambda (self super)
        (extend-record super
          'rho (sqrt (+ (sqr (get-field self x))
                        (sqr (get-field self y)))))))}

XXX

To compute a value from a specification,
the wrapper from a specification is combined with those of “parent” specifications it transitively extends
according to the rules of @emph{inheritance}
into a single wrapper,
and a “knot is tied” to resolve the self-references,
either in a pure functional way by using some variant of a fixed-point combinator,
or with side-effects by allocating a mutable record that the wrappers will update in place.
See the appendix for a detailed discussion, but in a pure functional language,
instantiation could be defined as follows assuming a fixed-point combinator
(see appendix for details).


@subsubsection{Methods}



Early object systems relied heavily on side-effects to construct and evaluate
their prototypes (and classes, when applicable), and so for them this model is but
an approximation of the semantics when the system is stable and no runtime side-effects
modify the inheritance structure.
On the other hand, static languages with Class OO have always always avoided side-effects at compile-time,
and actually behaved as in this pure functional model of inheritance.
Finally, many recent Prototype OO languages use this model as is;
indeed that is how the “extension” system of Nix is defined.


@subsubsection{OO without Records}

No need for objects and methods. Inheritance.
And yet, methods very useful for modularity.
More conflation to be at the same time a function, a table, a number, etc.
Or “just” follow the interface of all of them.

@subsubsection{OO without Messages}


Experiments have shown it is possible to have a “Prototype OO” without conflation,
and therefore without either “prototypes” or “objects”, just specifications and values @~cite{poof2021}.
Furthermore, almost all of the same patterns of software as with regular OO apply in this setting.
Still, using conflation is more @emph{modular} than not,
because it enables programmers to defer the question of which parts of a program are up for extension
and what the extensions will be until after runtime evaluation has started.
This modularity advantage however, is lost to Class OO in static languages,
where all class extensions happen at compile-time, and the conflation brings minor syntactic shortcuts
and major semantic confusion among programmers who have trouble conceptualizing
types and their incomplete specifications as separate, to the point that there is a long-standing
confusion between subclassing (relationship of a specification) and subtyping, and many researchers and language designers
have wasted years in an absurd quest to ignore, deny or paper over the difference between the two,
and search for typesystems that would

This conflation makes it very hard for many XXX

Meanwhile in Class OO, an “object” is an element of the type specified by a class.
Meanwhile the class is a conflation of the partial specification
of a type (and accompanying algorithms), and the type it specifies as least fixed point.
Class OO can thus be viewed as Prototype OO for types (or type descriptors),
which typically happens at compile-time in static languages@note{
Indeed, the meta-language of C++ templates offers a pure, dynamically typed, lazy, functional
programming language with pattern-matching, and with builtin Prototype OO.
It is the same programming paradigm as Jsonnet or Nix,
though for a very different domain target and with many quirks.
It is quite the opposite to the stateful, statically typed, eager, imperative language
with no pattern matching and with Class OO, that is C++ as a “base” language.
}
but may also happen at runtime in dynamic languages, or in static languages using reflection.

Finally, it is also possible to use inheritance, and thus have “object orientation”,
without objects: by not conflating either prototype and instance or class and type,
and instead keeping them cleanly separate @~cite{poof2021}.@note{
It thus appears that the expression “Object Orientation” (OO) is a misnomer,
since what unifies OO languages and systems is inheritance, that even does without objects at all,
while languages that provide everything but inheritance are distinctly not OO.

Kay who coined the term OO,
and later invented Smalltalk and eventually supported inheritance,
Dahl and Nygaard who first implemented objects in SIMULA 67,
or Kahn and Borning who built the second and first multiple inheritance systems
in their respective Lisp and Smalltalk graphical simulation environments,
were all aiming at a programming model of concurrent entities, “objects”,
behaving only on local knowledge and exchanging asynchronous messages,
thereby simulating real world interactions.
Their explicit goals do not at all include inheritance or OO, but rather
the style much later implemented and popularized by Erlang
and dubbed “Concurrency Oriented Programming” (COP) by its authors.

Instead, inheritance was a serendipitous discovery made along the way,
that was so useful that it would become (pun intended) the main feature
adopted by all the languages that would then start to be call “Object-Oriented”.
Smalltalk and most OO languages never fully embraced concurrency,
that remains an afterthought (if a thought at all) in most OO languages,
despite being an essential element of the original target programming model;
further method invocations in OO languages are almost always synchronous,
as opposed to asynchronous in Erlang or in the original SIMULA.

It is not unusual that a field is misnamed: a concept is usually named
as soon as it starts to become relevant to distinguish it
from other concepts it may have originated from, but before it is well-understood.
The name then reflects the intuitions, expectations and misunderstandings of early innovators,
before later theorists settle on what was the essence of their innovations.
}

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


@subsection{Single Inheritance}

@subsubsection{Direct superclasses and subclasses}
In single inheritance, each class has a single direct @emph{superclass}
that it inherits from, of which it is a (direct) subclass,
to which it contributes its increment of specification.
We say that the superclass is more generic, and the subclass more specific.

@subsubsection{Global structure of single inheritance}
The transitive (direct and indirect) superclasses of a given class form a list.
The set of all classes has the structure of a tree with some base class at its root
(or forest, i.e. set of disjoint trees, if there is no common base class).

@subsubsection{Prefix}
In SIMULA 67 @~cite{Simula1967}, in which inheritance was first implemented though not thus called,
the superclass was called a @emph{prefix class},
as its specification was notionally a prefix to the complete specification
for the subclass,
taken literally as concatenating definitions of functions and variables
with scope going in text order, with the subclass’s more specific code added
at the end within the scope of the prefix class’s more generic code.

@subsubsection{Suffix}
Already in 1967 though, the “prefix” class could also add a suffix
to the body of the subclass, the code of which would be evaluated
where an @code{inner} placeholder was “splitting” prefix and suffix.
Moreover, SIMULA 67 called “prefix sequence” the list of
a class and its transitive superclasses, but kept it in most-specific-first order,
which is contravariant with the notion of prefix coming before the suffix.

@subsubsection{Method Resolution}
When multiple superclasses define a same method, the most specific definition is used.
Most languages allow the body of this definition to in turn invoke
the method defined by the next most specific definition,
via a keyword @code{super} (Smalltalk, Java) or some other similar mechanism.@note{
SIMULA and its successor BETA @~cite{kristensen1987beta}
were special in instead having a superclass’s body specify
where subclass bodies are inserted, via the @code{inner} keyword.
BETA generalized classes to “patterns” that also covered method definitions the same way.
No other known language seems to use this technique,
although it is easily expressible using user-defined method combinations
in e.g. CLOS @~cite{bobrow88clos}.
}

@subsection{Multiple Inheritance}

@subsubsection{Method Resolution in Multiple Inheritance}
When each of multiple superclasses define a same method,
the simple resolution strategy used by single inheritance doesn’t directly apply
because the inheritance DAG defines a partial order, not a total order,
so there may be several potentially “conflicting” method definitions to choose from.

Some languages @~cite{Borning1979 Traits stroustrup1989multiple CecilMultimethods}
issue an error in case of such method definition conflict;
programmers must resolve conflict by having relevant subclasses explicitly define a method override.
This is a consistent strategy, but the least useful among all consistent strategies:
@itemize[
@item{Overrides fail the incrementality purpose of OO whenever they require users
to reimplement part or all of the functionality from superclasses.}
@item{Overrides might want to recursively call the methods of the class’s direct superclasses,
but that could lead to their transitively calling the method of a common indirect superclass multiple times,
and exponentially so as the inheritance DAG contains more such “diamond” configurations.}]
@; TODO cite Diamond Problem in C++ and learn about "virtual inheritance" and other C++ solutions.
@; C++ embraces exponential explosion unless you use virtual base classes.
@; your methods might also quickly check for multiple invocation and immediately return after the first time.
@; in the end you can try to layer as conventions the features the language doesn’t directly offer.
@; See also Malayeri & Aldrich’s 2009 "CZ: Multiple Inheritance without Diamonds" and its citations 43, 46.

Instead, we may realize that any solution that ensures each potentially applicable method
is considered once and only once (or at most once)
in computing the @emph{effective method} (semantics of calling the named method)
necessarily establishes a total “linear” ordering between these methods.

@subsubsection{Class linearization}
With @emph{class linearization}, a total ordering, or linearization, is chosen
that extends the partial ordering defined by each class’s inheritance DAG,
and also preserves the @emph{local ordering} of each class’s declaration
of its direct superclasses.
The resulting @emph{class precedence list} @~cite{bobrow86commonloops},
traditionally kept in most-specific-first order
(starting with the class, ending with the base class),
is then used to resolve methods as if the class had been defined through
single inheritance with that list of classes as its “prefix sequence”.
This approach was introduced by Flavors @~cite{Cannon1979},
and named by New Flavors @~cite{Moon1986Flavors}.

@subsubsection{Method Combinations}
Another innovation of Flavors
was the notion of method combinations:
users can specify for each method name,
how the methods with that name defined in all of a class’s class precedence list
would be combined into an @emph{effective method} for that class.

The method combination can be the standard combination after linearization above
(enriched with @code{before after} methods,
that can notably be used to emulate the behavior of SIMULA);
or it can apply a simple monoidal operation on their result
(@code{+ max min or and list append nconc progn});
or it can be whatever the user specifies,
including an error-on-conflict strategy if desired.

Later, the introduction of builtin @code{around} methods
would also enable code wrappings not expressible
merely with before and after methods.
Furthermore, in New Flavors @~cite{Moon1986Flavors},
CommonLOOPS @~cite{bobrow86commonloops} and
CLOS @~cite{gabriel1991clos},
as inspired by T’s unification of functions and objects @~cite{Rees82t:a adams88oopscheme},
a “generic function” would embody the “calling a method of a given name”
and become the locus for this specification of a method combination.
@; TODO quickly mention multi-methods, cite LOOPS, Cecil, Fortress and more.

Importantly for our discussion, the use of class-wide class precedence lists
ensures consistency of semantics across all classes, methods, method combinations,
effective methods, and generic functions.
Class precedence lists also offer a simple interface
for users defining their own method combinations.

@subsection{Comparison between single and multiple inheritance}

@subsubsection{Modularity Comparison}
Multiple inheritance is more modular than single inheritance,
allowing to divide program specifications
into more, smaller, more reusable classes,
also commonly called “mixins” (in the Lisp tradition)
or “traits” (in the Smalltalk, Mesa, SELF, Slate, Scala tradition)@note{
Unhappily, Rust has recently popularized the word “trait” to mean something completely different,
which is close to what Haskell previously called “typeclasses”,
the informal notion of “protocol” in CLOS,
or slightly more formal notion of protocol in Clojure:
(a) first the ability to define a set of related function names and type signatures,
and then the way that you can implement suitable functions for each of these names
for inputs (and, in Haskell and to a point CLOS, also outputs) of each for specified types
(or, in Haskell and CLOS, also tuple of types), with
(b) second the crucial property
that these traits, typeclasses or protocols can be defined @emph{after the fact},
so that new typeclasses can be defined for existing types,
and new types can be added to existing typeclasses.
This second property is in sharp contrast with “interfaces” in Java or C#,
wherein the author of the class must know in advance all the interfaces that the class will implement,
which must yet cannot anticipate any of the future extensions that users will need.
Users with needs for new protocols will then have to keep reinventing
variants of existing classes, or wrappers around existing classes, etc.
— and again when yet another protocol is needed.
},
such that each partial program specification
can be written with a smaller amount of information in the head of the programmer.

@subsubsection{Expressiveness Comparison}
Multiple inheritance is more expressive than single inheritance,
allowing partial specifications to be conceptualized
that would have previously required code duplication or roundabout protocols
that break modularity or incrementality.

@subsubsection{Performance Comparison}
Multiple inheritance is generally more expensive at runtime than single inheritance:
Notably, access to a method or attribute with single inheritance
can happen at a same statically computed array index for all subclasses of the class defining it.
By comparison, multiple inheritance in the most general case requires method or attribute access
to lookup some kind of hash-table, which, while still constant-time, is significantly more expensive.

A lot of work has been done to improve the performance of multiple inheritance,
through static method resolution when possible, @; TODO cite C++ ? type analysis ? sealing ?
and otherwise through caching @~cite{bobrow86commonloops}. @; TODO cite
But these improvements introduce complexity, and caching
increases memory pressure and still incurs a small runtime overhead,
even when successful at avoiding the full cost of the general case.
For this reason, many performance-conscious programmers
prefer to use or implement single inheritance when offered the choice.







@subsection{Mixin Inheritance}

@subsubsection{Comparative Expressiveness}
Mixin inheritance is more expressive than single inheritance, and
just as expressive as multiple inheritance, in that it enables
classes (mixins or traits) to be defined once without being tethered
to a single superclass (or chain of superclasses), and
combined and recombined in many compositions@xnote["."]{
  Actually, mixin inheritance can be argued to be more expressive than multiple inheritance
  unless multiple inheritance is also accompanied by some means of renaming
  classes, slots, and methods.
  However, in a language where classes are meta-level constants,
  renaming is a trivial extra-lingual operation;
  and in a language where classes (or prototypes) are first-class runtime values,
  renaming is a relatively simple operation though it may depend on reflection.
  Thus, in practice, we can usually dismiss the thin advantage in expressiveness of mixin inheritance.
}
Conceptually, composition of mixins allows to @emph{append} two lists of classes,
when single inheritance only allows to @emph{cons} a class to a fixed list.

@subsubsection{Comparative Modularity}
Mixin inheritance is less modular than multiple inheritance,
because it makes the programmer responsible for ensuring
there are no missing, repeated or misordered superclasses,
manually doing what multiple inheritance does for you
when computing its class precedence lists.
A notable bad situation is when the list of superclasses of a class is modified,
at which point all its transitive subclasses must be updated accordingly,
even if defined in completely different modules that the author cannot modify,
that he may have no idea exists, whose authors he cannot even notify.
This makes changes brittle, breaks modularity, and
effectively forces the entire inheritance DAG of a class to become part of its interface.
By contrast, multiple inheritance can automate all these troubles away,
and let programmers only have to worry about their own classes’s direct superclasses.

@subsubsection{No Further Comment}
Mixin inheritance definitely has its uses, if only as
a lower-level building block used in implementing more elaborate object systems.
Nevertheless, in the rest of this document, we dismiss mixin inheritance
for being a less modular and less performant alternative
to the combination of multiple inheritance and single inheritance we are seeking.



@subsubsection{Previous Art}
Many languages adopted single inheritance for its performance advantages,
yet were later extended with multiple inheritance for its expressiveness,
while trying to preserve the advantages of single inheritance where appropriate.

We will examine the cases of Common Lisp, Scala and Ruby:
Lisp in the early 1970s had “structs” that implemented records (0-classes)
and that by the mid 1970s could be extended with single inheritance (s-classes),
then by the late 1970s also adopted “classes” that could be extended with multiple inheritance (m-classes).
@; TODO cite
Meanwhile, Java in the mid 1990s defined its evaluation model around “classes”
that only support single inheritance (s-classes), but Scala in the mid 2000s
enriched the Java evaluation model with “traits” that support multiple inheritance (m-classes).
@; TODO cite
@; Scala 2004, Scala 3 2021
As for Ruby, though its “classes” only support single inheritance (s-classes),
by version 1.9 in 2007 they could be extended with multiple inheritance using “modules” (s-classes).
@; TODO cite
@; Ruby 1993, 1.9 2007

@subsubsection{Terminology}
Beware that the word “class” weakly implies multiple inheritance in the Lisp tradition,
where it contrasts with “struct” that strongly implies single inheritance.

By contrast, “class” weakly implies single inheritance
in the Smalltalk, Java and Scala tradition,
where it contrasts with “trait” that strongly implies multiple inheritance.

To confuse things further, in C++ tradition,
a @code{struct} is just a way to define a class
wherein all members (methods and variables) are public by default,
which has nothing to do with either single or multiple inheritance.
C++ always has multiple inheritance, although
superclasses reached along many paths are duplicated unless declared “virtual”,
which is a form of mixin inheritance.

This document follows the Lisp tradition in its terminology,
except in the section on Scala below where we will use Scala terminology,
but in double-quotes.

@subsection{Common Lisp}

@subsubsection{Separate Class and Struct Hierarchies}
Common Lisp @~cite{cltl2} has @emph{structs},
that sport single inheritance and are quite efficient,
as well as @emph{classes} @~cite{gabriel1991clos},
that sport multiple inheritance and are more expressive but slower.

However, to avoid inconsistencies, Common Lisp wholly prevents
a class from inheriting from a struct, or vice versa,
keeping the two hierarchies separate,
though offering a uniform interface to both.

@subsubsection{User-defined Hierarchies}
Through its MOP @~cite{AMOP},
Common Lisp also enables users to define metaclasses
wherein users can define their own class hierarchies,
that could conceivably combine single and multiple inheritance.
We are not aware of anyone using this mechanism to do so;
if someone did, it is unclear whether the mechanisms provided
would allow them implement the usual performance optimizations
allowed on its single inheritance fragment.

@subsection{Scala}

@subsubsection{Traits}
Scala extends Java’s “classes” that only support single inheritance
with “traits” @~cite{scalableComponentAbstractions2005}
that support multiple inheritance.

Scala “classes” and “traits” definitions may specify
at most one direct “superclass” and potentially many direct “supertraits”
that it “extends”.
Syntactically, developers specify direct superclasses and traits in least-specific-first order,
which is the reverse of the local precedence order.
But semantically, the Scala specification still discusses
class precedence lists (that it calls “class linearizations”)
in the traditional most-specific-first order.

@subsubsection{Scala superclasses}
Scala 2.13 requires that if a trait or class has a (single inheritance) superclass
in its inheritance hierarchy, then it shall declare as the first entity it extends
a (single inheritance) class that is more specific
than any of its indirect (single inheritance) superclasses.

Scala 3.3 relaxes this restriction by automatically computing
this most specific superclass from all the superclasses in its inheritance hierarchy,
so you can declare directly extending a superclass of it, or only traits.

We have been unable to find a documentation of this feature in Scala 3,
its precise behavior, design rationale, and implementation timeline,
even after contacting members of the Scala team.
However, this is clearly the Right Thing to do in this case,
as we’ll explain when discussing our solution.

@subsubsection{Scala linearization}
Scala uses a variant of the original LOOPS linearization algorithm @~cite{ducournau1992monotonic}:
The LOOPS algorithm simply concatenates all the class precedence lists
of a class’s direct superclasses, then removes all duplicates,
keeping the @emph{first} one (in most specific order) and removing latter copies,
following the same heuristic as Flavors @~cite{Moon1986Flavors},
The Scala algorithm does as much but keeps the @emph{last} duplicate instead,
following an opposite heuristic.

Although this change in heuristic is not explained by Scala authors,
we believe it was chosen because, unlike the LOOPS heuristic,
it always preserves the precedence list of the least-specific direct supertrait
(syntactically first, semantically last) as the tail of the defined class’s precedence list,
which is necessary when that last supertrait is a single-inheritance “class”,
or has a “superclass” more specific than “Object”.

Scala 2.13 in particular requires developers to specify in first syntactic position
a “trait” whose most specific “superclass” is no less specific than that of
any of the other direct supertraits; otherwise the compiler throws an error.

Although this behavior doesn’t seem to be documented,
Scala 3.3 takes a more “semantic” than “syntactic” approach:
it specially treats the “class” fragment of inheritance and behaves as if
the most specific of any of the supertraits’ most-specific “class” superclass had been specified first.
It is of course an error if the supertraits’ “superclasses” are not a total order,
with a single most-specific “class” among them.

@section{Our C4 Algorithm}

@subsection{Best Combining Single and Multiple Inheritance}

@subsubsection{Unifying Classes and Structs}
In modernizing the builtin object system of @(GerbilScheme),
we decided to unify hierarchies
of single inheritance structs and multiple inheritance classes,
that were theretofore separate, as in Common Lisp.
In doing so, we identified a maximally expressive way to combine them.

@subsubsection{Adding a Fifth Constraint}
We had recently adopted the C3 algorithm for class linearization,
its four constraints and its heuristic.
We decided to minimally complement it with an additional fifth constraint,
necessary and sufficient to support integration of single inheritance @emph{structs}
into multiple inheritance precedence lists.

@subsection{The Struct Suffix Constraint}

@subsubsection{Constraint}
@bold{The precedence list of a struct is a suffix of
the precedence list of all its subclasses.}

@subsubsection{Why the Suffix Constraint}
The above suffix constraint is precisely
the semantic prerequisite for the efficiency optimizations
enabled by single-inheritance:
access to methods can use a same statically computed index
for all subclasses of a class.

@subsubsection{Special Treatment of Struct Suffix}
Our algorithm enforces the Struct Suffix Constraint
by treating each class’s most specific struct superclass specially:
First, determine this most specific struct superclass,
and include its class precedence list as the tail of the one being computed.

Issue an error if multiple incompatible structs are used
where neither is a superclass of the other.
Of the remaining classes not in this precedence list, none is a struct;
apply the regular class linearization algorithm on those, based on C3.
Prepend the result to the struct suffix.

Our special treatment of the Struct Suffix is essentially equivalent to
Scala 3.3’s behavior regarding a class’s most-specific super “class”,
as described above.

@subsection{Advantages of C4}

@subsubsection{Struct declarations optional}
One advantage of the Depth-First, Most-Specific-First Traversal
that C3 and C4 use as heuristic to choose a next class for the precedence list is that
they will preserve the tail of a struct’s precedence list if the discipline is followed
to always place the most-specific struct last in the local ordering,
even without special support for struct.

Scala 2.13 enforces this discipline syntactically based on struct declarations,
while Scala 3 or the C4 algorithm automate it away.
But given C3 or the Scala 2.13 algorithm (but not e.g. the LOOPS algorithm),
it could be achieved without language-supported struct declaration
by developers who always follow the discipline without any mistake.

@subsubsection{Coherent Naming}
Our C4 algorithm, being a successor to C3, is named after the
four correctness constraints it respects, and omits Shape Determinism in the count,
though it also respects it.
It also doesn’t count among its naming constraints
the heuristic based on a depth-first, most-specific-first traversal.

@subsection{Single-Inheritance Yet Not Quite}

@subsubsection{Single-Inheritance among Structs}
When following the @emph{struct suffix} constraint,
the superstructs of a struct, i.e. the subset of its superclasses that are structs,
are totally ordered by @emph{transitive} inheritance:
the partial order induced by the inheritance DAG on the struct subset is total,
i.e. it is a list, and the inheritance graph of all structs is a tree
if there is a common base struct, or a forest otherwise.

@subsubsection{No Single-Inheritance among Structs plus Classes}
Yet the larger set of superclasses of a struct,
including its direct and indirect non-struct classes,
is a DAG that is not usually totally ordered.
In @emph{that} sense, the inheritance hierarchy of structs is @emph{not} single-inheritance;
a struct’s superclasses are not a list, the hierarchy of all structs is neither tree nor forest.

And that is actually a good feature: it means that structs can inherit from classes,
and benefit from the expressiveness and modularity of those classes in their definition.

@subsubsection{Preserving the Property that Matters}
Thus when “combining single and multiple inheritance”, it is not exactly “single inheritance”
that we preserve, but the more important struct suffix property.
Not the syntactic constraint on building a class,
but the semantic constraint on the invariants its subclasses will respect,
that themselves enable various optimizations.

It may be a surprising conclusion to the debate between proponents of multiple inheritance
and of single inheritance that in the end,
single inheritance did matter in a way,
but it was not exactly single inheritance as such that mattered,
rather it was the struct suffix property implicit in single inheritance.
The debate was not framed properly, and a suitable reframing solves the problem
hopefully to everyone’s satisfaction.

@section{Inheritance Examples}
@subsection{Example 1}

@(noindent) @image[#:scale 0.67]{C3_linearization_example.eps}

In this example lifted from Wikipedia @~cite{WikiC3},
we define a base class @code{O},
classes @code{A B C D E} that each inherit only from @code{O},
classes @code{K1} with direct superclasses @code{A B C},
@code{K2} with @code{D B E},
@code{K3} with @code{D A}, and @code{Z} with @code{K1 K2 K3}.
Using the C3 or C4 algorithm, we get the precedence list @code{Z K1 K2 K3 D A B C E O},
with each subclass having its subset of superclasses in the same order
in its own precedence list.

If, using the C4 algorithm, @code{C} were declared a struct, then
the suffix @code{C O} must be preserved,
and the precedence list would be changed to @code{Z K1 K2 K3 D A B E C O}.
If both @code{C} and @code{E} were declared structs, then there would be a conflict
between the suffixes @code{C O} and @code{E O}, and
the definition of @code{Z} would fail with an error.

In this class hierarchy, only @code{O}, one of @code{C E}, and/or @code{Z}
may be declared struct without causing an error
due to violation of the local precedence order.
Indeed, a class may not be declared a struct if it appears in a direct superclass list
before a class that is not one of its superclasses.
However, this criterion is not necessary to prohibit struct-ability,
and @code{K3} cannot be a struct either,
because its superclass @code{D} appears before @code{B E} among the direct superclasses of @code{K2},
which would break the struct suffix of @code{K3} when @code{Z} inherits from both @code{K2} and @code{K3}.

@subsection{Example 2}

@(noindent) @image[#:scale 0.066]{C3_linearization_example_2.png}
@;{
https://www.mermaidchart.com/app/projects/0d7dd2c2-0762-428e-a4be-2063fd491789/diagrams/26327f0d-1e07-4b9a-ba0e-94557d2ceac1/version/v0.1/edit
---
config:
  theme: mc
  look: classic
---
flowchart BT
    B("Boat")
    C("DayBoat")
    D("WheelBoat")
    E("EngineLess")
    F("SmallMultiHull")
    G("PedalWheelBoat")
    H("SmallCatamaran")
    I("Pedalo")
    I --> G --> E --> C --> B
    I --> H --> F --> C
    G --> D --> B
}

In this example from Ducournau et al. @~cite{ProposalMonotonicMultipleInheritance1994},
we have the class and direct superclass lists:
@code{Boat},
@code{DayBoat Boat},
@code{DayBoat WheelBoat},
@code{EngineLess DayBoat},
@code{PedalWheelBoat EngineLess WheelBoat},
@code{SmallMultihull DayBoat},
@code{SmallCatamaran SmallMultihull},
@code{Pedalo PedalWheelBoat SmallCatamaran}.

The C3 and C4 algorithms both compute the following precedence list for this class hierarchy:

@code{Pedalo PedalWheelBoat EngineLess SmallCatamaran SmallMultihull DayBoat WheelBoat Boat}.

Due to precedence constraints, any of @code{Pedalo Boat}, and
at most one of @code{WheelBoat DayBoat} could be declared a struct,
with @code{DayBoat} being the only one to change the precedence list, to:

@code{Pedalo PedalWheelBoat EngineLess SmallCatamaran SmallMultihull WheelBoat DayBoat Boat}.

Interestingly, either @code{WheelBoat} or @code{DayBoat} can be made a struct,
because they don’t appear directly in a same class’s direct-superclass list,
so there is no local precedence order constraint between the two.

If there were no @code{EngineLess} between @code{PedalWheelBoat} and @code{DayBoat},
then @code{DayBoat} appearing before @code{WheelBoat} would prevent the former
from being made a struct, with the definition of @code{PedalWheelBoat} triggering an error.

A general solution that can be used to ensure @code{DayBoat} is a struct
would be to swap the order of superclasses in the conflicting definition;
when the methods defined or overridden by the swapped superclasses are disjoint,
the swap will not otherwise change the semantics;
otherwise, the subclass can suitably override methods to compensate for the change.
And the other general solution in last resort is to introduce a do-nothing wrapper class
to shield a superclass from a local local precedence order constraint,
just like the @code{EngineLess} shields @code{DayBoat}.

@section{Conclusion: Best of Both Worlds}
@subsection{Findings}
@subsubsection{Restating the Obvious}
Our presentation of Object-Orientation and Inheritance
only included what should have been obvious and well-known lore by now.
Yet so far as our bibliographical search could identify,
a lot of it seems to be
unstated in academic literature, @; cite more papers and books that miss the point?
or implicitly assumed by ones and blatantly ignored by others,
or once mentioned in an otherwise obscure uncited paper
— and overall largely acted against in practice
by most language designers, implementers and users.

Without claiming originality in that part of this article,
we would like to insist on our simple explanation and rationale for each of:
@itemize[
@item{The relationship between OO, modularity and incrementality.}
@item{The relationship between prototypes, classes, objects and conflation.}
@item{The relative advantages of single, mixin and multiple inheritance.}
@item{Why linearization beats manual conflict resolution.}
@item{The importance of well-documented yet oft-ignored consistency constraints on linearization.}]

@subsubsection{Struct Suffix}
We identified the @emph{struct suffix} constraint as
the one semantic constraint necessary and sufficient
to achieve the optimizations associated with single inheritance,
even in the context of multiple inheritance.
The constraint was implicitly enforced by Scala,
but does not seem to have been identified and made explicit in any publication yet.

@subsubsection{C4 Algorithm}
We implemented a new C4 Algorithm that combines all the above features.
While each of these features may have been implemented separately in the past,
ours seems to be the first implementation to combine them.

@subsection{Implementation}
@subsubsection{Our Scheme}
We have implemented the C4 algorithm
in our open-source dialect of the Scheme programming language, @anonymize[""]{Gerbil Scheme,}
and it will be available in the next release@anonymize[""]{ 0.18.2}.

Our users can enjoy the benefits, as our language can legitimately claim
to have the single Best Inheritance mechanism of them all.
At least until other language implementers copy our language.

@subsubsection{Code Size}
The C4 algorithm itself is under 200 lines of code with lots of explanatory comments.

The entire object system is about 1400 lines of commented code for its runtime,
including all runtime optimizations enabled by single inheritance where appropriate.

The compile-time and macro-expansion-time support for the object system are harder to account for,
not being isolated in their own files but spliced all over.
We estimate they total between one and two thousand lines of code,
wherein the entire compiler and the language prelude each total a bit over 8000 lines of code.
However, they are also the parts of our system that implementers of other languages
will least care to reuse.

Overall, the complexity of our implementation is quite low, and
it shouldn’t be too much effort for a dedicated language implementer
to port our technology to their language.

@subsubsection{Open Source}
We invite all language implementers to likewise adopt C4
in the next version of their object system,
whether class-based or prototype-based, static or dynamic, etc.
Then your users too can enjoy the Best Inheritance in the World.

@section{Data-Availability Statement} @appendix
Our code is available in our github repository as part of our Scheme implementation.
We will reveal the address after deanonymization.

For the sake of artifact review, we will build an anonymized implementation
of the C4 algorithm isolated from the rest of our object system.
We will include a few execution test cases.
We will not attempt to anonymize a complete variant of our object system,
which would be overly costly and would easily fail to be anonymous.

The algorithm description we give above
should already be sufficient for any person skilled in the art
to reimplement and adapt the C4 algorithm to their own object system.
Furthermore, the artifact we provide will only allow a language implementer
to compare their implementation to ours and check for any bugs in their reimplementation.

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX BLAH XXXXXXXXXXXXXXXXXXXXXXXXXXXXX

In Smalltalk and Java tradition, the word “class” describes
entities with a single inheritance hierarchy;
some languages in this tradition (such as Mesa, SELF, Scala)
have entities that support multiple inheritance, that they call “trait”.
In Lisp, C++ or Python tradition, the word “class” describes entities
with a multiple inheritance hierarchy;
Lisp also supports entities with a single inheritance hierarchy, that they call “struct”;
To insist that a “class” is to be used in the context of multiple inheritance,
Lisp may call it a “mixin” rather than a “trait”,
but “mixin” in Programming Language literature has latter come to mean
something slightly different and more specific,
partaking of “mixin inheritance”.
Meanwhile, Python and C++ have no such single inheritance hierarchy;
but C++ calls “struct” a “class” whose “members” are public rather than private by default;
this concept extends the concept of “struct” in C wherein they are records with no inheritance at all.

Instead of “classes”, we will speak of @emph{prototypes} for the entities being specified in general,
since Prototype OO is more general than Class OO @~cite{poof2021},
the latter being “just” the special case of compile-time prototypes
for (compile-time descriptors for) types and associated method dispatch tables.
We will call those prototypes @emph{traits} when part of a multiple inheritance hierarchy,
in the Smalltalk tradition.
We will call those prototypes @emph{lines} when part of a single inheritance hierarchy,
a word we just made up, that may avoid confusion and bring the right connotation.
We will call those prototypes @emph{mixins} when part of a mixin inheritance hierarchy,
after Bracha, and as in Jsonnet.

A line is a bit like a family name, wherein if you inherit from it,
you also inherit from its own line, and so on, transitively to the beginning of remembered ancestry;
whereas a trait is more like a first name (a mixin even more so),
in that you have a thus-named ancestor, it doesn't constrain the name of the next ancestor.@note{
One proposed naming solution was to call an entity with single-inheritance
a “uniprototype”, “1-prototype”, “uniproto” or “1-proto”,
or maybe “uniclass” or “1-class” if it is a class;
similarly, an entity with multiple-inheritance would be
a “multiprototype”, “n-prototype”, “n-proto”, “ω-prototype” or “ω-proto”;
or maybe “multiclass”, “n-class” or “ω-class” if it is a class.
The above are all new words, a bit heavy to use, but quite suggestive and unambiguous.

m-prototype, m- (multi, many, mixin, modular, etc.)
s-prototype,   (single, struct, suffix).
Surprising conclusion: It's not a constraint on how it was built, but on how it was used(!)
(see also linear types vs uniqueness types).
Any valid definition for an s-class could just as well define an m-class and vice versa;
whether a class definition is for an s-class or m-class could as well be a boolean flag
(and indeed is in @(GerbilScheme)).

We previously used “struct” for a single-inheritance entity,
which has the advantage that it matches the Lisp tradition,
the oldest one that simultaneously tackled both multiple and single inheritance;
but it might have caused confusion in the majority of readers who are familiar with C or C++ but not Lisp,
and it did confuse at least one reader into believing we were discussing a Lisp-only problem.
We could also have tried to use the earliest name for each concept;
since (multiple) inheritance can be traced back to KRL, its entities could be called “frames”,
and for single inheritance tracing back to SIMULA (in concept) and later Smalltalk (in name),
its entities would be called “classes”;
but few remember KRL, and frames, and those who do might ascribe much more meaning to it
beside denoting something with multiple inheritance;
and the word “class” was invented even before single-inheritance by Tony Hoare to mean more of
an interface or type that many kinds of records could satisfy in an extensible way
(with what we would nowadays call subtyping), which fits multiple inheritance
as well if not better than single inheritance;
meanwhile the word “class” was also used in the context of multiple inheritance
in the same year (1976) as Smalltalk adopted single inheritance.
So this originalist naming might still be confusing.

Originalism would be even more confusing with respect to “mixin”:
the word was introduced by Flavors (in Lisp) to signify
any flavor (i.e. class) specifically intended for multiple inheritance, i.e. a trait
(even though any flavor (a.k.a. class) can be used with multiple inheritance already).
However, the term was later hijacked by Bracha and Cook to refer to mixin inheritance,
a more primitive model (less elaborate, but also more fundamental and more directly composable)
discussed below.
This latter technically more useful meaning has gained more adoption and recognition
in the programming language community,
whereas “mixin” in the original sense has but a minor use within the small Lisp community.

For reasons that will become apparent later, “suffix” could also have been chosen;
though, in some languages with reversed syntax, “prefix” might work, and
would refer back to “prefix classes” in SIMULA;
just the need to discuss which order to use though is enough to make us reject the name as confusing.

We eventually settled on calling a multiple-inheritance entity “trait”,
which is a well-accepted term, unambiguous enough,
while a completely new word, “line”, was chosen for single-inheritance entity,
which is in the lexical vicinity of both “trait” and “inheritance”,
and suggests a linear order.
}



If your language of choice is Common Lisp, then your “classes” are traits (including “mixin” classes),
and your “structs” are structs, all of them compile-time prototypes, though first-class through reflection.
If your language of choice is Smalltalk, then your “classes” are structs,
and there are no traits, and structs are compile-time prototypes, but first-class through reflection.
If your language of choice is Java, then your “classes” are structs,
though your “interfaces” are traits.
If your language of choice is Scala, then your “classes” are structs as in Java,
but your “traits” are traits indeed.
If your language of choice is C++, then your “virtual classes” are traits,
and non-virtual “classes” are mixins,
while your “structs” are just traits or mixins with public members
but are emphatically @emph{not} structs in the sense of this article.



by each language: mandating, connoting, prohibiting, either a single or multiple inheritance
hierarchy depending on the programming language, sometimes with inconsistent
variations in meaning across documents describing a same language,
or within a same document. Yet it should be fairly obvious to readers
from each OO tradition how to map the concepts we present and the words we use
to the concepts and words used for their programming language of choice,
and vice versa.
However, we will leave such mappings as an exercise to the reader,
and refrain from building a grand taxonomy of mappings or words and concepts
across all programming languages known and unknown, since,
as Whitehead famously quipped, Taxonomy is the death of science.


If  
to the terminology of the Lisp tradition, because it is
the oldest tradition that has been tackling those problems.@note{
Although it came first and directly or indirectly inspired
all OO languages and systems that followed,
SIMULA has many idiosyncrasies that set it and its successor BETA
apart from the wider OO tradition.
Also, neither tackled multiple (or mixin) inheritance,
making them irrelevant to most of the issues at stake.

Smalltalk, although it adopted inheritance only oh-so-slightly later than Lisp,
had a larger direct and indirect influence in spreading the ideas of OO.
Smalltalk itself never adopted multiple inheritance, but many systems
built on Smalltalk, directly inspired by Smalltalk, or otherwise
in a tradition that stems from Smalltalk, have:
ThingLab, Mesa, SELF, and arguably Scala, support @emph{traits}
with multiple inheritance.
}

See append
In a lazy language with implicit recursion, @r[Y] could be defined as follows:
@codeblock{
  (define (Y f) (f (Y f)))}
but that version would needlessly duplicate computations.
To share computations, a better version would be:
@codeblock{
  (define (Y f) (letrec ((x (f x))) x))}
And indeed, that’s exactly how the Nix extension system defines its basic fixed-point operator
(wherein the @r[a : b] syntax is λ-abstraction and the @r[let] is implicitly recursive)@~cite{nix2015}:
@codeblock{
  fix = f: let x = f x; in x}
}

In an eager language, without implicit recursion, but without sharing of computation results,
thus with much computation explosions, you can define @r[Y]
using the simpler composition combinator @r[B] and the duplication combinator @r[D]
(here in an applicative variant that protects the duplication under a @r[(λ (y) …)]):
@codeblock{
  (define B (λ (x y) (λ (z) (x (y z))))) ; composition
  (define D (λ (x) (λ (y) ((x x) y)))) ; duplication
  (define Y (λ (f) (D (B f D)))) ; fixed-point}
But XXX
To avoid exponential recomputations, though, it is preferrable to emulate laziness
by having wrappers be functions of two delayed computations,
with the delayed fixed-point combinator @r[Y^] as follows:
  (define (instantiate-wrapper^ w b) (Y^ (λ (s) (w s b))))
  (define (Y^ f) (letrec ((x (delay (f x)))) x)) ; fixed-point}
}:

The @r[Y] combinator could be defined as follows in 
    (let ((self (wrapper self base)))
       self))}
(Using Scheme syntax, but


This definition uses Scheme syntax, but assumes a lazy dialect of Scheme.
However, the Nix language is a pure functional defines


}:
@codeblock{
  (define (instantiate-wrapper wrapper base)
    (Y (λ (self) (wrapper self base))))}

in both cases, the “super” refers to the state of the record so far,
to be used eagerly to refer to partial values inherited so far,
while the “self” refers to the state of the record at the end of the evaluation,
for delayed use after the record is fully initialized.
Attempts to use some attribute before it was initialized may result in

In Prototype OO, this computation is typically called @emph{instantiation},
which can be expressed as follows a functional language,
using the usual Y fixed-point combinator.



You can implement arithmetics with side-effects into arrays.
That doesn't make arithmetics itself effectful in any shape or form.
Same goes with OO. Using side-effects to implement xxxx


@subsection{Claims}

The present paper claim the following original contributions:

@TODO{see defsection in poof.scrbl, use that here and everywhere.}

@itemize[
@;#:style enumparenalph
@item{Dispel common misconceptions as to what OO is about (@seclink{what_oo_is_not_about}).}
@item{Propose criteria for Modularity (@seclink{modularity})
  and Incrementality (@seclink{incrementality})
  in terms of information needed to make software modifications.}
@item{Elucidate how Incrementality and Modularity go together (@seclink{incremental_modularity}).}
@item{Map the basic concepts of OO to modularity and incrementality
  (@seclink{internal_incremental_modularity}),
  as embodied in the simplest kind of OO Prototypes
  using mixin inheritance (@seclink{simplest_prototypes}).}
@item{Explain how single inheritance
  is less expressive and modular than mixin inheritance (@seclink{single_inheritance}),
  that is less so than multiple inheritance (@seclink{multiple_inheritance}).}
@item{Show how “structs” with the performance benefits of single-inheritance
  can be expressed in a system with multiple-inheritance
  (@seclink{single_and_multiple_inheritance_together}).}
@item{Discuss how purity and laziness make OO more modular,
  and solve difficult initialization order issues (@seclink{laziness}).}
@; ^ and are actually used in practice in most OO languages—but only at the metalevel.
@item{Discuss how purity and laziness make OO more modular,
  and solve difficult initialization order issues (@seclink{laziness}).}
@; ^ and are actually used in practice in most OO languages—but only at the metalevel.
@item{Expose the conflation between prototypes and instances (or classes and types)
  at the heart of most OO, and why it contributes to modularity (@seclink{objects}).}
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

@(generate-bibliography)

Lens s t a b = { get: (s -> a) ; set: (s -> b -> t) }
LensVL s t a b = forall f. Functor f => (a -> f b) -> s -> f t
Lens' s a = Lens s s a a

methodLens : lens body self

mixin_function self super enriched = self -> super -> enriched | effective < enriched < super

instantiate : methodLens effective self -> mixin_function self base effective -> self

subproto : methodLens field self -> mixin_function field fieldsuper fieldbody -> mixin_function self super body
subproto l m = (λ (self : self  super : fieldsuper) (l.set self (m self super)))
subproto : methodLens field self -> mixin_function field fieldsuper fieldbody -> mixin_function self super body
subbase l b = l.get b

instantiate : methodLens self self effective body -> mixin_function self base effective -> base -> self
instantiate l m b = fix (λ (s) (l.set s (m s (l.get b))))




Aznavour: le temps
 les deux guitares
 desormais
 emmenez-moi


A vast array of ignoramus, including ADA and C++ designers and users, think multiple inheritance requires a way to resolve method conflicts, when Lisp's Flavors (1979) showed that methods could and should harmonously combine instead. Losers have conflicts, winners play win-win.

@appendix
@section{Classes as Prototypes}
Examples.


@;{Even some more recent languages that support multiple inheritance (@seclink{multiple_inheritance})
also support single inheritance for some classes (or “structures”),
and sometimes the consistent combination of the two
(@seclink{single_and_multiple_inheritance_together}).
}


History:
Not Burroughs B5000, though its builtin support for dispatch tables is cool.
First integrated OO is not Ivar Sutherland's Sketchpad (1963)
Not Alan Kay 1966 inventing the word, though it's an essential step
Arguably but also arguably not Dahl and Nygaard (1967) first implementing classes, though it is a breakthrough and quite close
Not Smalltalk 1972
Not Hewitt's Actors (1973, first implementation Irene Greif thesis 1975?, later Gul Agha?)
Not Frames or KRL (1975, "inheritance")
Smalltalk 1976


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




In the notable case of data records that may be later extended,
which is most relevant to OO,
access to named fields must go through some “associative array” (e.g. hash-table)
mapping identifiers to field index in the record.
These indexes may be cached between modifications, or wholly resolved at compile-time
if the extension is external or second-class.


@;{
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


"Code reuse".
Hated by detractors to OO.
Yet entire reason for OO, versus external extensibility.
Requires good factoring indeed.
Maintainership burden that is not as thoughtless as duplicating code,
yet ultimately more efficient since it doesn't require duplicating design and fixes.

@subsubsection{Modular Extensions}

Let us consider implementing modular extensibility in a pure functional setting.
If we modularly define extensions, our terms will take an argument @c{self}, the modularity context,
and return an extension, which takes an argument @c{super},
the previous “inherited” (record of) definitions,
and returns some extended (record of) definitions.
A simple type for a modular extension is then @r[M ⟶ E ⟶ E],
wherein terms are typically of the form @linebreak[] @r{(λ (self super) ...extended_super)}.

If we instead define extensions to modular definitions, our terms will take
an argument @c{super}, the previous “inherited” modular definition,
of type @r[M ⟶ E], and return an extended modular definition also of type @r[M ⟶ E],
and therefore is of type @r[(M ⟶ E) ⟶ M ⟶ E].
But since the point of modularity is to plug
the same element of @r[M] at the end through a fixed-point,
the construct contains the same useful information as @r[E ⟶ M ⟶ E], or as
@r[M ⟶ E ⟶ E] above, just with extra complexity in the composition.
We will therefore prefer the simpler “modular extension” point of view.

In the general case, the type @r[E] or @c{super} and the return value
will be that of a @emph{method} of a prototype, or sub-entity being incrementally defined,
whereas @r[M] will be some language-wide namespace, registering all
known (and yet unknown) computations, prototypes and library functions in the language ecosystem.
(This is notably the case with @c{nixpkgs}, wherein the role @r[M] is taken
by the argument @r[pkgs], top of the global namespace of packages
and other entities within the ecosystem (including library functions, etc.)).
In the simplest case, @r[E] and @r[M] will both be the same type,
that of a single target being modularly and extensibly specified, typically a record.


And no, the visitor pattern, even after you go through all the pain of it, doesn't fully capture the expressiveness of multiple dispatch with method combination, because it finds only one method.


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

}


@; Flavors combination vs C++ conflict https://x.com/Ngnghm/status/1980509375232885161
@; https://cs.pomona.edu/~kim/FOOPL/prelim.pdf

@; Discussion with James Noble and David Barbour: https://x.com/Ngnghm/status/1988891187340615763
@; multiple inheritance


Self's once "sender path" approach to multiple inheritance, like the "visitor pattern" approach to multiple dispatch, fails to capture semantics contributed by concurrent branches of a partial order, by eagerly taking the first available branch without backtracking.

I see a potential for confusion:
- *specifications* need identity (e.g. pointer equality) as DAG nodes when using multiple inheritance or dispatch.
- *targets* and elements of target types as such need no identity, but depending on what precisely you're specifying, may have one.

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

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX BLEH XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

@subsubsection{More Modular than Mixin Inheritance}

For instance, consider a dependency DAG such as follows,
where among other things,
@c{Z} depends on @c{K2} that depends on @c{D} that depends on @c{O}:

@(noindent) @image[#:scale 0.587]{C3_linearization_example.eps}

The only way to compute precedence lists for @c{O}, @c{A}, @c{B}, @c{C}, @c{D}, @c{E}
yields the respective precedence lists @c{[O]}, @c{[A O]}, @c{[B O]}, @c{[C O]}, @c{[D O]}, @c{[E O]}.
No problem.

However, consider the precedence list for @c{K1}.
If computed naively by concatenating the precedence lists
of the prototypes it directly depends on without eliminating duplicates,
you get @c{[K1 C O A O B O]}.
This can be a big problem if re-applying @c{O}
will undo some of the effects of @c{A} or of @c{B}.
The problem is the same for @c{K2} and @c{K3} and only worse for @c{Z}.
Even when all prototypes at stake are idempotent and commute,
this naive strategy will cause an exponential explosion of prototypes to mix
as the graph becomes deeper.
Meanwhile, a proper linearization as given by the C3 algorithm would be
@c{[K1 C A B O]} for @c{K1} and @c{[Z K1 C K3 A K2 B D E O]} for @c{Z}.
It avoids issues with duplicated prototypes, and grows linearly
with the total number of prototypes however deep the graph.

With mixin inheritance, developers would have to manually curate
the order in which they mix prototypes, extra-linguistically.
When using prototypes defined in other modules,
they would have to know not just the prototypes they want to use,
but all the detail about the transitive prototypes they depend on.
Their dependency DAG will not be a hidden implementation detail,
but part of the interface.
And when some upstream module modifies the dependency DAG of a prototype,
all the prototypes in all the modules that transitively depend on it
will have to be updated by their respective maintainers to account for the change.

This requires much more information to understood and provided by developers
than if these developers were instead using multiple inheritance,
that automates the production of that precedence list, and
its update when upstream modules are modified.
The transitive parts of DAG can largely remain a hidden implementation detail
from those developers who only care about some direct dependencies.
Thus, mixin inheritance is indeed less modular than multiple inheritance.


Double inheritance in NewtonScript: very similar to our prototypes + meta^n classes,
with single inheritance each.
https://www.newted.org/download/manuals/NewtonScriptProgramLanguage.pdf
