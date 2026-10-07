#lang scribble/base
@; -*- Scheme -*-
@(require "util/ltuo_lib.rkt")
@(set-chapter-number 8)

@title[#:tag "TfOO"]{Types for OO}
@epigraph{
  Fools ignore complexity. Pragmatists suffer it. Some can avoid it. Geniuses remove it.
  @|#:- "Alan Perlis"|}

@section{Thinking about Types for OO}

@subsection{Dynamic Typing}

The simplest way to deal with static types for OO is not to.
Many OO languages, like Smalltalk, Lisp, Ruby, Python, JavaScript, Jsonnet, Nix, etc.,
adopt this strategy:
the safety of program states is enforced at runtime with checks
that operations deal with the correct type of arguments,
issuing runtime errors when that is not the case
(e.g. calling the “first element in list” function on a number).
A static typist could just say that objects are elements of a monomorphic type @c{Record},
and that all accesses to methods are to be dynamically typed and checked at runtime,
with no static safety.

Advantages of dynamic typing include the ability to express programs that even the best
static typesystems cannot support, especially when state-of-the-art typesystems are too
rigid, or not advanced enough in their OO idioms.
Programs that involve dependent types, staged computation,
metaprogramming, long-lived interactive systems,
dynamic code and data schema evolution at runtime, etc.,
can be and have been written in dynamically typed systems
that could not have been written in existing statically typed languages,
or would have required reverting to unitypes with extra verbosity@xnote["."]{
  Bob Harper famously quipped:
  “A dynamically typed language is a statically typed language with
  only one static type.”
  To which I respond:
  “A statically typed language is a dynamically typed language with
  only one static typesystem.”
  And if that typesystem prevents you from expressing your program,
  it’s a vast hindrance rather than a help.
  Sadly, that is the case of typesystems proposed by the likes of Bob Harper,
  when it comes to writing modular extensible programs.
  As of 2026, the only popular language
  with a decent typesystem when it comes to OO is Scala
  (where I judge “popular” as being in the top 50 in the TIOBE index
  or the GitHub top programming languages @~cite{GitHub2022 TIOBE2026});
  though C++, Java, C#, Kotlin, Swift or TypeScript come close.
}

@subsection{Static Typing}

Types can help reason about programs.
Thinking in terms of types can help understand
what programs do or don’t do and how to write and use them.
Types thin down the space of valid programs in such ways that many common errors
are automatically caught, as by an error-correcting code:
if the error is not too large, the closest correct program can be automatically determined.

Furthermore, system-enforced static types can bring extra performance and safety,
help in refactoring, debugging programs,
some forms of type-directed metaprogramming, and more.
I have already started to go deeper by describing records as indexed products.
Let’s see how to model OO with more precise types.

On the other hand, static types can be onerous to implement,
tricky to get just right, and overly restrictive in what they allow.
And the wrong typesystem is much worse than no typesystem:
it imposes heavy costs with no benefits, and prevents programmers
from expressing their ideas directly.
Trying to fit the typesystem of Pascal, C++ or Java on top of Smalltalk or Lisp
would lead to a vast decrease in productivity as most common and useful idioms
get suddenly rejected by the typesystem.

@subsection{Partial Record Knowledge as Subtyping}

In a language with static types,
programmers writing extensible modular definitions should be able to specify types
for the entities they provide (an extension to) and require (a complete version of),
without having to know anything about the types of the many other entities
they neither provide nor require:
indeed, these other entities will be linked together with their modules into a complete program,
but may not have been written yet, and when they are, may be written by other people.

Now, with only modularity, or only extensibility, what’s more, second-class only,
you could contrive a way for the typechecker to always exactly know all the types required,
by prohibiting open recursion through the module context,
and generating magic projections behind the scenes (and a magic merge during linking).
But as I previously showed in @secref{IoMaE},
once you combine modularity and extensibility, what’s more, first-class,
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
will match the future actual module context, that may satisfy many more constraints;
meanwhile, the “positive” constraints about types for the identifiers being provided
may satisfy many more constraints than those actually required from other modules using it.

@section[#:tag "STfOO"]{Simple Types for OO}

@subsection[#:tag "TNMTfME"]{Trivial Non-Modular Types for Modular Extensions}

Given any typesystem that has functions,
the simplest way to type OO is with these types for Trivial Modular Extensions,
which are just a packaging of the @c{V → C → W} from @secref{ME},
with more explicit names for the variables,
making their role in inheritance more explicit:
@Code{
type TModExt inherited required provided =
  inherited → required → provided
mix : TModExt j r p → TModExt i r j → TModExt i r p
fix : top → TModExt top target target → target
}

The @c{mix} operator chains two modular extensions,
wherein the information (parameter @c{j}) provided by the parent (second argument)
is the information inherited by the child (first argument).
The @c{fix} operator, first takes a top value as a seed,
then second takes a specification for a target with the target itself as module context
and starting with the top value as a seed, and returns the target fixpoint.

In Prototype OO, the value @c{inherited} holds the methods defined so far by the ancestors;
the value @c{provided} consists in new methods and specialization of existing methods;
the top value is an empty record.
In Class OO, that value @c{inherited} holds methods and fields defined so far by the ancestors;
the value @c{provided} consists in the new or specialized fields and methods;
the top value is a type descriptor for an empty record type.
In both cases, the context @c{required} may narrowly define a prototype or class,
but may also more broadly define an entire namespace.

Now, these types are somewhat burdensome, and not modular:
@itemize[
  @item{The modular extensions being composed must agree on the type
    @c{required} or @c{target} of the module context.
    Each extension can be typed and compiled separately only after
    that context type has been specified,
    and cannot be reused with a different context type.
    In some languages with simple and rigid enough types, this means
    details of the target type may have to be wired in every extension,
    that are then not reusable with a different target type.
    Languages with existential types may alleviate this stricture to a point@~cite{Pierce1993}.}
  @item{
    Also, the user must provide a top value for that target type as initial seed for the fixpoint.
    That is simple enough when you can keep @c{top} separate from @c{target} and
    make it a unit type or empty record type.
    Some less-expressive typesystems may require you to identify the @c{top} and @c{target} type,
    in which case the target may have to be a record of fields that each have
    a null, empty or zero value that can be used as a top value;
    the downside being that the type is then “nullable”, and
    either does not distinguish “uninitialized” from “initialized to the default value”,
    or does distinguish them but then allows “uninitialized” at runtime after the fixpoint is computed.
    Either way, using simple types like this does not allow for richly constrained types
    wherein the tight constraints are only satisfied during the extension process itself.}]

Still, those simple types provide a template for better types,
that will typically be some variant of the simple types,
a restriction of them, an abstraction of them, a finite or infinite intersection of them.

@subsection[#:tag "SSfMSI"]{Strict Subtypes for Modular Single Inheritance}

To achieve modular types for OO, you need some notion of subtyping,
and/or intersection of types.
Without subtyping, you might need some kind of dynamic cast that defeats static typechecking,
or some syntactic duplication of definition of extensions
for each concrete context in which they will be used.
With subtyping, modular extensions can actually be modularly defined, composed and instantiated,
without unsafe cast, and without code duplication.
Subtyping increases modularity,
because a type now also stands for all its subtypes, past and future,
valid in all future instantiation contexts.

A subtype constraint @c{x ⊂ y}
(sometimes written @c{x ≤ y} or in ASCII @c{x <: y}
in the literature and in various languages) signifies that @c{x} is a subtype of @c{y},
that every element of @c{x} is also an element of @c{y}.
An intersection type @c{x ∩ y} is the largest type @c{z} such that
every element of @c{z} is also an element of @c{x} and an element of @c{y}.
We always have @c{(x ∩ y) ⊂ x} and @c{(x ∩ y) ⊂ y}.

Furthermore, usual OO with records as targets requires those features
to properly apply to indexed products for records,
such that a record type with extra fields is a subtype of the record type with given fields,
and a record type whose field types are subtypes of those of another is also a subtype.
For record-based extensions, row polymorphism @; TODO cite
is one way to abstract over extra fields whose presence is determined by the context of use.
It complements the inheritance types discussed here,
but is not required by inheritance itself.

Here is how I can define a type for Simple Strict Modular Extensions
in subtype-style, where @c{c ⇒ t} indicates a type @c{t} under a constraint @c{c}.
The strictness is expressed by the variable @c{provided} being constrained
to be a subtype of @c{inherited};
remove that constraint and you’re back to regular modular extensions:
@Code{
type SModExt⊂ inherited required provided =
  provided ⊂ inherited ⇒
  inherited → required → provided
mix : parentProvided ⊂ childInherited,
      required ⊂ childRequired,
      required ⊂ parentRequired,
      provided ⊂ inherited ⇒
        SModExt⊂ childInherited childRequired provided →
        SModExt⊂ inherited parentRequired parentProvided →
        SModExt⊂ inherited required provided
fix : self ⊂ required, provided ⊂ self ⇒ top → SModExt⊂ top required provided → self
}

Intersection style requires types to constitute a meet-semilattice,
which is stronger than just requiring them to constitute a partial order.
Because intersection style is stronger, you can also use subtype constraints
while using intersection style.
Many uses of intersections can instead be expressed through fresh type variables
with subtyping constraints, when only the common-subtype conditions are needed.
These constraints do not by themselves identify the greatest common subtype.
Even then, intersections make for simpler expressions and easier intuition.
Therefore, in the rest of this chapter, I will just assume intersection style
and use subtype constraints where only common-subtype conditions are needed.

In intersection style, the strictness is expressed by the result type
being the intersection of the @c{providedExtension} type and the @c{inherited} type,
thus a subtype of @c{inherited}.
Whereas in subtype style, @c{provided} covers all the information being returned,
in intersection style, @c{providedExtension} only covers the new information.
Note that the type @c{top} used with @c{fix} is usually chosen in practice at instantiation time
such that @c{top∩target = target}.
Here are my types in intersection style;
the strictness is in the intersection of the result with @c{inherited},
that if omitted (returning just @c{providedExtension}) leads back to regular modular extensions:
@Code{
type SModExt∩ inherited required providedExtension =
  inherited → required → (inherited∩providedExtension)
mix : SModExt∩ i∩p s q → SModExt∩ i r p → SModExt∩ i r∩s p∩q
fix : top → SModExt∩ top top∩target target → top∩target
}

In both styles above, the types refine the @c{V → C → V} from @secref{MOI}:
@c{inherited} and @c{provided} each separately refine the value @c{V} being specified;
that value can be anything: it need not be a record at all, and if it is,
it can have any shape or type, and need not have the same as the module context.
Meanwhile, @c{required} refines the module context @c{C}, and is (almost) always some kind of record.
The two need not be the same at all, and usually are not for (open) modular extensions,
unless and until you’re ready to close the recursion, tie the loops and compute a fixpoint.

The Simple Strict Types for Modular Extensions above work well
when following a discipline of single inheritance:
the @c{inherited} type parameter encodes all the information mixed “to the right”
of the current modular extension,
and the @c{required} and @c{provided} parameters can be inferred
with all the information available.
Using let-polymorphism, you might define a named mixin and
apply it to several single-inheritance chains,
with independent type instantiations at each use.
Computing a fixpoint requires establishing the relevant recursive type constraints,
but the resulting type may still contain generalized parameters.

@subsection[#:tag "SrSfMMI"]{Stricter Subtypes for Modular Mixin Inheritance}

With mixin inheritance, the @emph{effective} @c{super} information
passed “from the right” of a modular extension may vary,
and must be preserved and passed-through for processing by further modular extensions.
However, it is guaranteed to contain @emph{at least} the @c{inherited} information
expected by the current modular extension.
The @c{inherited} parameter in a modular extension’s type will thus not represent
the exact information passed as @c{super}, but a supertype thereof.
Similarly, the @emph{effective} @c{self} information passed as context may vary,
and contain more than the information known to be @c{required} by the current modular extension;
the @c{required} parameter in a modular extension’s type will thus not represent
the exact information passed as @c{self}, but a supertype thereof.
This especially matters if the same mixin variable or expression is to be mixed
with mixins of multiple different types.
The polymorphic modular extension types for such modular extensions and their primitives
are as follows:
@Code{
type PModExt inherited required providedExtension =
  ∀ super, self : Type
    self ⊂ required, super ⊂ inherited ⇒
    super → self → (super∩providedExtension)

mix : PModExt (j∩p) s q → PModExt i r p →
  PModExt (i∩j) (r∩s) (p∩q)
fix : top → PModExt top (top∩target) target → (top∩target)
}

Note how the parameters @c{i} and @c{j} can be used somewhat independently,
when they had to be combined into the single parameter @c{i} in @c{SModExt∩};
that’s an expression of modularity at the type-level.
Furthermore, the universal quantification (@c{∀}, forall)
of @c{super} and @c{self} ensures that the modular extension
can be defined once, and later be used in any way that satisfies the type dependencies.
You can now further abstract over the context of use of a mixin.

Type experts may also note how
the result type @c{super∩providedExtension} guarantees
preservation of the effective inherited interface.
Quantification over @c{super} further guarantees
that any information not named in @c{inherited} @emph{must}
be preserved and passed through, at least as far as type information goes.
Furthermore, by parametricity, @; TODO @~cite{Reynolds1983 Wadler1989}
and setting aside divergence, errors and reflection,
the inherited value is the only source of information the extension can inspect
outside its declared interface.
This would justify the idiom where the body of a method specification
“uses @c{(super method-id)} as a default when no overriding behavior is specified”,
that I mentioned in @secref{MOI}.
I believe this can be made a precise free theorem:
given suitable language restrictions on how methods may be defined,
an extension can override a finite number of methods,
possibly including all those in the explicitly inherited set,
while methods outside the interface it can inspect are passed through.
This restriction on extensions (and useful theorem for whoever analyses them)
is lifted if the language allows for runtime reflection on records and their available identifiers,
at which point an extension might respect the types yet
intercept methods not covered by the @c{inherited} type.
But many statically typed languages do not allow such reflection without using special extensions
(such as constraints on the typeclass @c{Data.Data.Data} in Haskell).

@subsection[#:tag "TMI"]{Typing Multiple Inheritance}

The precise meaning of specifications in multiple inheritance and optimal inheritance
depends crucially on the outcome of the linearization algorithm.
This poses challenges in assigning types to specifications:
either you must precisely encode this linearization at the type-level,
or you must ensure that the correctness of an imprecise type
does not depend on the exact linearization
(or, if using flavorless multiple inheritance, the exact conflict resolution strategy).

One simple way to achieve the precise type is to indeed expand away
the inheritance at compile-time, by encoding the linearization or conflict resolution
using templates or type-level metaprogramming,
reducing it to single inheritance or to flat structures without inheritance.
Subtype polymorphism is monomorphized away and only instances of concrete classes are typed,
at which point a precise monomorphic type is known.
See for instance @~cite{Rideau2026cxx}.
This strategy works because classes are second-class, and ancestry is a compile-time constant;
the strategy is not available for first-class OO.

As for imprecise types, they impose on programmers the discipline
of having the types for specifications constitute a meet-semilattice,
the meet being type intersection.
Intersection commutes, associates—thus it contributes the same type information
to the effective modular extension and the target,
regardless of the ancestor linearization order.
Intersection is also idempotent—and so it is no problem that
the type information for an ancestor is redundantly stated
in all descendants along each “diamond”.
The defining conditions of a semilattice are thus exactly those required
to type specifications as you define them, and extend them into further specifications;
these further specifications can only refine the overall intersection,
never invalidate the type information already derived;
that is what makes the typing itself both modular and extensible.
This is a recurring trade.
@principle{When type precision is sacrificed,
the correctness of the type approximation is turned into
a constraint on what programmers are allowed to do}:
while the exact runtime behavior of a specification
@emph{will} depend on the linearization order,
its compile-time type will not and must not.

The imprecise types then look like the @c{PModExt} I offered for mixin inheritance,
except the @c{i r p} parameters will encode
not only the types induced by the current modular extension,
but also the transitive intersection of the types induced
by all the specification’s ancestors.
Here is how I would encode those types in what is increasingly pseudo-code,
where
@c{Tag} is the type of tag for the specification identities,
@c{DAG} is a type of DAGs over some set of labels that is a finite subset of @c{Tag},
and @c{lol} is the local order over those labels, and @c{Vertex(lol)}
the type containing only the vertices of @c{lol}.
@c{pr}, @c{pi}, @c{pp} are the @c{lol}-indexed families of
@c{r}, @c{i}, @c{p} type parameters for the parents of the current specification,
@c{⋂ x} is the intersection of types in the type family @c{x}, and
@c{Π i:I . T} is the dependent product of types @c{T},
type of expressions that may depend on @c{i} for each index @c{i} in @c{I},
@c{Σ i:I . T} is the dependent sum of types @c{T},
type of pairs of an index @c{i} and an element of @c{T} at @c{i}.
Note how the polymorphic strictness of the modular extensions
is essential in ensuring that the resulting type does not depend
on the exact linearization order:
@Code{
type MISpec i r p =
  Σ lol : DAG .
  Σ pr pi pp : Vertex(lol) → Type .
  Σ pe : Type .
  p = pe ∩ ⋂ pp,
  r ⊂ ⋂ pr,
  i ∩ ⋂ pp ⊂ ⋂ pi ⇒
  { modExt : PModExt (i ∩ ⋂ pp) r pe ;
    parents : Π l : Vertex(lol) . MISpec (pi l) (pr l) (pp l) ;
    tag : Tag }}

The sketch above summarizes the interfaces contributed and required by the ancestors,
but does not fully validate their ordered composition:
The constraint @c{i ∩ ⋂ pp ⊂ ⋂ pi} checks that inherited requirements
are covered by the initial information and the combined ancestor contributions;
but it does not check that each contribution is available before it is required.
Indeed, intersection makes accumulated type information independent of order,
but does not make the availability of inherited inputs independent of order.
A complete treatment must therefore also validate these requirements
against the chosen linearization.
For statically known ancestry, this can be checked after computing the linearization,
as in the previous “precise” strategy;
for first-class specifications, it requires additional static evidence
or runtime validation supported by suitable representations of the contracts.
I leave that additional obligation outside the type sketch above.

@subsection[#:tag "LoST"]{Limitations of Simple Types}

The preceding modular-extension types soundly describe composition and preservation,
and the @c{MISpec} sketch additionally summarizes ancestry,
subject to the validation obligation just discussed.
Variants of them have often been independently reinvented
by many a programming language researcher. @; TODO cite
Now, given a parent modular extension with type @c{PModExt i r p},
composing it with a child modular extension in the same context @c{r}
may yield type @c{PModExt i r q} where @c{q} is a subtype of @c{p},
extended with the information contributed by the child.
However, the context is not preserved when computing a fixpoint:
instead, changes to @c{p} will feed back into @c{r} and into the @c{i} of further extensions;
this feedback can lead to a type @c{PModExt j s qq} whose relationship to @c{PModExt i r p} is unclear.

Moreover, the inferences and analyses made for a specification and its target
cannot in general be shared with an extension thereof and its own target,
that has its own fixpoint, hence its own @c{r} parameter.
Each concrete instantiation of a specification requires its own resolution to a self-type;
hopefully achieved automatically, but if not, manually by the programmer. @; TODO cite Pierce???
For the same reason, optimizations made, dynamic checks eliminated, cannot in general
be shared across separately-analyzed and especially separately-monomorphized branches of the code.
The @c{PModExt} formulation quantifies over effective self types,
but its interface parameters remain fixed within that quantification, and do not vary with self,
which limits the relationships expressible through this fixed contract.

Wouldn’t it be nice if there were a simple way to reflect inheritance into the typesystem,
such that you could analyze a specification once, and the analysis would directly apply
to extensions to the specification?
Thus you could regain some of the modularity that OO is supposed to help provide?

@section[#:tag "NNOOTT"]{The NNOOTT: Naïve Non-recursive OO Type Theory}

@subsection[#:tag "OST"]{Obvious Simple Theory}

Well, @citet{Hoare1965}, and for decades his successors,
already had a theory that wholly simplified the issue of typing OO,
that I will dub the Naïve Non-recursive Object-Oriented Type Theory (NNOOTT):
it consists in considering subprototyping / subclassing (a relation between specifications)
as the same as subtyping (a relation between targets).
Thus, in this theory, a subclass, that extends a class with new fields,
is (supposedly) a subtype of the parent “superclass” being extended@xnote["."]{
  The theory is implicit in the names of the @c{is} operator in C#,
  of the @c{typep} and @c{subtypep} predicates in Common Lisp,
  of the @c{is-a} vs @c{has-a} relations in many OO modeling publications,
  @; TODO Frames, semantic networks
  @; TODO check Bertrand Meyer books, Grady Booch, James Rumbaugh, GoF, UML (Booch, Rumbaugh, Jacobson).
  @;{ XXX Eiffel, Java, Smalltalk }
}

In my above model, suppose a parent specification @c{A}
produces a target of type @c{a} when independently instantiated,
and its extension @c{(mix B A)} produces a target of type @c{b}.
The NNOOTT infers @c{b ⊂ a} from this inheritance relationship alone,
without accounting for the parent interface’s possible dependence
on the different effective self types.
The types of descendants are subtypes of those of ancestors.
Type analyses and type-directed optimizations can be reused. All is well.

The NNOOTT is simple and intuitive, and
even has good didactic value to explain how inheritance works:
infer from inheritance that the target of a specification
is the subtype of the target of its parent or ancestor specifications.
The model accurately captures most simple uses of OO—indeed,
most common introductory examples to OO, including
points, shapes, animals, vehicles, employees, bank accounts, or GUI widgets.
You start from a record or record type, add a few fields to it,
valued in some constant type that doesn’t refer to “this” current type
(because why make things needlessly complicated?),
and there you are, with a subtype of the parent type.
The theory is obviously true in all these cases where you add one or a few simple fields,
surely by induction it’s true always?

However, this “Naïve Non-recursive OO Type Theory”, as the name indicates,
is a bit naïve indeed:
its conclusion holds in non-recursive cases,
and can even be extended in some further useful cases,
but does not hold in general.
Yet the NNOOTT is important to understand,
both for the simple cases it is good enough to cover,
and for its failure modes that tripped so many good programmers
into wrongfully trying to equate inheritance and subtyping.

@subsection[#:tag "LotN"]{Limits of the NNOOTT}

The NNOOTT works well in the non-recursive case, i.e.
when the types of fields do not depend on the type of the module context;
or, more precisely, when there are no circular “open” references between
the type a modular extension provides
and the type it inherits from the super it extends
or the type it requires from its module context.
In his paper on objects as coalgebras,
Bart Jacobs characterizes the types for the arguments and results of his methods
as being “(constant) sets” @~cite{Jacobs1995},
which he elaborates in another paper @~cite{Jacobs1996InheritanceAC}
as meaning “not depending on the ‘unknown’ type X (of self)@xnote[".”"]{
  Jacobs (@secref{OOinDM}) is particularly egregious in smuggling this all-important restriction
  to how his paper fails to address the general and interesting case of OO
  in a single word, furthermore, in parentheses, at the end of section 2,
  without any discussion whatsoever as to the momentous significance of that word.
  A discussion of that significance could in itself have turned this bad paper into a stellar one.
  Instead, the smuggling of an all-important hypothesis makes the paper misleading at best.
  His subsequent paper @~cite{Jacobs1996InheritanceAC}
  has the slightly more precise sentence I also quote,
  and its section 2.1 tries to paper over what it calls “anomalies of inheritance”
  (actually, the general case), by separating methods into a “core” part
  where fields are declared, that matter for typing inheritance,
  and for which his hypothesis applies, and “definitions” that must be reduced to the core part.

  The conference reviewing committees really dropped the ball on accepting those papers,
  though that section 2.1 was probably the result of at least one reviewer doing his job right.
  Did reviewers overall let themselves be impressed by formalism beyond their ability to judge,
  or were they complicit in the sleight of hand to grant their domain of research
  a fake mantle of formal mathematical legitimacy?
  Either way, the field is rife with bad science,
  not to mention the outright snake oil of the OO industry in its heyday:
  In the late 1980s, every new software product was claiming to be “object-oriented” @~cite{King1989},
  and in the 1990s, IBM would even hire comedians to become “evangelists”
  for their Visual Age Smalltalk technology, soon recycled into Java evangelists.

  Jacobs is not the only one, and he may even have extenuating circumstances.
  He may have been ill-inspired by Goguen (@secref{Goguen}),
  whom he cites, who also abuses the terminology from OO to make his own valid but loosely-related
  application of Category Theory to software specification.
  Jacobs may also have been pressured to make his work “relevant” by publishing in OO conferences,
  under pain of losing funding, and
  he may have been happy to find his work welcome even though he didn’t try hard,
  trusting reviewers to send stronger feedback if his work hadn’t been fit.
  The reviewers, unfamiliar with the formalism,
  may have missed or underestimated the critical consequences of a single word;
  they may have hoped that further work would lift a limitation
  they didn’t understand was essential to the approach.
  In other times, researchers have been hard pressed to join the bandwagon of
  Java, Web2, Big Data, Mobile, Blockchain or AI, or whatever trendy topic of the year;
  and reviewers for the respective relevant conferences may have welcomed
  newcomers with unfamiliar points of view.

  Even Barbara Liskov, future Turing Award recipient, was invited to contribute to OO conferences,
  and quickly dismissed inheritance to focus on her own expertise,
  which involves modularity without extensibility—and stated
  her famous “Liskov Substitution Principle” as she did @~cite{Liskov1987};
  brilliant, though not OO.
  That said, she did use the word “object-oriented” in print as far back as @citet{Jones1976}
  to describe her style of programming, a few months before Bobrow published
  the memo on KRL-0 that first used it right,
  so she did have a stake in the name, though
  her definition happily didn’t prevail.
  @citet{Wegner1987} rightfully calls it “object-based” but not “object-oriented”.

  Are those who talk and publish what turns out not to be OO at all at OO conferences,
  or those who invite them to talk and publish, being deliberately misleading?
  Probably not. Yet the public can be fooled just the same as if dishonesty were meant:
  though the expert of the day can probably make the difference,
  the next generation attending or looking through the archives
  may well get confused as to what OO is or isn’t about as they learn from example.
  At the very least, papers like that make for untrustworthy identification and labeling
  of domains of knowledge and the concepts that matter.
  The larger point here being that one should be skeptical of papers,
  even by some of the greatest scientists
  (none of Jacobs’, Goguen’s nor Liskov’s expertise is in doubt),
  even published at some of the most reputable conferences in the field (e.g. OOPSLA, ECOOP),
  because science is casually corrupted by power and money,
  and only more cheaply so for the stakes being low.

  This particular case from decades ago is easily corrected in retrospect;
  its underlying lie was of little consequence then and is of no consequence today;
  but the system that produced dishonest science hasn’t been reformed,
  and I can but imagine what kind of lies it produces to this day in topics
  that compared to the semantics of OO are both less objectively arguable,
  and higher-stake economically and politically.
}
This makes his paper inapplicable to most OO, but interestingly,
identifies a common subset of OO for which inheritance coincides with subtyping:
under the stated restrictions, the target of an extended specification
is a subtype of the target of the unextended specification.

The result can be extended to also work in the presence of such “open” references,
when they all occur in “positive” positions such that
information added only feeds back positively through the fixpoint.
@emph{Strict} extensions that satisfy this property are called “covariant”.
Extensions where all open references occur in “negative” positions (such as function arguments)
are called “contravariant”.
Those with open references in both kinds of positions are called “invariant”.
Finally, those strict extensions that contain no open references,
I will call “constant” like Jacobs above.
With suitable interpretation of (least) fixpoints for types,
it can be proven that, under the monotonicity hypotheses stated in the exercise below,
given two strict covariant extensions A and B,
the target @c{(fix top (mix B A))} is a subtype of the target @c{(fix top A)},
i.e. subclassing implies subtyping for classes defined covariantly only.

Now, the @emph{general} case is “invariant”, and while the other cases make for fun papers
and useful optimizations, they are a distraction as to how to lay the semantic foundations
for understanding the meaning of OO.

Indeed, in general, specifications may contain so-called “binary methods”
that take another value of the same target type as argument,
such as in very common comparison functions (e.g. equality or order)
or algebraic operations (e.g. addition, multiplication, composition), etc.
And beyond these already common methods,
any occurrence in a negative position will break the precondition
for the theorem that deduces subtyping from subclassing.
This includes occurrences in the setters of mutable fields.
And, in typeclass style (@secref{CSvTS}), this includes the arguments to constructors;
this matters when an algorithm involves constructing elements of the described type,
and not just consume existing ones.

Such methods are not an “advanced” or “anomalous” case, but quintessential.
Indeed the very first example in the very first paper about actual classes @~cite{Dahl1967},
involves recursive data types:
it is a class @c{linkage} that defines references @c{suc} and @c{pred} to the “same” type,
that classes can inherit from so that their elements shall be part of a doubly linked list.
This example, and any mutable data structure defined using recursion,
as well as any binary method, or contravariant or invariant extension,
will provide counterexamples to the general inference from inheritance to target subtyping.
Not only is such recursion a most frequent occurrence, I showed above in @secref{IoMaE} that
while you can eschew support for fixpoints through the module context
when considering modularity or extensibility separately,
open recursion through module contexts becomes essential when considering them together.
In the general and common case in which a class or prototype specification
includes self-reference, subtyping and subclassing are very different,
a crucial distinction that was first elucidated in @citet{Cook1989Inheritance}.

Now, one simple trick to “save” the NNOOTT is
to reserve static typing to non-self-referential methods,
while self-references are given constant interface types:
wherever a recursive self-reference to the whole would happen, e.g. in the type of a field,
programmers must instead declare the value as being of a dynamic “Any” type,
or some fixed base type (that doesn’t vary with inheritance),
so that there is no self-reference in the type, and the static typechecker is happy.
Thus, in a class @c{A} binary method @c{add : A → A}
(with the current element of type @c{A} as implicit first argument),
a subclass @c{B} would not have a binary method with @c{add : B → B}
with types that track the subclass, but still a binary method @c{add : A → A},
that would need to typecheck its argument for being of type @c{B} and downcast it
to use any B-specific information,
while whoever uses the results would also have to check that they are indeed of type @c{B}
and downcast them (again, to use any B-specific information from it).
Actually, with proper language support, you could make it so designated positive occurrences
may track the subclass, and the inherited method would then be @c{add : A → B},
and only the negative occurrence in an argument requires a dynamic typecheck and downcast.

@; TODO: we can improve the above by only adding dynamic checks in contravariant positions.
@; Check: as in Eiffel ? BETA ? C++ ? Java ? C# ?

In a language with support for dynamic types at runtime,
the programmer can declare methods with overly permissive types,
then compensate for the lack of a precise-enough type in the static typesystem
by using dynamic checks, explicit dereferences, typecasts (downcasts),
or (safe or unsafe) coercions.
Programs without type errors will have to pay for those extra checks at runtime,
while those with type errors will raise an error at runtime (or, in the unsafe case, misbehave).
In some languages, self-reference already has to go through
pointer indirection (e.g. in C++);
in others, through an explicit type-level wrapping step
(e.g. in Haskell, with a @c{newtype Fix} for fixpoints
which introduces one level of syntactic and type-level wrapping and unwrapping
every time you access the fixpoint,
while the open modular definition goes into a “generator”;
the (un)wrapping step is erased at compile time,
the runtime indirection coming instead from Haskell’s ordinary boxed lazy values).
Thus saving the NNOOTT may require dynamic checks to recover subclass-specific information
lost due to weakening the static self-type contract;
but examining where those checks go reveals that,
in the representations discussed above,
recursion itself already involves indirection even without such dynamic checks.
In other words, it makes us realize once again that @emph{recursion is not free} (@secref{RC}).

@subsection{Why NNOOTT?}

The NNOOTT was implicit in the original proto-OO paper @~cite{Dahl1967}
as well as in Hoare’s seminal paper that inspired it @~cite{Hoare1965}@xnote["."]{
  Hoare probably intended subtyping initially indeed for his families of record types;
  yet subclassing is what he and the Simula authors discovered instead.
  Such is scientific discovery:
  if you knew in advance what lay ahead, it would not be a discovery at all.
  Instead, you set out to discover something, but usually discover something else,
  that, if actually new, will be surprising when you eventually realize the mismatch.
  The greater the discovery, the greater the surprise.
  And you may not realize what you have discovered until analysis is complete much later.
  The very best discoveries will then seem obvious in retrospect,
  given the new understanding of the subject matter,
  and familiarity with it due to its immense success.
  And yet, there may be a lot of resistance initially against recognizing the discrepancy,
  which ironically is felt as a mistake by the discoverers of the new phenomenon,
  or by those of the discrepancy, or by the reviewers of either or both,
  and by topic experts in general...
  even though it is the symptom that there was quite an interesting discovery indeed!
}
It then proceeded to dominate the type theory of OO
until debunked in the late 1980s @~cite{Cook1989Inheritance}.
Even after that debunking, it has remained prevalent in popular opinion,
and still very active in academia and industry alike,
and continually reinvented even when not explicitly transmitted
@~cite{Cartwright2013 AbdelGawad2014}.
I readily admit it’s a naïve belief I too had
when I first tried to put types on my modular extensions.

The reasons why, despite being inconsistent, the NNOOTT was and remains so popular,
not just among the ignorant masses, but even among luminaries in computer science,
is well worth examining.

@itemize[
@item{
  The NNOOTT directly follows from the confusion between specification and target
  when conflating them without distinguishing them (@secref{PaC}).
  The absurdity of the theory also follows from the category error of equating entities,
  the specification and its target, that
  not only are not equivalent, but are not even of the same type.
  But no one @emph{intended} for “a class” to embody two very distinct semantic entities;
  quite on the contrary, Hoare, as well as the initial designers of
  Simula, KRL, Smalltalk, Director, etc.,
  were trying to have a unified concept of “class” or “frame” or “actor”, etc.
  Consequently, the necessity of considering the clumping together of two distinct entities
  was only fully articulated in the 2020s(!).}
@item{
  In the 1960s and 1970s, when both OO and type theory were in their infancy,
  and none of the pioneers of one were familiar with the other,
  the NNOOTT was a good enough approximation that even top language theorists were fooled.
  Though the very first example in OO could have disproven the NNOOTT,
  still it requires careful examination and familiarity with both OO and Type Theory
  to identify the error, and pioneers lacked the joint familiarity and
  had more urgent problems to solve.}
@item{
  The NNOOTT actually works quite well in the simple “non-recursive” case
  that I characterized above.
  In particular, the NNOOTT makes sense enough
  in the dynamically typed languages that (besides the isolated precursor Simula)
  first experimented with OO in the 1970s and 1980s,
  mostly Smalltalk, Lisp and their respective close relatives.
  In those languages, the “types” sometimes specified for record fields
  are often but suggestions in comments, dynamic checks,
  sometimes promises made by the user to the compiler;
  and if they are actual static guarantees that only work outside of the recursive case,
  well, that is already most of the benefit of static guarantees
  when most of the work is not that recursive case.
  It takes an advanced functional programming language or style,
  or a rare care for total correctness, for the recursive case to dominate the issue,
  which was never a mainstream concern.}
@item{
  In the 1980s and 1990s, theorists and practitioners, being mostly disjoint populations,
  did not realize that they were not talking about precisely the same thing
  when talking about a “class”.
  Those trained to be careful not to make category errors
  might not have realized that others were doing it in ways that mattered.
  The few at the intersection may not have noticed
  the discrepancy, or understood its relevance, when scientific modeling
  must necessarily make many reasonable approximations all the time.
  Once again, more urgent issues were on their minds.}
@item{
  Though the NNOOTT is inconsistent in the general case of OO,
  as obvious from quite common examples involving recursion,
  it will logically satisfy ivory tower theorists or charismatic industry pundits
  who never get to experience cases more complex than textbook examples,
  and pay no price for dismissing recursive cases as “anomalies” @~cite{Jacobs1996InheritanceAC}
  when confronted with them.
  Neither kind owes their success to getting a consistent theory
  that precisely matches actual practice.}
@item{
  The false theory will also emotionally satisfy those practitioners and their managers
  who care more about feeling (or looking) like they understand rather than actually understanding.
  This is especially true of the many who have trouble thinking about recursion,
  as is the case for a majority of novice programmers and vast majority of non-programmers.
  Even those who can successfully @emph{use} recursion,
  might not be able to @emph{conceptualize} it, much less criticize a theory of it.
  @;{ TODO locate study that measures the recursion-ables from the unable. }
}]

@section[#:tag "BtN"]{Beyond the NNOOTT}

@subsection[#:tag "ST"]{Self Types}

@subsubsection{Deconfusing Types for Specification and Target}
The key to fully dispelling the
“conflation of subtyping and inheritance” @~cite{Fisher1996}
or the “notions of type and class [being] often confounded” @~cite{Bruce1996}
is indeed first to have dispelled, as I just did previously,
the conflation of specification and target.
Thereafter, OO semantics becomes simple:
by recognizing target and specification as distinct,
one can take care to always treat them separately,
which is relatively simple,
at the low cost of unbundling them before processing,
and rebundling them together afterwards if needed.
Meanwhile, those who insist on treating them as a single entity with a common type
only set themselves up for tremendous complexity and pain (as did the authors cited above).

That is how I realized that what most people actually mean by “subtyping” in extant literature is
@emph{subtyping for the target of a class} (or prototype),
which is distinct from
@emph{subtyping for the specification of a class} (or prototype),
variants of the latter of which Kim Bruce calls “matching” @~cite{Bruce1997}.
@; TODO cite further
But most people, being confused about the conflation of specification and target,
fail to conceptualize the distinction, and either
try to treat them as if they were the same thing,
leading to logical inconsistency hence unsafety and failure;
or they build extremely complex calculi to do the right thing despite the confusion.
By having a clear concept of the distinction,
I can simplify away all the complexity without introducing inconsistency.

One can use the usual rules of subtyping @~cite{Cardelli1985} @; TODO cite
and apply them separately to the types of specifications and their targets,
knowing that “subtyping and fixpointing do not commute”,
or to be more mathematically precise,
@emph{fixpointing does not distribute over subtyping},
or, said otherwise, @principle{the fixpoint operator is not monotonic}
with respect to pointwise subtyping:
If @c{F} and @c{G} are parametric types,
i.e. type-level functions from @c{Type} to @c{Type},
and @c{F ⊂ G} (where @c{⊂}, sometimes written @c{≤} or @c{<:},
is the standard notation for “is a subtype of”,
and for elements of @c{Type → Type} means @c{∀ t, F t ⊂ G t}),
it does not necessarily follow that @c{Y F ⊂ Y G}
where @c{Y} is the fixpoint operator for types.

@subsubsection{Reconstructing Visibility Rules}
The widening rules for the types of specification
and their fixpoint targets are different;
in other words, forgetting a field in a target record, or some of its precise type information,
is not at all the same as forgetting that field or its precise type in its specification
(which introduces incompatible behavior with respect to inheritance,
since extra fields may be involved as an intermediate step in the specification,
and must be neither forgotten, nor overridden with fields of incompatible types).

If a language treats two entities as a single one syntactically and semantically,
as all OO languages seem to have done so far, @; ALL??
then its typesystem will have to encode in a weird way a pair of subtly different types
for each such entity, and the complexity will have to be passed on to the user:
an object, and each of its fields, will have two related but different declared types,
and potentially different visibility settings.
Doing this right involves a lot of complexity, both for the implementers and for the users,
at every place that object types are involved,
either specified by the user, or displayed to the user.
Then again, some languages may do it wrong by trying to have the specification fit the rules
of the target (or vice versa), leading to inconsistent rules and consistently annoying errors.

A typical way to record specification and target together is to annotate fields with visibility:
@c{public} (visible in the target)
yet possibly with a more specific type in the target
than in the specification (to allow for further extensions that diverge from the current target);
fields marked @c{protected} (visible only to extensions of the specification, not in the target);
and fields marked @c{private} (not visible to extensions of the specification,
even less so to the target; redundant with just defining a variable in a surrounding @c{let} scope).
@principle{The visibility annotations of mainstream OO languages
become intelligible when you unbundle their conflated meaning for a specification and its target.}
You can retrieve the familiar notions from C++ and Java just by reasoning from first principles
and thinking about distinct but related types for a specification and its target.
And if you didn’t conflate them and their types,
you could just use simpler visibility annotations independently on specification and target.

Now, my opinion is that it is actually better to fully decouple the types
of the target and the specification, even in an “implicit pair” conflating the two:
Indeed, not only does that mean that types are much simpler, it also means that
intermediate computations, special cases to bootstrap a class hierarchy,
transformations done to a record after it was computed as a fixpoint, and
records where target and specification are out of sync
because of effects somewhere else in the system, etc.,
can be safely represented and typed,
without having to fight the typesystem or the runtime.

@subsubsection[#:tag "AoSDI"]{Abstracting over Self-Dependent Interfaces}

To describe the feedback relationship between self as provided result
and self as required context, a more precise view of a modular extension is thus as
an entity parameterized by the varying type @c{self} of the module context
(that Bruce calls @c{MyType} @~cite{Bruce1996 Bruce1997}). @; TODO cite further
As compared to the previous parametric type @c{PModExt} that is parameterized by types @c{i r p},
this parametric type @c{ModExt} is itself parameterized by parametric types @c{i r p}
that each take the module context type @c{self} as parameter@xnote["."]{
  The letters @c{r i p}, especially if reordered,
  by contrast to the @c{s t a b} commonly used for generalized lenses,
  suggest the mnemonic slogan: “Generalized lenses can stab, but modular extensions can rip!”
}
I am not competent to make claims with respect to how easy or hard it is
to infer this kind of types:
@Code{
type ModExt (inherited : Type → Type)
            (required : Type → Type)
            (providedExtension : Type → Type) =
  ∀ super, self : Type
    self ⊂ required self, super ⊂ inherited self ⇒
        super → self → super∩(providedExtension self)
}

Notice how the type @c{self} of the module context
is @emph{recursively} constrained by @c{self ⊂ (required self)}.
This recursion matters inasmuch as the @c{required} operator does vary with its parameter,
its body literally including @c{self}-references.
Meanwhile, the type @c{super} of the value in focus being extended
is constrained by @c{super ⊂ (inherited self)},
but also appears in the returned value of type @c{super ∩ (providedExtension self)}.
There again, @c{self} (and not e.g. @c{super}) is used as the parameter:
self-references refer to the same fixpoint of the complete specification,
not to a supertype thereof, and not to the fixpoint of an ancestor specification.
That’s how the “extreme late binding” of Kay is translated into types.

Recursive constraints could already arise in particular instantiations
of the previous modular-extension types:
When a modular extension mentioned the target type (from the information it requires)
in the values it produces (from the information it provides),
the type analysis for its fixpoint could already produce a recursive network of type constraints.
The difference is that in this section, those networks of equations and inequations
can now be abstracted into a variable @c{required}, and
instead of discussing a particular type’s dependency on @c{self},
I can now abstract over that dependency as a type-level function.

Finally, notice how, as with the simpler modular extension types above,
the types @c{self} and @c{required self} refer to the module context
(and the part of it required by the extension),
whereas the types @c{super}, @c{inherited self} and @c{providedExtension self}
refer to some value in focus: the actual value to be extended,
the part specifically used by this extension, and the newly provided extensions to it.
The context and the value in focus needn’t at all be the same
in a general open modular extension;
but of course they will be the same in a complete modular extension
ready for instantiation via a fixpoint operator.

My two OO primitives then have the following types:
@Code{
fix : ∀ inherited, required, providedExtension : Type → Type,
      ∀ self, top : Type,
      self = inherited self ∩ providedExtension self,
      self ⊂ required self,
      top ⊂ inherited self ⇒
        top → ModExt inherited required providedExtension → self

mix : ModExt j∩p s q → ModExt i r p → ModExt i∩j r∩s p∩q
}

In the @c{fix} function, I implicitly define a fixpoint @c{self}
via suitable recursive subtyping constraints.
I could instead replace the first constraint with a definition
@c{self = Y (inherited ∩ providedExtension)}
and check the two subtyping constraints about @c{top} and @c{required}.
As for the type of @c{mix}, it looks the same as
the type I previously offered, except with @c{PModExt} replaced by @c{ModExt}.
However, there is an important though subtle difference:
with @c{ModExt}, the arguments being intersected
are not of kind @c{Type} as with @c{PModExt},
but @c{Type → Type}, where
given two parametric types @c{f} and @c{g},
the intersection @c{f∩g} is defined by @c{(f∩g)(x) = f(x)∩g(x)}.
Indeed, the intersection operation is defined polymorphically by induction on kinds,
so that it applies to operators of any arity that return types
that can then be intersected pointwise.

@subsection{Advantages of Typing OO as Modular Extensions}

By defining OO in terms of the λ-calculus, indeed in two definitions @c{mix} and @c{fix},
I can do away with the vast complexity of “object calculi” of the 1990s,
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
Indeed, defining OO types in terms of LaTeX deduction rules for ad hoc OO primitives
is just programming in an informal, bug-ridden metalanguage that few are familiar with,
with no tooling, no documentation, no tests, no actual implementation,
and indeed no agreed upon syntax much less semantics @~cite{Steele2017},
the very opposite of the formality the authors affect.

Not having OO-specific magic also means that when I add features to OO,
as I will demonstrate in the rest of this book, such as single or multiple inheritance,
method combinations, multiple dispatch, etc.,
I don’t have to update the language
to use increasingly more complex primitives for declaration and use of prototypes or classes.
By contrast, the “ad hoc logic” approach grows in complexity so fast
that authors soon may have to stop adding features
or find themselves incapable of reasoning about the result
because the rules for those “primitives” boggle the mind@xnote["."]{
  Authors of ad hoc OO logic primitives also soon find themselves
  incapable of fitting a complete specification within the limits of a conference paper,
  much less with intelligible explanations, much less in a way that anyone will read.
  The approach is too limited to deal even with the features of 1979 OO @~cite{Cannon1979},
  not to mention those of more modern systems.
  Meanwhile, readers and users (if any) of systems described with ad hoc primitives
  have to completely retool their in-brain model at every small change of feature,
  or introduce misunderstandings and bugs,
  instead of being able to follow a solidly known logic that doesn’t vary with features.
}
Instead, I can let logic and typing rules be as simple as possible,
yet construct my object features to be as sophisticated as I want,
without a gap in reasoning ability, or inconsistency in the primitives.

My encoding of OO in terms of “modular extension”, functions of the form
@c{mySpec (super : Focus, self : Context) : Focus}, where in the general “open” case,
the value under @c{Focus} is different from the @c{Context}, is also very versatile
by comparison to other encodings, that are typically quite rigid, specialized for classes,
and unable to deal with OO features and extensions.
Beyond closed specifications for classes, or for more general prototypes,
my @c{ModExt} type can scale down to open specifications for individual methods,
or for submethods that partake in method combination;
it can scale up to open specifications for groups of mutually defined or nested classes or prototypes,
all the way to open or closed specifications for entire ecosystems.

More importantly, my general notion of “modular extension”
opens an entire universe of algebraically well-behaved composability
in the spectrum from method to ecosystem;
the way that submethods are grouped into methods, methods into prototypes,
prototypes into classes, classes into libraries, libraries into ecosystems, etc.,
can follow arbitrary organizational patterns largely orthogonal to OO,
that will be shaped by the evolving needs of the programmers,
yet will at all times benefit from the modularity and extensibility of OO.

OO can be one simple feature orthogonal to many other features
(products and sums, scoping, etc.), thereby achieving @emph{reasonability} @~cite{Wlaschin2015},
i.e. ease of reasoning about OO programs.
Instead, too many languages make “classes” into a be-all, end-all ball of mud of
more features than can fit in anyone’s head, interacting in sometimes unpredictable ways,
thereby making it practically impossible to reason about them,
as is the case in languages like C++, Java, C#, etc.

@subsection{Typing First-Class OO}

I am aiming at understanding OO as @emph{first-class} modular extensibility,
so I need to identify what kind of types are suitable for that.
Typing individual prototypes that only contain fields of concrete primitive types is easy.
The hard part is to type @emph{classes} with abstract methods,
and more generally specifications
wherein the type of the target recursively refers to itself
through the open recursion on the module context.

Happily, my construction neatly factors the problem of OO
into two related but mostly independent parts:
first, understanding the target, and second, understanding its instantiation via fixpoint.

I already discussed in @secref{RCOO}
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
@~cite{Cardelli1985 Mitchell1985 Pierce1993}
@; TODO cite harper1994modules Remy1994
as per the Curry–Howard correspondence, it is a witness of the proposition according to which
“there is a type @c{T} that has this interface”, where the interface may include field getters
(functions of type @c{T → F} for some field value @c{F}),
some field setters (functions of type @c{T → F → T} for a pure linear representation,
or @c{T → F ⇝ 1} if I denote by @c{⇝} a “function” with side effects),
binary tests (functions of type @c{T → T → 2}),
binary operations (functions of type @c{T → T → T}),
constructors for @c{T} (functions that create a new value of type @c{T},
at least some of which without access to a previous value of type @c{T}),
but more generally any number of functions, including higher-order functions,
that may include the type @c{T} zero, one or arbitrarily many times
in any position “positive” or “negative”, etc.
So far, this is the kind of thing you could write with first-class ML modules, @; TODO cite
which embodies first-class modularity, but not modular extensibility.

Now, if the typesystem includes subtypes, extensible records, and
fixpoints involving open recursion,
e.g. based on recursively constrained types @~cite{Eifrig1995ISOOP Eifrig1995ILOOP}, then
those first-class module values can be the targets of modular extensions.
@;{TODO @~cite{Remy1994} ?}
And there we have first-class OO capable of expressing classes.

Regarding subtyping, however, note that when modeling a class as a type descriptor,
not only is it absolutely not required that a subclass’s target should be
a subtype of its superclass’s target (which would be the NNOOTT above),
but it is not required either that a subclass’s specification should be
a subtype of its superclass’s specification.
Indeed, adding new variants to a sum type, which makes the extended type a supertype of the previous,
is just as important as adding fields to a product type (or specializing its fields),
which makes the extended type a subtype of the previous.
Typical uses include extending a language grammar (as in @citet{Garrigue2000}),
defining new error cases, specializing the API of some reified protocol, etc.
Most statically typed OO languages historically mandate a subclass’s specification type
to be a subtype of its superclasses’ specification types.
Programmers work around this limitation by defining many subclasses of each class,
one for each of the actual cases of an implicit variant;
but this coping strategy requires defining a lot of subclasses,
makes it hard to track whether all cases have been processed;
essentially, the case analysis of the sum type is being dynamically rather than statically typed.

@subsubsection{Note on Types for Second-Class Class OO}
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

@subsection{First-Class OO Beyond Classes}

My approach to OO can vastly simplify the typing of OO,
because it explicitly decouples concepts that previous approaches implicitly conflated:
not only specifications and their targets,
but also modularity and extensibility,
and units of fixpointing and type descriptors.

By decoupling specifications and targets,
I can type the two separately, subtype them separately,
and sidestep the extreme complexity of
the vain quest to type and subtype them @emph{together}.
First-class OO can directly express “structures” of cooperating values, types and algorithms
parameterized by other values, types and algorithms;
these structures can be organized as records,
and defined as targets of modularly extensible specifications.
A common simple pattern is for such structures to be “classes”,
but much richer structures are also possible, making first-class OO
much more expressive than the second-class Class OO of popular languages.

By decoupling modularity and extensibility, I can type not just closed specifications,
but also open specifications, which makes everything so much simpler,
more composable and decomposable.
Individual specifications for classes, objects, methods, sub-methods, etc.,
can be typed with some variant of the @c{V → C → W} pattern (@secref{ME}),
composed, assembled in products or coproducts, etc.,
without a coupling that forces the unit of specification to coincide with the unit of fixpointing.

Finally, in typeclass style (@secref{CSvTS}),
the unit of fixpointing need not be a type descriptor:
it could be any value, including one that isn’t a “descriptor” at all;
it could be a descriptor for values and computations, but without any type component,
or a descriptor with multiple type components;
it could be a descriptor not just for one type or a finite family of types,
but even for an infinite family of types, etc.
In an extreme case, it could even be the entire ecosystem—a pattern actively used
in Jsonnet and Nix (though without formal types).
Most existing languages are limited by requiring all OO open recursion to go through classes,
each with a single implicit variable standing for a type descriptor
(even worse, often with undesired covariance constraints).
A better system will instead allow open recursion through as many implicit or explicit variables
as a program needs (without any such undesired constraints).

@section{OO Type Theory and Practice}

@subsection{Expressiveness vs Decidability}

Types for OO have long faced issues not just of consistency (@secref{NNOOTT}),
but also of decidability:
Make the typesystem too expressive, and not only does type inference become undecidable in theory,
but even type checking becomes undecidable in theory, and
they both may also become impossible to automate in practice.
Make the typesystem overly restrictive, and type inference can be easily automated,
but the formalism will fail to type the kind of general programs that make OO actually useful.
Finding the right balance of expressive power and decidability is therefore a challenge
when designing suitable types for OO.

@subsubsection{@(Fsub): System F with subtyping}

The relation between OO and subtyping goes all the way back to @citet{Hoare1965},
who thought subclasses would be the same as subtypes.
@citet{Cardelli1984} made this relation formal, and popular among type theorists.
Then, @citet{Cardelli1985} proposed @(Fsub)
as a framework in which to express those types:
it enriches System F @~cite{Girard1972}, a.k.a. the
polymorphic λ-calculus @~cite{Reynolds1974 Reynolds1985}, with subtyping.
However, @citet{Pierce1992} proved that subtype checking for @(Fsub)
is undecidable (and thus so is type inference), even on fully type-annotated terms:
while you can recursively enumerate all valid subtyping judgements,
making subtyping semi-decidable by construction,
there are types not subtypes of a type,
and terms not inhabiting a type, for which you can never decide in finite time whether that is the case.

Even System F was later found to have undecidable (though semi-decidable)
type inference and type checking,
where terms carry no explicit polymorphic type information @~cite{Wells1999}
(which is Curry-style, types separate from terms, and what most programmers use in practice).
System F typechecking without type inference, however, remains trivially decidable given
fully annotated types, including explicit type variables for type abstraction and type application
(which is Church-style, where types are compulsory parts of the terms, and
what most type theorists use in their theories).
The compromise situation is that programmers may only need @emph{some} type annotations
to make System F work.
However, no such remedy is available for @(Fsub):
the subtyping relation is itself undecidable, even with fully specified types,
so no amount of annotation can rescue the type checker against difficult cases.
Thus, Cardelli’s initial program for types for OO failed on both grounds
of consistency (@secref{NNOOTT}) and decidability—which
doesn’t diminish his great innovative contributions to the topic,
including launching the field of research itself.

@subsubsection{F-bounded Polymorphism}

Now, @citet{Canning1989} introduced F-bounded quantification
(or function-bounded quantification),
in which the bound on a type variable may refer to the variable itself:
@c{∀X ≤ F[X], G[X]}.
This technique provides self-reference in object types,
so that the type of a field can indeed depend on the type of the object,
notably enabling proper linked lists, binary methods, etc.,
overcoming limitations of the NNOOTT.
And indeed a subset of that team then had the proper tools to disprove the NNOOTT:
inheritance is not subtyping @~cite{Cook1989Inheritance}.
Being a conservative extension of @(Fsub), F-bounded polymorphism inherits (ha)
its undecidable subtype-checking problem:
even fully explicit types do not in general make type checking decidable,
though a complete semi-algorithm exists @~cite{Baldan1999}.
In practice, F-bounded quantification works well
within the nominal typesystems of languages like Java and Scala
(where you write @c{<T extends Comparable<T>>}),
but its theoretical foundations are less clean than one would like.

F-bounded quantification is often made to work by imposing restrictions
that prevent undecidability, or at least make it harder to fall into cases of non-termination:
@itemize[
  @item{Forcing top-level definitions to have explicit types
        obviates type inference for those definitions,
        helps with type inference for subterms,
        and also reduces the potential for undetected type confusion for programmers.}
  @item{Using nominal types rather than structural types makes type constraints more explicit,
        and generates finite class hierarchies that make subtyping easier;
        however, type parameters and wildcards can still generate
        an infinite set of instantiated types from a finite set of declarations.}
  @item{Variance declarations can restrict the more problematic cases
        involving non-monotonic recursion,
        like those used by Pierce to prove undecidability.}]

@citet{Kennedy2007} analyzed these restrictions precisely,
proving that undecidability in nominal subtyping with variance
follows from the combination of contravariant type constructors,
expansive inheritance (where type parameters generate
an infinite set of reachable types from finite declarations),
and multiple supertypes with the same head constructor—a
combination absent from Java, Scala, and .NET.
They also conjectured that this combination was necessary for undecidability;
however, @citet{Grigore2017} proved that even though Java doesn’t have
multiple supertypes with the same head constructor,
its type checking is nevertheless undecidable,
due to its powerful wildcards allowing use-site contravariance,
that can combine with Java’s expansive types.
Keeping (sub)typing decidable is evidently very difficult, and
not always worth the restrictions it requires.

In practice, many languages adopted F-bounded quantification,
with various heuristics to avoid unbounded recursion.
Java and Scala use it with nominal types;
TypeScript uses it with structural types instead, which still works,
though with sometimes horrible error messages.

@citet{Simons2005} offers quite an accessible introduction
to the theory of types for Class OO with F-bounded polymorphism.

@subsubsection{Recursively Constrained Types: Putting Constraints Apart}

A more radical approach was developed by
@citet{Eifrig1994} and @citet{Eifrig1995ISOOP},
who introduced @emph{recursively constrained types}:
type schemes of the form @c{∀X[C], T}
where @c{C} is a conjunction of subtyping constraints
kept @emph{separate} from the type structure.

This architectural separation is the key to decidability.
In @(Fsub), bounds are embedded inside quantified types,
and the contravariant subtyping rule for quantifiers
allows these bounds to nest without limit—which is precisely
what enables the encoding of undecidable problems.
By moving constraints into a separate, first-order system
with no quantifiers inside constraints,
Eifrig, Smith, and Trifonov ensure that constraint solving
reduces to well-understood fixpoint computations.
@citet{Eifrig1995ILOOP} demonstrated that this framework
is expressive enough to provide sound polymorphic type inference
for objects with records, width and depth subtyping, and recursive types—all
with a sound and complete, decidable type inference algorithm,
requiring no type annotations from the programmer.

The constrained-types approach influenced later theoretical work
such as @citet{Dolan2017}’s MLsub,
which achieves ML-style principal type inference with subtyping
using related constraint-based techniques, neatly grouping type constraints
in two opposite polarities for function inputs vs outputs, variable bindings vs uses,
type intersections vs unions, etc.
Constrained types achieve decidable inference
yet have seen less adoption in mainstream language design.

The lesson of constrained types is that the expressiveness-decidability tradeoff
is not as stark as @(Fsub)’s undecidability might suggest—one simply has
to stop trying to do everything inside the subtyping judgment on quantified types.

@subsection{OO Type Theory}

Types for OO is a vast topic of which I am not a specialist,
such that I am incapable of producing and presenting the Ultimate Theory.
Instead, I invite you to read some of the better papers I’ve managed to identify
and collect in my annotated bibliography at the end of this book,
in the hope that the notes I wrote on these papers will be helpful to you@xnote["."]{
  If I’m still alive to write a next edition to this book,
  I appreciate your feedback in updating or improving the list below,
  as well as this book in general.
}

My very favorite papers are @citet{Eifrig1995ISOOP Eifrig1995ILOOP},
that take the exact right approach to types for OO:
start from a sound, minimal yet expressive enough general-purpose type theory,
then build OO in a couple of simple λ-terms under this type theory.
A decade later comes @citet{Kiselyov2005}, who
also has the right attitude of just building OO on top of a general-purpose FP language,
but chooses Haskell as a now-practical substrate instead.
Also, I love @citet{Allen2011} because it shows you can just type
multiple dispatch and multiple inheritance, topics that most type theorists
don’t even try to address when considering OO, even though
it could have been three even greater papers if things were factored the right way.
And @citet{Dolan2017} seems like the right approach to think about type inference with subtypes.

Then come papers that bring useful insight, and sometimes substantial positive results,
though they ultimately fail to offer a satisfactory solution to the actual problem of
designing good OO with good types,
because some of their self-inflicted assumptions or constraints
prevent them from reaching such a solution:
@citet{Pierce1993}, @citet{Pierce2002},
@citet{Simons1995}, @citet{Simons2005},
@citet{Oliveira2009}, @citet{Amin2012},
@citet{Black2016}, @citet{Jones2016}.

Now there are papers that successfully type OO, and should be praised for it
and for their other innovations—yet take the bad approach of starting with
a toy calculus, which cannot generalize to anything useful in practice,
often with much complexity and many restrictions
so as to maintain the conflation of specification and target.
I am impressed but I want to tell the authors:
look, not a single soul gives one damn
about your toy object system—not even yourself, obviously,
since not even you care to use it to build any real software with it.
And your approach cannot possibly scale to a real object system.
Instead, you’ve rested logic on top of brittle OO,
when logic should instead be the solid foundation on top of which to build OO.
A travesty, an inversion of right and wrong, and a waste of tremendous brainpower.
@citet{Remy1994},
@citet{Fisher1994}, @citet{Fisher1996}, @; TODO @citet{Fisher1999}
@; TODO: Kim Bruce 1993 1994 1995, PolyTOIL
@citet{Bruce1996}, @citet{Bruce1997}@xnote["."]{
  There’s this recurring attitude of “types first” in much PL academia,
  as captured by the title of the book “Types and Programming Languages” @~cite{Pierce2002}
  instead of the other way around: “Programming Languages and Types”.
  In this approach, types are the main objects (ha!) of interest,
  and programs are mere accessories that matter chiefly for what types they inhabit.
  In the extreme case, that “chiefly” becomes “only”,
  and the afflicted authors assume program-irrelevance,
  i.e. proof-irrelevance seen through the Curry-Howard correspondence between programs and proofs.
  Church-style typing is often a dead giveaway for this approach:
  they consider that a program is not even a program until you’ve stated
  what type you’re interested in (with the extreme type theorists
  discarding the rest of the program after checking it inhabited the stated type).
  In this approach, the only point of OO is a challenge between type theorists:
  “can your typesystem do this?”
  When an author shows that his typesystem can—applause, the end.
  Most type theorists will gladly sacrifice what programs and software patterns can be expressed
  if only it makes things easier for the typesystem.
  The irony is that their only terms are then the types,
  and the actual typesystem that @emph{they} use is the system of their “kinds”,
  i.e. the types for types.
  For a lot of them, there is only one kind, i.e. their typesystem itself is monotyped,
  which is the same as for dynamic languages they loathe.
  The more advanced among them tend to use the simply typed λ-calculus for their kinds,
  something very basic. Then there are those who use dependent types,
  which can indirectly express arbitrary programs inside the typesystem...
  at which point they’re almost back to square one, just without any good tooling.

  But to those of us who build software in the industry, OO is a tool
  for the expression of otherwise unreachable or unaffordable patterns of thought,
  within the toolbox that is a programming language.
  Types in turn are meta-tools, that can make that toolbox even more useful, safer, handier,
  by helping us reason about those software patterns.
  Expressing those patterns is the purpose; OO enables it; types support it.
  Giving simple types to the simplest OO patterns is where the type show starts in earnest,
  not where it ends. Going further and covering as much of the useful pattern space as possible is
  the point—which requires identifying that useful pattern space,
  and if possible its meta-pattern. A problem largely shunned by type theorists.
  We reckon that programs come first and embrace Curry-style types.
  As for disregarding the differences between programs inhabiting the same type,
  or making the software patterns harder or impossible to express
  so as to go easy on the typesystem...
  to us it’s entirely missing the point.

  Now, type theorists do not owe developers submission to their priorities;
  in return, developers do not owe type theorists any interest in their typesystems.
  And so when you consider the OO vs FP flamewars again, you see that
  the dispute is never an argument about the technical incompatibility between two fields;
  it is a case of people disregarding each other’s concerns and
  then complaining that the others disregard theirs.
}

@; TODO Cook 1987 A self-ish model of inheritance ?
@; @citet{Cook1989 CookPalsberg1989}
@; Canning1989 Cook1989Inheritance

@; Leavens1988 Mugridge1991 Millstein2002
@; Pierce1991
@; GunterMitchell1994
@; FisherMitchell1995
@; Simons1995 Simons2005
@; Abadi1996Interpretation
@; Chambers1996
@; Pierce1997
@; Bono1999 Bono2002
@; Hirschowitz2002 Dreyer2007 Rossberg2008 Hirschowitz2009 Bessai2017
@; Politz2012
@; Oliveira2013

Finally, some publications, though some of the earlier ones may have been historical landmarks,
and though some may have contributed good ideas, are just bad cases of the NNOOTT,
with heaps of pointless formalism piled on top to hide just how misguided the authors are.
All but the earlier ones came after the NNOOTT had been disproved.
They should serve as cautionary tales for how even brilliant minds can go wrong
and have a negative impact on science when they become attached to flawed assumptions
they can’t let go even after these assumptions have been utterly debunked:
@citet{Cardelli1984}, @citet{Cardelli1985},
@citet{Abadi1996Primitive Abadi1996Theory},
@citet{Cartwright2013}, @citet{AbdelGawad2014}.

PS: Static analyses are at some level equivalent to abstract interpretations,
that are also equivalent to typesystems @~cite{Cousot1997}.
It’s just that they are usually not designed for human consumption,
not modular, and not meant to be stable across regular program extension;
they are only meant for compiler consumption under the assumption of
a fixed program that won’t be extended during execution
(at least not without invalidating the compiled code).
A relevant and interesting paper that comes to mind in the context
of modeling OO in terms of FP is @citet{Might2010}.
I am sure there are many others, but once again this is not my specialty.

@subsection[#:tag "OOTP"]{OO Type Practice}

I shook my head at theorists who put types on top of toy object systems rather than underneath;
but at least those I cited produced sound typesystems—unless otherwise stated.
What then shall I say about practitioners who do this at industrial scale
on top of object systems so overgrown that no one can conceivably hold them in their head,
much less reason about their logical soundness?
On what quicksand are they having millions of programmers build billions of lines of actual code?
What a waste at worldwide scale.

Thus, for instance, building a typesystem on top of Java has occupied the minds
of hundreds of top computer scientists over decades, publishing at top conferences,
pouring enormous resources into research and engineering
towards creating the best possible system given these constraints.
Could this typesystem pass the minimal bar for a typesystem, that of being sound?
No—static types don’t guarantee lack of dynamic type errors @~cite{Amin2016Unsound}.
At the same time the expressiveness of the typesystem was deliberately stunted and restricted
so the typesystem could guarantee termination in reasonable finite time. Did that succeed?
Also no—typechecking is Turing-complete @~cite{Grigore2017}.
Programmers are deprived of the power to do good,
but the power to do bad hasn’t been stopped one bit.

How could the endeavor fail despite such tremendous efforts?
Well, I’ll say it failed @emph{because} of the tremendous effort.
Not only do too many cooks spoil the broth, but
the very approach of trying to fit a typesystem
on top of an ad hoc object system of ever increasing complexity
is completely backwards.
@citet{Amin2016Unsound} notes how the peer review system is not designed to scale
beyond a few tens of pages per publication,
which is wholly insufficient to address the typesystem of Java,
or that of any of its industrial rivals.
But since these languages evolve by piling ever more features onto the concept of “class”,
even increasing the page limit to hundreds or thousands of pages
could never contain the required complexity.

Yet this complexity derives directly from the conflation and confusion of specification and target:
@itemize[
  @item{
    If the two were decoupled, you wouldn’t need types that simultaneously reflect
    the semantics of both specification and target.
    Each could separately be covered by its own very simple recursive type.
  }@item{
    If the two were decoupled, your unit of modular semantics wouldn’t be
    humongous closed specifications that must accrete all features
    yielding uncontrollable interactions@xnote["."]{
      The concept of class is the “Katamari” of semantics:
      just like in the 2004 game “Katamari Damacy”,
      it is an initially tiny ball that indiscriminately clumps together with everything on its path,
      growing into a Big Ball of Mud @~cite{Foote1997},
      then a giant chaotic mish mash of miscellaneous protruding features,
      until it is so large that it collapses under its own weight to become a star—and,
      eventually, a black hole into which all information disappears never to get out again.
      Languages like C++, Java, C#, Scala, have a concept of class so complex that it boggles the mind,
      and it keeps getting more complex with each release.
      As I like to say:
      “C++, like Perl, is a Swiss-Army chainsaw of a programming language.
      But all the blades are permanently stuck half-open while running full throttle.”
    }
    They would be small open specifications
    that you would assemble out of an orthogonal basis of small contained primitives,
    that follow a few simple rules that can simultaneously fit in a brain.
  }@item{
    If the two were decoupled, you wouldn’t need to segregate objects and classes
    from regular values and types.
    Modular extensible specifications could be used to compute any value or type,
    and once obtained, it wouldn’t otherwise matter for a value or type
    whether it was computed through such a process or not.
  }@item{
    If the two were decoupled, you wouldn’t need to constantly adjust your logic
    to sit on top of the shaking ground of an ever growing notion of class;
    you could have permanent solid foundations that actually sit beneath,
    and the changes on top.
}]

And so, to almost all of industry and academia alike,
composed of people most of whom are better and cleverer than me in more ways than one,
still I declare:
@principle{Programming: You’re Doing It Completely Wrong.}@xnote[""]{
  Zach Beane @hyperlink["https://xach.livejournal.com/170311.html"]{famously made}
  a @hyperlink["https://www.xach.com/img/doing-it-wrong.jpg"]{funny meme of John McCarthy},
  inventor of Lisp, ostensibly uttering that condemnation.
  The actual McCarthy, of course, was not the kind who would say anything like that,
  even if he might have thought so at times.
  Instead, he called himself an “extreme optimist”, viz,
  “a man who believes that humanity will probably survive even if it doesn’t take his advice.”
}

@exercise[#:difficulty "Easy"]{
  Browse some OO library you wrote, or know, or else the standard library
  of some language you know. Identify at least three classes and three method signatures
  for which the NNOOTT will give a good type, and at least three for which it will give a bad type.
  What is the criterion already?
  Can you explain in each case what makes treating subclassing the same as subtyping
  sound or unsound?
}
@exercise[#:difficulty "Easy"]{
  The chapter claims that the very first example in the very first OO paper
  involves recursive types that defeat the NNOOTT.
  Read the @c{linkage} class example in @~cite{Dahl1967}.
  Explain precisely which field types involve self-reference,
  and why a subclass of @c{linkage} cannot be a subtype of @c{linkage}
  under standard subtyping rules and still actually be a linkage
  between elements of the same type only as intended.
}
@exercise[#:difficulty "Easy"]{
  Using the @c{ModExt} type, manually work through the types for the code in @secref{MOO}.
}

@exercise[#:difficulty "Medium"]{
  The chapter mentions “binary methods” as a case where the NNOOTT fails.
  Implement a specification for @c{Comparable} values with a method
  @c{compare : Self → Self → Ordering}
  (where @c{Ordering} is the type for a choice between the symbols @c{< = > incomparable},
  or if your language has limits on identifiers, @c{LT EQ GT NA} or something).
  Assume a type @c{Real} that is a subclass of @c{Comparable} and has a subclass @c{Integer}.
  Show concretely how assuming @c{Integer ≤ Real} leads to a runtime type error@xnote["."]{
    Actually, there is One Weird Trick™ by which comparison operations
    can be considered regular unary methods rather than binary methods,
    and thus work with the NNOOTT:
    in languages with dynamic typing, where you can check the type of a value at runtime,
    comparisons can be done with all objects of the base type
    (e.g. @c{Any} in general, or @c{Real} in the above case),
    and when types mismatch, the comparison returns a symbol that means “incomparable”
    (or a boolean that is always false for equality; or the method can raise a dynamic error,
    if the language allows it, or if using a monad).
    The base type for the second argument never changes.
    Note how that trick doesn’t work for addition.
  }
}

@exercise[#:difficulty "Medium"]{
  Validity of the NNOOTT in the covariant case:
  Prove the monotonicity of fixpoints for monotone operators in general.
  Let @c{D} be a complete lattice, and order functions @c{D → D} pointwise.
  Prove that for monotone operators @c{F,G : D → D},
  @c{F ⊑ G ⇒ μF ⊑ μG}.

  Hint: Show that @c{μG} is a pre-fixpoint of @c{F}, then
  use the characterization of @c{μF} as the least pre-fixpoint of @c{F}.

  Now let @c{P,Q : D → D} be monotone, and @c{M : D × D → D} be monotone in both arguments.
  Consider the modular extension
    @c{ext(M,P) ≜ λs. M(P(s),s)},
  show that
    @c{P ⊑ Q  ⇒  ext(M,P) ⊑ ext(M,Q)},
  and therefore
    @c{μ(ext(M,P)) ⊑ μ(ext(M,Q))}.

  In particular, if @c{ext(M,P) ⊑ P}, conclude that @c{μ(ext(M,P)) ⊑ μP}.

  Relate the monotonicity hypotheses above to the requirement that
  the corresponding openly recursive variables occur only in positive positions,
  where @c{⊑} is @c{⊂} when @c{D} is a lattice of types.
  @; TODO cite Tarski1955 Park1969 Constable1985 Cook1989
  @; TODO maybe cite Scott1971/1972 Wand1979 Smyth1982(/Plotkin) Findler2002
}

@exercise[#:difficulty "Medium"]{
  If you did exercise @exercise-ref{07to08}, compare
  your attempt at typing OO with the treatment in this chapter.
  What aspects did you anticipate? What surprised you?
}

@exercise[#:difficulty "Hard, Recommended" #:tag "08to09"]{
  Now that you have a simple model for all the usual OO semantics as seen in non-Lisp languages,
  can you extend this model to cover advanced techniques from Lisp languages, including
  method combination and multiple dispatch (multi-methods)?
  Can you keep your model of multiple dispatch purely functional,
  solving the Haskell “orphan typeclass” issue?
  Can you model dynamic dispatch as well as static dispatch?
  Make an honest attempt, then keep your notes for after you read the next chapter.
}

@exercise[#:difficulty "Research"]{
  Provide a good model of types for Multiple Inheritance Specifications
  and/or Optimal Inheritance Specifications.
  What kind of dependent or constrained type is
  the local precedence order of a specification? Its precedence list?
}

@exercise[#:difficulty "Research"]{
  Implement a typesystem for a language including the applicative λ-calculus and
  an extension for lazy evaluation, with a type inference engine.
  Extend your typesystem so it should include
  recursively constrained types as in @citet{Eifrig1995ISOOP},
  or the more modern variant of @citet{Dolan2017}.
  Implement a minimal object system on top as in the previous chapter,
  or as in @citet{Eifrig1995ILOOP}.
  Now extend it to support universal and existential quantification.
  Add it to Gerbil Scheme or some other language.
  Get your system published.
}

@;{TODO

HOPEFULLY for 1st Edition:

In a first pass, keep things minimal without adding examples,
but justify in handwavy terms what each level of typing sophistication enables:

At the introduction of each feature, add a sentence or short paragraph
explaining the programming need it addresses:
- Let-polymorphism: independently instantiate a named extension at each use.
- Higher-rank polymorphism: preserve that flexibility when passing an extension to a function that uses it at several self types.
- Higher-kinded abstraction: describe combinators uniformly over arbitrary self-dependent interfaces.
- Intersections: express preservation and combination of compatible interfaces.

Add a recap at the end.
Cite Peyton Jones et al. for the machinery, and

In a second pass, add the actual examples to pommette.

A third one would integrate them into the chapter.

HOPEFULLY for 2nd Edition:

Build a series of gradually more sophisticated typecheckers in Scheme (using OO to extend them!)
for all the example programs and example typesystems,
showing the advantages and limitations of each typesystem.
Include wrong programs that the typesystems catch,
and correct programs that they nevertheless reject.
Illustrate what is wrong with the NNOOTT.
Include contortions by which some programs can nevertheless be typed with extra boilerplate.

Implement a UI for visualizing types, type constraints, or the many typesystems, on a given term.
Make it visually obvious which typesystem supports which program.

See how the type system interacts with macro expansion.

Now while Dolan-style constraint-based type inference should be straightforward,
dealing with record-as-functions, conflation, and finalization wrappers are still a research project.

Definitely not a 1st Edition issue.
}
