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
  The only popular language with a decent typesystem when it comes to OO is Scala.
}

@subsection{Static Typing}

Types can help reason about programs.
Thinking in terms of types can help understand
what programs do or don’t and how to write and use them.
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

Now, with only modularity, or only extensibility, what’s more second-class only,
you could contrive a way for the typechecker to always exactly know all the types required,
by prohibiting open recursion through the module context,
and generating magic projections behind the scenes (and a magic merge during linking).
But as I previously showed in @secref{IME},
once you combine modularity and extensibility, what’s more first-class,
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
mix : TModExt i r j → TModExt j r p → TModExt i r p
fix : top → TModExt top target target → target
}

The @c{mix} operator chains two modular extensions,
wherein the information (parameter @c{j}) provided by the parent (first argument)
is the information inherited by the child (second argument).
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
  @item{All modular extensions must agree on the type
    @c{required} or @c{target} of the module context.
    This means there can be no separate compilation of modular extensions,
    or typing of them before the entire program is written.
    In some languages with simple and rigid enough types, this means
    details of the target type may have to be wired in every extension,
    that are then not reusable with a different target type.
    Languages with existential types may alleviate this stricture to a point@~cite{PT1993STTFOOP}.}
  @item{
    Also, the user must provide a top value for that target type as initial seed for the fixpoint.
    This works well enough when the target is a record of fields
    that each have a null, empty or zero value that can be used as a top value;
    the downside being that the type is then “nullable”, and
    either do not distinguish “uninitialized” from “initialized to the default value”,
    or does distinguish them but then allows “uninitialized” at runtime after the fixpoint is computed.
    Either way, using simple types like this does not allow for richly constrained types
    wherein the tight constraints are only satisfied during the extension process itself.}]

Still, those simple types provide a template for better types,
that will typically some variant of the simple types,
restriction of them, abstraction of them, finite or infinite intersection of them.

@subsection[#:tag "SSfMSI"]{Strict Subtypes for Modular Single Inheritance}

To achieve modular types for OO, you need some notion of subtyping,
and/or intersection of types.
Subtyping increases modularity,
because a type now also stands for all its future subtypes.
Modular extensions can actually be modularly defined, composed and instantiated,

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
and a record type whose field types are subtypes or those of another is also a subtype.
And for modular types that can type extensions separately,
the typesystem also needs to be able to abstract over sets of extra fields
to be specified in a later module, using what is known as “row polymorphism”.

Then, I can define a type for Simple Strict Modular Extensions,
either in subtype-style or intersection-style,
where @c{c ⇒ t} indicates a type @c{t} under a constraint @c{c}:
@Code{
type SSModExt_subtype inherited required extended =
  provided ⊂ inherited ⇒
  inherited → required → extended

type SSModExt_intersection inherited required provided =
  inherited → required → (provided ∩ inherited)
}
In subtype style, the strictness is expressed by the variable @c{provided} being constrained
to be a subtype of @c{inherited}.
In intersection style, the strictness is expressed by the result type
being the intersection of the @c{provided} type and the @c{inherited} type,
thus a subtype of @c{inherited}.
Whereas in subtype style, @c{provided} covers all the information being returned,
in intersection style, @c{newlyProvided} only covers the new information.
Note that the type @c{top} used with @c{fix} is usually chosen in practice at instantiation time
such that @c{top∩target = target}.
In this book, I will use a mix of intersection style and subtype style. I then have:
@Code{
type SSModExt inherited required provided =
  inherited → required → (inherited∩provided)
mix : SSModExt i r p → SSModExt i∩p s q → SSModExt i r∩s p∩q
fix : top → SSModExt top top∩target target → top∩target
}

These types refine the @c{V → C → V} from @secref{MOI}:
@c{inherited} and @c{provided} each separately refine the value @c{V} being specified;
that value can be anything: it need not be a record at all, and if it is,
it can have any shape or type, and need not have the same as the module context.
Meanwhile, @c{required} refines the module context @c{C}, and is (almost) always some kind of record.
The two need not be the same at all, and usually are not for (open) modular extensions,
unless and until you’re ready to close the recursion, tie the loops and compute a fixpoint.

Unhappily, these Simple Strict Types for Modular Extensions only work well for single inheritance:
the @c{inherited} type parameter must encode all the information mixed “to the right”
of the current modular extension, and that information is only available
when following the discipline of single inheritance.

@subsection[#:tag "SrSfMMI"]{Stricter Subtypes for Modular Mixin Inheritance}

I can generalize the previous types to work with mixin inheritance,
by abstracting away (1) the type @c{super} of the @emph{effective} inherited value
at the time of instantiation,
by contrast with the type @c{inherited} of the information used from that value, and
(2) similarly the type @c{self} of @emph{effective} module context at the time of instantiation,
by contrast with the type @c{required} of the information used from that module context:
@Code{
type SrModExt inherited required provided =
  ∀ super, self : Type
    self ⊂ required, super ⊂ inherited ⇒
    super → self → (super∩provided)

mix : SrModExt i1 r1 p1 → SrModExt (i2∩p1) r2 p2 →
  SrModExt (i1∩i2) (r1∩r2) (p1∩p2)
fix : top → SrModExt top (top∩target) target → (top∩target)
}

Note how the parameters @c{i1} and @c{i2} can be used somewhat independently,
when they had to be combined into the single parameter @c{i} in @c{SSModExt};
that’s an expression of modularity at the type-level.
Furthermore, the universal quantification (@c{∀}, forall)
of @c{super} and @c{self} ensure that the modular extension
can be defined once, and later be used in any way that satisfies the type dependencies.
Mixin inheritance, not merely single inheritance, is now expressible.

Type experts may also note how the quantification also forces modular extensions to
“pass through” any information about methods not being handled as part of the @c{provided} type:
the idiom where the body of a method specification
“uses @c{(super method-id)} as a default when no overriding behavior is specified”,
that I mentioned in @secref{MOI},
is actually mandated by the above type’s universal quantifier!
At least it is mandated in language fragments that do not allow for runtime reflection
on records and their available identifiers,
which is usually the case in languages with Static Types
(absent, say, a constraint on the typeclass @c{Data.Dynamic} in Haskell,
that enabes such runtime reflection).

@subsection[#:tag "StSfMuI"]{Strictest Subtypes for Multiple Inheritance}

I admit I am not sure how exactly to write types for specifications
in multiple inheritance and optimal inheritance: there are type-level
constraints on the local precedence order and the precedence list
that require encoding the linearizarion algorithm (C4 or otherwise) into the type language.
As for the modular extensions themselves, they resemble those of mixin inheritance,
with the intersection of everything in the precedence list for the
inherited, required and provided parameters, respectively.

@section[#:tag "NNOOTT"]{The NNOOTT: Naive Non-recursive OO Type Theory}

@subsection[#:tag "OST"]{Obvious Simple Theory}

The @c{SSModExt} or @c{SrModExt} types, taken literally,
or similar types independently reinvented by many a programming language researcher,
lead to the simplest and most obvious theory for typing OO,
that I will dub the Naive Non-recursive Object-Oriented Type Theory (NNOOTT):
it consists in considering subprototyping / subclassing (a relation between specifications)
as the same as subtyping (a relation between targets).
Thus, in this theory, a subclass, that extends a class with new fields,
is (supposedly) a subtype of the parent “superclass” being extended@xnote["."]{
  The theory is implicit in the names of the @c{is} operator in C#,
  of the @c{typep} and @c{subtypep} predicated in Common Lisp,
  of the @c{is-a} vs @c{has-a} relations in many OO modeling publications,
  @; TODO Frames, semantic networks
  @; TODO check Bertrand Meyer books, Grady Booch, James Rumbaugh, GoF, UML (Booch, Rumbaugh, Jacobson).
  @;{ XXX Eiffel, Java, Smalltalk }
}

This model is simple and intuitive, and
has good didactic value to explain how inheritance works:
given two modular extensions, you can chain them as child and parent;
the combined specification yield the intersection of the provided methods and fields,
extending the intersection of the inherited methods and fields,
while using the intersection of the required module context.

The model accurately captures most simple uses of OO,
and isn’t exactly what the types above tell us?

However, this “Naive Non-recursive OO Type Theory”, as the name indicates,
is a bit naive indeed, and only works in simple non-recursive cases.
Yet the NNOOTT is important to understand,
both for the simple cases it is good enough to cover,
and for its failure modes that tripped so many good programmers
into wrongfully trying to equate inheritance and subtyping.

@subsection{Limits of the NNOOTT}

The NNOOTT works well in the non-recursive case, i.e.
when the types of fields do not depend on the type of the module context;
or, more precisely, when there are no circular “open” references between
types being provided by a modular extension,
and types it requires from the module context.
In his paper on objects as co-algebras,
Bart Jacobs characterizes the types for the arguments and results of his methods
as being “(constant) sets” @~cite{Jacobs1995ObjectsAC}@xnote[","]{
  Jacobs is particularly egregious in smuggling this all-important restriction
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
  may have missed or underestimated the critical consequences of a single word;
  they may have hoped that further work would lift the limitation.
  In other times, researchers have been hard pressed to join the bandwagon of
  Java, Web2, Big Data, Mobile, Blockchain or AI, or whatever trendy topic of the year;
  and reviewers for the respective relevant conferences may have welcomed
  newcomers with unfamiliar points of view.
  Even Barbara Liskov, future Turing Award recipient, was invited to contribute to OO conferences,
  and quickly dismissed inheritance to focus on her own expertise,
  which involves modularity without extensibility—and stating
  her famous “Liskov Substitution Principle” as she did @~cite{Liskov1987};
  brilliant, though not OO;
  that said, she did use the word “object-oriented” in print as far back as @citet{Jones1976}
  to describe her style of programming, one month before Bobrow published
  the memo on KRL-0 that first used it right,
  so she did have a stake in the name, though
  her definition happily didn’t prevail.
  @citet{Wegner1987} rightfully calls it “object-based” but not “object-oriented”.
  Are either those who talk and publish what turns out not to be OO at all at OO conferences,
  or those who invite them to talk and publish, being deliberately misleading?
  Probably not, yet, the public can be fooled just the same as if dishonesty were meant:
  though the expert of the day can probably make the difference,
  the next generation attending or looking through the archives
  may well get confused as to what OO is or isn’t about as they learn from example.
  At the very least, papers like that make for untrustworthy identification and labeling
  of domains of knowledge and the concepts that matter.
  The larger point here being that one should be skeptical of papers,
  even by some of the greatest scientists
  (none of Jacobs’, Goguen’s nor Liskov’s expertises are in doubt),
  even published at some of the most reputable conferences in the field (e.g. OOPSLA, ECOOP),
  because science is casually corrupted by power and money,
  and only more cheaply so for the stakes being low.
  This particular case from thirty years ago is easily corrected in retrospect;
  its underlying lie was of little consequence then and is of no consequence today;
  but the system that produced dishonest science hasn’t been reformed,
  and I can but imagine what kind of lies it produces to this day in topics
  that compared to the semantics of OO are both less objectively arguable,
  and higher-stake economically and politically.
}
which he elaborates in another paper @~cite{Jacobs1996InheritanceAC}
as meaning «not depending on the “unknown” type X (of self).»
This makes his paper inapplicable to most OO, but interestingly,
precisely identifies the subset of OO for which inheritance coincides with subtyping,
or, to speak more precisely,
for which subtyping of modular extensions coincides with subtyping of their targets.

Indeed, in general, specifications may contain so called “binary methods”
that take another value of the same target type as argument,
such as in very common comparison functions (e.g. equality or order)
or algebraic operations (e.g. addition, multiplication, composition), etc.
and beyond these, specifications may actually contain arbitrary higher-order functions
involving the target type in zero, one or many positions,
both “negative” (as an overall argument)
or “positive” (as an overall result), @; TODO cite Felleisen???
or as parameters to type-level functions, “templates”, etc.
These methods will break the precondition for subclassing being subtyping.

And such methods are not an “advanced” or “anomalous” case, but quintessential.
The very first example in the very first paper about actual classes @~cite{Simula1967},
involves recursive data types:
it is a class @c{linkage} that defines references @c{suc} and @c{pred} to the “same” type,
that classes can inherit from so that their elements shall be part of a doubly linked list.
This example, and any data structure defined using recursion,
will defeat the NNOOTT if examined closely.
Not only is such recursion a most frequent occurrence, I showed above in @secref{IME} that
while you can eschew support for fixpoints through the module context
when considering modularity or extensibility separately,
open recursion through module contexts becomes essential when considering them together.
In the general and common case in which a class or prototype specification
includes self-reference, subtyping and subclassing are very different,
a crucial distinction that was first elucidated in @citet{Cook1989Inheritance}.

Now, the NNOOTT can be “saved” by reserving static typing to non-self-referential methods,
whereas any self-reference must dynamically typed:
wherever a recursive self-reference to the whole would happen, e.g. in the type of a field,
programmers must instead declare the value as being of a dynamic “Any” type,
or some other “base” type or class,
so that there is no self-reference in the type, and the static typechecker is happy.
Thus, when defining a list of elements of type @c{A}, you could not write the usual recursive formula
@c{List(A) = 1 + A*List(A)} or the fixpoint @c{List(A) = Y (λ Self . 1 + A*Self)},
and would just write @c{List(A) = 1 + A*Any}.
Similarly, for trees with leaves of type @c{B}, you couldn’t write the recursive formula
@c{Tree(B) = B + List(Tree(B))}, and
would instead write just the non-recursive and dynamically typed
@c{Tree(B) = B + List(Any))}.

To compensate for the imprecision of the typesystem
when retrieving an element of the desired self-type,
some kind of explicit dereference, typecast (downcast), or coercion
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

@subsection{Why NNOOTT?}

The NNOOTT was implicit in the original OO paper @~cite{Simula1967}
as well as in Hoare’s seminal paper that inspired it @~cite{Hoare1965}@xnote["."]{
  Hoare probably intended subtyping initially indeed for his families of record types;
  yet subclassing is what he and the Simula authors discovered instead.
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
until debunked in the late 1980s @~cite{Cook1989Inheritance}.
Even after that debunking, it has remained prevalent in popular opinion,
and still very active in academia and industry alike,
and continually reinvented even when not explicitly transmitted
@~cite{Cartwright2013Inheritance abdelgawad2014domain}.
And I readily admit it’s the first idea I too had
when I tried to put types on my modular extensions,
as you can see in @citet{poof2021}.

The reasons why, despite being inconsistent, the NNOOTT was and remains so popular,
not just among the ignorant masses, but even among luminaries in computer science,
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
  Simula, KRL, Smalltalk, Director, etc.,
  were trying to have a unified concept of “class” or “frame” or “actor”, etc.
  Consequently, the necessity of considering two distinct entities
  was only fully articulated in the 2020s(!).}
@item{
  In the 1960s and 1970s, when both OO and type theory were in their infancy,
  and none of the pioneers of one were familiar with the other,
  the NNOOTT was a good enough approximation that even top language theorists were fooled.
  Though the very first example in OO could have disproven the NNOOTT,
  still it requires careful examination and familiarity with both OO and Type Theory
  to identify the error, and pioneers had more urgent problems to solve.}
@item{
  The NNOOTT actually works quite well in the simple “non-recursive” case
  that I characterized above.
  In particular, the NNOOTT makes sense enough
  in the dynamically typed languages that (beside the isolated precursor Simula)
  first experimented with OO in the 1970s and 1980s,
  mostly Smalltalk, Lisp and their respective close relatives.
  In those languages, the “types” sometimes specified for record fields
  are often but suggestions in comments, dynamic checks,
  sometimes promises made by the user to the compiler;
  and if they are actual static guarantees that only works outside of the recursive case,
  well, that is already most of the benefit of static guarantees
  when most of the work is not that recursive case.
  It takes an advanced functional programming language or style,
  or a rare care for total correctness, for the recursive case to dominate the issue,
  which was never a mainstream concern.
  }
@item{
  Even in the 1980s and 1990s, theorists and practitioners being mostly disjoint populations,
  did not realize that they were not talking about precisely the same thing
  when talking about a “class”.
  Those trained to be careful not to make categorical errors
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
  who care more about feeling like they understand rather than actually understanding.
  This is especially true of the many who have trouble thinking about recursion,
  as is the case for a majority of novice programmers and vast majority of non-programmers.
  Even those who can successfully @emph{use} recursion,
  might not be able to @emph{conceptualize} it, much less criticize a theory of it.
  @;{ TODO locate study that measures the recursion-ables from the unable. }
}]

@section{Beyond the NNOOTT}

@subsection{Self Types}

The key to dispelling the
“conflation of subtyping and inheritance” @~cite{Fisher1996}
or the “notions of type and class [being] often confounded” @~cite{bruce1996typing}
is indeed first to have dispelled, as I just did previously,
the conflation of specification and target.
Thereafter, OO semantics becomes simple:
by recognizing target and specification as distinct,
one can take care to always treat them separately,
which is relatively simple,
at the low cost of unbundling them apart before processing,
and rebundling them together afterwards if needed.
Meanwhile, those who insist on treating them as a single entity with a common type
only set themselves for tremendous complexity and pain.

That is how I realized that what most people actually mean by “subtyping” in extant literature is
@emph{subtyping for the target of a class} (or prototype),
which is distinct from
@emph{subtyping for the specification of a class} (or prototype),
variants of the latter of which Kim Bruce calls “matching” @~cite{SubtypingMatch1997}.
@; TODO cite further
But most people, being confused about the conflation of specification and target,
fail to conceptualize the distinction, and either
try to treat them as if it were the same thing,
leading to logical inconsistency hence unsafety and failure;
or they build extremely complex calculi to do the right thing despite the confusion.
By having a clear concept of the distinction,
I can simplify away all the complexity without introducing inconsistency.

One can use the usual rules of subtyping @~cite{cardelli1986understanding} @; TODO cite
and apply them separately to the types of specifications and their targets,
knowing that “subtyping and fixpointing do not commute”,
or to be more mathematically precise,
@emph{fixpointing does not distribute over subtyping},
or, said otherwise, @principle{the fixpoint operator is not monotonic}:
If @c{F} and @c{G} are parametric types,
i.e. type-level functions from @c{Type} to @c{Type},
and @c{F ⊂ G} (where @c{⊂}, sometimes written @c{≤} or @c{<:},
is the standard notation for “is a subtype of”,
and for elements of @c{Type → Type} means @c{∀ t, F t ⊂ G t}),
it does not necessarily follow that @c{Y F ⊂ Y G}
where @c{Y} is the fixpoint operator for types@xnote["."]{
  The widening rules for the types of specification
  and their fixpoint targets are different;
  in other words, forgetting a field in a target record, or its some of its precise type information,
  is not at all the same as forgetting that field or its precise type in its specification
  (which introduces incompatible behavior with respect to inheritance,
  since extra fields may be involved as intermediary step in the specification,
  that must be neither forgotten, nor overridden with fields of incompatible types).

  If the two entities are treated as a single one syntactically and semantically,
  as all OO languages so far have done, @; ALL??
  then their typesystem will have to encode in a weird way a pair of subtly different types
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
  I retrieve these familiar notions from C++ and Java just by reasoning from first principles
  and thinking about distinct but related types for a specification and its target.

  Now, my opinion is that it is actually better to fully decouple the types
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
As compared to the previous parametric type @c{SrModExt} that is parametrized by types @c{i r p},
this parametric type @c{ModExt} is itself parametrized by parametric types @c{i r p}
that each take the module context type @c{self} as parameter@xnote[":"]{
  The letters @c{r i p}, especially if reordered,
  by contrast to the @c{s t a b} commonly used for generalized lenses,
  suggest the mnemonic slogan: “Generalized lenses can stab, but modular extensions can rip!”
}
@Code{
type ModExt inherited required provided =
  ∀ super, self : Type
    self ⊂ required self, super ⊂ inherited self ⇒
        super → self → super∩(provided self)
}

Notice how the type @c{self} of the module context
is @emph{recursively} constrained by @c{self ⊂ required self}),
whereas the type @c{super} of the value in focus being extended
is constrained by @c{super ⊂ inherited self},
and a returned value of type @c{(provided self) ∩ super},
and there is no direct recursion there
(but there can be indirectly if the focus is itself referenced via self somehow).

Finally, notice how, as with the simpler NNOOTT variant above,
the types @c{self} and @c{referenced self} refer to the module context,
whereas the types @c{super} and @c{provided self} refer to some value in focus,
that isn’t at all the same as the module context
for open modular extensions in general.

My two OO primitives then have the following type:
@Code{
fix : ∀ inherited, required, provided : Type → Type, ∀ self, top : Type,
      self = inherited self ∩ provided self,
      self ⊂ required self,
      top ⊂ inherited self ⇒
        top → ModExt inherited required provided → self
mix : ModExt i1 r1 p1 → ModExt i2∩p1 r2 p2 → ModExt i1∩i2 r1∩r2 p1∩p2
}

In the @c{fix} function, I implicitly define a fixpoint @c{self}
via suitable recursive subtyping constraints.
I could instead make the last constraint a definition
@c{self = Y (inherited ∩ provided)}
and check the two subtyping constraints about @c{top} and @c{referenced}.
As for the type of @c{mix}, though it looks identical with @c{ModExt}
as the NNOOTT type previously defined with @c{SrModExt},
there is an important but subtle difference:
with @c{ModExt}, the arguments being intersected
are not of kind @c{Type} as with @c{SrModExt},
but @c{Type → Type}, where
given two parametric types @c{f} and @c{g},
the intersection @c{f∩g} is defined by @c{(f∩g)(x) = f(x)∩g(x)}.
Indeed, the intersection operation is defined polymorphically, and
in a mutually recursive way for types, functions over types, etc.

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
@c{mySpec (self : Context, super : Focus) : Focus}, where in the general “open” case,
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
that will be shaped the evolving needs of the programmers,
yet will at all times benefit from the modularity and extensibility of OO.

OO can be one simple feature orthogonal to many other features
(products and sums, scoping, etc.), thereby achieving @emph{reasonability}, @; TODO cite
i.e. one may easily reason about OO programs this way.
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
@~cite{cardelli1986understanding mitchell1988abstract PT1993STTFOOP}
@; TODO cite harper1994modules remy1994mlart
as per the Curry–Howard correspondence, it is a witness of the proposition according to which
“there is a type @c{T} that has this interface”, where the interface may include field getters
(functions of type @c{T → F} for some field value @c{F}),
some field setters (functions of type @c{T → F → T} for a pure linear representation,
or @c{T → F @(⇝) 1} if I denote by @c{@(⇝)} a “function” with side-effects),
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
Typical uses include extending a language grammar (as in @citet{garrigue2000code}),
defining new error cases, specializing the API of some reified protocol, etc.
In most statically typed OO languages, that historically mandate the subclass specification type
to be a subtype of its superclass specification types, programmers work around this limitation
by defining many subclasses of each class, one for each of the actual cases of an implicit variant;
but this coping strategy requires defining a lot of subclasses,
makes it hard to track whether all cases have been processed;
essentially, the case analysis of the sum type is being dynamically rather than statically typed.

@Paragraph{Note on Types for Second-Class Class OO}
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

My approach to OO can vastly simplify types for it, because it explicitly decouples
concepts that previously people implicitly conflated:
not only specifications and their targets,
but also modularity and extensibility,
fixpoints and types.

By decoupling specifications and targets, I can type them separately, subtype them separately,
not have to deal with the extreme complexity of
the vain quest of trying to type and subtype them together.

By decoupling modularity and extensibility, I can type not just closed specifications,
but also open specifications, which makes everything so much simpler,
more composable and decomposable.
Individual class, object, method, sub-method specifications, etc.,
can be typed with some variant of the @c{V → C → V} pattern,
composed, assembled in products or co-products, etc.,
with no coupling making the unit of specification the same as the unit of fixpointing.

Finally, with typeclass-style (as in @secref{CSvTS}),
the unit of fixpointing need not be a type descriptor;
it could be a value without an existential type, or a descriptor for multiple existential types;
it could be a descriptor not just for a finite set of types,
but even an infinite family of types, etc.
In an extreme case, it can even be not just a type descriptor,
but the entire ecosystem — a pattern actively used
in Jsonnet or Nix (though without formal types).

To build such records, one may in turn make them the targets of modular extensions,
such that first-class OO can express much more than mere “classes”,
especially so than second-class classes of traditional Class OO.
First-class OO can directly express sets of cooperating values, types and algorithms
parameterized by other values, types and algorithms.

@section{OO Type Theory and Practice}

@subsection{OO Type Theory}

Types for OO is a vast topic of which I am not a specialist,
for which I am incapable of producing and presenting the Ultimate Theory.
Instead, I invite you to read some of the better papers I’ve managed to identify
and collect in my annotated bibliography, with the hope that
the notes I wrote on these papers will be helpful to you@xnote["."]{
  Inasmuch as I’m still alive to write a next edition to this book,
  I appreciate your feedback in updating or improving the list below,
  as well as this book in general.
}

My very favorite papers are @citet{isoop1995 iloop1995},
that take the exact right approach to types for OO:
start from a sound, minimal yet expressive enough general-purpose type theory,
then build OO in a couple of simple λ-terms under this type theory.
A decade or two later come @citet{Kiselyov2005HaskellOOS} and @citet{Hoop}, who
also have the right attitude of just building OO on top of a general-purpose FP language,
but choose Haskell as a now-practical substrate instead.
Also, I love @citet{Allen2011Type} because it shows you can just type
multiple dispatch and multiple inheritance, topics that most type theorists
don’t even try to address when considering OO, even though
it could have been three even greater papers if things were factored the right way.

Then come papers that bring useful insight, though they
ultimately fail to offer a positive solution to the actual problem
designing good OO with good types,
because it is incompatible with some of their self-inflicted assumptions or constraints:
@citet{PT1993STTFOOP}, @citet{Pierce2002TAPL},
@citet{MonadsMixins}, @citet{Amin2016DependentOT},
@citet{EssenceOfInheritance2016}, @citet{oiwc2016}.
@; TODO Cook 1987 A self-ish model of inheritance ?
@; @citet{Cook1989} ? @citet{Cook1989Denotational} ?

Now there are papers that successfully type OO, and should be praised for it
and for their other innovations—yet take the bad approach of starting with
a toy calculus, which cannot generalize to anything useful in practice,
often with much complexity and many restrictions
so as to maintain conflation of specification and target.
I am impressed but I want to tell the authors:
look, not a single soul cares one damn
about your toy object system—not even yourself, obviously,
since not even you care to use it to build any real software with it.
And your approach cannot possibly scale to a real object system.
Instead, you’ve rested logic on top of brittle OO,
when logic should instead be the solid foundation on top of which to build OO.
A travesty, an inversion of right and wrong, and a waste of tremendous brainpower.
@citet{remy1994mlart},
@citet{Fisher1994}, @citet{Fisher1996}, @; TODO @citet{Fisher1999}
@; TODO: Kim Bruce 1993 1994 1995, PolyTOIL
@citet{Bruce1996Typing}, @citet{SubtypingMatch1997}.

Finally, some publications are just bad cases of the NNOOTT,
with heaps of pointless formalism piled on top to hide just how misguided the authors are,
even though most of them came even after the NNOOTT had been utterly debunked.
They should serve as cautionary tales for how even brilliant minds can go wrong
and have a negative impact on science when they become attached to flawed assumptions
they can’t let go even after these assumptions have been debunked:
@citet{Cardelli1984}, @citet{Cardelli1986Understanding},
@citet{Abadi1996Primitive AbadiCardelli1996ToO},
@citet{Cartwright2013Inheritance}, @citet{abdelgawad2014domain}.

@subsection[#:tag "OOTP"]{OO Type Practice}

I shook my head at theorists who put types on top of toy object systems rather than underneath;
but at least those I cited produced sound typesystems.
What then shall I say about practitioners who do this at industrial scale
on top of object systems so overgrown that no one can conceivably hold them in their head,
much less reason about their logical soundness?
On what quicksands are they having millions of programmers build billions of lines of actual code?
What a waste at world-wide scale.

Thus, for instance, building a typesystem on top of Java has occupied the minds
of hundreds of top computer scientists over decades, publishing at top conferences,
pouring billions of dollars in research and engineering
into creating the best possible system given these constraints.
Could this typesystem pass the minimal bar for a typesystem, that of being sound?
No—types give you no guarantees @~cite{Amin2016Unsound}.
At the same time the expressiveness of the typesystem was deliberately stunted and restricted
so the typesystem could guarantee termination in reasonable finite time. Did that succeed?
Also no—typechecking is Turing-equivalent @~cite{Grigore2017}.
Programmers are deprived of the power to do good,
but the power to do bad hasn’t been stopped one bit.

How could the endeavour fail despite such tremendous efforts?
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
    Each could separately be covered its own very simple recursive type.
  }@item{
    If the two were decoupled, your unit of modular semantics wouldn’t be
    humongous closed specifications that must acrete all features
    yielding uncontrollable interactions@xnote["."]{
      The concept of class is the “Katamari” of semantics:
      just like in the 2004 game “Katamari Damacy”,
      it is an initially tiny ball that indiscriminately clumps together with everything on its path,
      growing into a Big Ball of Mud @~cite{Foote1997BBoM},
      then a giant chaotic mish mash of miscellaneous protruding features,
      until it is so large that it collapses under its own weight to become a star—and,
      eventually, a black hole into which all information disappears never to get out again.
      Languages like C++, Java, C#, Scala, have a concept of class so complex that it boggles the mind,
      and it keeps getting more complex with each release.
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
    to sit on top of the ever shaking ground of an ever growing notion of classes;
    you could have permanent solid foundations that actually sit beneath,
    and the changes on top.
}]

And so, to the almost entirety of industry and academia alike,
composed of people most of whom are better and cleverer than me in more ways than one,
still I declare:
  @;{ TODO insert meme picture?
      https://xach.livejournal.com/170311.html
      https://www.xach.com/img/doing-it-wrong.jpg
  }
@principle{Programming: You're Doing It Completely Wrong.}@xnote[""]{
  Zach Beane famously made a funny meme of John McCarthy, inventor of Lisp,
  ostensibly uttering that condemnation.
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
  Can you explain in each case what makes treating subclassing as subtyping the same
  sound or unsound?
}

@exercise[#:difficulty "Easy"]{
  The chapter claims that the very first example in the very first OO paper
  involves recursive types that defeat the NNOOTT.
  Read the @c{linkage} class example in @~cite{Simula1967}.
  Explain precisely which field types involve self-reference,
  and why a subclass of @c{linkage} cannot be a subtype of @c{linkage}
  under standard subtyping rules and still actually be a linkage
  between elements of the same type only as intended.
}

@exercise[#:difficulty "Easy"]{
  Using the @c{ModExt} type, manually work through the types for the code in @secref{MOO}.
}

@exercise[#:difficulty "Medium"]{
  The chapter mentions “binary methods” as a case where NNOOTT fails.
  Implement a specification for @c{Comparable} values with a method
  @c{compare : Self → Self → Ordering} (where @c{Ordering} is @c{LT | EQ | GT}).
  Assume a type: @c{Number} is a subclass of @c{Comparable} and has a subclass @c{Integer}.
  Show concretely how assuming @c{Integer ≤ Number} leads to a runtime type error@xnote["."]{
    Actually, there is One Weird Trick™ by which comparison operations
    can be considered regular unary methods rather than binary methods,
    and thus work with the NNOOTT:
    in languages with dynamic typing, where you can check the type of a value at runtime,
    comparisons can be done with all objects of the base type
    (e.g. @c{Any} in general, or @c{Number} in the above case),
    returning a boolean that is always false when types mismatch.
    The base type never changes, and the method remains covariant.
    Note how that trick doesn’t work for addition, though.
  }
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
  recursively constrained types as in @citet{isoop1995}.
  Implement a minimal object system on top as in the previous chapter, or as in @citet{iloop1995}.
  Now extend it to support universal and existential quantification.
  Add it to Gerbil Scheme or some other language.
  Get your system published.
}
