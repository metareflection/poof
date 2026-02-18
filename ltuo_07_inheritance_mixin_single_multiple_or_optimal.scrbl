#lang scribble/base
@; -*- Scheme -*-
@(require "util/ltuo_lib.rkt")
@(set-chapter-number 7)

@title[#:tag "IMSMO"]{Inheritance: Mixin, Single, Multiple, or Optimal}
@epigraph{
Inheritance of methods encourages modularity by allowing
objects that have similar behavior to share code.
Objects that have somewhat different behavior can combine the
generalized behavior with code that specializes it.

Multiple inheritance further encourages modularity by
allowing object types to be built up from a toolkit of
component parts.
@|#:- @elem{David Moon @~cite{Moon1986Flavors}}|
}
@section[#:tag "MxI"]{Mixin Inheritance}
@subsection{The Last Shall Be First}

What I implemented in the previous chapters is mixin inheritance
(see @secref{MIXIO}):
the last discovered and least well-known variant of inheritance.
And yet, I already discussed above that object prototypes with mixin inheritance
are used to specify software configurations at scale. @;TODO secref
I further claim that it is the most fundamental variant of inheritance,
since I built it in two lines of code, and
will proceed to build the other variants on top of it.

@subsection{Mixin Semantics}
I showed above (see @secref{MFCME})
that mixin inheritance involves just
one type constructor @c{ModExt} and two functions @c{fix} and @c{mix},
repeated here more concisely from the previous chapter:
@Code{
mix : (t → s → u) → (u → s → v) → t → s → v
fix : t → (t → s → s) → s

(def (mix p c t s) (c (p t s) s))
(def (fix t m) (Y (m t)))}


@section[#:tag "SI"]{Single Inheritance}

@subsection{Semantics of Single Inheritance}

@principle{In Single Inheritance, the specifications at stake are open modular definitions},
as studied in @secref{MFCM},
simpler than the modular extensions of mixin inheritance from @secref{MFCME}@xnote["."]{
  In @citet{Cook1989}, @citet{bracha1990mixin},
  Cook calls “generator” what I call “modular definition”,
  and “wrapper” what I call “modular extension”.
  But those terms are a bit too general, while Cook’s limitation to records
  makes his term assignment a bit not general enough (only closed and specialized for record).
  Even in the confines of my exploration of OO,
  I already used the term “wrapper” in a related yet more specific way
  when discussing wrapping references for recursive conflation in @secref{RC};
  and a decade before Cook, Cannon @~cite{Cannon1979} also used a notion of wrapper
  closer to what I use, in Flavor’s predecessor to CLOS @c{:around} methods @~cite{CLtL2},
  or in the more general case, to CLOS declarative method combinations.
  @; TODO seclink
  The term “generator” is also too generic, and could describe many concepts in this book,
  while being overused in other contexts, too.
  I will thus stick with my expressions “modular definition” and “modular extension”
  that are not currently in widespread use in computer science, that are harder to confuse,
  and that I semantically justified by reconstructing their meaning from first principle.
}
Modular definitions take a @c{self} as open recursion argument
and return a record using @c{self} for self-reference.
Unlike modular extensions, they do not take a @c{super} argument,
since they are only inherited from, but don’t themselves inherit, at least not anymore:
what superclass they did inherit from is a closed choice made in the past,
not an open choice to make in the future; it is baked into the modular definition already.
The semantics can then be reduced to the following types and functions:
@; TODO CITE Cook
@Code{
fixModDef : (s → s) → s
extendModDef : s ⊂ t ⇒ (t → s → s) → (t → t) → s → s
baseModDef : s → top

(def fixModDef Y)
(def (extendModDef mext parent self)
  (mext (parent self) self))
(def (baseModDef _) top)
}

I already showed how the instantiation function for a closed modular definition was simply
the fixpoint combinator @c{Y}.
The case of extending a modular definition is more interesting.
First, I will simply remark that
since extending works on open modular definitions, not just on closed ones like instantiating,
the value under focus needs not be the same as the module context.
But more remarkably, extension in single inheritance requires
you use a modular @emph{extension} in addition to an existing modular @emph{definition}.

When building a modular definition through successive extensions,
an initial known existing modular definition is needed as a base case to those extensions;
this role is easily filled by the base modular definition @c{baseModDef},
that given some modular context, just returns the @c{top} value.
Now the recursive case involves a different kind of entities, modular extensions.
But I showed that modular extensions were already sufficient by themselves
to define mixin inheritance.
Why then ever use single inheritance,
since it still requires the entities of mixin inheritance in addition to its own?
Might one not as well directly adopt the simpler and more expressive mixin inheritance?

@subsection[#:tag "CMSI"]{Comparing Mixin and Single Inheritance}

@Paragraph{Mixin Inheritance is Simpler than Single Inheritance, @emph{assuming FP}}
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
these were extremely advanced concepts that very few mastered,
even among language designers.
Unlike mixin inheritance, single inheritance does not require any FP,
and many languages have or had single inheritance and no FP, including
Simula, many Pascal variants, early versions of Ada or Java or Visual Basic.
@;{TODO cite}

Neither lexical scoping nor higher-order functions are required for single inheritance
because the “modular extension” conceptually present in the extension of a modular definition
need never be explicitly realized as a first-class entity:
literally using my above recipe to implement a class or prototype definition with single inheritance
would involve building a modular extension, then immediately applying it with @c{extendModDef},
only to forget it right afterwards;
but instead, most OO languages support some special purpose syntax for the definition,
and process it by applying the extension to its super specification as it is being parsed,
without actually building any independent first-class entity embodying this extension.
Instead of applying a second-order functions,
OO languages follow a generation schema for first-order code,
in a special metaprogram part of their compiler.
The semantics of this special purpose syntax are
quite complex to explain without introducing FP concepts;
but neither implementors nor users need actually conceptualize that semantics
to implement or use it.
@;{ As for those clever enough to figure out that semantics,
   they tend to be clever enough not to need it to be simplified for them,
   and not to care enough to simplify it for others. }

@Paragraph{Mixin Inheritance is More Expressive than Single Inheritance}
Single inheritance can be trivially expressed in terms of mixin inheritance:
a single inheritance specification is just a list of modular extensions composed
with a base generator at one end;
you can achieve the same effect by composing your list of modular extensions,
and instantiate it as usual with the top value as the seed for inheritance.
Single inheritance can be seen as a restrictive style in which to use mixin inheritance,
wherein the modular extensions considered as specification will be tagged
so they are only ever used but as the second argument (to the right) of the @c{mix} function,
never the first.
Meanwhile, those extensions used as the first argument (to the left) of the @c{mix} function
must be constant, defined on the spot, and never reused afterwards.
Thus, single inheritance is no more expressive than mixin inheritance.

Conversely, given a language with FP and dynamic types or sufficiently advanced types,
you can implement first-class mixin inheritance on top of first-class single inheritance by
writing a function that abstracts over which parent specification
a specification will inheritance from, as in Racket née PLT Scheme @~cite{Mixins1998 Flatt2006Mixins}.
In terms of complexity, this construct puts the cart before the horse,
since it would be much easier to build mixin inheritance first, then single inheritance on top.
Still, this style is possible, and may allow to cheaply leverage and extend existing infrastructure
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
You can still express single inheritance as a stunted way of using mixin inheritance.
But you can’t express mixin inheritance on top of single inheritance anymore.
Single inheritance only allows you to build a specification as a list of modular extensions
to which you can add one more modular extension at a time (as in @c{cons}),
“tethered” to the right.
Meanwhile, mixin inheritance allows you to build a specification as a list of modular extensions
that you can concatenate with other lists of modular extensions (as in @c{append}),
mixable both left and right.
If you have already defined a list of modular extensions, and want to append it in front of another,
single inheritance will instead force you to duplicate the definition of each and every
of those modular extensions in front of the new base list.
See next section for how that makes single inheritance less modular.

Finally, since the two are equivalent in the context of first-class OO with higher-order functions,
but different in the more common context of second-class OO
without higher-order second-class functions,
it makes sense to only speak of single inheritance in a context where
the language syntax, static typesystem, dynamic semantics,
or socially enforced coding conventions, or development costs
somehow disallow or strongly discourage modular extensions as first-class entities.

@Paragraph{Mixin Inheritance is More Modular than Single Inheritance}
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

@Paragraph{Mixin Inheritance is Less Performant than Single Inheritance}
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
And code using this fast strategy for access to methods and fields of a specification
is still valid for all descendants of that specification, and can be shared across them.

By contrast, when using mixin inheritance,
because the code for a method cannot predict in advance what other modular extensions
will have been mixed in before or after the current one, and thus cannot assume
any common indexes between the many instances of the prototype or class being specified;
in the general case, a hash-table lookup will be necessary to locate
any method of element field provided by an instance of the current specification,
which is typically ten to a hundred times slower than fixed offset access.
Some caching can speed up the common case somewhat, but it will remain noticeably slower
than fixed offset access, and caching cannot wholly avoid the general case.

The simplicity of implementation and performance superiority of single inheritance
makes it an attractive feature to provide even on OO systems that otherwise support
mixin inheritance or multiple inheritance (that has the same performance issues as mixin inheritance).
Thus, Racket’s default object system has both single and mixin inheritance,
and Common Lisp, Ruby and Scala have both single and multiple inheritance.
Users can selectively use single inheritance when they want more performance
across all the subclasses of a given class.

@exercise[#:difficulty "Easy"]{
  Using the @c{extendModDef} function from this section,
  build a single-inheritance hierarchy with three levels: @c{Animal} → @c{Mammal} → @c{Dog}.
  Each should add one method, and override one (if there is one to override).
  Verify that a @c{Dog} instance has all three methods.
}

@exercise[#:difficulty "Easy"]{
  Identify a language with builtin mixin inheritance and first-class functions,
  but doesn’t provide builtin single inheritance.
  Implement single inheritance on top of mixin inheritance,
  and test it with simple inheritance hierarchies@xnote["."]{
    Jsonnet is an obvious choice. I admit I don’t know any other language.
    Alternatively, you could define mixin inheritance the usual way in Nix or Scheme,
    and define single inheritance on top.
  }
}

@exercise[#:difficulty "Medium"]{
  Identify a language with single inheritance only,
  but either first-class OO and higher-order functions,
  or second-class OO yet function-like entities at the type-level.
  Implement mixin inheritance on top of it@xnote["."]{
    For an easy time, pick JavaScript or Lua as a language with first-class OO and FP.
    For a somewhat harder time, pick C++ or Nim as a language
    with second-class OO but powerful enough “templates”.
  }
}

@exercise[#:difficulty "Medium"]{
  Demonstrate the modularity limitation of single inheritance:
  Define a specification @c{Point} for a type with fields @c{x} and @c{y}.
  Define specifications @c{Colored} (adds field @c{color})
  and @c{Weighted} (adds field @c{weight}).
  Now try to define @c{ColoredWeightedPoint} that has both.
  With single inheritance, you must duplicate one of them.
  Show the duplication, then show how mixin inheritance avoids it.
}

@exercise[#:difficulty "Hard"]{
  Implement the optimization that makes single inheritance fast:
  assign fixed numeric indices to methods as they are declared,
  and implement method lookup as array access rather than hash-table lookup.t3
  Benchmark the difference on a hierarchy with 10 levels and 100 method calls.
}

@section[#:tag "MI"]{Multiple Inheritance}

@subsection[#:tag "C&IS4MI"]{Correct and Incorrect Semantics for Multiple Inheritance}

With multiple inheritance (see @secref{MULIO}), a specification can declare
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
and didn’t provide its own overriding definition @~cite{Borning1977 Ingalls1978 Traits}.
This situation was deemed a “conflict” between inherited methods,
which would result in an error, at compile-time in the more static systems.
@; ??? Early Lisp systems would let users resolve things themselves ???
@; LOOPS 1983 provides you tools to resolve things yourself... Yikes. CommonLoops linearizes.
@; TODO triple check how KRL, Ani did it
Flavors @~cite{Cannon1979} identified the correct solution,
that involves cooperation and harmony rather than conflict and chaos.
Failing to learn from Flavors, C++ @~cite{Stroustrup1989Multiple}
(and after it Ada) not only adopts the conflict view of Smalltalk,
it also, like Simula @~cite{Krogdahl1985} or CommonObjects @~cite{Snyder1986Encapsulation},
tries to force the ancestry DAG into a tree!
Self initially tried a weird resolution method along a “sender path”
that each time dived into the first available branch of the inheritance DAG
without backtracking @~cite{parentsSharedParts1991},
but the authors eventually recognized how wrongheaded that was,
and reverted to—sadly—the conflict paradigm @~cite{self2007hopl}@xnote["."]{
  Like the “visitor pattern” approach to multiple dispatch, the
  Self’s once “sender path” approach to multiple inheritance
  fails to capture semantics contributed by concurrent branches of a partial order,
  by eagerly taking the first available branch without backtracking.
  In the end, like the “conflict” approach to method resolution though in a different way,
  it violates of the “linearity” property I describe in @secref{CiMR},
  which explains why it cannot be satisfying.
}

I will focus mainly on explaining the correct,
@emph{flavorful} semantics for multiple inheritance, discovered by Flavors,
and since then widely but sadly not universally accepted.
But I must introduce several concepts before I can offer a suitable formalization;
and along the way, I will explain where the @emph{flavorless} dead end of
“conflict” stems from.

@subsection{Specifications as DAGs of Modular Extensions}

I will call “inheritance hierarchy”, or when the context is clear, “ancestry”,
the transitive closure of the parent relation, and “ancestor” is an element of this ancestry.
With single inheritance, this ancestry is a list.
With multiple inheritance, where each specification may inherit from multiple parents,
the ancestry of a specification is not a list as it was with single inheritance.
It is not a tree, either, because a given ancestor can be reached through many paths.
Instead, the ancestry of a specification is a Directed Acyclic Graph (DAG).
And the union of all ancestries of all specifications is also a DAG,
or which each specification’s ancestry is a “suffix” sub-DAG
(i.e. closed to further transitive parents, but not to further transitive children),
of which the specification is the most specific element.

Note that in this book, I will reserve the word “parent” for a specification
another “child” specification depends on, and the word “super” to the partial target,
the value that is inherited as argument passed to the child’s modular extension.
This is consistent with my naming the second argument to my modular extensions @c{super}
(sometimes shortened to @c{t}, since @c{s} is taken for the first @c{self} argument)
and the second argument to my @c{mix} function @c{parent} (sometimes shortened to @c{p}).
Extant literature tends to confuse specification and target
as the same entity “class” or “prototype” without being aware of a conflation,
and so confusing “parent” and “super” is par for the course in that literature.
My nomenclature also yields distinct terms for “parent” and “ancestor”
where the prevailing nomenclature has the slightly confusing “direct super” and “super”
(or “direct superclass” and “superclass”, in a literature dominated by Class OO).

The ancestor relation can also be viewed as a partial order on specifications,
and so can the opposite descendant relation.
In the single inheritance case, this relation is a total order over a given specification’s ancestry,
and the union of all ancestries is a tree, which is a partial order but more restricted than a DAG.
I can also try to contrast these structures with that of mixin inheritance, where
each mixin’s inheritance hierarchy can be viewed as a composition tree,
that since it is associative can also be viewed flattened as a list,
and the overall hierarchy is a multitree… except that an ancestor specification
(and its own ancestors) can appear multiple times in a specification’s tree.

@subsection{Representing Specifications as DAG Nodes}

To represent a specification in multiple inheritance,
one will need not just a modular extension, but a record of:
@itemize[#:style enumparenalph
@item{a modular extension, as in mixin inheritance,
  that contributes an increment to the specification,}
@item{an ordered list of parent specifications it inherits from,
  that specify increments of information on which it depends, and}
@item{a tag (unique name, fully qualified path, string, symbol, identifier, number, etc.)
  to uniquely identify each specification as a node in the inheritance DAG.}]

Most languages support generating without side-effect some kind of tag
for which some builtin comparison operator will test identity with said entity.
Many languages also support such identity comparison directly on the specification record,
making the tag redundant with the specification’s identity.
To check specification identity, I will thus use @c{eq?} in Scheme, but
you could use @c{==} in Java, @c{===} in JavaScript, address equality in C or C++, etc.
Implementation of multiple inheritance will also be significantly sped up if
records can be sorted by tag, or by stable address or hash,
so that looking up a record entry, merging records, etc., can be done efficiently;
but I will leave that as an exercise to the reader.
Side-effects could also be used to generate unique identifying numbers for each specification;
but note that in the case of second-class OO, those effects would need be available at compile-time.
If the language lacks any of the above features, then users can still implement multiple inheritance
by manually providing unique names for specifications; but maintaining those unique names
is a burden on users that decreases the modularity of the object system, and
can become particularly troublesome in nested and computed specifications.
Interestingly, the λ-calculus itself crucially lacks the features needed
for DAG node identity; a tag must be externally provided,
or side-effects (or a monad encoding) are required for a counter.

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
  { getModExt : ModExt r i p ;
    parents : DependentList j: (ModExt (pr j) (pi j) (pp j)) ;
    tag : Tag }}

@subsection[#:tag "DMRMI"]{Difficulty of Method Resolution in Multiple Inheritance}

Then comes the question of how to instantiate a multiple inheritance specification into a target.
It seems obvious enough that the inheritance DAG of modular extensions
should be reduced somehow into a single “effective” modular definition:
only then can specifications of large objects and ecosystems
be composed from specifications of many smaller objects, methods, etc.,
such that the effective modular definition for a record of methods
is the record of effective modular definitions for the individual methods.
What then should be the “super” argument passed to each modular extension,
given the place of its specification in the ancestry DAG?

@Paragraph{The Diamond Problem}
One naive approach could be to view the inheritance DAG as some kind of attribute grammar,
and compute the (open modular definition for) the super at each node of the DAG
as a synthetic attribute@xnote[","]{
  Beware that what is typically called “child” and “parent” in an attribute grammar
  is inverted in this case relative to what is “child” and “parent” in the inheritance DAG.
  For this reason, computing effective modular extensions from ancestor to descendant
  along the inheritance DAG makes that a synthesized attribute rather than an inherited attribute
  along the attribute grammar. This can be slightly confusing.
}
by somehow combining the modular definitions at each of the supers.
After all, that’s what people used to do with single inheritance:
synthesize the modular definition of the child from that of the parent
and the child’s modular extension.
Unhappily, I showed earlier in @secref{MFCM} that there is no general way to combine
multiple modular definitions into one, except to keep one and drop the others.
Modular extensions can be composed left and right,
but modular definitions can only be on the right and on the left must be a modular extension.

The difficulty of synthesizing a modular definition is known as
the “diamond problem” @~cite{bracha1992jigsaw Inheritance1996}@xnote[":"]{
  Bracha says he didn’t invent the term “diamond problem”,
  that must have already circulated in the C++ community;
  his thesis quotes Bertrand Meyer who talks of “repeated inheritance”.
}
Consider a specification C with two parents B1 and B2 that both have a common parent A.
The contribution from A has already been baked into the modular definitions of each of B1 and B2;
therefore trying to keep the modular definitions of both B1 and B2
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
      thereby failing in large part the Criterion for Extensibility (see @secref{CfE}).}
@item{Even this reimplementation in general is impractical or at times impossible;
      the whole point of modularity is that
      the person doing the extensions is not an expert in the code being extended
      and vice versa (division of labor);
      the source code for the module being extended might not even be available for legal reasons,
      and not be understandable even when it is;
      even when the code is available, understandable and legally copyable,
      it may not be affordable to keep up with changes in code from a different project,
      with people moving at a speed and in a direction incompatible with one’s own schedule.
      This is a big failure for Modularity (see @secref{CfM}).}
@item{Users trying to circumvent the broken inheritance mechanism still have to invent their
      own system to avoid the same exponential duplication problem that the
      implementers of the OO system have punted on.
      They are in a worse position to do it because they are mere users;
      and their solution will involve non-standard coding conventions
      that will not work across team boundaries.
      This is another big failure for Modularity (again see @secref{CfM}).}]

Now, if computing a modular definition from parent modular definitions,
conflict detection and picking a winner are the only consistent solutions,
and the latter is not much better than the former, less symmetrical,
and more prone to wasting hours of programmer time by silently doing the wrong thing.
Which means, better behavior has to @emph{not} be simply based on
synthesizing a child’s modular definition from its parents’ modular definition.

@; TODO See also Malayeri & Aldrich’s 2009 "CZ: Multiple Inheritance without Diamonds" and its citations 43, 46.
@; TODO Discuss Snyder’s idiotic "make ancestry a tree" idea reprised by C++ and Ada.
@; TODO refer to later implementation of conflict on top of method combination.

@Paragraph{Cooperation not Conflict}
To find a better consistent behavior than conflict requires a reassessment
of what better designed attribute than a modular definition
should be synthesized from the inheritance DAG if any.
Step back@xnote["."]{
  As Alan Kay said, “Perspective is worth 80 IQ points”.
}
From what can you extract a modular definition, that isn’t bothered by diamonds?
How about a modular extension?
Well, there are these individual modular extensions that you can compose.
Ah, but you can’t just compose everything with exponential repetitions.
How then do you find an ordered list of modular extensions to compose without repetition,
and how can you maximize modularity as you determine that list?
And, if you step back further—what are all the consistency constraints
that this ordered list should satisfy, and how do you know?

@subsection[#:tag "CiMR"]{Consistency in Method Resolution}

Here are important consistency properties for method resolution to follow,
also known as constraints on the method resolution algorithm.
There is sadly no consistent naming for those properties across literature,
so I will propose my own while recalling the names previously used.

@Paragraph{Inheritance Order: Consistency with Inheritance}
A specification’s modular extension shall always be composed
“to the left” of any of its ancestors’,
where sequential effects and computation results flow right to left.
Thus children may have as preconditions the postconditions of their parents.

Thus, if @c{field-spec} declares @c{record-spec} as a parent,
every method defined or overridden by the former can safely assume
that indeed there will be a properly initialized record
into a specific field of which to define or override the value.
Overrides will happen after initialization, and
will not be cancelled by a duplicate of the initialization.
Similarly, if a specification adds some part to a design,
it can declare a dependency on @c{base-bill-of-parts} as a parent,
and then it can be confident that when it registers a part,
the part database will already be initialized, and will not be overwritten later.

This property is so fundamental it is respected by all OO languages since Simula @~cite{Simula1967},
and may not have been explicitly named before as distinct from inheritance itself.

@Paragraph{Linearity: Conservation of Information}
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
will be counted once and only once, even—and especially—when contributed by independent
specifications that are in no mutual ancestry relation.

Languages that respect this linearity property replace conflict with @emph{cooperation}.
In these languages, there exist trust and positive sum games
between developers and between their specifications;
each specification always contributes its extension once and only once to the final result,
and all these contributions combine into a harmonious whole.

Languages that fail to respect the linearity property
institute distrust and negative sum games
between developers and between their specifications,
who have to fight over which specifications will prevail and have their extensions applied,
which specifications will be defeated and see their extensions dropped by the system;
developers will then have to reimplement or otherwise reproduce the effects
of the extensions that would have been contributed but were dropped;
or they will have to live without.

Languages that see “conflict” in independent method specifications,
fail to respect the linearity property.
Self’s once “sender path” approach to method resolution also failed to respect the linearity property
@~cite{parentsSharedParts1991 self2007hopl};
and after the authors realized the failure, they revert to plain conflict, which still fails.

Hypothetical languages that would require users to manually synthesize attributes
from the inheritance DAG so as to extract semantics of methods,
could conceivably do that in modular ways that respect the linearity property;
but that would be a lot of hard work, and in the end would yield a result no better
than if it had been automated.

I am naming this property linearity after the similar notion from “linear logic”, @; TODO cite
wherein the preservation of computational resources corresponds to
some operator being “linear” in the mathematical sense of linear algebra.

This property was the groundbreaking innovation of Flavors @~cite{Cannon1979}.
Flavors’ flavor of multiple inheritance, which includes many more innovations,
was a vast improvement in paradigm over all its predecessors,
and sadly, also over most of its successors.

@Paragraph{Linearization: Consistency across Methods}
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
Since CommonLoops @~cite{Bobrow1986CommonLoops}, it has been customary to call this order
the @emph{precedence list} of the class, prototype or specification, a term I will use,
and to keep it in most-specific-first order:
descendents to the left, ancestors to the right,
the opposite of the order used by my @c{mix} and @c{mix*} functions@xnote["."]{
  The Simula manual has a “prefix sequence” but it only involves single inheritance
  (that it calls concatenation semantics).
  The original Flavors paper just mentions that
  “the lattice structure is @emph{flattened} into a linear one”,
  and the original source code caches the list in a field called @c{FLAVOR-DEPENDS-ON-ALL}.
  The New Flavors paper speaks of “ordering flavor components”.
  The LOOPS manual talks of precedence, but not yet of precedence list.
  CommonLoops and CLOS have a precedence list.
  Whatever name they use, they all happen to keep their precedence lists
  in most-specific-first order.
}

Thanks to this property, methods that marshal (“serialize”) and unmarshal (“deserialize”)
the fields of a class can follow matching orders and actually work together.
Methods that acquire and release resources can do it correctly,
and avoid deadlock when these resources include holding a mutual exclusion lock@xnote["."]{
  Note that consistent linearization of methods within a class
  is sufficient to order locks this holds if locks only if the locks are class-specific.
  If the locks are shared with objects of other classes,
  you need consistency across all classes that use those locks,
  and potentially across all classes.
  See monotonicity and global consistency below.
}
Ordering inconsistencies can lead to resource leak, use-before-initialization, use-after-free,
deadlock, data corruption, security vulnerability, and other catastrophic failures.

This property was also one of the major innovations of Flavors @~cite{Cannon1979}.
As I will show, it implies that the semantics of multiple inheritance
can be reduced to those of mixin inheritance
(though mixin inheritance would only be formalized a decade later).
It is the first of three eponymous constraints of C3 @~cite{Barrett1996C3}.
Inheritance order and linearity together imply linearization,
especially since some methods involve sequential computations,
and a uniform behavior is mandated over all methods.

Interestingly, all Class OO languages, even the “flavorless” ones,
necessarily have some variant of this property:
when they allocate field indexes and initialize instance fields,
they too must walk the inheritance DAG in some total order preserving
the linearity of slots, initialized in inheritance order.
Unhappily, they do not expose this order to the user,
and so pay the full costs without providing the full benefits@xnote["."]{
  A clever C++ programmer might recover the linearization implicit in object initialization
  by having all classes in his code base follow a design pattern
  whereby constructors help compute the effective methods for the class as Flavors would do.
  Unhappily, “static” member initialization does not rely on such linearization,
  only instance member initialization does; thus object constructors would have to do it
  the first time an object of the class is instantiated;
  but the test for this first time would slow down every instantiated a little bit,
  which defeats the “need for speed” that often motivates the choice of C++.
  Also, since this design pattern requires active programmer cooperation,
  it will not work well when extending classes from existing libraries,
  though this can be worked around in ugly ways
  if those classes didn’t keep crucial functionality “private”.
}

Now, a @emph{valid} concern about linearization is
that when two extensions ignore their super argument,
and the system puts one in front of the other, the second and everything after the first one
is actually ignored, and it might not be obvious which,
and there probably should be at least some warning@~cite{Snyder1986Encapsulation},
if not an outright error.
However, if that were actually a problem practically worth addressing,
then you could have a solution similar to that of languages like Java or C++
that have you annotate some methods with a keyword @c{override}
to signify that they modify a previous method;
another option would be to have users annotate their methods with an opposite keyword @c{base}
(or deduce it from the method body ignoring the @c{super} argument),
and issue a warning or error if one inherits two different @c{base} definitions for a method,
and tries to call the super method (either through an override, or through the lack thereof).
One advantage of this approach is that it does not affect the runtime semantics of methods
in Class OO, it only filters code at compile-time (though with prototypes, this “compile-time”
might be interleaved with runtime).
However, whether such a feature is worth it depends crucially on its costs and benefits.
The benefits would be the ability to find and locate bugs that are not easily found and located
by existing forms of testing and debugging, which is not obvious.
The costs, which are obvious, are that it increases the cost of writing programs,
forcing programmers to insert a lot of dummy methods that call or reimplement the “right” base method.
Instead, with linearization, the issue of which method to prefer is perfectly under
the control of the programmer, thanks to one cheap tool: the local order.

@Paragraph{Local Order: Consistency with User-Provided Order}
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
that calls it “local ordering”.
CommonLoops @~cite{Bobrow1986CommonLoops} adopted it as
“local precedence”, “local ordering”, and “local precedence list”.
CLOS @~cite{Bobrow1988CLOS CLtL2} adopts it as “local precedence order”.
Ducournau et al. speak of “local ordering” or “local precedence order”
@~cite{Ducournau1992 Ducournau1994}.
C3 says “local precedence order”.
It is the second of the three eponymous constraints of C3 @~cite{Barrett1996C3}.
Among popular “flavorful” languages,
Python, Perl, Lisp and Solidity notably respect this constraint,
but Ruby and Scala fail to.

For even more user control, and thus more expressiveness, some systems might accept
specification of more precise ordering constraints between its parents and ancestors
(I will later propose an algorithm that does just that):
instead of one totally ordered list of parents,
they might accept a partial order between parents,
to be specified e.g. as a list of totally ordered lists of parents instead of a single one.
If the list is itself a singleton, then it is the same as if a total order was specified.
If the list is made of singletons, then it is the same as if
there were no specified local order constraint between parents.
Every partial order can be expressed that way,
even if only with a list of lists of two elements, one list for each pair of comparable elements.

@Paragraph{Monotonicity: Consistency across Ancestry}
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

This property was described but not specifically named by @citet{Baker1991CLOStrophobia}
in corollary 2 of theorem 1, when discussing
the consistency properties of linearization in CLOS (or in this case, the lack thereof).
This property was first explicitly described by @citet{Ducournau1992}
then implemented by @citet{Ducournau1994},
and is the third of the three eponymous constraints of C3 @~cite{Barrett1996C3}.
Among popular “flavorful” languages, Python, Perl and Solidity respect this constraint,
but Ruby, Scala and Lisp fail to.
(Though at least in Common Lisp you can use metaclasses to fix this issue for your code.)

@Paragraph{Shape Determinism: Consistency across Equivalent Ancestries}
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

This property was first described @~cite{Ducournau1992}
under the nondescript name “acceptability”.
It received little attention, maybe because most (all?) popular OO systems
already respect it implicitly. The C3 algorithm respects it,
but not enough to name it and count it among the constraints
it purports to implement @~cite{Barrett1996C3}@xnote["."]{
  There are thus effectively four constraints enforced by C3,
  just like there are effectively four musketeers as main protagonists in
  The Three Musketeers @~cite{Dumas1844}.
}

As an alternative to Shape Determinism, you could establish a global ordering of
all defined specifications across a program,
e.g. lexicographically by their name or full path,
or by assigning a number in some traversal order,
or from a hash of their names or definitions, etc.
This ordering could then be used by a linearization algorithm
as a tie-breaking heuristic to choose which ancestor to pick next
while computing a precedence list,
whenever the constraints otherwise allow multiple solutions.
But the instability of such a heuristic when the code changes
would lead to many @emph{heisenbugs}.

@Paragraph{Global Precedence: Consistency across All Ancestries}

@citet{Baker1991CLOStrophobia} introduces the property that there should exist
a global precedence list, i.e. a total ordering of specifications
compatible with every specification’s precedence list.
Unlike Shape Determinism, this is not a local property of every specification,
but a global property of the program, that enables
global optimization techniques and correctness enforcement.
For instance, a global ordering of all specifications
ensures no deadlock from using specification-associated locks.
Also, ancestor checking can be done by consulting a bitmap with global indexes,
the number of static method combinations could be minimized, etc.

Baker suggests that the order could be based on specification definition order,
where forward references are prohibited and local order must match previous load order.
Unlike the proposed alternative to Shape Determinism above,
this order would be authoritative as the actual linearization order,
not just a tie-breaking heuristic in case of multiple possible solutions.
Baker’s solution is great for statically compiling code,
as long as the load order is guaranteed to be consistent
in presence of separate compilation and incremental source code modifications.
Dynamic code update, e.g. in an interactive environment, make this property harder to enforce;
some mechanism may trigger recompilation or reconfiguration of all subsequent specifications
after a change to any given specification.

A dummy mother-of-all precedence list can be produced at the end of static compilation
to check global consistency, with an error issued if incompatibilities are detected
(and if possible actionable error messages).
At the very least, an order can be enforced on lock-issuing specifications.

@subsection{Computing the Precedence List}

Consider a function @c{compute-precedence-list}
that takes a specification using multiple inheritance (and possibly more features)
and returns a list of specifications, that is
a linearization of the argument specification’s ancestry DAG,
satisfying the linearization property above.
Further assume that the above returned precedence-list
starts with specification itself, followed by its ancestors
from most specific to most generic (left to right).
This is the convention established both by Flavors’ “class precedence list”
and, maybe surprisingly, also by Simula’s “prefix sequence”,
though in the case of Simula this convention is contravariant with
the order in which the bodies of the “prefix classes” are concatenated into
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
Unhappily, this approach fails at respecting either Local Order or Monotonicity.

Another approach is to consider the precedence list a synthesized attribute,
and compute a child’s precedence list from those of its parents.
That’s the only reasonable way to ensure monotonicity.
However, the naive way to do it, by concatenating the lists then removing duplicates,
like LOOPS @~cite{Bobrow1983LOOPS}
or after it (though removing from the other end) Scala @~cite{scalableComponentAbstractions2005},
preserves neither Local Order nor Monotonicity.
The somewhat more careful algorithm used by CommonLoops @~cite{Bobrow1986CommonLoops}
and after it by CLOS (with minor changes) @; TODO check what those changes are
preserves Local Order, but not monotonicity.
The slightly complex algorithm by Ducournau et al. @~cite{Ducournau1994},
and the latter somewhat simpler C3 algorithm @~cite{Barrett1996C3 WikiC3},
synthesize the precedence list while preserving all desired properties.
C3 was notably adopted by OpenDylan, Python, Raku (Perl), Parrot, Solidity, PGF/TikZ.

I provide in @secref{C4} below an informal description of
my extension to the C3 algorithm, and, in appendix, the complete code.

@subsection{Mixin Inheritance plus Precedence List}

How then can one use this precedence list to extract and instantiate a modular definition
from the modular extensions of a specification and its ancestors?
By extracting the list of these modular extensions in that order, and
composing them as per mixin inheritance:

@Code{
compute-precedence-list : MISpec ? ? ? → DependentList ? (MISpec ? ? ?)
effectiveModExt : MISpec r i p → ModExt r i p
fixMISpec : top → MISpec p top p → p

(define effectiveModExt (λ (mispec)
  (foldr mix idModExt (map getModExt (compute-precedence-list mispec)))))
(define fixMISpec (λ (top) (λ (mispec)
  (fix top (effectiveModExt mispec)))))}

The @c{map} function is the standard Scheme function to map a function over a list.
The @c{foldr} function is the standard Scheme function to fold a list with a function
(also known as @emph{reduce} in Lisp and many languages after it).
The type parameters to @c{MISpec} in @c{compute-precedence-list} were left as wildcards above:
the precise dependent type, involving existentials for @c{l pr pi pp} such that
every specification in the list can be composed with the reduced composition
of the specifications to its right, is left as an exercise to the reader.

I have thus reduced the semantics of multiple inheritance
to mixin inheritance (or, in this case equivalently, single inheritance)
by way of computing a precedence list.

Complete implementations of prototypes using multiple inheritance
in a few tens of lines of code are given
in my previous paper using Scheme@~cite{poof2021},
or in a proof of concept in Nix@~cite{POP2021}.
My production-quality implementation in @(GerbilScheme)@~cite{GerbilPOO}
including many features and optimizations fits in few hundred lines of code@xnote["."]{
  285 lines with lots of comments, 163 lines after stripping comments and blank lines.
  Even converted to plain Scheme, with additional utility functions,
  it’s under 400 lines of code with comments, under 300 stripped.

  The entire object system under 2000 lines of commented code for its runtime,
  including all runtime optimizations enabled by single inheritance where appropriate.
}

@subsection{Notes on Types for Multiple Inheritance}

As usual, @c{effectiveModExt} works on open specifications,
whereas @c{fixMISpec} only works on closed specifications.
The present formalization’s ability to deal with open specifications
and not just closed ones crucially enables finer granularity for modular code.

Now, note how multiple inheritance relies on subtyping of specifications in a way that
single inheritance and mixin inheritance don’t:
In those simpler variants of inheritance, the programmer controls precisely
what are the next modular extensions to be composed with, and so does not need to rely on subtyping;
indeed, I showed when introducing wrappers for conflation that
sometimes one really wants to use modular extensions
that do not follow the usual subtyping constraints
(in that case in @secref{RC}, @c{qproto-wrapper} that wraps the value into a pair).
By contrast, with multiple inheritance, a specification only controls the relative order of its
ancestors in the precedence list that will be composed, but its modular extension must remain
correct when used as part of a descendant specification, in which case other modular extensions
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
@~cite{AbadiCardelli1996ToO Pierce2002TAPL eopl3 plai}. @TODO{more?}
The wider academic literature is also lacking in proper treatment of
types for multiple inheritance, with some notable exceptions like
@citet{CecilMultimethods} or @citet{Allen2011Type},
and even then, there has been zero interest whatsoever in flavorful multiple inheritance
in the programming language part of academia outside of the Lisp community@xnote["."]{
  Interestingly, work on flavorful multiple inheritance was done
  mostly by MIT students or graduates who had a career in the industry
  (Flavors itself was written by an undergrad at MIT, and
  Dylan was made by former MIT hackers after the demise of Symbolics),
  plus a few academics from France.
  Plus there is one recent paper by a French mathematician working
  on a system written in Python. @; TODO SageMath
}
@TODO{cite more: Jonathan Aldrich ? Odersky ?}
Much of the focus of the literature is on subtyping,
with a deemphasis or outright avoidance of fixpoints and self-recursion,
leading many authors to confuse subtyping of specification and target.
Subtyping is then often studied in the context of single inheritance,
which is ironic since subtyping isn’t quite as important without multiple inheritance.

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
than about ease of writing performant implementations with simpler typesystems,
will choose multiple inheritance over the less expressive and less modular alternatives:
see for instance Common Lisp, C++, Python, Scala, Rust.
@TODO{cite Scala OO model. What else? Kathleen Fisher’s thesis?}

@subsection{Comparing Multiple and Mixin Inheritance}

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

@Paragraph{Multiple Inheritance is less Simple than Mixin Inheritance}
Obviously, multiple inheritance requires the system to implement some extra logic
either to handle proper ancestor linearization, or to implement
some really bad method resolution algorithm instead.
This makes multiple inheritance more complex to implement,
and more complex to explain especially if you don’t do it properly.

But is that complexity intrinsic to the problem it is solving, or is it extrinsic?
In other words, does multiple inheritance solve a problem you would have to face anyway
with mixin inheritance, or does it introduce concepts you do not need?
And assuming it is a problem you face anyway, does it solve it in the simplest manner?

@Paragraph{Multiple Inheritance is as Expressive as Mixin Inheritance}
Mixin inheritance is clearly not less expressive as multiple inheritance,
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
would instead export a multiple inheritance specification to use once,
that defines a helper that it possibly calls once, but can be thereafter called many times.
Therefore I can say that multiple inheritance is as expressive as mixin inheritance in practice.

@Paragraph{Multiple Inheritance is more Modular than Mixin Inheritance}
I will now compare the two variants of inheritance from the point of view of modularity.
Multiple inheritance requires somewhat more sophistication than mixin inheritance,
adding the cognitive burden of a few more concepts,
which at first glance might be seen as detrimental to modularity.
Yet, I will argue that inasmuch as modularity matters,
these concepts are already relevant,
and that multiple inheritance only internalizes modularity issues
that would otherwise still be relevant if left as external concepts.
Moreover, multiple inheritance automates away a crucial cross-module task
that mixin inheritance requires users to handle manually,
thereby reducing modularity.

Thus, consider the issue of dependencies between modular extensions.
I showed that in practice, the common modular extension @c{field-spec} depends on @c{record-spec},
while part specifications in my notional example depend on @c{base-bill-of-parts}.
More generally, a specification may depend on a method having been implemented in an ancestor
so that its inherited value may be modified in a wrapper (in this case, the “database” of parts),
or, in some languages, just declared so it may be used (though with a proper typesystem
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
then users who want to define a specification @c{C} that uses both @c{B1} and @c{B2},
will experience the very same diamond problem as when trying to synthesize
a modular definition from an attribute grammar view of of multiple inheritance in @secref{DMRMI}:
the pre-composed dependencies (@c{A} in this case) would be duplicated in the mix of
@c{(mix B1precomposed B2precomposed) = (mix (mix B1 A) (mix B2 A))};
these copies would badly interfere, in addition to leading to an exponential resource explosion
as you keep pre-composing deeper graphs.
Therefore, pre-composing modular extensions is the same non-solution
that led to the “conflict” view of multiple inheritance,
based on a naive conceptualization of how to generalize single inheritance.
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
@principle{with Mixin Inheritance, a specification’s dependencies become part of its interface}.

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
  This is actually the kind of activity I engaged in for years,
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
  By contrast, my experience with the OCaml and Haskell ecosystems is that
  their strong static types without lenient subtyping creates
  very tight coupling between specific versions of libraries,
  with costly rippling of changes.
  What results is then “DLL hell”, as it becomes hard to find coherent sets
  of inter-compatible libraries that cover all the needs of your program,
  and to keep them up-to-date when one library requires a bug fix,
  even though there might be a library to cover each of your needs.
}

One thing that @emph{could} actually help deal with dependencies without multiple inheritance
would be a rich enough strong static typesystem such that
the @c{r}, @c{i} and @c{p} parameters (for required, inherited and provided)
of my parameterized type @c{ModExt r i p} can identify whether modular extensions
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

All in all, @principle{Multiple Inheritance is more modular than Mixin Inheritance},
that is more modular than single inheritance, that is more modular than no inheritance;
the modularity issues one experiences with one kind of inheritance are still superior
to the lack of modularity issues one experiences with the kinds of inheritance
lacking the modularity about which to have issues to begin with.

@Paragraph{Multiple Inheritance is slightly less Performant than Mixin Inheritance}
Compared to mixin inheritance, multiple inheritance involves this extra step of
computing a specification’s precedence list.
The linearization algorithm has a worst case complexity O(dn),
where d is the number of parents and n the total number of ancestors.
Yet, in practice the extra step is run at compile-time for second-class OO,
and does not affect runtime;
moreover, it is often quite fast in practice
because most OO hierarchies are shallow@xnote["."]{
  A study of Java projects on GitHub @~cite{Prykhodko2021DIT}
  found that the vast majority of classes had fewer than 5 ancestors,
  including the base class @c{Object}. But that is a language with single inheritance.
  A survey I ran on all Common Lisp classes defined by all projects in Quicklisp 2025-06-22
  (minus a few that couldn’t be loaded with the rest) using @c{ql-test/did}
  shows that the 82% of the 18939 classes (including structs) have only 1 parent,
  99% have 3 or fewer,
  the most has 61, @c{MNAS-GRAPH::<GRAPH-ATTRIBUTES>};
  90% have 5 or fewer non-trivial ancestors,
  99% have 16 or fewer, the most has 63, @c{DREI:DREI-GADGET-PANE}.
  (By non-trivial, I mean that I count neither the class itself, nor
  the 3 system classes shared by all user-defined classes.)
  As for the 2841 structs that use single inheritance
  (and not usually to define many methods, just for data fields),
  63% had no non-trivial ancestor, 86% has 0 or 1, 96% had 2 or fewer, 99% had 3 or fewer,
  99.9% had 4 or fewer, 1 had 5, 1 had 6, @c{ORG.SHIRAKUMO.BMP::BITMAPV5INFOHEADER}.
  As can be seen, multiple inheritance leads to a very different style of programming,
  with more done in “traits” or “mixins”, than when only single inheritance is available.
}

Multiple inheritance otherwise involves the same runtime performance issues as mixin inheritance
compared to single inheritance (see @secref{CMSI}):
in general, method or field access requires a hash-table lookup
instead of a fixed-offset array lookup, which is typically 10-100 times slower.

Now, a lot of work has been done to improve the performance of multiple inheritance,
through static method resolution when possible, @; TODO cite C++ ? type analysis ? sealing ?
and otherwise through caching @~cite{Bobrow1986CommonLoops}. @; TODO cite SBCL?
But these improvements introduce complexity, and caching
increases memory pressure and still incurs a small runtime overhead
even when successful at avoiding the full cost of the general case,
while not eliminating the much slower behavior in case of cache miss.
For all these reasons, many performance-conscious programmers
prefer to use or implement single inheritance when offered the choice.

@exercise[#:difficulty "Easy"]{
  Draw the inheritance DAG for the following specifications:
  @c{A} has no parents; @c{B} and @c{C} both have parent @c{A};
  @c{D} has parents @c{B} and @c{C} in this (local precedence) order.
  This is the classic “diamond”. Label each edge with the parent-child relationship.
}

@exercise[#:difficulty "Easy"]{
  Using the diamond from the previous exercise, manually compute the precedence list
  for @c{D} using depth-first left-to-right traversal
  (the algorithm from the original Flavors, or Ruby).
  Then compute it respecting the constraints of C3.
  Are they the same? If not, what constraint does depth-first violate?
}

@exercise[#:difficulty "Medium"]{
  Using the @c{base-bill-of-parts} specification that tracks parts as a base,
  have a specification @c{A} that adds a chassis,
  @c{B} and @c{C} each inheriting from @c{A} that add respectively
  “wheels” and an “engine”, and @c{D} that inherits from both @c{B} and @c{C},
  that adds a “body” and completes the toy car.
  Show that naively combining @c{B}’s and @c{C}’s modular definitions
  would overwrite away either the wheels or the engine.
  Show that combining their “precomposed” mixins @c{B A} and @c{C A} into @c{B A C A}
  would duplicate the effects of @c{A}, which is incorrect.
  Show that the “conflict” semantics, the parts list is wrong
  whichever way you “resolve” the conflict, whether in favor of @c{B} or @c{C}.
  Show that with proper linearization, each part appears exactly once and only once.
}

@exercise[#:difficulty "Medium"]{
  Implement a simple depth-first linearization algorithm.
  Which of the properties I listed does it preserve? Which does it fail to preserve?
  Can you exhibit a short counter-example for each case of property not preserved?
}

@exercise[#:difficulty "Hard"]{
  With the help of an AI if needed, implement then use flavorful multiple inheritance
  from @secref{IMSMO} on top of C++ templates.
  You will thereby both prove that C++ can implement the same multiple inheritance
  patterns as Lisp, Ruby, Python, Scala do—but also how much effort that takes,
  and how uncolloquial the resulting style is.
}

@exercise[#:difficulty "Hard"]{
  With the help of an AI if needed, implement then use flavorless multiple inheritance
  from @secref{IMSMO} on top of CLOS.
  You will thereby both prove that CLOS can implement the same multiple inheritance
  patterns as C++, ADA, Smalltalk do—but also how much effort that takes,
  and how uncolloquial the resulting style is.
}

@section[#:tag "OISMIT"]{Optimal Inheritance: Single and Multiple Inheritance Together}

@subsection{State of the Art in Mixing Single and Multiple Inheritance}

With all these variants of inheritance and their tradeoffs
naturally comes a question:
Is there some optimal inheritance, that is
equally or more expressive, modular and performant than any other variant?
It would have to be at least as expressive as mixin inheritance and multiple inheritance,
as modular as multiple inheritance,
and as performant as single inheritance.

Now, as far back as 1979,
@; January 1979 chinual chapter 17 is about DEFSTRUCT, but
@; the documentation for the :INCLUDE option is "to be supplied"
@; Flavors comes later that year.
Lisp offered both single inheritance with its @c{struct}s,
and multiple inheritance with its @c{class}es (nées flavors).
@; David Moon’s MacLisp Reference Manual (April 1974) does not mention DEFSTRUCT.
@; Ani/Director is from 1976, but never widely used.
Since 1988, the Common Lisp Object System (a.k.a. CLOS) @~cite{Bobrow1988CLOS CLtL2}
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

  Now, one could also conceivably use Common Lisp metaclasses @~cite{AMOP}
  to re-create arbitrary user-defined inheritance mechanisms,
  including my Optimal Inheritance below.
  The semantics of it would be relatively easy to re-create.
  However, it might still be hard to do it in a way that
  the underlying implementation actually benefits from the optimization opportunities
  of single inheritance, at least not in a portable way.
}

Since then, Ruby (1995) and Scala 2 (2004) have done better,
wherein “single inheritance” structs and “multiple inheritance” classes
can extend each other—except that
Ruby calls these entities respectively “classes” and “modules”
whereas Scala, following the Smalltalk tradition,
calls them respectively “classes” and “traits” @~cite{scalableComponentAbstractions2005}@xnote["."]{
  It is bad enough that “class” denotes entities with multiple inheritance in Lisp or Python,
  but specifically entities with single inheritance in Smalltalk, Ruby or Scala.
  It doesn’t help that the Scala documentation is not consistent about that naming,
  and uses the word “class” ambiguously, sometimes to mean suffix specifications only,
  sometimes to mean “class-or-trait”, specifications both infix and suffix.
  To further confuse terminology, a C++ @c{struct} is just a regular class,
  that always supports (flavorless) multiple inheritance—there are no
  “single inheritance entities” in C++;
  the only difference is that classes defined with the @c{struct} keyword
  have all their members public by default rather than private as with the @c{class} keyword,
  which makes the @c{struct} statement backward compatible with the equivalent statement in C.
  Thus there is a “struct/class” distinction in C++, but as a minor syntactic feature
  that has nothing to do with either single or multiple inheritance.
  And to make things even more confusing, “multiple inheritance” in C++ is not
  what it is in Ruby, Scala, Lisp, Python and other flavorful languages;
  instead it’s a mix of flavorlesss “conflict” inheritance (for “virtual” classes),
  and weird path-renamed duplicated inheritance à la CommonObjects (for the non “virtual”)
  that tries hard to fit the square peg of a multiple inheritance DAG into the round hole of a tree.
  Anyway, the conclusion here once again is that the word “class” is utterly useless
  at conveying precise connotations about inheritance
  outside the context of a specific language.
}
Ruby and Scala combine the two forms of inheritance in ways broadly similar to each other
and to the optimal inheritance that I propose below
(Ruby did so about 10 years earlier than Scala 2,
but without an academic publication to cite, though also without static types).
However, Ruby uses a variant of the Flavors algorithm
(depth-first traversal),
and Scala a variant of the LOOPS algorithm
(concatenation of precedence lists then removal of duplicates),
and neither respects Local Order nor Monotonicity,
making them less modular than they could be, and sub-optimal@xnote["."]{
  Interestingly, and although this change and its rationale are not explained
  in Scala documentation or papers,
  Scala removes duplicates from the beginning of the concatenate list
  when LOOPS removes them from the end.
  This change makes the algorithm preserve the longest suffix possible,
  which crucially matters for the Scala “single inheritance” fragment;
  LOOPS instead preserves the longest prefix possible, which serves no purpose,
  and sacrifices the suffix, when preserving the suffix could have helped
  with opportunistic optimizations even in the absence of suffix guarantees.
  Note that Ruby and my C4 algorithm also preserve the longest suffix possible,
  which matters for the same reasons.
}

Note that Scala 2 further requires the user to explicitly merge the “suffixes” by hand,
and include the most specific suffix ancestor
as the semantically last parent of a specification@xnote[";"]{
  I say semantically last, as Scala, per its documentation,
  keeps precedence lists in the usual most-specific-first order.
  However, syntactically, Scala requires users to specify parents in the opposite
  most-specific-last order, so your suffix parent (a “class” in Scala)
  must be syntactically specified @emph{first} in Scala 2.
  As an exception, the most specific suffix ancestor need not be explicitly specified
  if it is the top class @c{Object}.
}
Scala 3 by contrast relaxes this restriction, and, like Ruby, will automatically merge
the “suffixes” and identify the most specific suffix ancestor
(or issue an error if there is an incompatibility in suffixes)@xnote["."]{
  I was unable to find any trace anywhere in the Scala 3.3 documentation
  of this slight change in syntax and semantics,
  its precise behavior, design rationale, and implementation timeline;
  and the Scala team declined to answer my inquiries to this regard.
  Nevertheless, this is clearly an improvement,
  that makes Scala 3 as easy to use as Ruby or @(GerbilScheme) in this regard:
  by comparison, Scala 2 was being less modular, in requiring users to do extra work
  and make the “most specific class ancestor” a part of a trait’s interface,
  rather than only of its implementation.
}

@subsection{The Key to Single Inheritance Performance}

In this section, I will use the respective words “struct” or “class” as per the Lisp tradition,
to denote specifications that respectively do or do not abide by
the constraints of single inheritance (with according performance benefits).
My discussion trivially generalizes beyond specifications for type descriptors
conflated with their targets to any specification;
only the inheritance structure of my specifications matters to this discussion.

As seen in @secref{CMSI}, what enables the optimizations of single inheritance is
that the indexes to the fields and methods of a specification’s target
are also the indexes of the same fields and methods in the targets of its extensions.
These indexes are computed by walking the specification’s ancestry
from least specific to most specific ancestor.
In the terminology of multiple inheritance,
this is a walk along the reverse of the precedence list.
And for the walks to yield the same results,
after putting those lists in the usual order,
can be stated as the following property, that I will call the @emph{suffix property}:
@principle{the precedence list of a struct is a suffix of that of every descendant of it}.

Now this is semantic constraint, not a syntactic one,
and it can very well be expressed and enforced
in a system that has multiple inheritance.
Thus it turns out that indeed, a struct can inherit from a class, and a class from a struct,
as long as this property holds: the optimizations of single inheritance are still valid,
even though structs partake in multiple inheritance!
Interestingly, the ancestry of a struct then is not a linear (total) order anymore:
a few classes may be interspersed between two structs in the precedence list.
However, the subset of this ancestry restricted to structs, is a linear (total) order,
as every struct in a given specification’s ancestry is either ancestor or descendant
of every other struct in that same ancestry.
Thus structs are still in single inheritance with respect to each other,
even though they are part of multiple inheritance in their relationship with classes.

Since the suffix property is the thing that matters,
I will name “suffix” the generalization of structs
(in Lisp lingo, or classes in Smalltalk lingo)
from classes (here meaning prototypes for type descriptor)
to prototypes (for any target) and arbitrary specifications (without conflation).
By contrast I will call “infix” the specifications
that are explicitly not declared as suffix by the programmer,
and just say specification (or prototype if conflated with target,
or class if furthermore the target is a type descriptor)
when it can be either infix or suffix.
Thus, in Lisp lingo, a “struct” is a suffix specification,
a “class” is a specification and a “mixin” is an infix specification.
In Smalltalk lingo, a “class” is a suffix specification,
and a “trait” is an infix specification@xnote["."]{
  Interestingly, my “suffix” is the same as the “prefix” of Simula.
  Simula calls “prefix” a superclass, precisely because its single inherited behavior
  comes before that of the class in left to right evaluation order
  of its code concatenation semantics.
  But in multiple inheritance, modular extensions are composed right-to-left,
  and in a tradition that goes back to Flavors (and maybe beyond),
  the precedence list is also kept in that order.
  And so my “suffix” actually means the same as Simula’s “prefix”.
  Now, since Simula only has single inheritance, all its classes are “prefix”
  (i.e. my “suffix”).
  by contrast, in a multiple inheritance system, regular classes are infix, and
  their precedence list, while an ordered sublist of an descendant’s precedence list,
  is not necessarily at the end of it, and is not necessarily contiguously embedded.
  It can also be confusing that Simula calls “prefix sequence” the list of superclasses
  that it keeps in the same order as the precedence list of Flavors and its successors,
  from most specific to least specific, which is opposite to the order of “prefixing”.
  Finally, a “final” class in Java or C++ could be called “prefix” by symmetry,
  because its precedence list is the prefix of that of any transitive subclass
  (of which there is only one, itself); but that would be only introduce more
  terminological confusion, without bringing any useful insight,
  for this prefix property, while true, is not actionable.
  It is better to leave suffix and prefix as twisted synonyms.
}

@subsection{Best of Both Worlds}

An inheritance system that integrates multiple inheritance and single inheritance in the same hierarchy
using the suffix property, yet also respects the consistency constraints of multiple inheritance,
can best existing inheritance systems.
Suffix specifications (such as Lisp structs) can inherit from
infix specifications (such as Lisp classes), and vice versa,
in an expressive and modular multiple inheritance hierarchy DAG.
Yet suffix specifications being guaranteed that their precedence list is the suffix of any
descendant’s precedence list, methods and fields from these specifications will enjoy
the performance of single inheritance;
fast code using fixed-offset access to these can be shared across all descendant specifications.
I call that @emph{Optimal Inheritance}@xnote["."]{
  Optionally, the specification DAG is made slightly simpler with
  the empty specification, declared suffix, as the implicit ancestor of all specifications, and
  the empty record specification, declared suffix,
  as the implicit ancestor of all record specifications.
  But this only actually helps if the system allows for the after-the-fact definition of
  multimethods, “protocols” or “typeclasses” on arbitrary such specification.
  @; TODO secref
}

Note that while suffix specifications with respect to each other are in
a “single inheritance” hierarchy as guaranteed by the suffix property,
being in such a hierarchy is not enough to guarantee the suffix property;
and suffix specifications are still in a “multiple inheritance” hierarchy with other specifications.
Thus when “combining single and multiple inheritance”, it is not exactly “single inheritance”
that is preserved and combined, but the more important struct suffix property.
The crucial conceptual shift was to move away from the syntactic constraint on building a class,
and instead focus on the semantic constraint on the invariants for descendants to respect,
that themselves enable various optimizations.
It may be a surprising conclusion to the debate between proponents of multiple inheritance
and of single inheritance that in the end,
single inheritance did matter in a way,
but it was not exactly single inheritance as such that mattered,
rather it was the suffix property implicit in single inheritance.
The debate was not framed properly, and a suitable reframing solves the problem
hopefully to everyone’s satisfaction.

In 2024, @(GerbilScheme) @~cite{GerbilScheme} similarly modernized its object system by
unifying its single inheritance and multiple inheritance hierarchies
so its “struct”s and “class”es (named in the Lisp tradition) may extend each other.
The result ended up largely equivalent to the classes and modules or traits of Ruby or Scala,
except that @(GerbilScheme) respects all the consistency constraints of the C3 algorithm,
that it further extends to support suffixes, in what I call the C4 algorithm.
It is the first and so far only implementation of @emph{optimal inheritance}@xnote["."]{
  Interestingly, Ruby, Scala and @(GerbilScheme) seem to each have independently
  reinvented variants of the same general design based on the suffix property.
  This is good evidence that this is a universally important design.
  However, only with @(GerbilScheme) was the suffix property formalized
  and was the C3 algorithm used.
}

@subsection[#:tag "C4"]{C4, or C3 Extended}

The authors of C3 @~cite{Barrett1996C3 WikiC3},
after Ducournau et al. @~cite{Ducournau1992 Ducournau1994},
crucially frame the problem of ancestry linearization in terms of
constraints between the precedence list of a specification and those of its ancestors:
notably, the “monotonicity” constraint states that
the precedence list of an ancestor must be an ordered subset
of that of the specification, though its elements need not be consecutive.
I define my own algorithm C4 as an extension of C3,
that in addition to the constraints of C3, also respects the @emph{suffix property}
for specifications that are declared as suffixes.
This means that an optimal inheritance specification, or @c{OISpec}
by extending the @c{MISpec} of multiple inheritance
with a new field @c{suffix?} of type @c{Boolean},
that tells whether or not the specification requires all its descendants
to have its precedence list as a suffix of theirs.
(In a practical implementation, you could add more flags, for instance
to determine if the specification is sealed, i.e. allows no further extensions.)
@;{TODO also cite Dylan for that, and Scala}

I give a complete Scheme implementation of C4 in the appendix,
but informally, the algorithm is as follows,
where the steps tagged with (C4) are those added to the C3 algorithm
(remove them for plain C3):
@itemize[#:style'ordered
  @item{@bold{Extract parent precedence lists}:
        For each parent appearing in the Local Order
        (which in general can be several lists of totally ordered parents),
        add the precedence list of the parent to the list of list of candidates,
        if said parent didn’t appear as ancestor already.
        Maintain a table of ancestor counts, that counts how many times each ancestor appears
        in the precedence lists so far;
        use it to determine if a parent appeared already;
        you will also use it later in the algorithm during candidate selection.}
  @item{@bold{Split step}:
        Split each precedence list into:
        @itemize[
          @item{@bold{Prefix}:
            a prefix precedence list containing only infix specifications,
            up to (but not including) the most specific suffix specification (if any), and}
          @item{@bold{Suffix}:
            a suffix precedence list from that most specific suffix specification to the end
            (the empty list if no such suffix specification).}]
        If using singly linked lists, keep your prefixes in reverse order for later convenience.
        (In plain C3: everything is in the prefixes; the suffixes are empty.)}
  @item{@bold{Suffix merge step}:
  Merge all suffix lists into a single merged suffix list.
  The suffix property requires these lists to be in total order: given any two suffix lists,
  one must be a suffix of the other. If not, raise an incompatibility error.
  The merged suffix is the longest of all suffix lists@xnote["."]{
     Note that instead of working on the lists, you can work only on their heads,
     that are “suffix specifications”, and skip over infix specifications.
     To speed things up, each specification could also have a field
     to remember its “most specific suffix ancestor”.
     And for a space-time tradeoff, you can also keep the “suffix ancestry”
     of these suffix specifications in a vector you remember
     instead of just remembering the most specific one,
     allowing for O(1) checks for subtyping among suffix specifications.}}
  @item{@bold{Local Order Support Step}:
     append the local order (list of lists of parents) to the end of the list of prefixes;
     (if the prefix lists are kept reversed, also reverse each local order list for consistency;)
     call the elements of those lists candidates, the list candidate lists,
     and this list the candidate list list.}
  @item{@bold{Cleanup Step}:
     @itemlist[
       @item{Build a hash-table mapping elements of the merged suffix list
             to their (positive) distance from the tail of the list.}
       @item{For each candidate list,
         remove redundant suffix specifications from its end:
         @itemlist[
           @item{Start from the tail of the candidate list@xnote["."]{
               You may already have gotten that list in reverse order at the previous step;
               or you can reverse it now;
               or if you keep your precedence lists in arrays, just iterate from the end.}}
           @item{if the list is empty, discard it and go to the next list.}
           @item{otherwise, consider the next element, and look it up in the above hash-table;}
           @item{if it was present in the merged suffix list,
               with an increased distance from its tail from the previous seen element if any,
               then remove it from the candidate list and go to the next element;}
           @item{if it was present in the merged suffix list,
               but the distance from its tail decreased,
               throw an error, the precedence lists are not compatible;}
           @item{if it was not present in the merged suffix list,
               then keep it and the rest of the elements,
               now in most specific to least specific order
               (again, if the candidate lists were reversed),
               and process the next candidate list;}
           ]}]}
  @item{@bold{C3 merge on cleaned prefixes}:
     @itemlist[
       @item{From the table of ancestor counts created in the first step,
             for each candidate list, decrement the ancestor count of its head element.}
       @item{Repeatedly, and until all the lists are empty,
         identify the next winning candidate in the candidate list list:
         @itemlist[
           @item{The winning candidate is the first head of a candidate list (in order)
              that has a count of zero in the ancestor count table.}
           @item{If no candidate won, throw an error.}
           @item{If one candidate won, add it to the tail of the merged prefix.
              Then, for each candidate list of which it was the head:
              @itemlist[
                 @item{pop it off the candidate list;}
                 @item{if the rest of the candidate list is empty, remove it from the candidate lists;}
                 @item{otherwise the candidate list is not empty, promote its next element as head,
                    and decrement its count of the new head in the ancestor count table.}]}]}]}
  @item{@bold{Join Step}: Append the merged prefix and the merged suffix.}
  @item{@bold{Return Step}: Return the resulting list; also the most specific suffix.}]

@;{TODO examples of the working algorithm, of incompatibility cases,
        of discrepancies with C3.
   this example bug: direct supers (A) (S A) (B A S)).
}

Note that the C3 algorithm as published by @~cite{Barrett1996C3},
has complexity O(d²n²) where d is the number of parents, n of ancestors,
because of the naive way it does a linear membership scan
in the tails of the lists for each parent@xnote["."]{
  A worst-case example is, two parameters d and n,
  to find a linear order of size n, more or less evenly divided in d segments of size n/d,
  and for each segment head S@(ᵢ), define a specification T@(ᵢ) that S@(ᵢ) as its single parent;
  Make the local precedence order the T@(ᵢ) sorted from shortest to longest ancestry.
  The T@(ᵢ) serve to defeat the local order property in allowing the S@(ᵢ)
  to be put on the tails list in this pessimal order.
  Then each candidate Θ(n) times will be consulted an amortized Θ(d) times,
  each time causing
  (a, without optimization) an amortized Θ(d) searches through lists of amortized size Θ(n), or
  (b, with optimization) a Θ(1) table search followed by an amortized Θ(d) table maintenance adjustment.
  Worst case is indeed Θ(d²n²) for the unoptimized algorithm, Θ(dn) for the optimized one.
}
But by maintaining a single hash-table of counts on all tails,
I can keep the complexity O(dn), a quadratic improvement, that bring the cost of C3 or C4
back down to the levels of less consistent algorithms such as used in Ruby or Scala.
Unhappily, it looks like at least the Python and Perl implementations
are missing this crucial optimization; it might not matter too much because
their d and n values are I suspect even lower than in Common Lisp@xnote["."]{
  As mentioned in a previous note, in loading almost all of Quicklisp 2025-06-22, I found
  d≤3 99% of the time, d=61 max, n≤5 90% of the time, n≤19 99% of the time, n=66 max.

  Now at these common sizes, a linear scan might actually be faster than a hash-table lookup.
  However, a good “hash-table” implementation might avoid hashing altogether and fallback
  to linear scan when the size of the table is small enough
  (say less than 20 to 100, depending on the cost of hashing and consulting a table),
  all while preserving the usual API, so users are seamlessly upgraded to hashing
  when their tables grow large enough, and behavior remains O(1).
}

There is also a space vs time tradeoff to check subtyping of suffixes,
by using a vector (O(1) time, O(k) space per struct,
where k is the inheritance depth the largest struct at stake)
instead of a linked lists (O(k) time, O(1) space per struct).
However, if you skip the interstitial infix specifications,
suffix hierarchies usually remain shallow@xnote[","]{
  As mentioned in a previous note, in loading almost all of Quicklisp 2025-06-22, I found
  that k≤4 in 99.9% of cases, k=6 max.
  Note though that the pressure on struct inheritance is less for Lisp programs
  than say Java programs, since in Lisp, inheritance for behavior purpose is usually done
  through classes not structs. That said, if you could mix both kinds of inheritance,
  the same would probably still be said of structs, with behavior moved to classes
  that structs inherit from.
}
so it’s a bit moot what to optimize for.

@subsection[#:tag "CTBH"]{C3 Tie-Breaking Heuristic}

The constraints of C3 or C4 do not in general suffice
to uniquely determine how to merge precedence lists:
there are cases with multiple solutions satisfying all the constraints.
This is actually a feature, one that allows for additional constraints to be added@xnote["."]{
  For instance, some classes could be tagged as “base” classes for their respective aspects
  (like our @c{base-bill-of-parts} in @seclink{IME}), and we could require base classes
  to be treated before others. This could be generalized as assigning
  some “higher” partial order among groups of classes (metaclasses),
  that has higher priority than the regular order, or then again “lower” orders, etc.
}
at which point the linearization algorithm must use some heuristic
to pick which candidate linearization to use.

The C3 algorithm (and after it C4) adopts the heuristics that,
when faced with a choice,
it will pick for next leftmost element the candidate that appears leftmost
in the concatenation of precedence lists.
I believe (but haven’t proved) that this is also equivalent to
picking for next rightmost element the candidate
that appears rightmost in that concatenation@xnote["."]{
  Exercise: prove or disprove this equivalence.
}
Importantly, I also believe (but again, haven’t proved) this heuristic maximizes
the opportunity for a specification’s precedence list to share a longer suffix with its parents,
thereby maximally enabling in practice the optimizations of single inheritance
even when specifications are not explicitly declared “suffix”.

One interesting property of the C3 heuristic is that,
even if the language does not explicitly support suffix specifications,
by following the same discipline that Scala 2 forces upon you,
of always placing the most-specific suffix specification last in each specification’s
local precedence order (removing any of its now redundant ancestors for that order, if necessary),
you will obtain the same result if the language explicitly supported suffix specifications
using the C4 algorithm.
Thus, even without explicit language support for suffix specifications,
you could enjoy most of the optimizations of de facto single inheritance
if the implementation were clever enough to opportunistically take advantage of them.
This is interestingly a property shared by the Ruby and Scala algorithm,
but not by the original LOOPS algorithm that Scala tweaked.
This seems to be an important property for a tie-break heuristic,
that should probably be formalized and added to the constraints of C4@xnote["."]{
  I haven’t thought hard enough to say what other interesting properties, if any,
  are sufficient to fully determine the tie-break heuristic of C3,
  or of a better variant that would also respect the hard constraints of C3.
  I also leave that problem as an exercise to the reader.
}

@exercise[#:difficulty "Easy"]{
  Explain in your own words why the “suffix property” enables single-inheritance
  optimizations even in a multiple-inheritance system.
  What would break if a struct’s precedence list were @emph{not} a suffix
  of its descendant’s precedence list?
  Show an example of inheritance hierarchy for which the single-inheritance optimization
  do not apply.
}

@exercise[#:difficulty "Medium"]{
  Consider this example lifted from Wikipedia @~cite{WikiC3},
  with a base specification @code{O},
  specifications @c{A B C D E} that each inherit only from @c{O},
  specifications @c{K1 K2 K3} with respective parents (in total local order)
  @c{A B C}, @c{D B E} and @c{D A},
  and specification @c{Z} with parents @c{K1 K2 K3}.
  What are respective precedence lists of all these specifications with the C3 algorithm,
  all of them being assumed to be infix specifications?
}
@;{
Using the C3 or C4 algorithm, we get the precedence list @code{Z K1 K2 K3 D A B C E O},
with each subclass having its subset of ancestors in the same order
in its own precedence list.
}


@(if (render-html?)
  @image[#:scale 0.7]{C3_linearization_example.png}
  @image[#:scale 0.55]{C3_linearization_example.eps})

@;{
If, using the C4 algorithm, @code{C} were declared a suffix specification, then
the suffix @code{C O} must be preserved,
and the precedence list would be changed to @code{Z K1 K2 K3 D A B E C O}.
If both @code{C} and @code{E} were declared suffix specifications,
then there would be a conflict between the suffixes @code{C O} and @code{E O}, and
the definition of @code{Z} would fail with an error.

In this class hierarchy, only @code{O}, one of @code{C E}, and/or @code{Z}
may be declared a suffix specification without causing an error
due to violation of the local precedence order.
Indeed, a class may not be declared a struct if it appears in a direct superclass list
before a class that is not one of its superclasses.
However, this criterion is not necessary to prohibit struct-ability,
and @code{K3} cannot be a struct either,
because its superclass @code{D} appears before @code{B E} among the direct superclasses of @code{K2},
which would break the struct suffix of @code{K3}
when @code{Z} inherits from both @code{K2} and @code{K3}.
}

@exercise[#:difficulty "Medium"]{
  In the previous example, which specifications could be declared as suffix
  without changing any precedence list?
  Which could be declared as suffix in a way that would change some precedence list
  but would still the entire ancestry valid?
  Which would cause C4 to issue an error trying to compute a precedence list for @c{Z}
  if declared suffix?
}

@exercise[#:difficulty "Medium"]{
  Same questions with this example from @~cite{Ducournau1994},
  with the following lists of specification and its parents in local order:
  @c{Boat},
  @c{DayBoat Boat},
  @c{DayBoat WheelBoat},
  @c{EngineLess DayBoat},
  @c{PedalWheelBoat EngineLess WheelBoat},
  @c{SmallMultihull DayBoat},
  @c{SmallCatamaran SmallMultihull},
  @c{Pedalo PedalWheelBoat SmallCatamaran}.
}

@(if (render-html?) ;; 0.16, 0.10
  @image[#:scale 0.14]{C3_linearization_example_2.png}
  @image[#:scale 0.10]{C3_linearization_example_2.png})

@;{
The precedence list is:
@c{Pedalo PedalWheelBoat EngineLess SmallCatamaran SmallMultihull DayBoat WheelBoat Boat}.

Due to precedence constraints, any of @c{Pedalo Boat}, and
at most one of @c{WheelBoat DayBoat} could be declared a suffix specification,
with @c{DayBoat} being the only one to change the precedence list, to:
@c{Pedalo PedalWheelBoat EngineLess SmallCatamaran SmallMultihull WheelBoat DayBoat Boat}.

Interestingly, either @c{WheelBoat} or @c{DayBoat} can be made a suffix,
because they don’t simultaneously appear in any given class’s parent list,
so there is no local precedence order constraint between the two.

If there were no @c{EngineLess} between @c{PedalWheelBoat} and @c{DayBoat},
then @c{DayBoat} appearing before @c{WheelBoat} would prevent the former
from being made a suffix specification,
with the definition of @c{PedalWheelBoat} triggering an error.

A general solution that can be used to ensure @c{DayBoat} is a suffix specification
would be to swap the order of superclasses in the conflicting definition;
when the methods defined or overridden by the swapped superclasses are disjoint,
the swap will not otherwise change the semantics;
otherwise, the subclass can suitably override methods to compensate for the change.
And the other general solution in last resort is to introduce a do-nothing wrapper class
to shield a superclass from a local local precedence order constraint,
just like the @c{EngineLess} shields @c{DayBoat}.
}

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

@exercise[#:difficulty "Medium, Recommended"]{
  If you did exercise @exercise-ref{06to07}, compare your previous sketches
  of single and multiple inheritance with the treatment in this chapter.
  What did you get right? What surprised you?
  Did you anticipate the importance of linearization and the suffix property?
}

@exercise[#:difficulty "Hard, Recommended" #:tag "07to08"]{
  Based on my model of OO as modular extensibility,
  and on the informal explanations in @secref{WOOiIO},
  sketch how you would assign Types to OO.
  Save your answer to compare with the treatment in @secref{TfOO}.
}

@exercise[#:difficulty "Hard"]{
  With the help of AI if needed, implement the C4 algorithm as described above,
  in your favorite programming language.
  Or else, translate the version from Gerbil Scheme into it.
  Also port the Gerbil test suite to check that your implementation works correctly.
  To make it harder, make sure your algorithm is in O(nd),
  that sub-suffix checking is O(1), @emph{and} that a fast linear search
  rather than hash-table search is used if the tables are small enough.
  To make it a research-level project, get your favorite programming language
  to accept flavorful multiple inheritance with C4 linearization as its OO inheritance mechanism.
}

@exercise[#:difficulty "Research"]{
  Implement a two-level inheritance mechanism wherein
  each specification is an element of a specification-sort,
  the specification-sorts are themselves in a partial order to be linearized,
  and every specification comes after (respectively before)
  every specification of a higher-priority (respectively lower-priority) specification-sort.
  An actionable error is issued if there is no solution.
}

@exercise[#:difficulty "Research"]{
   See if there are other interesting properties or better heuristics for linearization,
   or prove that there aren’t.
   Prove or disprove the conjectures I haven’t proved about heuristics for C4 in @secref{CTBH}.
}
