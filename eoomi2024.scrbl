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
          syntax/parse/define
          "util/examples-module.rkt"
          "util/enumitem.rkt"
          "util/util.rkt"
          (for-label racket))

@(define-simple-macro (c a ...) (elem #:style 'tt a ...))
@(define-simple-macro (Code a ...) (verbatim a ...))
@(define-simple-macro (r a ...) (racket a ...))
@(define-simple-macro (Definitions a ...) (examples/module poof #:no-result a ...))
@(define-simple-macro (Examples a ...) (examples #:eval poof #:no-result a ...))
@(define-simple-macro (Checks a ...) (examples #:eval poof #:label #f a ...))
@(define-simple-macro (λ formals body ...) (lambda formals body ...))
@(define-simple-macro (TODO body ...) '())

@(define (anonymize x . y) x)
@(define (omega) "ω")
@(define (GerbilScheme) @anonymize["our Scheme implementation"]{Gerbil Scheme})
@(define (principle . x) (bold (emph x)))

@(declare-examples/module poof racket
   (provide (all-defined-out))
   (require syntax/parse/define))
@examples/module[poof #:hidden
   (define (Y f) ;; fixed-point operator
      (define (self x) (f self x))
      self)
]

@(define-bibtex-cite "poof.bib" ~cite citet generate-bibliography)

@section[#:tag "foo"]{FOO}

In the notable case of data records that may be later extended,
which is most relevant to OO,
access to named fields must go through some “associative array” (e.g. hash-table)
mapping identifiers to field index in the record.
These indexes may be cached between modifications, or wholly resolved at compile-time
if the extension is external or second-class.

@subsection[#:tag "incremental_modularity"]{Incremental Modularity}

@subsubsection{Reduction to Smaller Problems}

@; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX HERE XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

@subsubsection{Embodying Specification}
To embody some concept in the functional programming core of a language that has one,
you will necessarily use a function.
Since the concept is the specification of a computation,
the function must eventually return that specific computation as an output value,
given some inputs to be determined.
The type of its output is the type of the target value.

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
with the computation @c{self} as input for self-reference,
and returns as output a computation with the specified structure
that uses @c{self} in an @emph{open recursion} for all “self-reference”
to aspects the final computation (possibly further refined, extended or overridden).
That function then specifies (part of)
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

@subsection[#:tag "simplest_prototypes"]{Simplest prototypes}
@subsubsection[#:tag "mixin_functions"]{Mixin Functions}
The very simplest possible design for @emph{prototypes}
is thus as “mixin” functions with the following minimal type:
@Code{Mixin self super = self ⊂ super ⇒ self → super → self}
where @c{self} is the type of the computation as completely specified,
@c{super} the type of the computation as partially specified so far,
@c{self ⊂ super ⇒} is the constraint that @c{self} should be a subtype of @c{super},
and @c{Mixin} is the name introduced by Cannon@~cite{Cannon1979}
and reprised and popularized by Cook and Bracha@~cite{bracha1990mixin}.

The mixin instantiation and inheritance primitives are as follows:
@Code{
type Mixin inherited referenced defined =
  inherited → referenced → inherited⋂defined

build :: top → Mixin top target target → target
build = λ base mixin ↦ Y (mixin base)

inherit :: Mixin i1 r1 d1 → Mixin i2⋂d1 r2 d2 → Mixin i1⋂i2 r1⋂r2 d1⋂d2
inherit = λ parent child super self ↦ child (parent super self) self
}

@subsubsection{Elucidating Mixin Instantiation}
The @c{build} function above computes a fixed-point @c{target}
for a @c{mixin} given as extra argument a type-appropriate @c{base} value
that serves as seed of the computation being instantiated:
an empty record @c|{{}}|, a function that always fails @c{⊤ = λ _ ↦ ⊥}, etc.
The type of @c{base}, a.k.a. @c{top}, is thus
a base type for the specified computation:
a supertype of the type @c{instance} being computed, a.k.a. @c{self}.
In a monomorphic setting, @c{base} is just @c{instance} itself;
with a rich-enough type system, it can be a “top” type
for many distinct types of computations, carrying no information.

The @c{Y} combinator is the usual fixed-point combinator, chosen to match
the variant of λ-calculus being used (e.g. using eager or lazy evaluation).
@;TODO explaining footnote or citation.
@;The definition matches the @c{fix} function from the introduction
@;modulo α-renaming, well-named since its essence is to extract a fixed-point.

@subsubsection{Elucidating Mixin Inheritance}
Mixin inheritance combines two @emph{mixins} @c{child} and @c{parent}
into one that given two @emph{instances} @c{instance} and @c{inherited}
passes @c{(parent instance inherited)} as the @c{super} argument to @c{child}.

By the time the complete @c{instance} and @c{inherited} value so far
are provided (if ever), the combined mixin itself may be but part of
a wider combination, with further mixins both to the right and to the left.
The provided @c{instance} will then be the fixed-point of
the entire wider combination (involving further children to the left,
then @c{child} and @c{parent}, then further parents to the right).
Meanwhile, the @c{inherited} value will only contain the information from
applying the further parent mixins to the right to the provided @c{base} object.
The @c{parent} will be able to extend (enrich or override)
any method definition from the @c{inherited} computation;
the @c{child} may further extend it, and further mixins to the left yet more.

The function matches the @c{mix} function from the introduction
modulo α-renaming, well-named since its essence is to compose or “mix” mixins.
The function is associative, with identity mixin @c{idm = λ s t ↦ t}.
As usual, a change of representation from @c{p} to @c{cp = inherit p}
would enable use regular function composition for @c{mix},
whereas @c{fix} would retrieve @c{p} as @c{cp idm};
but that would make the types unnecessarily more complex.
@; many isomorphic ways: change the order of super and self,
@; use fix and mix, or cfix and idm as basic combinators, etc.

@subsubsection[#:tag "stricter_types"]{Stricter, More Modular Types}

The types given in @seclink{mixin_functions} work well,
but then must be carefully chosen so the @c{self} and @c{super}
used during mixin definition should precisely match those used
during mixin composition and instantiation.
This is not a problem if a mixin is used only once
(as in single inheritance, see @seclink{single_inheritance}),
but it is a problem in the more general case of mixin inheritance
(and in multiple inheritance, see @seclink{multiple_inheritance}).

A more refined type that can be used for mixins is then:
@Code{Mixin inherited referenced defined =
        ∀ super ⊂ inherited ⇒
           super → referenced → super⋂defined}
where @c{inherited} is the type intrinsic to the mixin
indicating which inherited methods from the super argument are actually being used,
whereas @c{super} will be the set of methods effectively defined
in the super argument, depending depending on the context of instantiation.

This type is an intersection of all variants of the previous type
for subtypes @c{eself} and @c{esuper} of @c{self} and @c{super} respectively.
It allows a mixin to be defined in its most general form,
then used multiple times, each in a distinct more specialized context,
making the mixin definition and its typechecking @emph{more modular}.
In exchange for this modularity, the mixin is restricted
to only act in a uniform manner, that monotonically preserves
arbitrary additional information passed as arguments to it.

@; For instance, the mixin is not allowed to query which set of fields
@; will be effectively used in the super in practice to decide
@; which fields it will itself define.
@; It is not allowed to rename fields from the super, etc.

@; Try with higher kinds for self and super, so it’s structurally required
@; that the mixin should use the eself parameter for reference,
@; and return an extended super for its structure?

@subsubsection[#:tag "minimal_design_maximal_outreach"]{Minimal Design, Maximal Outreach}
We have just derived from first principles a minimal design
of prototypes-as-mixin-functions
to embody modular increments of software specification
inside a functional programming language.
And this design closely reproduces that of existing models and languages:
@itemize[
#:style enumparenalph
@item{It reproduces the earliest general semantic model of OO@~cite{bracha1990mixin}.}
@item{It also reproduces the formal semantics (though not the implementation) of objects
in the pure lazy dynamic functional prototype object language Jsonnet@~cite{jsonnet},
a popular choice to generate distributed software deployment configurations
for Kubernetes or AWS, and was started as a conceptual cleanup of}
@item{the Google Control Language GCL@~cite{gclviewer2008} (née BCL, Borg Control Language),
which has been used to specify all of Google’s distributed software deployments
since about 2004 (but uses dynamic rather than static scoping,
causing dread among Google developers).}
@item{It furthermore reproduces not just the semantics but the actual implementation
of “extensions”@~cite{nix2015} as a user-level library
in the pure lazy dynamic functional language Nix;
these extensions are heavily used by NixOS@~cite{dolstra2008nixos},
a Nix-based software distribution for Linux and macOS, one with thousands of contributors.@note{
These extensions were reinvented semi-independently by Peter Simons,
who did not know anything about their relationship to Prototypes, Mixins or OO,
but was inspired by examples by and discussions with Andres Löh and Conor McBride,
who were more versed in this literature.
}}]
@; TODO see if NixOps, DisNix, flakes, use extensions or a variant thereof, if so mention it.

The main difference between our minimal model and the above works is that
our model generalizes them by not being tied to any specific encoding of records,
or indeed to records at all (see @seclink{instances_beyond_records})

This simplest of object-oriented designs,
purely functional prototypes as mixin functions,
has thus been proven capable to literally support
specification and deployment of software on a world-wide scale.
As we’ll see, this design embodies the primitive core of OO,
to which other forms of OO can be reduced.
In the end, we can rightfully claim that the essence of OO
in historical intent as well as practical extent is
the incremental modularity embodied as language entities,
and that prototypes are the most direct form of this embodiment.
@TODO{cite to substantiate}

@subsection{Working with Records}

@subsubsection{Records, Methods, Instances}
Most OO tradition, including the precedents cited above, follows
the historical restriction of only enabling modular and incremental specification of
@emph{“records”} mapping names to values@~cite{hoare1965record Cook1989}.
The names, the values they are bound to, and/or the bindings,
are at times called @emph{“methods”}, “slots”, “fields”, “attributes”, “properties”, “members”,
“variables”, or otherwise, depending on the specific sub-tradition.

The records themselves will be suitably wrapped into a proper computation result @emph{instance}:
a class (in Class OO),
an object (in Prototype OO),
a typeclass (in FP with typeclasses, though its users may deny the OO tradition),
wherein the record will embody the “method dispatch table”,
“attribute set”, “dictionary” or whatchamacallit of the aforementioned entity.

Note that this meaning of the word @emph{instance} itself comes from the Prototype OO tradition,
and does not match what the meaning of the word in the class OO tradition;
in the latter tradition, “instance” instead refers to an element of the class seen as a type,
whereas that type would be the instance in the prototype OO tradition.
For now we will focus on the simplest and most primitive kind of OO, Prototype OO,
in its simplest form where the instances are the records themselves.
We will extend our point of view in @seclink{inheritance} and later.

@subsubsection[#:tag "encoding_records"]{Encoding Records}
We will assume that, either with some language primitives,
some “builtin modules” to import from,
or some variant of Church encoding, our Functional Language
is suitably extended with the usual essential data structures:
numbers, booleans, strings, tuples, lists.
Record keys can be of a language-appropriate type with a decidable equality predicate:
integers (sometimes as named constants at the meta-level),
strings, or optionally symbols (interned strings) or
identifiers (source code tracking entities).

Records can be defined from the empty record @c{rtop} and
a constructor @c{rcons k v r} that given a key @c{k}, a value @c{v} and
a previous record @c{r} returns a new record that extends @c{r}
with a new or overriding binding of @c{k} to @c{v}.
The three simplest encodings of a record would then be
as a function, an @emph{alist}, or a mapping table, as follow.

Records as functions is the simplest encoding, and
accessing the value for a key is done by just calling the function with the key.
However, overriding and deletion will leak memory and access time;
also they don’t support iteration over bindings —
an introspection operation that is very much desired in contexts like I/O automation,
though best kept hidden in contexts like analysis or restriction of software effects.
The two constructors are as follows:
@Code|{ftop = ⊤ = λ _ ↦ ⊥
fcons = λ k v r m ↦ if m == k then v else r m}|

The traditional Lisp “alist” (association list) data structure,
singly-linked list of (key,value) pairs,
solves the previous encoding’s issues with memory leak and lack of introspection,
but is still inefficient with linear-time operations.
Its two constructors are as follows:
@Code|{atop = []
acons = λ k v r ↦ [(k,v), ...r]}|

Records as pure mapping tables can provide logarithmic-time operations;
but their implementation can be complex if not provided as a language primitive.
Binding accessor, binding presence test, binding deletion, etc.,
are left as an exercise to the reader.
We will write their constructors as follows:
@Code|{mtop = {}
mcons = λ k v r ↦ {k: v, ...r}}|

In our previous article@~cite{poof2021} we showed how you could start with
a simple of records as function, use OO style to incrementally and modularly specify
a more elaborate mapping table data structure, and thereafter use that data structure
in the definition of more efficient further records.
That’s our first case of a “meta-object protocol”@~cite{amop}, one that illustrates
how to @emph{bootstrap} more elaborate variants of OO from simpler variants.

@subsubsection{Mixins and Helpers for Records}
Abstracting over the specific encoding for records,
the primitive way to define a mixin that adds a method to a record being specified is with:
@Code{methodG = λ rkons k f s t ↦ rkons k (f s t) t}
wherein the argument @c{k} is a key naming the method,
@c{f} is a function that takes the instance @c{s} of type @c{self} and
a inherited record @c{t} of type @c{super} and returns a value @c{v}
to which to bind the method in a record that extends the inherited record,
according to the record encoding defined by @c{rkons}.

In practice, OO language implementations provide a fixed builtin encoding for records,
with specialized instantiation function @c{fixR} and method-addition mixin @c{methodR}:
@Code{fixR = λ mixin ↦ fix mixin rtop
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
      fixR* = fix* rtop}

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
Thus @principle{merging disjoint commuting mixins embodies modularity, but not incrementality}:
incrementality can still be achieved in an extralinguistic way by
rebuilding modules in different ways from smaller modules;
but to achieve it intralinguistic, you need a way to operate on existing modules,
which by definition is not commutative.

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
that in general depends on other aspects being defined by other prototypes.

@; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX HERE XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

@section[#:tag "inheritance"]{Mixin, Single, and Multiple Inheritance}

@subsection{Mixin Inheritance}
@subsubsection{The Last Shall Be First}
The inheritance@~cite{inheritance1996} mechanism described above
is called @emph{mixin inheritance}.

It is also relatively more obscure, probably because, in addition to the above,
it is less modular than the more complex but previously discovered
multiple inheritance (discussed below in @seclink{multiple_inheritance}).

And yet, we already saw above in @seclink{minimal_design_maximal_outreach} that
object prototypes with mixin inheritance are used to specify software configurations at scale.
An elaborate form of mixin inheritance is also notably used in the class-based OO system
used by Racket’s GUI@~cite{Flatt06schemewith}.

@subsubsection{Mixin Semantics}
We saw above (@seclink{simplest_prototypes}) that mixin inheritance involves just
one type constructor @c{Mixin} and two functions @c{fix} and @c{mix}:
@Code{Mixin self super = self ⊂ super ⇒ self → super self
fix : Mixin self top → top → self
fix = λ mixin top ↦ Y (λ self ↦ mixin self top)
mix : Mixin self super → Mixin super duper → Mixin self duper
mix = λ child parent self duper ↦ child self (parent self duper)}

@subsection[#:tag "single_inheritance"]{Single inheritance}

@subsubsection{Semantics of Single Inheritance}
In single inheritance, the prototypes at stake,
i.e. the entities that embodied increments of modularity,
are not the mixin functions of mixin inheritance,
but simpler @emph{generators} that only take a @c{self} as open recursion parameter
and return a record using @c{self} for self-reference.
The semantics can reduced to the following types and functions:
: @; TODO CITE Cook
@Code{Gen self = self → self
Y : Gen self → self
base : Gen top → top
base = λ _ ↦ rtop
extend : Mixin self super → Gen super → Gen self
extend = λ mixin parent self ↦ mixin self (parent self)}
@;     = λ mixin parent self ↦ (mix mixin λ self _ ↦ parent self) self base

Note how @c{Gen self} is the type of generators for instances of type @c{self};
the instantiation function for a generator is the usual fixed-point combinator @c{Y};
the @c{base} object to extend is the generator that always returns the empty record
(for whichever encoding is used for records);
and the @c{extend} function creates a child generator from a parent generator
and a mixin (as in mixin inheritance above), where @c{self} is constrained
to be a subtype of @c{super}.

Mind again that in the single-inheritance paradigm,
@emph{the prototype is the generator, not the mixin}.
A prototype-as-generator may thus be the @c{base} generator
that returns the empty record @c{rtop} or otherwise base instance,
or a generator created by extending
a @emph{single} @c{parent} generator with a @c{mixin}.
Since the same constraint applies recursively to the parent generator,
a prototype-as-generator can be seen as repeatedly extending that @c{base} generator
with an ordered list of mixins to compose.
Just like in mixin inheritance, an @emph{instance} can thus still be seen as
the fixed point of the composition of a list of elementary mixins
as applied to a base instance.
However, since generators, not mixins, are the prototypes,
the “native” view of single inheritance is more to see the parent specified in @c{extend}
as a direct super prototype, and the transitive supers-of-supers as indirect super prototypes;
each prototype is considered as not just the mixin it directly contributes,
but as the list of all mixins directly and indirectly contributed.

@subsubsection{Single Inheritance with Second-Class Mixins}
While single-inheritance requires some form of mixin,
most single-inheritance object systems don’t allow mixins as
first-class entities that can be independently composed.
Rather mixins are only linear second-class syntactic entities
and can only be used once, immediately, as part of an extension.
You cannot consider a mixin or list of mixins independently,
and @emph{append} such lists together;
you cannot abstract a base or super instance away from a generator to extract its mixin;
you can only @emph{cons} a single new elementary mixin
to the list of mixins implicit in a previous generator and already applied to its base.

This will particularly matter when we see that in most Class OO languages, @; TODO REF
prototype inheritance happens in a restricted language at the type level,
one with limited abstraction and no way to express appending from consing.

Then again, if language starts with single-inheritance OO, but
@emph{does} allow mixins as first-class entities that can be composed,
then it actually supports mixin inheritance, not just single inheritance,
just like the Racket class system does@~cite{Flatt06schemewith},
or like typical uses of extensions in Nix go.
It thus only makes sense to speak of single inheritance in a context where
the language syntax, static type system, dynamic semantics,
or socially-enforced coding conventions
somehow disallow or strongly discourage mixins as first-class entities.

@subsubsection{Lack of expressiveness and modularity}
The limitations to single inheritance translate into lack of expressiveness
relative to mixin inheritance.
Thus, in an OO language with single inheritance,
you can define a prototype @c{Point} with two coordinates @c{x} and @c{y}
with two children prototypes @c{ColoredPoint} and @c{WeightedPoint}
that respectively extend it with an attribute @c{color} and an attribute @c{weight}.
But if you want a @c{WeightedColoredPoint} that has both @c{color} and @c{weight} attributes,
you have to choose at most
one of the two prototypes @c{ColoredPoint} and @c{WeightedPoint} to inherit from,
and repeat all the definitions of the other’s mixin.

In case you want a prototype to possess all the methods defined
in each of two or more long mixins or long lists of mixins are involved,
you will have to repeat all the definitions from all but one existing list of mixins.
You can always resort to copy/pasting the definitions from one class to the other;
but that is unreliable and fragile as maintenance operations now need to happen
simultaneously in multiple copies that the developer must track down,
and that can easily grow subtly out-of-synch as the developer is fallible.
Worse, this is an extra-linguistic means, so that
inasmuch as you then still achieve incremental modularity,
it is no longer @emph{within} the language, only @emph{outside} it.
By contrast with mixin inheritance or multiple inheritance,
you could easily combine together
all the elementary mixins from each of the many prototypes-as-mixins
that you want to simultaneously extend.

This concludes our proof that single inheritance is strictly
less expressive@~cite{eppl91}
and less modular @;TODO CITE / TODO REF
than mixin and multiple inheritance.

@subsection[#:tag "multiple_inheritance"]{Multiple inheritance}
@subsubsection{More Sophisticated}
A third kind of inheritance is @emph{multiple inheritance},
that historically appeared before mixin inheritance was formalized@~cite{Cannon1979},
and that is more sophisticated than the two above.
It was popularized by the Lisp object systems
Flavors, Common Loops, New Flavors and CLOS@~cite{bobrow88clos},
then by Self and C++. @;TODO cite
@; cite New Flavors and CommonLoops 1986 ?
@; C++ started in 1982, only has multiple inheritance since v2.0 in 1989.
These days it is notably used in Python, Scala or Rust.

Like mixin inheritance, multiple inheritance allows developer
to create a new prototype using more than one existing prototype as super prototype,
lifting the main limitation of single inheritance.
Like single inheritance, multiple inheritance allows developer to declare dependencies
between prototypes, such that a prototype can have indirect, transitive dependencies
implicitly included as super prototypes, as well as direct super prototypes.

@subsubsection{Prototypes as a DAG of mixins}

Since each prototype inherits from multiple parents rather than a single one,
the inheritance hierarchy is not a list, but a Directed Acyclic Graph (DAG).
Each prototype is a node in the overall DAG.
The super prototypes explicitly listed as dependencies it inherits from
when defining it are called its @emph{direct supers}.
But the set of all a prototype’s super prototypes
includes not only those direct supers, but also indirectly
the supers of those supers, etc., in a transitive closure.

The prototype’s supers thus constitute a DAG,
that is an “initial” sub-DAG of the DAG of all prototypes,
that includes all the prototypes directly or indirectly “above”
the considered prototype, that is at the very bottom of its DAG.
The super prototype relation can also be viewed as a partial order on prototypes
(and so can its opposite sub prototype relation).

This is in contrast with single inheritance, where this relation is a total order,
each prototype’s super hierarchy constitute a list, and
the overall hierarchy of all prototypes is a tree.
This is also in contrast with mixin inheritance, where
each mixin’s inheritance hierarchy can be viewed as a composition tree,
that since it is associative can also be viewed flattened as a list,
and the overall hierarchy is a multitree… except that a super prototype
(and its own supers) can appear multiple times in a prototype’s tree.

Each prototype is thus a node in the inheritance DAG.
To represent it, a prototype will then be not just a mixin function,
but a tuple of:
@itemize[#:style enumparenalph
@item{a mixin function, as in mixin inheritance,
  that contributes an increment to the modular specification,}
@item{an ordered list of direct super prototypes it inherits from,
  that specify increments of information on which it depends, and}
@item{a unique name (fully qualified path, string, symbol, identifier, or other tag)
  to identify each prototype as a node in the inheritance DAG.}]

@subsubsection{Precedence Lists}

Then comes the question of how to instantiate a prototype’s inheritance DAG
into a complete specification,
of how to reducing it to a @emph{generator} as for single inheritance.

A general solution could be to compute the instance, or
some seed value based on which to compute the instance,
as an @emph{inherited attribute} of that inheritance DAG. @TODO{cite attribute grammars?}
For instance, a generator (as in single-inheritance above) of which to take a fixed-point,
could be computed by having each mixin function be of type @c{self → super → self}
where each direct super prototype is of type @c{super_i}
and @c{super} is the @emph{product} of the @c{super_i}.

However, the increment of specification from each prototype
must be taken into account @emph{once and only once} in the overall specification;
and the order in which these increments are taken into account
must be @emph{consistent} from one method computation to another.
If individual mixin functions had to take a tuple or list of inherited attributes,
they would have a hard time untangling the already mixed in effects of other mixins,
to reapply them only once, what more in a consistent order.

Therefore, multiple inheritance uses a more refined mechanism, wherein
the inheritance DAG for a prototype is reduced to a list of prototypes,
the @emph{precedence list}.
An instance is then as per mixin inheritance (or, equivalently, single inheritance),
by combining the mixin functions of the supers,
in the order given by the precedence list, with a universal top value.
Each mixin function remains of type @c{self → super → self},
where the @c{super} argument is the @emph{intersection} of the types @c{super_i},
not just of its direct super prototypes, but, effectively,
of all the super prototypes after it in the precedence list
(see @seclink{stricter_types} for handling that gap).

The precedence list is itself computed,
either by walking the DAG @~cite{Cannon1979 bobrow88clos scalableComponentAbstractions2005}
or as an inherited attribute,
using the prototype names to ensure the unique appearance
of each super in the resulting list.
The precedence list can be viewed as a total order that extends and completes
the partial order of the inheritance DAG.
Modern algorithms like C3@~cite{Barrett96amonotonic WikiC3}
further ensure “monotonic” consistency between the precedence list of a prototype
and those of its supers, such that the former extends the latter
as well as the list of supers itself.

Complete implementations of prototypes using multiple inheritance
in a few tens of lines of code are given
in our previous paper using Scheme@~cite{poof2021},
or in a proof of concept in Nix@~cite{POP2021}.
Our production-quality implementation in Gerbil Scheme@~cite{GerbilPOO}
including many features and optimizations fits in about a thousand lines of code.

@subsubsection{More Expressive than Mixin Inheritance}
Multiple inheritance requires measurably more sophistication than mixin inheritance,
and hence an additional cognitive burden.
Why would anyone use that instead of just using mixins?
Because it is more expressive, and more modular,
and its cognitive burden pays for itself by alleviating
the other cognitive burdens from developers.

The multiple inheritance is no less expressive than mixin inheritance
is simple enough to prove: you can macro-express@~cite{eppl91}
mixin inheritance within multiple inheritance.
Replace each mixin function by a prototype using that mixin function,
a empty direct super list.
Keep around lists of such prototypes rather than mix them,
then before you instantiate, create a single prototype with an identity mixin
that depends on the list of mixins as direct super prototypes,
where each mixin was given a fresh name,
to ensure that multiple copies are all used indeed.

This trick with fresh names at the last minute is necessary to defeat
multiple inheritance otherwise ensuring that a given prototype
(as identified by its name) will be used once and only once in the precedence list.
But this unicity is actually a feature that the users usually want
(and if they somehow do want multiple uses of a mixin,
they can explicitly use multiple copies of it with distinct names).

@subsubsection{More Modular than Mixin Inheritance}
In practice there is always a dependency order between prototypes,
whether it is reified as an automatically managed in-language entity
as with multiple inheritance,
or left as an extra-language entity that developers must manually keep track of
as with mixin inheritance.
Thus, a prototype may depend on a method having been declared or implemented
by a (transitive) parent, so it may use or override it.
That parent that must appear before it in the precedence list of prototypes
(in the right-to-left order of application to the base instance
with the convention we use above).
Moreover, each prototype should appear only once in the precedence list,
because its effects may not be idempotent,
or may cancel the effects of other prototypes found in between two copies.

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

@subsubsection[#:tag "single_and_multiple_inheritance_together"
   ]{Single and Multiple Inheritance Together}

Some languages such as CLOS@~cite{bobrow88clos}
allow for both single-inheritance @c{struct}s and multiple-inheritance
@c{class}es with uniform ways of defining object and methods.
Thus, programmers can benefit from the performance advantage in slot access
or method dispatch possible where there is no multiple-inheritance,
while still enjoying the expressiveness and modularity of multiple-inheritance
in the general case. They can explore without constraint, and
simply change a flag when later optimizing for performance.

However, in CLOS, structs and classes constitute disjoint hierarchies.
Some languages further allow structs and classes to inherit from each other,
within appropriate constraints.
Thus Scala@~cite{scalableComponentAbstractions2005}
allows a single struct to inherit from classes
(except, to fit the Java and Smalltalk traditions rather than Lisp tradition,
it calls the single-inheritance structs “classes”, and the multiple-inheritance classes “traits”).
Gerbil Scheme supports the least set of constraints that preserve the coherence
of both structs and classes, by suitably extending the C3 algorithm.

C3 crucially frames the problem of superclass linearization in terms of
constraints between the precedence lists of a class and of its superclasses:
notably, the precedence list of a superclass must be an ordered subset
of that of the class, though its elements need not be consecutive.
To support structs and their optimizations, we only need add a constraint that
the precedence list of a struct must be a suffix of that of its substructs
(when considered in the order from most specific to least specific, as is
customary in languages with multiple inheritance, after the Lisp original).

At that point, we realize that what characterizes structs is not exactly
“single inheritance” since a struct can now have multiple superclasses,
and a class can now inherit from a struct indirectly via multiple superclasses.
There is still single inheritance of sorts between structures, in the sense
that the superstructures of a structure constitute a finite total order,
when you ignore the other classes in the inheritance.
But by this observation, by ignoring these other classes, fails to characterize structs.
Instead, what characterizes structs is this “suffix” constraint on precedence lists,
which include all classes, not just structs.
This characterization in turn harkens back to the original Simula name
of “prefix” for a superclass:
Simula was then considering its single-inheritance precedence list
in the opposite order, from least specific to most specific superclass
(though the vocabulary to say so didn’t exist at the time).
And this semantic constraint can be expressed
in a system that has multiple inheritance.

@subsubsection{Under-Formalized}
Many notable papers offer proper treatment of
multiple inheritance as such@~cite{allen2011type}.
@TODO{cite more: Jonathan Aldrich ? Odersky ?}
However, multiple inheritance often remains
unjustly overlooked, summarily dismissed,
or left as an exercise to the reader in academic literature
that discusses the overall formalization of
programming languages and OO@~cite{Abadi97atheory tapl eopl3 plai}. @TODO{more?}

Many computer scientists interested in the semantics of programming languages
seem to either fail to understand or fail to value
the modularity enhancement from multiple inheritance
over single inheritance or mixin inheritance;
or they are not ready to deal with the extra complexity
needed to formalize multiple inheritance, for instance due to
requiring richer type systems.@~cite{Cardelli1984ASO}

And yet languages that care more about expressiveness, modularity and incrementality
than about ease of writing performant implementations with simpler type systems,
will choose multiple inheritance over the less expressive and less modular alternatives:
see for instance Common Lisp, C++, Python, Scala, Rust.
@TODO{cite Scala OO model. What else? Kathleen Fisher’s thesis?}

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

@subsection{Class OO as Type-Level Prototype OO}

@subsubsection{Type Prototypes}
Having fully elucidated Prototype OO in the previous sections,
including its notion of Object as conflationg of Prototype and Instance,
we can now fully elucidate Class OO including its notion of Class:
@principle{A Class is a Prototype for a Type}.

Class OO is therefore a special case of Prototype OO,
though one where prototype computations only happen at the type-level.
The instances incrementally specified by these prototypes are @emph{types}—or
more precisely @emph{type descriptors},
usually available at compile-time only in Class OO languages,
in a form of staged metaprogramming. @TODO{cite}

Thus when we claimed in @seclink{not_about_classes} that
the situation of classes in OO was similar to that of types in FP,
we meant it quite literally.

@subsubsection{Class OO makes classes Second-Class}
Now, the language in which these type prototypes are defined and composed is
not the usual “base language” that the programmer
is usually programming in (e.g. C++, Java, C#),
but instead a distinct @emph{type-level language} in which the types and the
base-level functions operating on them are being incrementally specified.

The type-level language used in a language with Class OO
is usually is restricted in expressiveness,
in an often deliberate attempt to keep it from being “Turing-equivalent”.
This attempt sometimes succeeds (as in OCaml), but more often than not
utterly fails, as computational power emerges from unforeseen interactions
between language features added over time (as in C++, Java, Haskell).
@TODO{cite Nada on Java type system constraints}
The attempts do usually succeed, however, at making these type-level languages
require a completely different mindset and very roundabout design patterns
to do anything useful, a task then reserved for experts.

Computationally powerful or not, the type-level language of a Class OO language
is almost always very different from the base language:
the type-level languages tend to be pure functional or logic programming languages
with pattern-matching and laziness but without any I/O support,
even though the base languages themselves tend to be
eager stateful procedural languages with lots of I/O support
and often without pattern-matching or laziness
(or limited ones as afterthoughts).

In the end, classes are thus not @emph{first-class} entities in Class OO
(subject to arbitrary programming at runtime),
but @emph{second-class} entities (restricted to limited compile-time programming),
though many languages offer limited reflection capabilities at runtime.
By contrast, classes are first-class entities in Prototype OO;
and indeed, one of the first applications of Prototype OO in any language
is often to build rich runtime type descriptors, that include features
not usually expressible with compile-time type descriptors
or their runtime representation as sometimes accessible through “reflection”,
such as extra constraints, context-dependent I/O, property-based testing support, etc.

@subsubsection{More Popular yet Less Fundamental}
Class OO was historically discovered (1967)
nine years before Prototype OO (1976),
and remains overall more popular in the literature.
The most popular OO language, JavaScript, started with Prototype OO only (1995),
but people were constantly reimplementing classes on top, and twenty years later
classes were added to the language itself@~cite{EcmaScript:15}.

And yet we will argue that Class OO is less fundamental than Prototype OO:
it can indeed be very easily expressed in terms of Prototype OO and implemented on top of it
(as exemplified many times over in JavaScript),
such that inheritance among classes is indeed a special case of
inheritance among the underlying prototypes,
whereas the opposite is not possible:
Class OO offers little to no advantage in implementing Prototype OO
over directly implementing it on top of FP,
and it is not universally possible to build Prototype OO such that
a prototype’s inheritance structure is verily the inheritance of an underlying class
(since the former is always first-class but the latter usually second-class).

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

@subsubsection[#:tag "mutation"]{Mutation}
The performance optimizations and semantic issues related to mutability in OO.

Also, what the relationship between object systems
that allow mutation of the inheritance DAG (Smalltalk, Self, CLOS)
and their pure sematic models?

Inasmuch as mutation is seen as meaning
“anything can become anything else at the drop of a hat”,
then the static semantics of everything is essential trivial;
there is total chaos and uncertainty in the mind of the software analyst.
But inasmuch as mutation is seen as meaning
“inheritance hierarchies are being set up before they are used,
but don’t change while being used
though they might change before, after and between uses”,
with mutation happening at some notional meta-level or staging area
with respect to the inheritance hierarchy,
then the pure semantic model does help describe how the system behaves
while the inheritance hierarchy is being used in a given locally unchanging state.

Now, if a system uses mutation to crucially modify “itself” in general
and its inheritance hierarchy in particular while executing,
then indeed the pure semantic model will prove insufficient
to describe the behavior of the system.
A more refined, lower-level model of how mutation of the inheritance hierarchy
interferes with flow control in ongoing operations will become necessary.
Yet the pure system remains a benchmark for how the system does or should behave
in the extents during which the inheritance hierarchy was left undisturbed.

@subsubsection{Monotonicity}
Why Subclassing is rarely Subtyping, and other questions of monotonicity,
    (co-, contra- and in-) variance in Functor Mixins and Fixed-Point Operators.

@subsubsection[#:tag "typeclasses"]{Typeclasses}
The relationship between Classes and Typeclasses.
    How typeclasses make object creation less ad hoc and more modular.

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

@section{What Object-Orientation @emph{is}}

@subsection{Incremental Specification}

@subsubsection{Records}

Colored Point

Linked

@subsection{Conflation}

@subsubsection{Prototypes as Conflation}
@itemize[
@item{The (extensible, composable) partial @emph{specification} of an open computation}
@item{The @emph{value} computed by declaring this specification complete and “closing” the computation}]

Herein by “conflation” we mean that depending on the context, one or the other is meant:
when composing a prototype with other prototypes or otherwise extending it,
the partial specification is meant;
when calling a method on the prototype, the computed value is meant.
Formally, the prototype is a cartesian product, with implicit cast
via projection to either factor depending on what the context requires @~cite{poof2021}.

@subsubsection{Classes as Conflation}
In Class OO, the entities of interest are @emph{prototypes for types},
considered together with associated functions.
The conflation a type specification and its specified type is then called a @emph{class},
and not an “object” as in Prototype OO;
instead the word “object” in Class OO denotes
an element of such a specified type—a very different notion.
This is a big source of confusion for people trying to compare Prototype OO and Class OO,
especially since in both cases the value of a Prototype and the element of a Class type
are usually @emph{records}.
When only the type specification is intended to be used, by inheriting from it,
OO practitioners speak of an @emph{abstract class}.
When only the type is intended to be used, by creating and manipulating elements of it,
OO practitioners speak of a @emph{concrete class}.
OO practitioners have thus established practical vocabulary to deal with this distinction,
that theorists had neglected.

@subsubsection{Conflation vs Confusion}

Now, conflation without proper distinction is @emph{confusion}.
Most OO practitioners and theorists, unable to properly conceptualize
specification and value as distinct entities, become mightily confused.
Theorists’ attempts at giving a semantics to OO then fail by being overly complicated
for trying to keep dealing with both entities conflated at all time,
@; XXX cardelli abadi, pierce
or overly simplistic because they fail to even notice that there are conflated notions to explain,
and only model the simplest fixed-point behavior
while neglecting any of the more advanced aspects of OO,
often being ignorant of the practice and purpose of OO to begin with. @; XXX cook, sk
Yet once you tease these two notions apart, the semantics of OO becomes quite simple@~cite{poof2021}.

The worst symptom of this confusion between specification and value is
the vain quest whereby too many language designers have tried to identify
subclassing (a useful syntactic relationship between specifications)
and subtyping (a useful semantic relationship between values). @; XXX Eiffel, Java, C#, Smalltalk
This identification might have seemed sensible in the 1960s and 1970s when OO was new and misunderstood,
and might actually make sense enough in dynamic languages where “types” don’t recurse past one data cell,
as in Lisp and Smalltalk where OO was pioneered.
But it never made sense in static languages, and has been formally known to be false
since the 1980s. @; XXX cardelli
What more, this identification is absurd on its face
to who understands the distinction and relationship between specification and value,
and the fact that the fixed-point operator is not monotonic.

@subsubsection{Specifications}

The semantics of prototype specifications can be described in terms
of @emph{wrapper} functions of two arguments @r[self] and @r[super] @~cite{Cook1989 bracha1990mixin}@note{
Note that Flavors @~cite{Cannon1979} introduced the term “wrapper” to mean
something altogether different, which in the more modern CLOS @~cite{gabriel1991clos}
you would express using an @r[:around] method, or,
in the more general case, a declarative method combination.
Since there are now better words for more fleshed out variants of that concept,
we will dismiss this earlier use of the word “wrapper” in the context of OO,
and instead stick to meaning used by Cook @~cite{Cook1989} to model inheritance.
}:
@itemlist[
@item{
The @r[self] argument refers to the value computed by the complete specification.
It enables “late binding”, so that the specification can create circular references,
use aspects of the computation specified by other prototype wrappers that it will be composed with,
and otherwise access the global computation of which it specifies one aspect,
thus embodying the @emph{modular} side of OO.
The wrapper function is said to be in an “open recursion”, @; XXX cite Pierce ?
meaning that after composing this wrapper with other wrappers into a complete specification,
this variable will be computed as a fixed point of the specified computation.@note{
@r[self] is the variable or keyword conventionally used to describe the object itself
(fixed point of the computation) within the specification of a prototype,
in Smalltalk, Lisp, Scheme, SELF, Python, Jsonnet, Nix, many more languages,
and in the literature expressing OO semantics in terms of functional programming.
In SIMULA, and after it in C++, Java, JavaScript or Scala, the @r[this] keyword is used instead.
Though SIMULA came first, we’ll follow the Smalltalk, Lisp and semantics-elucidating traditions,
that were the first to introduce and model “inheritance” as such, and especially multiple inheritance.
Mind, though, that we are currently discussing Prototype OO,
where the @r[self] has a related but notably different meaning
from what @r[self] or @r[this] would mean in a Class OO language.
See below the discussion of the meaning of “object” in Prototype OO vs Class OO.}}
@item{
The @r[super] argument refers to the partial value computed so far
from @emph{parent} specifications (composed to the right, with the usual composition order);
the rightmost seed value of @r[super] when computing the fixed point is a base value,
typically an empty record, possibly enriched with metadata or ancillary fields for runtime support.
@r[super] embodies the @emph{incremental} side of OO,
enabling a specification to build on values @emph{inherited} from parent specifications,
add aspects to them, override aspects of them, modify aspects of them, etc.,
in an increment of computation.@note{
@r[super] is the keyword or variable used in Smalltalk and in many languages and object systems after it,
such as Java or Python, to access inherited methods. In CLOS you’d use @r[call-next-method].
In C++ you can’t quite express that concept in general, because you have to name
the superclass whose method (or “(virtual) member function”) you want to call,
so you can’t directly write traits that inherit “super” behavior along the class precedence list.
Naming the super works for the single inheritance subset, and
by painfully having traits (or “mixins”) take a @r[Super] parameter
@; @~cite{smaragdakis2000mixin}
and manually reimplementing class ordering and passing each trait its super
when defining a concrete class, one could express virtual inheritance,
at the cost of much syntactic overhead, and loss of modularity,
because you have to manually compute the class precedence list of your concrete classes
and pass the correct super argument to each trait.
C++ therefore does not fully support Class OO with multiple inheritance,
but allows programmers to achieve a similar effect at great expense, sacrificing some modularity.
Still, even though this feature isn’t directly available to programmers,
the internal semantics of C++ inheritance can be expressed in terms of such @r[super] variable
in a wrapper function.
}}]

For instance, the wrapper function for a prototype that extends the
@r[super] record value with a field @r[rho] computed as the hypothenuse
of a right triangle of sides @r[x] and @r[y]
(being fields of the final value @r[self] to be defined by other composed prototypes)
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

XXX

@subsubsection{Methods}

Hoare@~cite{hoare1965record}, who inspired SIMULA @~cite{Simula1967 dahl1972chapter}
calls “class” a family of @emph{record} types@note{
Hoare probably intended subtyping initially indeed for his families of types;
but subclassing is what he and the SIMULA authors discovered instead.
Such is scientific discovery:
if you knew in advance what lied ahead, it would not be a discovery at all.
Instead, you set out to discover something, but usually discover something else,
that, if actually new, will be surprising.
The greater the discovery, the greater the surprise.
And you may not realize what you have discovered until analysis is complete much later.
The very best discoveries will then seem obvious in retrospect,
given the new understanding of the subject matter,
and familiarity with it due to its immense success.
}.
Hoare after ALGOL calls @emph{record} the lower-level computer representation
of a tuple of data elements distinguished by their name, called @emph{fields};
and he calls @emph{object} a higher-level entity that will be represented as a record.
He calls “attribute” a data element of an object that will be represented by a @emph{field},
though other authors or systems call it “property”, “slot”, “member”, etc.
There are then procedures that access those attributes.
Smalltalk, and many Lisp object systems, and after them Java and many other languages,
call @emph{method} a procedure associated with accessing or modifying such an attribute,
handling a message, or otherwise running computations associated with a prototype or a class.
It can be viewed as a more general notion than that of “attribute”,
or the notion of “attribute” can be generalized to include it.
Either way, we will focus on methods.

Now, a method in Prototype OO corresponds to what in Class OO is called a “static method”,
since it is associated directly to the prototype (which in Class OO, is a class).
A (non-static) method in Class OO corresponds in Prototype OO to
a method with an extra argument, being the element of the class’s specified type
to which the method is applied. From the more general point of view of Prototype OO,
that we are using in this paper, a method call in Class OO is thus just syntax sugar
for calling the class's method with the element on which the method is called.




Early object systems relied heavily on side-effects to construct and evaluate
their prototypes (and classes, when applicable), and so for them this model is but
an approximation of the semantics when the system is stable and no runtime side-effects
modify the inheritance structure.
On the other hand, static languages with Class OO have always always avoided side-effects at compile-time,
and actually behaved as in this pure functional model of inheritance.
Finally, many recent Prototype OO languages use this model as is;
indeed that is how the “extension” system of Nix is defined.

@subsection{OO without Conflation}

@subsubsection{OO without Records}

No need for objects and methods. Inheritance.
And yet, methods very useful for modularity.
More conflation to be at the same time a function, a table, a number, etc.
Or “just” follow the interface of all of them.

@subsubsection{OO without Messages}

Multimethods


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

@subsubsection{Inheritance}
The way OO, whether prototypes or classes, combines increments of specification
with other such increments, is called @emph{inheritance} @~cite{Winograd1975 inheritance1996}.
There are three kinds of inheritance in widespread use:
single-inheritance, multiple-inheritance, and mixin inheritance.

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

@subsubsection{Simplicity}
Single inheritance is the simplest form of inheritance,
at least in the context of first-order code and logic,
which explains why it was discovered first.

@subsection{Multiple Inheritance}

@subsubsection{Early History}

@subsubsection{Global Structure of Multiple Inheritance}

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

@subsubsection{Last but not least}
Mixin inheritance was discovered last, in 1990 @~cite{bracha1990mixin},
while attempting to elucidate inheritance in the paradigm of programming language semantics
as opposed to the previously prevalent paradigm of computing systems @~cite{gabriel2012}.
It is actually the simplest form of inheritance
in the context of higher-order functions:
a @emph{practical} implementation literally takes but two short function definitions
@~cite{nix2015 poof2021}.

@subsubsection{Composing Mixins}
With mixin inheritance, classes (or “mixins”) are defined as
the composition of two or more superclasses
whose method definitions each override those of the superclasses it inherits from.
Since composition is monoidal, the semantics of the resulting class
is the same as if defined using single inheritance
from the precedence list obtained by flattening all composed superclasses in order.

@subsubsection{Comparative Expressiveness}
Mixin inheritance is more expressive than single inheritance, and
just as expressive as multiple inheritance, in that it enables
classes (mixins or traits) to be defined once without being tethered
to a single superclass (or chain of superclasses), and
combined and recombined in many compositions.@note{
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

@section{Constraints on Linearization}

@subsection{Consistency Matters}

@subsubsection{Consistency Constraints}
Cannon @~cite{Cannon1979}, Moon @~cite{Moon1986Flavors} then
Ducournau et al. @~cite{ducournau1992monotonic ProposalMonotonicMultipleInheritance1994}
have discussed the good consistency properties that one may (or may not) expect
from a class linearization algorithm.

These properties ensure that an object system shall compute a class’s class precedence list
in a way that provides a consistent ordering of all methods
across all generic functions and all classes — and all tuples of classes,
when using multimethods @~cite{bobrow86commonloops bobrow88clos gabriel1991clos CecilMultimethods allen2011type}.

@subsubsection{Matching Methods}
This consistency notably matters when resources or locks
are acquired by some methods of some objects,
that must be updated in matching order,
or released in matching reverse order,
by the same methods or other methods,
in the same objects or other related objects.
Inconsistency can lead to resource leak, use-before-initialization, use-after-free, deadlock,
data corruption, and other catastrophic failures.

@subsection{Ordering Consistency}

@subsubsection{Linearization}
The most important constraint, @emph{linearization} @~cite{Cannon1979},
states that the precedence list of a class
is a linearization (total ordering extension)
of its inheritance DAG (viewed as a partial ordering).
All languages that use linearization for method resolution follow this constraint;
those that don’t fall short on the OO purposes of incrementality and modularity.

@subsubsection{Local Ordering}
The second constraint, the
(preservation of the) @emph{local precedence order} @~cite{Moon1986Flavors},
states that the list of a class’s direct super classes
(as specified by the programmer)
is a sublist of its precedence list
(seen as an ordering, e.g. all elements are present in the same order,
but not necessarily consecutively).
Many languages fail to follow this constraint.

@subsubsection{Monotonicity}
The third constraint, @emph{monotonicity} @~cite{ducournau1992monotonic},
states that a class’s precedence list is included as a sublist
(seen as an ordering, as above) in the class precedence list of each of its subclasses.

@subsection{Shape Determinism}

@subsubsection{Only Shape Matters}
A fourth constraint, that we call @emph{Shape Determinism},
states that the result of the linearization algorithm
must only depend on the @emph{shape} of the inheritance graph,
and may not depend on the names of the classes or any global information.
Thus, class inheritance graphs that are isomorphic up to some renaming of classes
must have identical linearizations up to the same renaming of classes.

@subsubsection{Original Name}
This constraint was called @emph{acceptability} by Ducournau et al.
who introduced it @~cite{ducournau1992monotonic}.
However, this name is too generic and fails to convey
either the intent or content of the constraint.
We considered the name “stability”, but Ducournau et al. use that name
for another property of multiple inheritance
that is subsumed by linearization.

@subsubsection{Rationale}
Shape Determinism matters for code maintainability:
thanks to it, renaming a class, moving it in the file hierarchy,
or otherwise modifying it without changing the shape of the inheritance hierarchy
shall not introduce unstable semantic change in the program,
especially if such a change is itself somehow necessary as part of debugging.

@subsubsection{Alternatives to Shape Determinism}
Instead of Shape Determinism, a global ordering could be established between
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

@subsection{Constraint-Respecting Algorithms}

@subsubsection{Inconsistent Algorithms}
While all OO languages with linearization respect the first constraint;
most fail some or all of the latter constraints.

@subsubsection{First Solution}
The first proposed solution that satisfies all the above constraints
@~cite{ProposalMonotonicMultipleInheritance1994} was a bit complex,
building upon previous partial solutions.

@subsubsection{C3}
A latter solution, the C3 algorithm @~cite{Barrett96amonotonic},
“simply” follows the constraints,
computing the precedence list head-first with a tie breaking heuristic used
when multiple linearizations are compatible with the constraints.

@subsubsection{Depth-First Traversal}
Given the arbitrary but practical choice to build the precedence list
from its head, a heuristic for determining which of multiple valid candidates to pick
as the next in the list (when more than one is possible) is equivalent to establishing
an otherwise arbitrary priority order or traversal among the candidates.@note{
Building from the tail would be equivalent, mutatis mutandis;
building from the middle out would require a more complex algorithm,
yet ultimately the same argument would apply.}
Furthermore, given the Shape Determinism constraint, this traversal must only depend on
the shape of the inheritance DAG, not on unique identifier attached to e.g.
the name or source location of the classes.

C3 uses a Depth-First Traversal, prioritizing classes appearing earlier
among direct superclasses and their precedence list, so they appear earlier;
classes appearing later in this traversal also appear later in the precedence list.
In particular, a class’s precedence list will share as much of a tail as possible
with the precedence list of its last direct superclass, which in turn favors
sharing of slot indexes and partially combined method code.
By contrast, the opposite traversal would minimize this sharing, and
a breadth-first traversal would enable less of it.

@subsubsection{Naming}
C3 was named after the fact that it respects three ordering constraints it purports to enforce,
citing Ducournau et al. @~cite{ducournau1992monotonic ProposalMonotonicMultipleInheritance1994}.
The authors did not count Shape Determinism among these constraints,
though, implicitly, C3 enforces it.@note{
There are thus effectively four constraints enforced by C3,
just like there are effectively four musketeers as main protagonists in
The Three Musketeers @~cite{Dumas1844}.
}

@subsubsection{Adoption}
C3 has since been adopted by OpenDylan, Python, Raku, Parrot, Solidity, PGF/TikZ, and more.

@section{State of the Art in Combining Single and Multiple Inheritance}

@subsection{Prolegomena} @; XXX rename

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

@;------>8------>8------>8------>8------>8------>8------>8------>8------>8------

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
