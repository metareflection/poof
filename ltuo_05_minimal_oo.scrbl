#lang scribble/base
@; -*- Scheme -*-
@(require "util/ltuo_lib.rkt")
@(set-chapter-number 5)

@title[#:tag "MOO"]{Minimal OO}
@epigraph{
  Perfection is achieved, not when there is nothing more to add,
  but when there is nothing left to take away.
  @|#:- "Antoine de Saint-Exupéry"|
}
Now that I’ve given an informal explanation of OO and
what it is for (Internal Extensible Modularity),
I can introduce formal semantics for it, starting with a truly minimal model:
(1) No classes, no objects, just specifications and targets.
(2) The simplest and most fundamental form of inheritance, mixin inheritance.
Mixin inheritance is indeed simplest from the point of view of post-1970s formal logic,
though not from the point of view of implementation using mid-1960s computer technology,
at which point I’d be using single inheritance indeed.

@section{Minimal First-Class Extensibility}

@subsection{Extensions as Functions}

I will start by formalizing First-Class Extensibility in pure FP,
as it will be easier than modularity, and a good warmup.
To make it clearer what kind of computational objects I am talking about,
I will be using semi-formal types as fourth-class entities,
i.e. purely as design-patterns to be enforced by humans.
I leave a coherent typesystem as an exercise to the reader,
though I will offer some guidance and references to relevant literature (see @secref{TfOO}).

Now, to extend to some computation returning some value of type @c{V},
is simply to do some more computation, starting from that value,
and returning some extended value of some possibly different type @c{W}.
Thus, in general an “extension” is actually an arbitrary transformation,
which in FP, will be modeled as a function of type @c{V → W}.

However, under a stricter notion of extension,
@c{W} must be the same as @c{V} or a subtype thereof:
you can add or refine type information about the entity being extended,
or adjust it in minor ways;
you may not invalidate the information specified so far,
at least none of the information encoded in the type.
When that is the case, I will then speak of a “strict extension”.

Obviously, if types are allowed to be too precise,
then any value @c{v} is, among other things, an element of
the singleton type that contains only @c{v}, at which point
the only allowed transformation is a constant non-transformation.
Still, in a system in which developers can @emph{explicitly} declare
the information they @emph{intend} to be preserved,
as a type @c{V} if any (which funnily is non-trivial if @emph{not} @c{Any}),
it makes sense to require only extensions that strictly respect that type,
i.e. functions of type @c{V → V}, or @c{W ⊂ V ⇒ V → W}
(meaning @c{V → W} under the constraint that @c{W} is a subtype of @c{V},
for some type @c{W} to be declared, in which case the function is also of type @c{V → V})@xnote["."]{
  And even without static types, the pure lazy functional language Jsonnet @~cite{jsonnet}
  allows programmers to specify dynamically checked constraints
  that objects must satisfy after they are instantiated (if they are, lazily).
  These constraints can ensure a runtime error is issued
  when a desired invariant is broken, and cannot be disabled or removed by inheriting objects.
  They can be used to enforce the same invariants as types could, and richer invariants too,
  albeit at runtime only and not at compile-time.
}

@subsection{Coloring a Point}

The prototypical type @c{V} to (strictly) extend would be the type @c{Record} for records.
Assuming for the moment some syntactic sugar, and postponing discussion of precise semantics,
I could define a record as follows:
@Code{(define point-a (record (x 2) (y 4)))}
i.e. the variable @c{point-a} is bound to a record that associates
to symbol @c{x} the number @c{2} and to symbol @c{y} the number @c{4}.

A sample (strict) extension would be the function @c{paint-blue} below,
that extends a given record (lexically bound to @c{p} within the body of the function)
into a record that is a copy of the previous
with a new or overriding binding associating to symbol @c{color} the string @c{"blue"}:
@Code{(define (paint-blue p) (extend-record 'color "blue" p))}
Obviously, if you apply this extension to that value with @c{(paint-blue point-a)}
you obtain a record equal to what you could have directly defined as:
@Code{(record (x 2) (y 4) (color "blue"))}

Readers familiar with the literature will recognize the “colored point” example
used in many OO papers. Note, however, that in the present example,
as contrasted to most such papers, and to further examples in subsequent sections:
@itemlist[#:style enumparenalph
@item{I am extending a point @emph{value} rather than a point @emph{type},}
@item{the value is a regular record, and not an “object” by any means, and}
@item{indeed I haven’t started modeling the modularity aspect of OO yet.}]

@subsection{Extending Arbitrary Values}

The type @c{V} of some values being extended could be anything.
The possibilities are endless, but here are a few simple real-life examples
of strict extensions for some given type:
@itemize[
@item{The values could be numbers, and then
your extensions could be adding some increment to a previous number,
which could be useful to count the price, weight or number of parts in a project being specified.}
@item{The values could be bags of strings, and
your extensions could append part identifiers to the list of spare parts or ingredients to order
before starting assembly of a physical project.}
@item{The values could be lists of dependencies, where each dependency
is a package to build, action to take or node to compute,
in a build system, a reactive functional interface or a compiler.}]

A record (indexed product) is just a common case because it can be used to encode anything.
Mathematicians will tell you that products (indexed or not)
give a nice “cartesian closed” categorical structure to the set of types
for values being extended. What it means in the end is that
you can decompose your specifications into elementary aspects that you can combine
together in a record of how each aspect is extended.

@subsection{Applying or Composing Extensions}

The ultimate purpose of an extension @c{ext} is
to be applied to some value @c{val},
which in Scheme syntax is written @c{(ext val)}.

But interestingly, extensions can be composed, such that from two extensions
@c{ext1} and @c{ext2} you can extract an extension @c{(compose ext1 ext2)},
also commonly written @c{ext1 ∘ ext2},
that applies @c{ext1} to the result of applying @c{ext2} to the argument value.
And since I am discussing first-class extensions in Scheme,
you can always define the @c{compose} if not yet defined, as follows;
@c{compose} is an associative operator with the identity function @c{id} as neutral element:
@Code{
(def compose (λ (ext1 ext2) (λ (val) (ext1 (ext2 val)))))
(def identity (λ (val) val))}

Now if I were discussing second-class extensions in a restricted compile-time language,
composition might not be definable, and not expressible unless available as a primitive.
That would make extensions a poorer algebra than if they could be composed.
With composition, strict extensions for a given type of values are a monoid
(and general extensions are a category).
Without composition, extensions are just disjointed second-class constants without structure.
I will show later that this explains why in a second-class setting,
Single inheritance is less expressive than mixin inheritance and multiple inheritance.

@subsection{Top Value}

Now, sooner or later, composed or by itself, the point of an extension is
to be applied to some value.
When specifying software in terms of extensions,
what should the initial base value be, to which extensions are applied?
One solution is for users to be required to somehow specify an arbitrary base value,
in addition to the extension they use.

A better solution is to identify some default value containing
the minimum amount of information for the base type @c{V}
that is being strictly extended.
Each extension then transforms its “inherited” input value
into the desired extended output value,
possibly refining the type @c{V} along the way,
such that the initial type is the “top” type of this refinement hierarchy.
The initial base value is also called a “top value” @c{⊤},
and somewhat depends on what monoidal operation is used to extend it
as well as the domain type of values.

@itemize[
@item{For the type @c{Record} of records, @c{⊤ = empty-record} the empty record.}
@item{For the type @c{Number} of numbers, @c{⊤ = 0} if seen additively,
or @c{1} if seen multiplicatively,
or @c{-∞} (IEEE floating-point number) seen with @c{max} as the operator, or @c{+∞} with @c{min}.}
@item{For the type @c{Pointer} of pointers into a graph of records, @c{⊤ = null},
the universal null pointer@xnote["."]{
  Hoare called his 1965 invention of null his “billion dollar mistake”
  @~cite{Hoare1965 Hoare2009}.
  Now, to Hoare’s credit, his invention of classes in the same article,
  for which he vaguely suggests the semantics of single inheritance
  as Dahl and Nygaard would implement after his article,
  yet that he (and they) wrongfully assimilate to subtyping,
  was a trillion dollar happy mistake.
  Overall, the effect of his article @~cite{Hoare1965} was probably net vastly positive.
}}
@item{For the type @c{Type} of types (in a compiler, at the meta-level),
@c{⊤ = Any}, the top type (“contains everything, about which you know nothing”) that you refine,
or the bottom type @c{⊥ = Nothing}
(“contains nothing, about which you know everything”) that you extend.}
@item{For any function type in a language with partial functions, @c{⊤ = abort},
a function that never returns regularly,
and instead always aborts regular evaluation and/or throws an error.}
@item{For the type @c{Lazy} of lazy computations,
which would be an appropriate default in a language with lazy evaluation,
even and especially a pure functional language with partial functions,
such as Haskell or Nix,
@c{⊤ = (lazy ⊥)} is a universal top value, where @c{⊥ = (abort)} is
a computation that never returns
(but may issue an abnormal termination, if the language supports that).
Indeed, @c{(lazy ⊥)} carries no useful information
but can be passed around, and has “negative infinite” information
if you try to force, open or dereference it, which is much less than
the 0 information provided by a regular null value.
In Scheme, one may explicitly use the @c{delay} primitive to express such laziness,
though you must then explicitly @c{force} the resulting value rather than
having the language implicitly force computations whenever needed.}
@item{Last but not least for the type @c{Any} of arbitrary values of any type,
which would be an appropriate default in a language with eager evaluation,
any value could do as default, but often there is a more colloquial default.
In C, you would use 0 in integer context and the @c{NULL} pointer in pointer context.
In modern C++, similarly but with the @c{nullptr} instead of @c{NULL}.
In Java, @c{null}.
In Lisp, @c{NIL}.
In Scheme, the false boolean @c{#f}, or, in some dialects, a special unit value @c{(void)}
(the null value @c{'()} being traditionally used only for lists).
In JavaScript, @c{undefined}, @c{null} or an empty record @c{{}}.
In Python, @c{None}.
Various languages may each offer some value that is easier to work with as a default,
or some library may offer an arbitrary value to use as such.
Some strongly typed applicative languages will offer no universal such value, though,
and then some ad hoc arbitrary value must be provided for each type used.}]

In the context of extensions for objects,
OO languages usually use @c{Record} as their top type,
or some more specific @c{Object} subtype thereof that carries
additional information as instance of Prototype or Class (depending on the language),
with some null value or empty object as their top value.
Your mileage may vary, but in my examples below,
I will be more ambitious as to how widely applicable OO techniques can be,
and, using Scheme as my language, I will choose the @c{Any} type as my top type,
and the false boolean @c{#f} as my top value:
@Code{
(def top #f)}

@subsection{Here there is no Y}

Looking at the type signature @c{(V → V) → V}
for the process of obtaining a value from a strict extension
(and every extension is strict for the top type),
one may be tempted to say “I know, this is the type of the fixpoint combinator Y”
and propose the use of said combinator.

The idea would indeed work in many cases, as far as extracting a value goes:
for any lazy type @c{V} (and similarly for function types),
any extension that would return a useful result applied to @c{(lazy ⊥)}
passing it as argument to the lazy @c{Y} combinator would also yield a result.
And indeed the results will be the same if the extension wholly ignores its argument,
as is often the intent in those situations.
Typically, you’d compose extensions, with one “to the right”
ignoring its argument, overriding any previous value
(or lack thereof, as the default default is a bottom computation),
and returning some default value that is more useful in context
(including not being a bottom computation);
further extensions “to the left” then build something useful from that default value.

However, if there is no such overriding extension, then
the results would not necessarily be the same between applying the extension or passing it to @c{Y}.
For instance, given the extension @c{(λ (x) (lazy-cons 1 x))}
for some @c{lazy-cons} function creating a co-inductive stream of computations
(as opposed to an inductive list of values as with the regular Scheme @c{cons}),
applying the extension to @c{(lazy ⊥)} yields a stream you can destructure once,
yielding @c{1} as first value, and aborting with an error if you try to destructure the rest.
Meanwhile, applying the @c{Y} combinator yields an infinite stream of 1’s.

Then comes the question of which answer is more appropriate.
Using the @c{Y} combinator only applies to functions and lazy values,
the latter being isomorphic to nullary functions
that always return the same cached result;
it doesn’t apply and therefore isn’t appropriate in the general case of eager values.
Meanwhile, applying extensions to a top value is always appropriate;
it also corresponds better to the idea of specifying a value by starting from
no specific information then refining bit by bit until a final value is reached;
it does require identifying a type-dependent top value to start from,
but there is an obvious such value for the types where the fixpoint combinator applies;
finally, it is easier to understand than a fixpoint combinator and arguably more intuitive.

All in all, the approach of applying extensions to a top value
is far superior to the approach of using a fixpoint combinator
for the purpose of extracting a value from an extension.
Thus, as far as one cares about extensibility:
@emph{here, there is no Y}@xnote["."]{
  With apologies to Primo Levi @~cite{Levi1947}.
}

@exercise[#:difficulty "Easy"]{
  Discuss what top values may be appropriate or inappropriate for the follow types:
  boolean, string, comparison function, integer, floating-point number,
  point (record of two coordinates @c{x} and @c{y}), in Scheme,
  and/or in your favorite language.
  Justify your choices.
}

@exercise[#:difficulty "Easy"]{
  Write an extension for ASCII strings that downcases the contents
  (converts all letters to lowercase), and another that capitalizes the string
  (makes the first letter uppercase).
  Do the two extensions commute when you compose them?
  (i.e. are the results the same when you switch the order?
  or else does the order matter?)
}

@exercise[#:difficulty "Easy"]{
  Write an extension that takes records representing
  a rectangle of given @c{length} and @c{width},
  that extend them with a new field @c{area}, being the area of the rectangle.
}

@exercise[#:difficulty "Medium"]{
  Write an “discount” extension @c{(discount-spec percent)},
  of the form @c{(λ (_self price) ...)}
  that takes a price and reduces it by some specified percentage.
  The percentage being a first argument to pass to the extension;
  or, if you want to be nitpicky,
  to the function that creates the actual extension as a closure.

  Note: using binary floating point numbers is malpractice in the context of accounting,
  that may get you fired or sued.
  Always use integers (multiples of the minor unit of accounting)
  or fixed-point decimal numbers (in terms of the major unit of accounting),
  and round to the nearest multiple of the applicable minor unit
  (e.g. one cent of a dollar, for retail prices in the USA in 2026).
}

@exercise[#:difficulty "Medium"]{
  The text argues against using Y combinator for extensibility.
  Create an example demonstrating the difference between
  applying an extension to a top value and using the Y combinator on the extension.
  Show a case where they produce different behaviors.
  Is the difference merely a matter of performance, or one of semantics?
  In your example, which best corresponds to the notion of extending a specification?@xnote[""]{
    Hint: you may define a lazy stream extension: @c{(λ (x) (lazy-cons 1 x))}.
    Apply it to @c{top = (lazy ⊥)} and compare the behavior to that of @c{(Y f)}.
    What happens when you force the first few elements?
  }
}

@exercise[#:difficulty "Hard"]{
  Identify some domain on which to apply extensibility:
  for instance, configuration files for a given program you use.
  Define an appropriate type for these configurations,
  a top value and some simple extensions for that types.
  If possible, reuse existing parsers and printers (e.g. for JSON, YAML, .INI, etc.),
  or else build simple ones.
}

@section[#:tag "MFCM"]{Minimal First-Class Modularity}

@subsection{Modeling Modularity (Overview)}

To model Modularity in terms of FP,
I need to address several issues, each time with functions:
@itemize[
@item{First, programmers must be able to define several independent entities.
For that, I introduce records, as functions from identifiers to values.}
@item{Second, programmers must be able to use existing modularly-defined entities.
For that, I introduce the notions of module context as a record,
and modular definition as function from module context to entity.}
@item{Third, programmers must be able to publish the entities they define
as part of the modular context that can be used by other programmers.
For that, I introduce the notion of module as record,
and (open) modular module definition, as function from module context to module.}
@item{Last, programmers need to be able to link together
those independent modular definitions into complete programs that end-users can run.
For that, I introduce closed modular module definitions,
and show how to resolve the open references.}]

End-users are provided with a “linked” program as a resolved module context
from which they can invoke a programmer-defined “main” entry-point with their choice of arguments.

@subsection{Records}

Before I model modularity as such, I shall delve deeper into the modeling of records,
that are the usual substrate for much of modularity.

@Paragraph{Record Nomenclature}
I will follow Hoare @~cite{Hoare1965} in calling
“record” the concrete representation of data that contains zero, one or many “fields”.
A typical low-level implementation of records is as
consecutive “words” (or “characters”) of “computer store” or “storage space”,
all of them typically mutable.
But for the sake of studying semantics rather than performance,
this book will discuss higher-level implementations, immutable,
and without notion of consecutive words.
By contrast, Hoare uses “object” to refer to more abstract entities
from the “problem” being “modeled”, and an “object” has higher-level “attributes”.
An object and some or all of its attributes might be simply represented
as a record and its fields, but other representations are possible.

Other authors at times have used other words for records or other representations of the same concept:
struct, structure, data structure, object, table, hash-table,
tuple, named tuple, product, indexed product, entity, rows, etc.
Their fields in various traditions and contexts may have been called such things as:
methods, slots, attributes, properties, members, variables, functions, bindings, entries, items,
keys, elements, components, columns, etc.
Mapping which terms are used in any given paper to those used in this or another paper
is left as an exercise to the reader.

@Paragraph{Typing Records}
I could be content with a simple type @c{Record}, but then
every access would be require some kind of type casting.
Instead, I will propose slightly more elaborate types.

Given types @c{V}, @c{W}, @c{X}... for values,
@c{I}, @c{J}... for identifiers or other indexes,
consider records or sets of bindings as values of an index product
@c{∏R = i:I → R@(ᵢ)} wherein to each value @c{i} in the index set @c{I},
the record will associate a value of type @(Ri),
where @c{R} is a schema of types, a function from @c{I} to @c{Type}.

To simplify this model, a pure functional record of type @c{∏R}
can be viewed as a function indexed by the domain @c{I} of indexes
to @(Ri) for each @c{i}.
When invoked on a value not in @c{I}, the function may return any value, or diverge.
To further simplify, and for the sake of modeling records as first-class functions,
when using Scheme, for indexes I will use symbols (interned strings)
instead of identifiers (that in Scheme are symbols enriched with
scoping and provenance information, used for second-class macro-expansion).

For example, where @c{Number} is the type of numbers and @c{String} the type of strings,
I could have a record type
@Code{
type ∏R = {x: Number, y: Number, color: String}
}
and a point @c{point-c} of type @c{∏R} defined as follows:
@Code{
(define point-c
  (record (x 3) (y 4) (color "blue")))
}

@Paragraph{Implementing Records} @; TODO secref to chapter 9?
I will call @emph{identifier} some large or infinite type of values
over which equality or inequality can be decided at runtime by using a suitable primitive,
or calling a suitable function.
Since I use Scheme for my examples, I will use the symbols for identifiers
(that I will later extend with booleans),
the @c{equal?} primitive for testing equality, and
the @c{(if @emph{condition then-clause else-clause})} special form for conditionals.
In other programming languages that lack symbols as a builtin functionality,
they can be implemented as interned strings, or instead of them,
uninterned strings or numbers can be used as identifiers.
And if somehow you only care about the pure λ-calculus, there are many embeddings and encodings
of booleans, unary or binary numbers, and lists or trees thereof, that will do.

Programming languages usually already provide some data structure for records
with second-class identifiers, or “dictionaries” with first-class identifiers,
or have some library that does.
For instance, while the core Scheme language has no such data structure,
each implementation tends to have its own extension for this purpose, and
there are multiple standard extensions for records or hash tables.
Be mindful, though, that mutable variants of these data structures
(e.g. mutable hash-tables) may not be appropriate to use
in the context of pure functional programming that I am currently discussing,
though they might be used (with caution) in the context of mutable objects.
Also, many papers and experiments use a linked list of (key, value) pairs,
known in the Lisp tradition as an alist (association list),
as a simple implementation for records;
alists don’t scale beyond a few hundreds of elements,
but don’t need to in the context of the present experiments.
Nevertheless to make the semantics of records clear, I will provide
a trivial purely functional implementation, that has even more scaling issues,
but that is even simpler.

The basic reference operator is just function application:
@Code{
(def (record-ref key rec) (rec key))
}
The empty record can be represented as a function that always fails,
such as @c{(λ (_) (error "empty record"))}@xnote["."]{
  As a convention, I will call @c{_} a generic variable that is never used,
  and prefix with @c{_} the names of variables that are not used in a specific functions,
  even though they are used by other functions satisfying the same interface.
  Thus @c{_string} is a name for a string variable that is not used by the present function,
  @c{_self} a variable usually called @c{self} that is ignored in the current function, etc.
  This convention makes it easier to identify at a glance
  which parts of a higher-order functional interface are used or ignored by a given function.
  Some compilers enforce this kind of conventions, making them part of the language.
}
Now, the “fail if not present” representation is great when implementing a (notionally)
statically typed model. But for a dynamic environment, a “nicer” representation
is as a function that always returns a language-wide top value, such as @c{undefined} in JavaScript.
This is especially the case in this book
where I don’t have space to discuss error handling protocols.
But you should use what makes sense in your language.
In portable Scheme, I will use:
@Code{
(def empty-record (λ (_) #f))
}

To extend a record with one key-value binding, you can use
@Code{
(def (extend-record key val rec) (λ (i)
  (if (equal? key i)
     val
     (rec i))))}

Note how this trivial implementation does not support
getting a list of bindings, or removing a binding.
Not only does one not need these features to implement OO@xnote[","]{
  I generate HTML for my presentations using exactly this implementation strategy.
  The Scheme implementation I use has builtin record support, and
  there are now somewhat portable libraries for records in Scheme;
  but I made it a point to use a minimal portable object system
  to show the feasability and practicality of the approach;
  and this way the code can be directly ported to any language with higher-order functions.
}
they constitute a “reflection” API
that if exposed would interfere with various compiler optimizations,
the use of which is actively rejected when statically typing records.
However, there are other reasons why my implementation is not practical for long-running programs:
it leaks space when a binding is overridden,
and the time to retrieve a binding is proportional to the total number of bindings
(including overridden ones) instead of being logarithmic in the number of visible bindings only
as it could be in a pure functional implementation based on balanced trees,
or constant time, for a stateful implementation
based on either known field offsets within an array,
or hashing@xnote["."]{
  The nitpicky would also account for an extra square root factor
  due to the limitations of physics@~cite{MythOfRAM2014}.
}

@Paragraph{Merging Records}
Given a list of bindings as pairs of an identifier and a value,
you can define a record that maps each identifier to the value in the first
binding with that identifier in the list, if any
(or else, errors out, diverges, returns null, or does whatever a record does by default).
Conversely, given a list of identifiers and a record, you can get a list of bindings
as pairs of an identifier and the value that the record maps it to.
Most practical implementations of records support
extracting from a record the list of all identifiers it binds;
but this “reflection” feature is not necessary for most uses of records,
and indeed breaks the record abstraction that some typesystems enforce.
Indeed, the trivial implementation I will use,
wherein records are functions from identifier to value, doesn’t support this feature;
and some more elaborate implementations will deliberately omit runtime reflection,
and only support second-class knowledge of what identifiers are bound in a record.

Finally, given a list of records and for each record a set of identifiers
(that may or may not be the set of all identifiers bound by it,
possibly extracted via reflection), you can merge the records along those sets of identifiers,
by converting in order each record to a list of bindings for its given set of identifiers,
appending those lists, and converting the appended result to a record.

@subsection{Modular definitions}

Now I can introduce and model the notion of modular definition:
a modular definition is a way for a programmer to specify
how to define an entity of some type @c{E} given some modular context,
or just @emph{module context}, of type @c{C}.
The module context contains all the available software entities,
that were defined in other modules by other programmers
(or even by the same programmer, at different times,
who doesn’t presently have to hold the details of them in their limited brain).
And the simplest way to model a modular definition as a first-class value
is as a function of type @c{C → E}, from module context to specified entity@xnote["."]{
  A Haskeller may well interpret “modular” in this book as meaning
  “in the reader monad of a module context”, in that a modular something
  will be a function from the module context to that something.
  This is correct, but beware that this is only half the story.
  The other half is in what it means for something to be a module context,
  rather than any random bit of context.
  And we’ll see shortly that the answer involves open recursion and fixpoints.
  Informally, modular means that locally you’re dealing with part of a whole,
  and globally, the whole will be made out of the parts in the end.
}

Typically, the module context @c{C} is a set of bindings mapping identifiers
to useful values, often functions and constants,
whether builtin the language runtime or available in its standard library.
Now, I already introduced types for such sets of bindings: record types.
And as a language grows in use and scope, the module context will include
further libraries from the language’s wider ecosystem,
themselves seen as sets of bindings of their respective record types,
accessed hierarchically from a global module context,
that now contains “modules” (records) as well as other values
(or segregated from regular values at different levels of the module hierarchy).
In any case, the global module context is typically a record of records, etc.,
and though many languages have special restrictions on modules as second-class entities,
for my purpose of modeling the first-class semantics of modularity,
I may as well consider that at runtime at least, a module is just a regular record,
and so is the global module context, of type @c{C = ∏R}.

For instance, I could modularly specify a function @c{ls-sorted} that returns
the sorted list of filenames (as strings) in a directory (as a string),
from a module context of type @c{∏R} that provides
a function @c{ls} of type @c{String → List(String)} and
a function @c{sort} that sorts a list of strings:
@;{@Code{
type R = { ls: String → List(String), sort: List(String) → List(String) }
}}
@Code{
(define ls-sorted (λ (ctx) (compose (ctx 'sort) (ctx 'ls))))
}

Note how in the above code snippet, I model records as functions from symbol to value,
and to extract a binding from a record, one thus calls it with a symbol as single argument.
The functions extracted are then chained together, as in the @c{compose} function
I defined earlier (that I could have used if I extracted it from the module context,
or could otherwise assume it was a language builtin).

@subsection[#:tag "OMD"]{Open Modular Definitions}

Now, programmers usually do not specify just a single entity of type @c{E},
but many entities, that they distinguish by associating them to identifiers.
That is, they modularly define a @emph{module} of type @c{∏P}.
A modular module definition is thus “just” a function from record to record:
the input record is the modular context of type @c{∏R}, and
the output record is the specified module of type @c{∏P}.
I will say that the identifiers bound in @c{∏R}
are @emph{required} by the modular definition (or @emph{referenced} by it),
whereas the identifiers bound in @c{∏P} are @emph{provided} by the modular definition
(or @emph{defined} by it).

In general, I call a modular definition “open”,
inasmuch as some entities may be @emph{required} that are not @emph{provided}
by the modular definition, and must be provided by other modular definitions.
Now, the notion that the modular definition “provides” entities supposes
that these entities will be available, bound to well-known identifiers in the module context.
There are many strategies to realize this provision:
@itemize[
@item{The modular definition can be a paired with a single identifier,
under which the entity is intended to be bound in a complete module context.}
@item{The modular definition can provide multiple bindings,
wherein the entity defined is a module, to be merged as a record into
the complete module context.}
@item{The first strategy above can be adapted to a hierarchical namespace of nested records,
by pairing the modular definition with a path in the namespace, i.e. a list of identifiers.}
@item{For multiple bindings, the second strategy can also be paired with a path;
or the merging of records can be recursive, at which point a strategy must be devised
to determine how deep to recurse, for instance by somehow distinguishing “modules”
from regular “records”.}]

@; TODO secref Optics

In the end, programmers could somehow specify how their modular definition will extend
the module context, with whatever merges they want, however deeply nested—which
will lead them to modular extensibility.
But for now, I will abstract over which strategy is used to
assemble many modular definitions together.

@subsection{Linking Modular Module Definitions: Y}

I will call a modular module definition “closed” when,
after whatever assembly of individual modular definitions happened,
it specifies the global module context of an entire program,
wherein every entity required is also provided.
A closed modular module definition is thus of type @c{∏R → ∏R}.

Then comes the question: how can I, from a closed modular module definition,
extract the actual value of the module context, of type @c{∏R},
and thereby realize the program that was modularly specified?

This module realization function I am looking for is
of type @c{(C → C) → C} where @c{C = ∏R}.
Interestingly, I already mentioned a solution:
the fixpoint combinator @c{Y}.
And whereas it was the wrong solution to resolve extensions,
it is precisely the right solution to resolve modular definitions:
the @c{Y} combinator “ties the knots”,
links each reference requiring an entity to the definition providing it,
and closes all the open loops.
It indeed does the same in a FP context that an object linker does
in the lower-level imperative context of executable binaries:
link references in open modular definitions to defined values in the closed result.

If there remain identifiers that are required but not provided,
there will be an error—or non-termination, or some default value that will not make sense,
at runtime, or at compile-time, depending on how sophisticated the exact implementation is
(a typechecker might catch the missing requirement, for instance).
If there remain identifiers that are provided but not required,
and they are not otherwise (meant to) be used via reflection,
then a “tree shaker” or global dead code optimizer may eliminate them.


@; TODO move to an appendix, with a summary of the conclusions?
@subsection[#:tag "DSF"]{Digression: Scheme and FP}
@epigraph{Purely applicative languages are poorly applicable. @|#:-"Alan Perlis"|
}
Here are two ways in which Scheme departs from the theoretical model of Functional Programming,
that affect their suitability to modeling Object Orientation.
These discrepancies also apply to many (but not all) other programming languages.

@Paragraph{Function Arity}
Functional Programming usually is written with unary functions (that take exactly one argument),
and to express more than one argument, you “curry” it:
you define a function of one argument that returns a function that processes the next argument, etc.,
and when all the arguments are received you evaluate the desired function body.
Then to apply a function to multiple arguments, you apply to the first argument,
and apply the function returned to the second argument, etc.
The syntax for defining and using such curried functions is somewhat heavy in Scheme,
involving a lot of parentheses.
By contrast, the usual convention for Functional Programming languages
is to do away with these extra parentheses:
in FP languages, two consecutive terms is function application, which is left-associative,
so that or even just @c{f x y} is syntactic sugar for @c{((f x) y)};
and function definition is curried, so that @c{λ x y . E}
is syntactic sugar for @c{λ x . λ y . E}.

Thus, there is some syntactic discrepancy that makes code written in
the “native” Functional style look ugly and somewhat hard to follow in Scheme.
Meanwhile, colloquial or “native” Scheme code may use any number of arguments as function arity,
and even variable numbers of arguments, or, in some dialects, optional or keyword arguments,
which does not map directly to mathematical variants of Functional Programming;
but it is an error to call a function with the wrong number of arguments.

One approach to resolving this discrepancy is to just cope with
the syntactic ugliness of unary functions in Scheme, and use them nonetheless,
despite Lots of Insipid and Stupid Parentheses@xnote["."]{
  Detractors of the Lisp language
  (that like many languages was customarily written in uppercase until the mid-1980s,
  and is usually capitalized since)
  and its many dialects and derivatives,
  among which Scheme is prominent, invented the backronym
  “Lots of Insipid and Stupid Parentheses” to deride its syntax.
  While Lispers sometimes yearn for terser syntax—and
  at least one famous Schemer, Aaron Hsu, jumped ship to APL—to them,
  the parentheses, while a bit verbose, are just a familiar universal syntax that allows them
  to quickly understand the basic structure of any program or data,
  even when they are unfamiliar with the syntactic extensions it uses.
  By contrast, in most “blub” languages,
  as Lispers call non-Lisp languages @~cite{Graham2001avg},
  parentheses, beyond function calls,
  carry the emotional weight of “warning: this expression is complex,
  and doesn’t use the implicit order of operations”.
  Non-Lispers see parentheses as lacking cues from the other kinds of brackets their languages have;
  but these cues in Lisp are present, just in the head identifier of an expression.
  Matching parentheses can also be confusing,
  especially when not using the parentheses-aware, indentation-enforcing,
  semi-structured text editors that Lispers have been using since the 1970s, or
  when failing to format code properly (which those editors also help automate).
  Ironically, the syntactic and semantic abstraction powers of Lisp
  allow for programs that are significantly shorter than their equivalent
  in a mainstream language like Java,
  and as a result have not just fewer parentheses overall,
  but fewer brackets of any kind.
  It is therefore not the number of parentheses, but their density, that confuses
  mainstream programmer, due to unfamiliarity and emotional connotations.
  Now, it may well be that the same abstraction powers of Lisp make it unsuitable
  for a majority of programmers, incapable of mastering such abstraction.
  As an age of AI programmers approaches that will have abstraction powers vastly superior
  to the current average human programmer, it remains to be seen what kind of syntaxes
  will make them more efficient, when working in isolation, with each other, or with humans.
  And maybe even statistical AIs will need the rapid feedback of algorithmic editors
  to balance their parentheses.
}

A second approach is to adopt a more native Scheme style over FP style,
with a variety of different function arities, making sure that a function is always called
with the correct number of arguments. This approach blends best with the rest of the Scheme
ecosystem, but may hurt the eyes of regular FP practitioners, and
require extra discipline (or extra debugging) to use.

The third approach, that I will adopt in this book, is to use Scheme macros
to automatically curry function definitions and function applications:
@itemize[
  @item{Thus, anonymous functions defined with @c{λ} (unicode character)
    as opposed to the builtin Scheme @c{lambda} (ascii string)
    will be automatically curried, defining a chain of unary functions.
    Furthermore, calls to the variables they bind will also be automatically curried.
    Calls with insufficient arguments will return a partially applied function,
    rather that throw an error.}
  @item{Additionally, functions defined with @c{def}, as opposed to the builtin Scheme @c{define},
    will also be automatically curried as with @c{λ};
    calls to these functions and to variables they bind will also automatically curried.}
  @item{To define regular Scheme functions, especially for the sake of
    functions accepting variable numbers of arguments to lighten the syntax,
    I will still use the regular @c{lambda} and @c{define}.
    In other FP languages, you might instead use explicit list arguments,
    or record arguments for heterogeneous types.}
  @item{Regular scheme functions can use the @c["@"] macro to explicitly call curried function
    with uncurried arguments when the function isn’t bound to an autocurrying variable.}]

The macros defining @c{λ}, @c{def} and @c["@"] fit within fifty lines of code.
Production-quality Scheme might use native Scheme style instead, for extra performance;
but the performance penalty for curried code should remain relatively small,
especially with a sufficiently optimizing compiler.
I will thus write @c{(f x y)} where functional programmers write @c{f x y} or @c{(f x) y},
@c{(λ (x y) expr)} where functional programmers write @c{λ x . λ y . expr} or @c{λ x y . expr},
and @c{(def (foo x y) expr)} where functional programmers write
@c{foo x y = expr} or @c{f = λ x . λ y . expr}.
And the code will actually run in Scheme after a short prelude.

@Paragraph{Many Y combinators}
First, there are many variants to the fixpoint (or fixed-point) combinator Y,
and the pure applicative Y combinator you could write in Scheme’s
pure subset of the λ-calculus is actually quite bad in practice.
Here is the applicative Y combinator, that I will call Ye (for Y, eager)
expressed in terms of the composition combinator B and
the self-application combinator U (called Ue for U, eager)@xnote[""]{
  A simple way to test the applicative/eager @c{Ye} combinator,
  or the subsequent variants @c{Yex} and @c{Yes}
  is to use it to define the factorial function:
  first define the applicative recursion scheme for factorial:
  @c{(def (eager-pre-fact f n) (if (<= n 1) n (* n (f (1- n)))))}
  then you can define factorial as
  @c{(def fact (Ye eager-pre-fact))}
  and you can then test that e.g. @c{(fact 6)} returns @c{720}.
}@xnote[""]{
  Also note that the self-application combinator U @~cite{Kiselyov2024Y},
  sometimes called the duplication combinator Δ, or ω
  (because @c{Ω = (ω ω)} is the canonical λ-term that never terminates),
  does the heavy lifting of the Y combinator,
  hence the double use of it in the definition of Y = U (B U).
  It can be viewed as doing half the job of Y, and is
  the essence of the object encoding in @secref{CwUAoS}.
}:
@Code{
(def (B x y z) ;; composition
  (x (y z)))
(def (Ue x y) ;; eager U
  (x x y))
(def (Ye f) ;; eager Y
  (Ue (B f Ue)))
(def (Yex f) ;; eager Y, expanded
  ((λ (x y) (x x y))
   (λ (x) (f (λ (y) (x x y))))))
}
@; Test: ((Ye eager-pre-fact) 6) ;==> 720
@; Test: ((Yex eager-pre-fact) 6) ;==> 720
The Y combinator works by composing the argument function @c{f}
with indefinite copies (duplications) of itself (and accompanying plumbing).
In this applicative variant, the first, minor, issue with this combinator is
that it only works to compute functions,
because the only way to prevent an overly eager evaluation of a computation
that would otherwise diverge is to protect this evaluation under a λ.
I happen to have chosen a representation of records as functions,
such that the applicative Y still directly applies;
if not, I may have had to somehow wrap my records in some sort of function,
at which point I may as well use the lazy Y below,
or switch to representing modular contexts as records of functions,
instead of functions implementing or returning records.

The second, major, issue with the applicative Y is that the pure applicative λ-calculus
by itself has no provision for sharing non-fully-reduced computations,
only for sharing (fully-reduced) values@xnote[";"]{
  You could of course emulate sharing by implementing state monadically,
  or through a virtual machine interpreter, inside the applicative λ-calculus,
  and then reimplementing your entire program on top of that richer calculus.
  But that would be a global transformation, not a local one,
  and in the end, it’s the new stateful paradigm you would be using,
  not the pure applicative λ-calculus anymore.
}
therefore the fixpoint computations are duplicated,
and any information used along the way will have to be recomputed
as many times as computations are duplicated, which can grow exponentially fast
as the computation involves deeper sub-computations.
In some cases, the eager evaluation may never terminate at all when lazy evaluation would,
or not before the end of the universe.
And of course, if there are any non-idempotent side effects,
they too will be potentially duplicated a large number of times.

There are several potential alternatives to
the practically inapplicable applicative Y combinator:
(1) a stateful Y combinator,
(2) a lazy Y combinator, or
(3) a second-class Y combinator.

A stateful Y combinator is what the @c{letrec} construct of Scheme provides
(and also its @c{letrec*} variant, that the internal @c{define} expands to):
it uses some underlying state mutation to create and initialize a mutable cell
that will hold the shared fixpoint value, with the caveat that you should be careful
not to access the variable before it was initialized@xnote["."]{
  A variable won’t be accessed before it is used if you’re immediately binding the variable
  to a λ expression, but may happen if you bind the variable to a function application expression,
  wherein the variable is passed as argument without wrapping it in a λ,
  or the λ it is wrapped in is called before the evaluation of this expression completes.
  The Scheme language does not protect you in this case,
  and, in general, could not protect you
  without either severely limiting the language expressiveness,
  or solving the halting problem.
  Various languages and their implementations,
  depending on various safety settings they might have or not,
  may raise a “variable not bound” exception,
  use a special value such as @c{#!void} or @c{null} or @c{undefined}
  that is not usually part of the expected type,
  or access uninitialized memory potentially returning nonsensical results
  or causing a latter fandango on core, etc.
}
If the variable is only accessed after it is initialized,
and the rest of the program is pure and doesn’t capture intermediate continuations
with Scheme’s famous @c{call/cc}, the mutation cannot be exposed as a side-effect,
and the computation remains overall pure (deterministic, referentially transparent),
though not definable in terms of the pure applicative λ-calculus.
Note however how in the definition below, @c{p} still needs be a function,
and one must η-convert it into the equivalent but protected @c{(η p) = (λ (y) (p y))}
(a syntactic definition that delays the evaluation of @c{p},
not a function call with the value of @c{p}, which would defeat the purpose),
before passing it to @c{f}, to prevent access to the variable @c{p} before its initialization
(and @c{f} must also be careful not to invoke this protected @c{p} before returning).
Here is the Y combinator, eager, stateful:
@Code{
(def (Yes f) (letrec ((p (f (η p)))) p))
}
@; Test: ((Yes eager-pre-fact) 6) ;==> 720

A second solution is to use a lazy Y, defined as Yl below.
In a language like Nix (where @c{λ (f)} is written @c{f:}, and @c{let} like Scheme @c{letrec}
recursively binds the variable in the definition body), you can simply define
@c{Y = f: let p = f p; in p}. In Scheme, using the convention that
every argument variable or function result must be protected by @c{delay},
and one must @c{force} the delayed reference to extract the result value,
you would write@xnote[":"]{
  Again, a simple way to test the lazy Y combinator Yl is to use it
  to define the factorial function. First define the lazy “recursion schema” for the factorial:
  @c{(def lazy-pre-fact (λ (f n) (if (<= n 1) n (* n ((force f) (1- n))))))}
  Then the factorial function is
  @c{(def fact (Yl lazy-pre-fact))}
  and you can then test that e.g. @c{(fact 6)} returns @c{720}.
  Note that I do without wrapping of @c{n} in a @c{delay},
  but @c{f} itself is a delayed function value to fit the calling convention of @c{lazy-Y},
  and I therefore must @c{force} it before I call it.
  The subsequent variants of the lazy @c{Yl} can be tested in the same way.
  The earliest variant of @c{Yl} is Turing’s Θ formula @~cite{Turing1937 Curry1958}.
  @; TODO Look into Kleene1935, Kleene1936x2
  @; https://chatgpt.com/c/698b8f11-a504-832d-9187-1fcb3d62640f
}
@Code{
(def (Yl f) (letrec ((p (f (delay p)))) p))
}
Or, if you want variant based on combinators,
here are respectively the lazy B (composition),
lazy U (self-application), lazy Y written with combinators,
and lazy Y with expanded definition:
@Code{
(def (Bl x y z)
  (x (delay ((force y) z))))
(def (Ul x)
  ((force x) x))
(def (Ylc f)
  (Ul (delay (Bl f (delay Ul)))))
(def (Ylx f) ((λ (x) ((force x) x)) (delay (λ (x) (f (delay ((force x) x)))))))
}
@; Test: ((Yl lazy-pre-fact) 6) ;==> 720
@; Test: ((Ylc lazy-pre-fact) 6) ;==> 720
@; Test: ((Ylx lazy-pre-fact) 6) ;==> 720
One advantage of a lazy Y is that evaluation is already protected by the @c{delay} primitive,
enabling self-reference for computations returning any type of data.
The lazy Y can thus apply to any kind of computation, not just to functions.
However, if you consider that @c{delay} is no cheaper than a @c{λ} and indeed uses
a @c{λ} underneath, that’s not actually a gain, just a semantic shift.
What the @c{delay} does buy you, on the other hand, compared to a simple applicative thunk,
is naming and sharing of computations before they are evaluated,
without duplication of computation costs or side-effects@xnote["."]{
  Whether wrapped in a thunk, an explicit delay, an implicitly lazy variable,
  a call-by-name argument, or some other construct, what is interesting is that
  ultimately the fixpoint combinator iterates indefinitely on a @emph{computation},
  and this wrapping is a case of mapping computations into values in an otherwise
  call-by-value model that requires you to talk about values.
  In a calculus such as call-by-push-value@~cite{Levy1999CBPV},
  where values and computations live in distinct type universes,
  the fixpoint combinator would clearly be mapping
  computations to computations without having to go through the universe of values.
  Others may say that the fixpoint operation that instantiates prototypes
  is coinductive, rather than inductive like the definitions of data types.
  @; TODO cite who???
}
(Note that @c{delay} can be easily implemented on top of any stateful applicative language,
though a thread-safe variant, if needed, is somewhat trickier to achieve.)

Here is one implementation of laziness that works in a single-threaded environment:
it takes a thunk as argument, and only calls the thunk the first time around,
thereafter memoizes the result of that first invocation and returning it.
@Code{
(define (compute-once thunk)
  (let ((computed? #f)
        (value #f))
    (λ _
      (or computed?
          (let ((result (thunk)))
            (or computed?
                (begin
                  (set! computed? #t)
                  (set! value result)))))
      value)))
}
If you already assume @c{delay} and @c{force}, you could write it as
@c{(λ (thunk) (let ((x (delay (thunk)))) (λ _ (force x))))}.
This approach transforms laziness into functions,
and you can use the stateful Y on that function and get the same as a lazy Y on its result.

A third solution, often used in programming languages with second-class OO only
(or languages in which first-class functions must terminate), is
for the @c{Y} combinator (or its notional equivalent) to only be called at compile-time,
as a metaprogram, and only on modular definitions
that abide by some kind of structural restriction
that guarantees the existence and well-formedness of a fixpoint,
as well as e.g. induction principles to reason about said fixpoint.
Also, the compile-time language processor usually doesn’t expose any side-effect to the user,
such that there is no semantic difference whether its implementation is itself pure or impure,
and uses a fixpoint combinator or any other representation for recursion.
Since I am interested in first-class semantics for OO, I will ignore this solution
in the rest of this book, and leave it as an exercise for the reader.
@;{TODO CITE Aaron Stump from U Iowa, etc.}
@; TODO: Fix as a metaprogram. Kirill Gobulev.

I have implemented variants of my minimal OO system in many combinations
of the above solutions to these two issues, in Scheme and other languages.
For the rest of this book, I will adopt a style where most functions are unary,
but the syntax to define and use them implicitly uses curry with @c{def} and @c{λ};
I will also be assuming @c{Y = Yes} (eager, stateful) as my fixed-point operator,
unless explicitly mentioned otherwise.
As a result, the reader should be able both to easily copy and test
all the code in this book at their favorite Scheme REPL,
and also easily translate it to any other language
that sports first-class higher-order functions@xnote["."]{
  I can’t leave the topic of the Y combinator without citing Oleg’s fantastic page on the topic,
  even though it doesn’t directly address any of my concerns above: @citet{Kiselyov2024Y}.
}.

And with these issues settled, I will close this digression
and return to rebuilding OO from first principles.

@exercise[#:difficulty "Easy"]{
  Play with the simple record system I implemented.
  With what function can you extract all the values for multiple fields
  in a single function call?
  (You may use functions from your language’s standard library.)
}
@exercise[#:difficulty "Easy"]{
  Use existing library functions in your Scheme implementation of choice
  to actually implement the example of modularly sorting a list of files.
}
@exercise[#:difficulty "Medium" #:tag "alist0"]{
  Implement a trivial record system based on alists (lists of pairs of symbol and value)
  instead of functions from symbol to value (you can reuse library functions if available).
  Reimplement and evaluate the same examples using this record system instead of the one I used.
  Implement conversions between the two representations.
  What limitation do you notice in one direction?
}
@exercise[#:difficulty "Medium"]{
  Extend the @c{once} function above to:
  (a) support non-nullary functions wherein provided arguments are passed through
    the first time around and ignored afterwards; optionally, also support multiple values;
  (b) detect attempts at reentrancy, and issue an error if detected,
    with backtrace if available in your language implementation.
}
@exercise[#:difficulty "Medium"]{
  Study how your language implementation supports
  concurrent evaluation of lazy expressions in multiple processor threads,
  wherein only the first thread actually tries to call the thunk;
  see how it detects and supports non-local exits (e.g. error) from within the thunk, etc.
  For instance, in Gerbil Scheme, look for the implementation of function @c{make-atomic-promise},
  as internally used by the form @c{delay-atomic}.
  Alternatively, to make exercise hard, try implementing it all yourself from low-level primitives
  before you see how it is done; or try to port the Gerbil Scheme implementation
  to your favorite language if that language doesn’t yet support this feature
  (and then send a patch upstream, or publish a new library if the feature is not desired upstream).
}
@exercise[#:difficulty "Hard"]{
  Compare three implementations of the Y combinator:
  eager @c{Ye}, eager stateful @c{Yes} and lazy @c{Yl} on some real-world application.
  For instance, look at the slides I wrote @~cite{poof2021},
  instrument the code to count how many times the @c{plan-slide} function is called
  and display the result at the end, and see how that count changes if using
  the eager (applicative) @c{Ye} instead of the eager stateful @c{Yes}.
  For extra points, modify the code to use lazy evaluation,
  and see if there is a difference when using the lazy @c{Yl}.
}
@exercise[#:difficulty "Hard"]{
  Devise an example that illustrates how the eager (applicative) @c{Ye}
  can cause exponential recomputations with the depth of recursive definitions,
  compared to the eager, stateful @c{Yes}.
}
@exercise[#:difficulty "Hard"]{
  Devise an example that illustrates how the eager stateful @c{Yes}
  without the fixed-point functions also caching their results
  can still cause exponential recomputations compared to using the lazy @c{Yl}
  or explicitly using state to cache results (which amounts to the same).
}

@section[#:tag "MFCME"]{Minimal First-Class Modular Extensibility}

@subsection[#:tag "ME"]{Modular Extensions}

One can combine the above extensibility and modularity in a minimal meaningful way,
as modular extensibility.
I will call “modular extension” an extension to a modular definition.
Thus, given a module context of type @c{C} (typically a record with @c{C = ∏R}),
a type @c{V} for the “inherited” value being extended
and @c{W} for the extended value being “provided”,
an (open) modular extension is a function of type @c{V → C → W}.
When @c{W} is the same as @c{V}, or a subtype thereof, I will call it
a strict (open) modular extension.
When @c{W = V = ∏P} for some record type @c{∏P},
I will call it a modular module extension.
When @c{V = C = W}, I will call it a closed modular extension,
which as I will show can be used as a specification for a value of that type.

@subsection{Composing Modular Extensions}

While you could conceivably merge such modular extensions,
the more interesting operation is to compose them, or more precisely,
to compose each extension under the module context and bound identifier,
an operation that for reasons that will soon become obvious,
I will call mixin inheritance (of modular extensions):
@Code{
(def (mix p c t s)
  (c (p t s) s))
}
The variables @c{p} and @c{c} stand for “parent” and “child” specifications,
wherein the target value @c{t} “inherited” by the composed function
will be extended first by @c{p} then by @c{c},
each time within the module context @c{s}.

The variable @c{t} is also called @c{super} in many contexts,
for reasons that will become obvious@xnote[";"]{
  The @c{super} argument refers to the partial value computed so far
  from @emph{parent} specifications;
  the initial seed value of @c{super} when computing the fixed point is a “base” or “top” value,
  typically an empty record, possibly enriched with metadata or ancillary fields for runtime support.
  @c{super} embodies the @emph{extensible} side of OO,
  enabling a specification to build on values @emph{inherited} from parent specifications,
  add aspects to them, override aspects of them, modify aspects of them, etc.,
  in an extension to the computation so far.
  @c{super} is the variable or keyword used in Smalltalk
  and in many languages and object systems after it,
  such as Java or Python, to access inherited methods.
  In CLOS you’d use @c{call-next-method}.
  In C++ you cannot directly express that concept in general, because you have to name
  the superclass whose method (or “(virtual) member function”) you want to call,
  so you can’t directly write traits that inherit “super” behavior along the class precedence list;
  but it works if you restrict yourself to single inheritance,
  or if you use template metaprogramming to arrange to pass a superclass,
  or list or DAG of superclasses, as argument to your template,
  and manually reimplement mixin inheritance @~cite{Smaragdakis2000Mixin},
  or if you’re adventurous, multiple inheritance, on top of C++.
}
and the variable @c{s} is also called @c{self} in the same contexts,
for the same reasons@xnote["."]{
  The @c{self} argument is the one involved in open recursion or “late binding”;
  it embodies the @emph{modular} side of OO.
  It is called @c{self} because it is destined to be bound as the fixpoint variable
  of a fixpoint operator, wherein it refers to the very entity being defined.
  The name @c{self} is used in Smalltalk, Scheme, Self, Python, Jsonnet, Nix,
  many more languages, and in a lot of the literature about OO semantics.
  In Simula, and after it, in C++, Java, JavaScript or Scala, the @c{this} keyword is used instead.
  Note however, that I am currently discussing a variant of Prototype OO,
  as in Self, Jsonnet, Nix, JavaScript, where the @c{self} or @c{this}
  is indeed the open recursion variable.
  In Class OO language, the definition being one of a type descriptor, not of a record,
  the open recursion variable would instead be something like @c{Self}, @c{MyType} or @c{this.type},
  though there is even less standardization in this area.
  See below the discussion of the meaning of “object” in Prototype OO vs Class OO.
  @; TODO secref Classes
}
Modular extensions and their composition have nice algebraic properties.
Indeed, modular extensions for a given context form a category,
wherein the operation is composition with the @c{mix} function,
and the neutral element @c{idModExt} is the specification that “extends”
any and every value by returning it unchanged, as follows@xnote[":"]{
  As usual, a change of representation from @c{m} to @c{mm = (λ (p) (mix p m))},
  with inverse transformation @c{m = mm idModExt},
  would enable use of the regular @c{compose} function
  for composition of specifications (with information flow left-to-right).
  Haskellers and developers using similar composition-friendly languages
  might prefer this kind of representation,
  the way they like van Laarhoven lenses @~cite{oconnor2012lenses};
  yet, Oliveira @~cite{MonadsMixins} or
  the @c{Control.Mixin.Mixin} library (part of the @c{monadiccp} package),
  instead both use the same representation as mine, with @c{super} before @c{self}.
  Meanwhile, the Nix standard library, or the original paper @~cite{Bracha1990Mixin}
  use the opposite order with @c{self} before @c{super}.
  The approaches are all equivalent semantically, but readers must be wary
  to properly translate between calling conventions when consulting different sources.
}
@Code{
(def (idModExt t _s)
  t)
}
Note that since I decided to put the parent before the child as argument,
this “mix” is contravariant with the composition of functions of @c{c} and @c{p},
and the flow of information in this syntax goes left-to-right.
The opposite call convention is also possible, with various minor tradeoffs.
or you could have @c{c p t s} or @c{p c s t} with contravariant order between
the specification arguments during mixing and the target arguments during fixing.
ultimately, the order of arguments is immaterial, up to a simple isomorphism.

@subsection{Closing Modular Extensions}

A closed modular extension is
a function of type @c{C → C → C},
i.e. a modular extensible module specification where @c{V = C = W}.
In the common case that @c{C} is a record, this means that
an extension is provided for every identifier required.

As with closed modular module definitions before, the question is:
how do you get from such a closed modular module extension
to an actual module definition where all the loops are closed,
and every identifier is mapped to a value of the expected type?
And the way I constructed my model, the answer is simple:
first, under the scope of the module context,
you apply your extension to the top value for a module context
(usually, that’s the empty record);
then you have reduced your problem to a regular modular module definition
@c{∏R → ∏R}, at which point you only have to compute the fixpoint.
I will call this operation instantiation for modular extensions:
@Code{
(def (fix t m)
  (Y (m t)))
}
In this expression,
@c{t} is the top value for the type being specified (typically the empty record, for records),
@c{m} is the modular extension, and
the fixpoint variable for the module context being computed
is the argument to which @c{(m t)} is applied when @c{Y} calls it.

@subsection{Default and non-default Top Type}
Assuming some common top type @c{Top} and default value @c{top} in that type
(I will use @c{Any} and @c{#f} in my example Scheme implementation),
I will define the common instantiation operation for modular extensions:
@Code{(def fixt (fix top))}
or to inline @c{fix} in its definition:
@Code{(def (fixt m) (Y (m top)))}
Note that if the language-wide top type is too wide in some context:
for instance I chose @c{Any} as my top type in Scheme, with @c{#f} as my top value; but
you may want to choose the narrower @c{Record} as your top type,
so as to be able define individual methods,
with a @c{empty-record} as default value.

Then you can mix to the left of your modular extension,
a modular extension that precedes to it, and that throws away
any the previous value or computation (i.e. ignores its @c{super} argument)
and returns the new default value regardless of context (ignores its @c{self} argument;
unless that default is extracted from the context):
@Code{
(def (record-spec _super _self)
  empty-record)
}
I could then equivalently define a variant of @c{fix}
specialized for records in any of the following ways:
@Code{
(def fix-record (fix empty-record))
(def fix-record (λ (m) (Y (m empty-record))))
(def (fix-record m) (fixt (mix record-spec m)))
}
Note that because it ignores its @c{super} argument and thus throws away any inherited value,
the @c{record-spec} modular extension must appear first (as parent), or at least
before any modular extension the result of which isn’t to be ignored.
Why not make @c{empty-record} the language-wide default?
Because the language-wide default will apply not just to the specification of records,
but also to the specification of individual fields of each record,
and in this more general context, the default value @c{#f} is
possibly more efficient at runtime, and definitely more colloquial—therefore more efficient
in the most expensive resource, human-time.

@subsection[#:tag "MOI"]{Minimal OO Indeed}

The above functions @c{mix} and @c{fix} are indeed isomorphic
to the theoretical model of OO from Bracha and Cook @~cite{Bracha1990Mixin}
and to the actual implementation of “extensions” in nixpkgs @~cite{nix2015}@xnote["."]{
  My presentation of mixin inheritance is actually slightly more general than what
  Bracha, Cook or Simons did define, in that my definition is not specialized for records.
  Indeed, my closed modular extensions work on values of any type;
  though indeed to fully enjoy modularity, modular extensions work better
  when the module context is a record, or somehow contains or encodes a record.
  But my theory also importantly considers not just closed modular extensions,
  but also the more general open modular extensions, for which it is essential
  that they universally apply to target values of any type, with a module context of any type.
  I can therefore claim as my innovation a wider, more general understanding of mixin inheritance,
  of which there is no evidence in earlier publications.
}.
This style of inheritance was dubbed “mixin inheritance” by Bracha and Cook@xnote[";"]{
  The name “mixin” originally comes from Flavors @~cite{Cannon1979},
  inspired by the ice cream offerings at Emack & Bolios.
  As for the concept itself, it was inspired both by
  previous attempts at multiple inheritance in KRL @~cite{Bobrow1976} or ThingLab @~cite{Borning1977},
  combined with the ADVISE facility @~cite{teitelman1966}.
  However, Flavors offers full multiple inheritance (and was the first system to do it right),
  whereas the “mixins” of Bracha and Cook are a more rudimentary and more fundamental concept,
  that does not include automatic linearization of transitive dependencies.
  Also, “mixins” in Flavors are not distinguished by the language syntax or by the compiler;
  they are just abstract classes not meant to be instantiated,
  but to be inherited from (the word “abstract class” didn’t exist back then);
  a mixin in Flavors need not make sense as a base class, and
  can instead be inherited as part of many different hierarchies.
  Since the word implies no special processing by the compiler or by the human operator,
  it can be dispensed with in the original context, and gladly assigned
  a new, useful, technical meaning.
  But that doesn’t mean the context that made the word superfluous should be forgotten,
  quite the contrary.
  I will get back to Flavors when I discuss multiple inheritance. @;TODO secref
}
and the two functions, that can easily be ported to any language with first-class functions,
are enough to implement a complete object system.

How does one use these inheritance and instantiation functions?
By defining, composing and closing modular extensions of type @c{V → C → V} where
@c{V} is the type of the value under focus being extended,
and @c{C} is the type of the module context:
@Code{
(def (my-spec super self) body ...)}
where @c{super} is the inherited value to be extended,
@c{self} is the module context,
and @c{body ...} is the body of the function, returning the extended value.

In the common case that @c{V = ∏P},
and with my trivial representation of such records as @c{∏P = I → P}
where @c{I} is the type of identifiers,
a typical modular module extension will look like:
@Code{
(def (my-spec super self method-id) body ...)}
where @c{method-id} is the identifier for the method to be looked up,
and the body uses @c{(super method-id)} as a default when no overriding behavior is specified.

Alternatively, this can be abstracted in terms of using a mix of one or multiple
calls to this method-defining specification, that specifies a single method
with a given @c{key} as name for a recognized value of @c{method-id},
and a given open modular extension function @c{compute-value}
that takes the @c{inherited} value @c{(super method-id)} and the @c{self} context as arguments
and returns an extended value for the method at @c{key}:
@Code{
(def (field-spec key compute-value super self method-id)
  (let ((inherited (super method-id)))
    (if (equal? key method-id)
        (compute-value inherited self)
        inherited)))
}
Note how @c{field-spec} turns an open modular extension for a value
into an open modular extension for a record (that has this value under some key).
In this case, the module context @c{self} is the same,
whereas the @c{super} value for the inner function @c{compute-value}
is the specialized @c{(super method-id)} value extracted from the record,
passed as the first (inherited) argument.
That’s an example of how open modular extensions themselves have a rich algebraic structure,
wherein you can combine, compose, decompose, extract, and otherwise
operate on open modular extensions to get richer open modular extensions,
and eventually build a closed modular extension that you can instantiate.

@; secref ch9
Now, where performance or space matters,
you would use an encoding of records-as-structures instead of records-as-functions.
Then, instead of calling the record as a function with an identifier,
you would invoke a dereference function with the record as first argument
and the identifier as second argument.
But with some small overhead, the records-as-functions encoding is perfectly usable
for many simple applications@xnote["."]{
  I notably use this technique to generate all my slides in a pure functional way
  in Racket (a close cousin of Scheme). @; TODO CITE
  Interestingly, I could define a generic specification for
  slides that indicate where they are in the presentation,
  highlighting the name of each new section
  in an otherwise constant list of all sections.
  That specification uses the @c{self} context
  to extract the list of sections in the presentation,
  including sections not yet defined.
  It might seem impossible in an eager language, and without side-effects,
  to import data from slides that will only be defined later
  into a whichever slide is being defined now;
  and yet the Y combinator achieves this feat,
  and although I use the stateful Y for performance,
  a pure applicative Y also works without too much slowdown,
  because the substructures I recursively examine remain shallow.
}
Also, in a more practical implementation,
the inherited value in the @c{field-spec} would be made lazy,
or would be wrapped in a thunk, to avoid unneeded computations (that might not even terminate);
or for more power, the @c{compute-value} function
would directly take @c{super} as its first argument,
and @c{(super method-id)} would only be computed in the second branch.
In a lazy context, @c{lazy-field-spec} could also directly use @c{extend-lazy-record}
@; TODO secref to future definition of lazy variants?
to add a binding to a record without having to eagerly compute the bound value.

Whichever way simple modular extensions are defined,
they can thereafter be composed into larger modular extensions
using the @c{mix} function, and eventually instantiate a target record
from a modular extension using the @c{fix} function.
Since I will be using records a lot,
I will use the specialized @c{fix-record} function above.
Note that since my targets are records, my minimal object system is
closer to Prototype OO than to Class OO,
though, as I will show, it doesn’t offer “prototypes” per se,
or “objects” of any kind.

@subsection{Minimal Colored Point}

I will demonstrate the classic “colored point” example in my Minimal Object System.
I can define a modular extension for a point’s coordinates as follows:
@Code{
(def coord-spec
  (mix (field-spec 'x (λ (_inherited _self) 2))
       (field-spec 'y (λ (_inherited _self) 4))))
}
The modular extension defines two methods @c{x} and @c{y},
that respectively return the constant numbers @c{2} and @c{4}.

I can similarly define a modular extension for a record’s @c{color} field as follows:
@Code{
(def color-spec
  (field-spec 'color (λ (_inherited _self) "blue")))
}
Indeed, I will check that one can instantiate a point specified by combining
the color and coordinate modular extensions above, and that the values
for @c{x} and @c{color} are then as expected:
@Code{
(def point-ac (fix-record (mix coord-spec color-spec)))
(point-ac 'x) ;⇒ 2
(point-ac 'color) ;⇒ "blue"}
Consider how @c{x} is computed.
@c{fix-record} provides the @c{empty-record} as the top value for composition of modular extensions.
Then, modular extensions are applied under call-by-value evaluation,
with left-to-right flow of information from parent to child.
When querying the composed modular extension for method @c{x},
the parent specification @c{coord-spec} is applied first (as it has the leftmost position),
and matches the key @c{x};
it is then passed the top value @c{#f} extracted as a fallback default
when trying to read a missing field from @c{empty-record};
it proceeds to ignore that value, and return @c{2};
that value is then returned unchanged by @c{color-spec} (the child, right position),
since @c{x} does not match its key @c{color}.
Similarly, the query for method @c{color} returns the string @c{"blue"}.

However, this colored point example is actually trivial:
there is no collision in method identifiers between the two modular extensions,
and thus the two modular extensions commute;
and more importantly, the values defined by the modular extensions are constant
and exercise neither modularity nor extensibility:
their value-computing functions make no use of their @c{self} or @c{super} arguments.
They could have been defined with constant field helpers:
@Code{
(def (constant-spec value _super _self)
  value)
(def (constant-field-spec key value)
  (field-spec key (constant-spec value)))
}
I will now show more interesting examples.

@subsection{Minimal Extensibility and Modularity Examples}

I will illustrate extensibility with this example wherein the function @c{add-x-spec}
accepts an argument @c{dx}, and returns a modular extension that
overrides method @c{x} with a new value that adds @c{dx} to the @c{inherited} value:
@Code{
(def (add-x-spec dx)
  (field-spec 'x (λ (inherited _self) (+ dx inherited))))
}
Now I will illustrate modularity with another example wherein @c{rho-spec}
specifies a new field @c{rho} bound to the Euclidean distance
from the origin of the coordinate system to the point, using the Pythagorean theorem.
I assume two functions @c{sqrt} for the square root (builtin in Scheme)
and @c{sqr} for the square (which could be defined as @c{(λ (x) (* x x))}).
Note how the coordinates @c{x} and @c{y} are modularly extracted
from the module context @c{self}, which is the record being defined;
and these coordinates are not provided by @c{rho-spec},
but have to be provided by other modular extensions to be composed with it using @c{mix}:
@Code{
(def rho-spec
  (field-spec 'rho (λ (_inherited self)
    (sqrt (+ (sqr (self 'x)) (sqr (self 'y)))))))
}
I can check that the above definitions work,
by instantiating the composed modular extensions
@c{rho-spec}, @c{coord-spec} and @c{(add-x-spec 1)},
and verifying that the @c{x} value is indeed @c{3}:
i.e., first (parent-to-child, left-to-right) specified to be @c{2} by @c{coord-spec},
then incremented by @c{1} by @c{(add-x-spec 1)}.
Meanwhile @c{rho} is @c{5}, as computed by @c{rho-spec} from the @c{x} and @c{y} coordinates:
@Code{
(def point-r
  (fix-record (mix rho-spec (mix coord-spec (add-x-spec 1)))))
(point-r 'x) ;⇒ 3
(point-r 'rho) ;⇒ 5
}
This demonstrates how modular extensions work
and indeed implement the basic design patterns of OO.

Because @c{mix} is associative, instead of using long chains of nested binary calls to @c{mix},
I will instead use a n-ary @c{mix*} that can defined as follows
(though in practice I would use a longer definition with more optimizations):
@Code{
(define (uncurry2 f) (lambda (x y) ((f x) y)))
(define (mix* . l) (foldl (uncurry2 mix) idModExt l))
}
With this @c{mix*}, the leftmost argument is the least specific ancestor,
and the rightmost argument is the most specific descendent (overriding previous ones).
Now, note how trying to instantiate @c{(add-x-spec 1)} or @c{rho-spec} alone would fail:
the former relies on the @c{super} record to provide a useful inherited value to extend,
whereas the latter relies on the @c{self} context to modularly provide @c{x} and @c{y} values.
Neither modular extension is meant to stand alone,
but instead to be a mixin, in the sense of Flavors—an abstract class, or a trait,
members of some programming language ecosystems would say.
That not every specification can be successfully instantiated
is actually an essential feature of modular extensibility,
since the entire point of a specification is to contribute some @emph{partial} information
about one small aspect of an overall computation,
that in general depends on other aspects being defined by complementary specifications.

@subsection[#:tag "IME"]{Interaction of Modularity and Extensibility}

Without extensibility, a modular module specification need never access
the identifiers it specifies via the global module context (@c{self}),
since it can more directly access or inline their local definition
(though it may then have to explicitly call a fixpoint locally if the definitions
are mutually recursive, instead of
implicitly relying on a global fixpoint via the module context).

For instance, consider the following modular definition,
to be merged with other specifications defining disjoint sets of identifiers.
In this definition, the @c{case} special form of Scheme selects a clause to execute
based on which constant, if any, matches its first argument.
This definition provides (a) a @c{start} method that returns the constant 5;
(b) a @c{length} utility function that computes the length of a list,
using the open recursion through @c{self} for its recursion;
(c) a @c{size} method that subtracts the @c{start} value from the length
of a list returned by the @c{contents} method,
while using the @c{length} method, provided above, for the length computation.
The @c{contents} method is required but not provided;
it must be modularly provided by another modular definition.
@Code{
(def (my-modular-def self method-id)
  (case method-id
    ((start) 5)
    ((length) (λ (l) (if (null? l) 0
                         (+ 1 ((self 'length) (cdr l))))))
    ((size) (- ((self 'length) (self 'contents)) (self 'start)))
    (else #f)))
}
Since by my disjointness hypothesis,
the global specification for @c{start}, @c{length} and @c{size}
will not be overridden, then @c{(self 'start)} and @c{(self 'length)}
will always be bound to the values locally specified.
Therefore, the value @c{5} may be inlined into the specification for @c{size},
and a fixpoint combinator or @c{letrec} can be used to define the @c{length} function,
that can also be inlined in the specification for @c{size}.
The @c{contents} method, not being provided, must still be queried through open recursion.
@Code{
(def my-modular-def-without-global-recursion
  (let ((start% 5))
    (letrec ((length% (λ (l) (if (null? l) 0
                                 (+ 1 (length% (cdr l)))))))
      (λ (self method-id)
        (case method-id
          ((start) start%)
          ((length) length%)
          ((size) (- (length% (self 'contents)) start%))
          (else #f))))))
}
By contrast, with extensibility, a modular extensible module specification may usefully
require @emph{a value} for a method for which it also provides @emph{an extension} (and not a value).
The value received will then be, not that returned by the extension,
but the final fully-resolved value bound to this identifier
after all the extensions are accounted for.
That information obviously cannot be provided locally by the specification,
since it involves further extensions that are not knowable in advance,
many different variants of which can be instantiated, in the future@xnote["."]{
  Unless the specification ignores its @c{super} argument, the value may also depend
  on an inherited value that is not provided yet.
  However, using this @c{super} argument and providing an updated, extended value from it,
  is something the specification is explicitly allowed and expected to do.
  But the specification cannot expect that extended value to be the @emph{final} value
  that the “effective method” will return.
  Thus, the problem is not with the inherited @c{super} argument,
  but with the fact that the value returned by the current method is itself
  the @c{super} value inherited by further methods.
}

Thus, consider the base specification for a parts-tracking protocol below.
It provides a @c{parts} method, returning a list of parts, initialized to the empty list;
also a @c{part-count} method that returns the length of the @c{parts} list;
and it is otherwise pass-through for other methods.
The @c{part-count} method crucially accesses the final value of the @c{parts} method
through the module context @c{self}, and not the currently available initial empty value:
@Code{
(def (base-bill-of-parts super self method-id)
  (case method-id
    ((parts) '())
    ((part-count) (length (self 'parts)))
    (else (super method-id))))
}
You cannot inline the empty list in the call to @c{(self 'parts)}
because the method @c{parts} can be extended, and indeed
such is the very intent and entire point of this @c{base-bill-of-parts} specification!
Even future extensions cannot inline the value they reach,
unless they are guaranteed that no further extension will extend the list of parts
(a declaration known as “sealing”, after Dylan).
@;{CITE Andrew Shalit, The Dylan Reference Manual: The Definitive Guide to the New Object-Oriented Dynamic Language 1996}

The interaction between modularity and extensibility therefore
expands the scope of useful opportunities for modularity,
compared to modularity without extensibility.
@principle{Programming with modularity and extensibility is more modular than with modularity alone}.
This makes sense when you realize that when the software is divided into
small modular extensions many of which conspire to define each bigger target,
there are more modular entities than when there is only
one modular entity for each of these bigger targets.
Modular extensibility enables modularity at a finer grain.

There is another important shift between modularity alone and modularity with extensibility,
that I quietly smuggled in so far,
because it happened naturally when modeling first-class entities using FP.
Yet this shift deserves to be explicitly noted,
especially since it is not quite natural in other settings
such as existed historically during the discovery of OO
or still exist today for most programmers:
Modularity alone was content with a single global module context that everyone linked into,
but @principle{the whole point of extensibility is that you will have many entities
that will be extended in multiple different ways};
therefore when you combine the two, it becomes essential that module contexts
be not a single global entity, but many local entities.
This shift from singular and global to plural and local is essential for Class OO,
and even more so for Prototype OO.

@exercise[#:difficulty "Easy"]{
  Reproduce the examples from the chapter.
  Then define your own points of various colors and names,
  with 2d or 3d coordinates, with or without using specifications.
}

@exercise[#:difficulty "Easy"]{
  Verify that @c{idModExt} is indeed a neutral element for
  the composition of modular extensions via @c{mix}.
  What happens if you instantiate it with @c{(fix top)}?
}

@exercise[#:difficulty "Easy"]{
  Write a specification that defines polar coordinates for a 2d point
  with defined cartesian coordinates, and another specification the other way around.
  Test your specifications with various points.
  What happens if you compose those two specifications, try to instantiate them,
  and extract coordinates?
}

@exercise[#:difficulty "Medium"]{
  Prove that @c{mix} is associative. Test it in practice.
}

@exercise[#:difficulty "Medium"]{
  Write modular extensible specifications that rely on @c{base-bill-of-parts}
  to contribute a chassis, axles, wheels, etc., where, to simplify, each part is just a string,
  e.g. @c{"front left wheel"}, and each specification contribute one or many parts.
  Define overall specifications for a toy car, a toy truck, a toy motorcycle, a toy bicycle,
  while trying to share as much code as possible between the different toys.
  You may define functions that take arguments, manipulate strings, and return specifications.
  Use the @c{parts} and @c{part-count} functions on each of your complete specifications.
}

@exercise[#:difficulty "Medium, Recommended"]{
  If you did exercise @exercise-ref{04to05}, compare your previous formalization with mine.
  See what surprised you—how your formalization of modular extensibility compares to mine:
  whether they are equivalent, which can be expressed in terms of the other,
  which is more minimal, what insights you missed, what features I left out (at least so far),
  and how your understanding has evolved already.
}

@exercise[#:difficulty "Hard, Recommended" #:tag "05to06"]{
  Based on this minimal model of OO as modular extensibility,
  and on the informal explanations in @secref{WOOiIO},
  sketch how you would (1) rebuild Prototype OO as a layer on top of the model,
  (2) rebuild Class OO on top of Prototype OO, and
  (3) assign Types to OO.
  Assume mixin inheritance for now, i.e. via using the @c{mix} function.
  Save your answer to compare with the treatment in @secref{ROOfiMC}.
}

@exercise[#:difficulty "Hard" #:tag "5alist"]{
  Define an API for manipulating records, as a set of methods.
  Provide an implementation of that API with functions in the style I used.
  Provide another implementation of it with alists as in @exercise-ref{alist0}.
  Provide another implementation of it with balanced binary trees (say, red-black trees).
  Define that red-black-tree implementation in multiple specifications:
  one for binary trees, an abstract one for balancing and rebalancing based on some metadata,
  a more concrete one for red-black-trees.
  Along the way, you may also have to define an API for order and comparisons.
  In the end, you will have bootstrapped a more efficient representation for records
  from a simple one.
  Write functions to convert between the two. Notice the limitations in one direction.
  Can you use alist- or tree- based records directly with the Y combinator?
  If not, explain why not, and consider if wrapping in a thunk, delay or once may help.
  With or without wrapper, do it.
}

@exercise[#:difficulty "Research"]{
  Port this minimal OO system to your language of choice.
  What issues are you facing and why?
  Can you make the system usable?
  Make a library out of it that you actually use?
  (See for instance the slides next to the source code for this book,
  at @url{https://github.com/metareflection/poof}.)
  Can you attract users beside yourself?
  What patches to your language implementation did you need,
  or would you need to implement the system better?
  If your language has static types, how do you deal with them?
}
