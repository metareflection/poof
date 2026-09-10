#lang scribble/base
@; -*- Scheme -*-
@(require "util/ltuo_lib.rkt")
@(set-chapter-number 9)

@title[#:tag "EtSoO"]{Extending the Scope of OO}
@epigraph{
  If I have seen further it is by standing on the shoulders of giants.
  @|#:- "Isaac Newton"|
}
@section[#:tag "OfOO"]{Optics for OO}

@subsection{Optics: A Pure Functional Approach to Pointers (and more)}

This section will introduce lenses and “optics” in general—tools
functional programmers use to edit (sometimes deeply) nested data structures.
Why bother? Why not “just” assume stateful side-effects, and
follow pointers along a path to the data you want to edit in place?

Because when it comes to semantics, pointers and side-effects are actually
much more complex and indirect, involving some global store with non-local effects—which
makes them much harder to reason about.
By all means, when it comes to the efficient implementation of objects (@secref{EOI}),
I will use side-effects galore.
But as long as one is investigating what objects @emph{mean},
what can be expressed with them, and why—then
the semantic simplicity of pure functional programming matters, a lot.
And optics are indeed “just” the pure functional analogue to pointers within data structures,
to paths to follow from one pointer to another—used in systematic ways.

@subsection{Focused Specifications}

Before I can revisit familiar features of advanced OO systems such as
accessors, method combinations or multimethods, I must once again
introduce some new elementary concept that will much simplify their formalization:
@emph{focused} specifications.

A specification, whether a modular definition, a modular extension,
a multiple inheritance specification, an optimal inheritance specification, etc.,
can be @emph{focused} by enriching it (via conflation or explicit product)
with two access paths:
a path from the top of the program state to the module context being referenced, and
a path from the top of the program state to the method being extended.
Thus, instead of being “free floating”, your specification will be “located”
within the context of the greater program state.
Furthermore, to keep formalizing OO features in terms of pure functional semantics,
these access paths I will formalize as functional @emph{lenses} (@secref{SRoL}).

The approach I herein propose to specifying OO software is a game-changer,
with the potential to make OO even more modular than it was thus far:
open method specifications can now be considered individually,
then grouped incrementally into larger algebraically coherent chunks;
at each step, every specification can be parsed, defined, typed, analyzed, proved correct,
algebraically combined and manipulated, and generally reasoned about,
at whichever granularity it makes sense.
Until now, you couldn’t even start to semantically process a definition, much less reason about it,
until after it was part of a potentially very large class providing its context,
one that soon ends up providing more semantic context
than can safely fit within anyone’s ability to reason correctly.

Before I discuss new features, I will start by showing how focused specifications
can simplify the formalization of individual classes or prototypes within an ecosystem,
or of regular methods within a prototype.
The cost of introducing lenses is why the notion of focused specification
would have been overkill and a distraction in the formalism of previous chapters,
when handwaving the relationship between open and closed modular extensions was good enough.
But since I am going to pay the price anyway for the sake of explaining advanced features,
I may as well enjoy the benefits for basic features as well.

@subsection[#:tag "SRoL"]{Short Recap on Lenses}

A lens @~cite{Foster2007 OConnor2012 Pickering2017} @;TODO cite Bancilhon1981 Oles1982 Kmett2015
is the pure Functional Programming take on what in stateful languages would typically be
a C pointer, ML reference, Lisp Machine locative, Common Lisp place, etc.:
a way to pinpoint some location to inspect and modify within the wider program’s state.
A lens is determined by a “view”, a function from a “source” to a “focus”;
and an “update”, a function from a change to the focused data to a change
of the wider state from “source” to an updated “target”@xnote["."]{
  In more stateful languages, a more popular view is that of
  a pair of a “getter” and a “setter”;
  this maps well to lower-level primitives of stateful languages.
  But while updates can be composed by themselves independently of the views,
  setters do not compose by themselves:
  to compose an outer setter with an inner setter, you also need the outer getter,
  to recover the current focused value on which the inner setter operates.
  Thus, getter and setter do not constitute
  an @emph{orthogonal}, independently compositional basis @; TODO cite VanWijngaarden1965 Wegner1987
  on which to formalize lenses, whereas view and update do.
  In the Haskell lens libraries, the “update” function is instead called “over”;
  maybe because it “applies a function @emph{over} a change in focus”;
  maybe also because the word “update” was taken by other operations;
  in any case, I don't find “over” a particularly revealing name.
  I’ll stick to “update”.
}
As a function from source to focus and back, it can thus also be seen as generalizing
paths of fields and accessors, e.g. field @c{bar} of the third element of field @c{foo}
of the entry with key @c{(42, "baz")} within a table.

@subsubsection{Monomorphic Lens}
A monomorphic lens (or simple lens) can be seen as
a pair of a view function @c{s → a} and an update function @c{(a → a) → s → s}.
The view function allows you to get a current “inner” value under focus, of type @c{a},
from the “outer” context, of type @c{s}.
The update function allows you to see how a local change in the “inner” value under focus
transforms the “outer” context being focused:
@Code{
type MonoLens s a =
       { view : s → a ; update : (a → a) → s → s }
}

@subsubsection{Polymorphic Lens}
A polymorphic lens (or “stabby” lens), of type @c{PolyLens s t a b}, generalizes the above:
you still have a view function @c{s → a} to extract an inner value from the outer context,
but your update function now has type @c{(a → b) → s → t},
so that the inner change in value can involve different input and output types,
and so can the outer change in context.
But the type parameters @c{s} and @c{a} shared between the view and update types
express the constraint that you are updating the same thing you are viewing.
Monomorphic lenses are a special case of polymorphic lenses
where the updates don’t affect the types of either the focused value of the context value.
@Code{
type PolyLens s t a b =
       { view : s → a ; update : (a → b) → s → t }
type MonoLens s a = PolyLens s s a a
}

@subsubsection{Skew Lens}
A skew lens (or “irpjsq” lens, pronounced “earp jusq”)
is my further generalization of the above,
invented specifically to fit the needs of modular extensions in this book:
now the types for the update are not required to be the same as those for the view,
so you don’t have to be looking exactly at the change you’re experiencing.

The view @c{s → r} goes from an outer context @c{s} to an inner context @c{r}
(where “r” is for required, and “s” is just the next letter),
and the update goes from an extension @c{i → p} to @c{j → q}
(where “i” is for inherited, “p” is for provided, and “j” and “q” are just the next letters).
Polymorphic lenses are a special case of skew lenses.
@Code{
type SkewLens i r p j s q =
       { view : s → r ; update : (i → p) → j → q }
type PolyLens s t a b = SkewLens a a b s s t
}

@subsubsection{View and Update}
I can also give separate types for View and Update:
@Code{
type View r s = s → r
type Update i p j q = (i → p) → j → q
type SkewLens i r p j s q =
  { view : View r s ; update : Update i p j q }
}

@subsubsection{Getter and Setter}
There are cases when one may prefer the familiar view of lenses
as involving a getter and a setter, instead of a view and an update.
The getter and the view are the same thing, so that’s easy.
On the other hand, the setter and the update are slightly harder.
@Code{
type Setter s t b = b → s → t
lens←getter*setter : View a s → Setter s t b → PolyLens s t a b
setter←lens : PolyLens s t a b → Setter s t b

(def (lens←getter*setter get set)
  (make-lens get (λ (f s) (set (f (get s)) s))))
(def (setter←lens l)
  (λ (b) (l 'update (λ (_a) b))))
}
Note how you need a matching getter and setter to achieve a polymorphic lens.
To achieve a skew lens, you would need two getters and a setter:
one getter for the view, another getter and a setter for the update;
the two getters needn’t match, but the second getter and the setter must.
Now with the second getter and the setter, you could do strictly more than update;
therefore this interface would be more restrictive than that of a skew lens:
it would both require more work from programmers, and yet
reject many interesting skew lenses from which no such second getter can be meaningfully extracted.

@subsubsection{Composing Lenses}
I can compose view, update and lenses as follows,
with the obvious identity lens:
@Code{
compose-view : View s t → View r s → View r t
(def (compose-view v w)
  (compose w v))

compose-update : Update j q k r → Update i p j q → Update i p k r
(def compose-update compose)

make-lens : View r s → Update i p j q → SkewLens i r p j s q
(def (make-lens v u)
  (extend-record 'view v
    (extend-record 'update u
      empty-record)))

compose-lens : SkewLens j s q jj ss qq → SkewLens i r p j s q →
                 SkewLens i r p jj ss qq
(def (compose-lens l k)
  (make-lens
    (compose-view (l 'view) (k 'view))
    (compose-update (l 'update) (k 'update))))

id-lens : SkewLens i r p i r p
(def id-lens
  (make-lens identity identity))

(define compose-lens* (op*←op1.1 compose-lens id-lens))
}

You’ll notice that @c{compose-view} is just @c{compose} with flipped arguments,
and @c{compose-update} is just @c{compose}.
@c{compose-lens} just composes each component with the proper function@xnote["."]{
  As usual, you can represent your lenses such that you can compose them with the
  regular @c{compose} function, by pre-applying the @c{compose-lens} function to them.
  Haskellers use a further condensed representation as a single composable function
  that some claim is more efficient, “van Laarhoven” lenses,
  but I will avoid it for the sake of clarity.
}
Views, Updates and Lenses each form categories of their own,
wherein composition is associative, and identities are neutral elements.

@subsubsection[#:tag "FL"]{Field Lens}
Given some record representation, a view for a field of identifier key @c{k}
is just a function that, given as context argument the record @c{r},
returns the field value @c{r.k} or, in Scheme syntax, with my records-as-functions, @c{(r 'k)}.
Meanwhile, an update gives you a change in this overall context record
given a change for that field under focus.
More sophisticated representations will have more sophisticated lenses,
but here is what it looks like in my trivial representation of records
as functions from identifiers to values.
@Code{
(def (field-view key s)
  (s key))
(def (field-update key f s)
  (extend-record key (f (s key)) s))
(def (field-lens key)
  (make-lens (field-view key) (field-update key)))

(define (field-lens* . keys)
  (apply compose-lens* (map field-lens keys)))
}

To access the subfield @c{bar} of the field @c{foo} of an object @c{x},
you can use @c{(field-lens* 'foo 'bar)} with @c{x}.
Note that the order of lenses in @c{field-lens*} is covariant with
the usual notation @c{x.foo.bar}.
Some syntactic sugar could help you achieve a similar notation, too,
but that would require implementation-dependent extensions to Scheme
(see how it is done in Gerbil Scheme).

A @c{(field-lens key)} can be a simple lens of type @c{MonoLens s a}
when applied to a record of type @c{s} that has a field @c{key} of type @c{a}.
But it can also be used as a polymorphic lens or skew lens,
where you view the field @c{key} of your context and
also modify the field @c{key} of the value you extend,
but the two need not be the same, and the modification need not preserve types.

In practice, I also use a variant @c{field-lens~} that
when applied to a context that is the default value @c{#f}
treats it as if it had been the @c{empty-record}, and a variant @c{field-lens~*}
that goes through a series of fields in order, with such defaulting at each level.
Similarly, I use functions @c{field-view~}, @c{field-view~*},
@c{field-update~} and @c{field-update~*} for the view and update aspects of those lenses,
and @c{field-spec~} and @c{field-spec~*} for skew lenses that
zoom the focus onto such defaulted fields@xnote["."]{
  As an exercise, implement these functions.
  Or then again just look how I write them in pommette.
}

@subsection[#:tag "FME"]{Focusing a Modular Extension}

@subsubsection{Skewing a Modular Extension}

You may have noticed that I used the same letters @c{i r p}
to parameterize a @c{SkewLens} (plus their successors)
as to parameterize a @c{ModExt}. This is not a coincidence.
You can focus a modular extension by looking at it through a matching skew lens:
@Code{
skew-ext : SkewLens i r p j s q → ModExt i r p → ModExt j s q
(def (skew-ext l m super self)
  (l 'update (λ (inner-super) (m inner-super (l 'view self)))
             super))
}

Thus with a @c{SkewLens i r p j s q},
I can change a modular extension from parameters @c{i r p} to @c{j s q}.
The fact that this is exactly what works for arbitrary open modular extensions
justifies why I choose skew lenses and their view/update interface,
as opposed to the more limited monomorphic or polymorphic lenses
or the more naive getter/setter interface, that would work on closed modular extensions.

Now, the type @c{SkewLens i r p j s q} works well with
the Trivial Modular Extensions from @secref{TNMTfME}:
@Code{
type TModExt inherited required provided =
  inherited → required → provided
}
But it is not enough to work with the stricter and recursive modular extensions of @secref{ST}:
@Code{
type ModExt inherited required newlyProvided =
  ∀ super, self : Type
    self ⊂ required self, super ⊂ (inherited self) ⇒
        super → self → super∩(newlyProvided self)
}
To work with @c{ModExt}, you need an accompanying stricter and recursive type for skew lenses:
@Code{
type SSkewLens inherited required newlyProvided
               jnherited sequired newlyQrovided =
  ∀ previous, final : Type
    final ⊂ sequired final, previous ⊂ (jnherited final) ⇒
  ∃ super, self : Type
    self ⊂ required self ∧ super ⊂ (inherited self) ∧
  { view: final → self ;
    update : (super → super ∩ (newlyProvided self)) →
             (previous → previous ∩ (newlyQrovided final)) }
}

Alternatively, you could just change the point of view and define a lens
as a transformation between modular extensions.
However, note how the formula for skew lenses remains essentially the same,
whichever types you assign to it, at least up to some linear isomorphism:
you may rearrange the chairs on the deck, or change the order of arguments and results
for each function, and even add “phantom” type-level arguments and results
corresponding to “witnesses” for universal or existential type quantifiers.
But in the end, the computation is the very same, all you do is
be more or less precise about the set of computation contexts
in which the function can be evaluated without type error.

@subsubsection{Metaphors for Modular Extensions and Skew Lenses}

A modular extension can be conceived as a @emph{sensactor}:
it has a sensor, the input from the module context,
and an actuator, the output of an extension to the value under focus.

A single skew lens can change both the module context and the extension focus.
A @c{SkewLens i r p j s q} can transform an inner @c{ModExt i r p}
into an outer @c{ModExt j s q}.
As always, note that in general, @c{r} (required, the module context)
is largely independent from @c{i p} (inherited and provided, the extension focus).
They only coincide just before the end of the specification,
to obtain a closed modular definition you can resolve with a single use of the Y combinator.

One way to think of a skew lens is as similar to the pair of a periscope
through which a submarine pilot may look outside,
and the wheel or yoke through which he may pilot his boat:
the submarine pilot is the sensactor, and the skew lens
transforms the I/O loop of the pilot into the I/O loop of the submarine.
Similarly, one may consider the laparoscope that a surgeon may use,
and the separate instrument with which he will operate on the patient:
the “skew lens” transforms the surgeon I/O into the instrument I/O.

It is a common case that the actuator is within the frame of the sensor,
such that the sensactor may observe not only what they are modifying, but also a wider context.
In formal words, there are two polymorphic lenses @c{l} and @c{k}
such that the skew lens view is @c{(l 'view)}, and the skew lens update is
@c{(compose-lens l k 'update)}, further specializing the previous view.
But that is not necessary in the general case.
Just like competent painters can put paint on canvas while looking at their model,
not at their hand, the “skew lens” does not imply that the hand is seen by the eye,
that the actuator is somehow visible in the frame of the sensor.
The actuator may involve a bigger extension focus (heating the whole room, not just the thermometer),
or an extension focus completely disjoint from the context being observed
(like a caricaturist reproducing features observed in his model,
but in an exaggerated style).

Now, inasmuch as you consider those function types as a model for stateful mutation
with in-memory pointers, that means that the “context view pointer” @c{v}
and the “focus update pointer” @c{u} are independent.
In a mutable variant of a skew lens, you will have two pointers,
one read-only and one read-write, not just a single pointer
as with monomorphic or polymorphic lenses.
This also explains why the “getter and setter” interface doesn’t work for skew lenses:
the getter for the context does not match that for the focused element to update,
so you also need a second getter for the latter, and a setter for the latter but not the former.

@subsubsection{Focused Specification}

A @emph{focused specification} will be the pair of a skew lens and a specification.
Above, the specification was a modular extension;
but in general, it may as well be a modular definition,
a multiple inheritance specification, an optimal inheritance specification, etc.,
depending on the kind of specification you’re interested in.

The skew lens says what exactly the specification is modifying,
relative to a known place—typically the entire ecosystem, but
potentially any other place you may currently be looking at.

Now, some of you may notice, the match between skew lens
and modular specification is not just perfect, it is a bit @emph{too perfect}:
what if instead of mixin inheritance, one wanted to use
single, multiple or optimal inheritance?
How is a multiple (or optimal) inheritance specification to be used?
What then happens to the local order, precedence list, suffix flag
(and possibly conflated target), the other parts of a specification (and prototype)?
How are these components affected or not affected by a skew lens,
and what happens when the skew lens is refocused?

The “has-a” solution (@secref{BHU}) is simpler, and applies to sub-elements of specifications.
It is more universal in what kind of values it can process
as an element under focus located within some context;
these elements, as a common but not universal special case,
can be a specification or prototype that you want to update.
The update would typically (though not necessarily) involve specializing the element.
This specialization would consist in
composing with a modular extension (for single or mixin inheritance), or
prepending to the local order (as a total order, i.e. as a list of parents,
or better, as a partial order, i.e. as a list of lists of parents,
for multiple or optimal inheritance).
This approach works well, and takes advantage of regular skew lenses
with their usual categorical laws.
But it fails to specifically leverage the context view part of skew lenses,
and only leverages their focus update part;
thus it fails to extend the notion of focused specification
from modular extensions to richer forms of specifications.

The “is-a” solution (@secref{BHU}) is more elaborate, and
extends skew lenses to input and output prototypes,
including all additional fields, and not just modular extensions.
Applying a suitably extended skew lens would modify each of these,
presumably specializing them (as above).
You could specialize the context as well as the focus,
and the inheritance DAG from a more general lens would remain a superset
of the inheritance DAG from a more specialized lens, that only contains
the changes affecting that context and focus.
This approach may further involve advanced types that constrain
the specializations to behave in various ways.
With this approach, a focused specification keeps extending the simple case of modular extensions
into richer object systems.

@;{
You might try to further decompose those lenses into elementary lenses
that separately handle each aspect of a prototype, or further into
consumers, matcher, destructurers, destructors, or continuations
that take them into input (the negative part),
and producers, constructors, initializers and other terms
that yield them as output (the positive part),
with more or less elaborate computations in between.
}

@subsection{Adjusting Context and Focus}

@subsubsection{Adjusting both together}

A monomorphic lens, or simple lens, can refocus a closed specification focus
into another closed specification focus, such that a local closed specification
can be turned into a global closed specification for the complete ecosystem.
Thus, when specifying a value @c{foo.bar} in the ecosystem,
you will use @c{(compose-lens (field-lens 'foo) (field-lens 'bar))}
to focus your modular extensions.

As is a theme in this book, though,
and as far as I know has never been discussed before in the OO literature,
the interesting entities are the @emph{open} specifications, not just the closed ones.
This is what makes skew lenses interesting.
If you considered only closed specifications, you would only consider monomorphic lenses
(or maybe polymorphic lenses when you specifically want to distinguish types
for the ecosystem before and after extension).
But then, you wouldn’t be able to formalize the advanced notions
I am going to discuss in the rest of this chapter.

@subsubsection{Adjusting the Extension Focus}
Given a focus on a specification,
one can focus on a specific method of that specification
by further adjusting the extension focus using @c{u = (field-update key)}
where @c{key} is the identifier for the method.
Thus, @c{(compose-lens (field-lens* 'foo 'bar) (update-only-lens (field-update 'baz)))}
or equivalently @c{(update-lens (field-lens* 'foo 'bar) (field-update 'baz))}
will let you specify a method @c{baz}
for the specification under @c{foo.bar} in the ecosystem, where:
@Code{
update-only-lens : Update i p j q → SkewLens i r p j r q
(def (update-only-lens u)
  (make-lens identity u))

update-lens : SkewLens i r p j s q → Update ii pp i p →
     SkewLens ii r pp j s q
(def (update-lens l u)
  (make-lens (l 'view) (compose (l 'update) u)))
}

More generally, given a lens @c{l} focusing on the specification,
and a lens update @c{u} to refocus on just the extension focus,
the lens @c{(compose-lens l (update-only-lens u))} will adjust focus on
the method selected by @c{u}
of the specification at @c{l}.
This is a common case when specifying
a sub-method within a method (more on that coming),
a method within a specification,
a specification within a library,
a library within the ecosystem—all while keeping the broader entity
as context when specifying the narrower one.

@subsubsection{Broadening the Focus}

At times, you may want to make the focus broader than the module context.
Then, you can use a lens with “negative focal length”:
instead of narrowing the focus to some subset of it,
it broadens the focus.
Thus you can zoom out rather than only zoom in.
Zooming out can take you back to where you were previously,
or to a completely different place.
You are responsible for zooming out to the “right” place
that keeps the program meaningful in its context,
e.g. by remembering the context that was forgotten in some narrower scope.

For instance, given a broader context @c{c : s}
and a lens @c{l : MonoLens s a}, the following “reverse lens” broadens the focus,
by completing the “rest” of the reverse focus with data from the context.
Beware though that if you use the update more than once, you will always get answers
completed with data from the same non-updated context.
If you want to update the context each time, you have to update the reverse lens
with the updated context every time.
These “reverse lenses” therefore do not fully satisfy the usual lens laws,
and are called “lenses” in a loose or generalized way.
@Code{
reverse-view : s → MonoLens s a → View s a
reverse-update : s → MonoLens s a → Update s s a a
reverse-lens : s → MonoLens s a → MonoLens a s

(def (reverse-view c l a)
  (setter←lens l a c))
(def (reverse-update c l f a)
  (l 'view (f (reverse-view c l a))))
(def (reverse-lens c l)
  (make-lens (reverse-view c l) (reverse-update c l)))
}

@subsubsection{Adjusting the Context}

The module context contains the data based on which a modular extension may compute its extension.
Sometimes, you may want to narrow the context, to match the already narrowed extension focus;
or to broaden the context to the next broader entity;
or to locally override some configuration in the context;
or to locally instrument some of the context entities
(e.g. for debugging, testing, profiling performance, etc.);
or just to switch the context to something different for any reason.
You can also use the view to narrow the context in terms
of visibility, access rights, or some other system of permissions or capabilities,
so the extension may only invoke safe primitives,
or primitives that were suitably wrapped in a “security proxy” for safety.
Or you can focus the context on one object to specify extensions for another object
that somehow “mirrors” or reacts to the object in context,
or logs its history, backs up or persists its data, does resource accounting, etc.

To adjust the context without adjusting the extension focus, use:
@Code{
view-only-lens : View r s → SkewLens i r p i s p
(def (view-only-lens v)
  (make-lens v identity))

view-lens : SkewLens i r p j s q → View rr r →
    SkewLens i rr p j s q
(def (view-lens l v)
  (make-lens (compose-view (l 'view) v) (l 'update)))
}

@subsection[#:tag "OfSaP"]{Optics for Specifications and Prototypes}

With this optics toolbox, I can construct nested OO specifications
in terms of composing a lot of small localized pure functions.

@subsubsection{Specification Methods}

As explained in the previous section,
given a lens @c{l} to focus on a specification from the environment
and an update @c{u} to focus the extension on a method or sub-method within that specification,
one can extend that method of that specification with a modular extension @c{m}, with:
@Code{
(skew-ext (update-lens l u) m)
}
For instance, to move a widget registered under the name “foo” 50 pixels to the right,
you might use:
@Code{
(skew-ext (update-lens (field-lens* 'widgets 'foo)
                       (field-update 'x-pos))
          (λ (super _self) (+ super 50)))
}
Helper functions might provide terser syntax for common cases,
but what matters for the purpose of this book is that
the semantics of such method definitions can be described
by composing a few simple first-class functions.
Reasoning about method definitions, proving correctness or security properties about them,
building automation to define methods in systematic ways that minimize programmer hassle
and maximize the chances that the result is correct and performant, etc.,
all that is not just possible, but simple, using this pure functional approach to OO
based on composing open modular extensions.

@subsubsection[#:tag "PS"]{Prototype Specification}

I’ll assume for now that prototypes are records implemented with the @c{poi} encoding
from @secref{POI}@xnote["."]{
  Note how POI and multiple inheritance in general rely on equality of generated identity tags,
  which some of you may consider an unwanted side-effect.
  In this case, you may use @c{rproto} from @secref{CfR} that relies only on mixin inheritance,
  which is pure. Or you may request users to manually provide identity tags.
  Or you may restrict your prototypes to the second-class usage at meta-level
  where you somehow find this side-effect more acceptable
  (and where second-class restrictions might make source location alone
  enough to identify entities).
}
Then, if you have a lens @c{l} to focus on a prototype,
you may further focus on the prototype’s specification
(in the case of POI, the list of arguments to @c{make-poi})
by further composing @c{l} with the following lens, after which you can further use lenses
to modularly extend the specification methods as above:
@Code{
(def (poi-spec-view poi)
  (list (poi-mod-ext poi) (poi-suffix? poi) (poi-parents poi)))
(def (poi-spec-setter args _)
  (apply make-poi args))
(def poi-spec-lens
  (lens←getter*setter poi-spec-view poi-spec-setter))
(def poi-modext-lens
  (compose-lens poi-spec-lens list-first-lens))
(def poi-suffix?-lens
  (compose-lens poi-spec-lens list-second-lens))
(def poi-parents-lens
  (compose-lens poi-spec-lens list-third-lens))
}

The entire point of @c{POI} (after @c{rproto}, @secref{CfR})
is that the target view is @c{identity}.
However, what the basis for target update should be is an interesting question.
There are many options; none of them seems universally correct;
any of them can be a feature or a bug, depending on user intent, but each is often a bug;
and so the error behavior is probably the safest one to use by default:
@itemlist[
  @item{If you update some fields of the target, then the target will not be in sync
    with the specification anymore unless the user keeps it so the hard way.
    If some further program extends that prototype, it will restart from the specification,
    and ignore any update otherwise made to fields.}
  @item{If you try to have an update function that arbitrarily changes the specification
    to be @c{constant-spec} that constantly returns the current state of the record,
    then the result remains extensible, but in a way that forgets the formulas,
    and remembers only the current values.}
  @item{If you update the target then the magic specification field will be erased by default,
    and the object will not be extensible through inheritance anymore
    (overriding the metadata field under magic key @c{#f})
    unless you make it so again the hard way by explicitly providing a new metadata field.}
  @item{If you try to update the target, an error will be thrown,
    and you won’t later have to debug very surprising behavior.}]
To a first approximation, this corresponds to these @c{poi-target-update} functions
as the basis for what a @c{poi-target-update} will do:
@Code{
(def (poi-target-update/OutOfSync u poi)
  (u poi))
(def (poi-target-update/OverwriteSpec u poi)
  (make-poi (constant-spec (u poi)) #f '()))
(def (poi-target-update/NoMoreSpec u poi)
  (u (extend-record #f #f poi)))
(def (poi-target-update/Error u poi)
  (abort "cannot update a poi target"))
}

@subsubsection[#:tag "NS"]{Nested Specifications}
What is interesting about the approach above is that you get
the semantics of nested specifications for free:
a modularly extensible specification can itself contain
nested modularly extensible specifications,
and extensions to the outer specification can override the inner ones
by extending them with the usual OO extension mechanism (inheritance).
And the same outermost or global fixpoint will automatically compute
all the inner fixpoints of all the inner specifications, without any particular issue.
As mentioned before, that’s actually a case where it is very nice to have
conflation of specification and target, so you need
neither to think constantly about which you’re referring to,
nor to insert heavy syntactic markers all over the place.

Of course, dynamic OO languages, whether Prototype OO languages or Class OO languages with reflection,
have always been able to express these nested specifications and their overriding the hard way.
But credit where credit is due, BETA@~cite{Kristensen1987}
was the first language that explicitly supported the overriding of nested specifications@xnote[","]{
  Simula had nested classes, but not nested @emph{virtual} classes:
  you couldn’t override a nested class while overriding a class.
}
with its virtual patterns, and the author of the successor gBeta @~cite{Ernst2000}
explicitly explored the resulting notion of “family polymorphism”. @;TODO cite Ernst2001 Ernst2006
This proves that the “Scandinavian school” advanced OO in more ways
than by first implementing classes in Simula 67@~cite{Dahl1967};
although their variant of family polymorphism is limited by the often undesirable constraint
that overriding of patterns should be monotonic, i.e. the new pattern must inherit from the old one.

Other notable languages that explicitly support nested specifications,
and lift this monotonicity limitation, include
Newspeak@~cite{Bracha2008} with its nested classes (in a stateful Class OO language), and,
in a pure functional and prototypical setting,
Jsonnet @~cite{Cunningham2014} and Nix @~cite{Simons2015}.
Nix, which implements extensions equivalent to Jsonnet’s objects
in a few lines of λ-calculus as per @secref{MOO} and @secref{RPOO},
does not provide any special operator to support nested extensions,
and doesn’t need to: nesting extensions is already a common idiom in defining
Nix packages, configurations, and their overrides.

Scala@~cite{Odersky2005} is the only mainstream language that can express this semantics
with a proper static typing discipline, though in a slightly roundabout way,
using abstract type members and path-dependent nested types.
In C++, templates can express anything, and expose an explicit @c{Self} argument
with which to express family polymorphism using CRTP @~cite{Coplien1995};
but it can be a challenge to get the appropriate amount of
statically typed runtime substitutability between objects defined from such classes.

@subsubsection[#:tag "IoNaI"]{Interaction of Nesting and Inheritance}

When nesting specifications, only a subset of the specifications for the outer context
will contain nested specifications affecting a particular inner focus.
The ancestor hierarchy of specifications relevant for the inner focus is thus
a sub-DAG of the ancestor hierarchy of specifications for the outer context.

In the case of single inheritance or even mixin inheritance,
the semantics of instantiation is simple and obvious:
hierarchy DAGs are actually total orders, and the hierarchy at an inner focus
is a subset of that at an outer context,
from which the irrelevant nodes (whose modular extension doesn’t affect the inner focus)
can be simply removed, as they contribute nothing to the semantics.

But the case of multiple inheritance
(either flavorful as in gBeta @~cite{Ernst2000}, or flavorless) is not so simple:
nodes that are irrelevant with respect to their modular extension
are actually relevant with respect to introducing conflicts (in the flavorless case)
or ordering constraints (in the flavorful case).
Thus, you can’t filter out ancestors based on their direct modular extension not modifying the focus;
you can only filter out an ancestor if neither it nor any of its transitive ancestors
modifies the focus.

Now how does ancestry management work when writing specifications with multiple inheritance?
At the top-level, the parent DAG of a specification looks very much
like some kind of module imports:
you specify parent specifications you depend on, in some local order (list or DAG).
When a specification contains nested specifications, the outer order induces an inner order.
When an entire specification is nested as some inner focus, the semantics is the same as if
each inner parent had been lifted into an outer parent at the context level containing
the nested modular extension with appropriately lifted ancestors, recursively
(the lifting otherwise preserving identity).

Now, whichever form of inheritance you use,
the things being specified at times may themselves be specifications, prototypes, classes.
They may be further overridden in a covariant way (towards more specialized variants);
or they may be overridden in a contravariant way (towards less specialized variants);
or they may be overridden in invariant ways—yet still have a default value that may change;
or they may follow arbitrary kinds of method combinations (@secref{MC}).

Now, BETA (with single inheritance) and gBeta (with multiple inheritance)
only support the covariant pattern where subpatterns further-specialize.
But there are many cases where you don’t want this covariance,
and instead want to use the modular extensions within your specifications to
edit, compose, and otherwise specify prototypes and classes in non-monotonic ways.
And so Newspeak, Jsonnet or Nix (all of them with mixin inheritance) make it cheap
to use the covariant nested pattern (especially with @c{+:} in Jsonnet),
but allow arbitrary overrides in general—which
may involve specialization of prototypes.

@subsubsection[#:tag "NP"]{Nested Prototypes}
@epigraph{
  Yo dawg, I heard you like prototypes, @linebreak[]
  so we put prototypes in your prototypes @linebreak[]
  so you can prototype while you prototype
  @|#:- @elem{riffing on an
          @hyperlink["https://imgflip.com/i/azsjnz"]{Internet meme},
          @hyperlink["https://knowyourmeme.com/sensitive/memes/xzibit-yo-dawg"]{c. 2007}}|
  @; NB: knowyourmeme somehow redirect the original URL to add that "sensitive" in its path.
}
OO is a useful way to modularly and incrementally write software,
and not just at the toplevel of a project.
In general, many definitions nested inside your project will themselves benefit
from being defined using OO.
And the inheritance structure of those inner definitions follows its own logic,
largely independent from the toplevel inheritance structure of the project,
and not a trivial sub-structure thereof.

As I already mentioned (@secref{MoC}, @secref{SSAoCPS}, @secref{LSAoCMM}),
that’s where prototypes and their conflation really shine,
compared to managing specifications and their targets separately:
Define your nested objects, and you can always use them both for target queries
and specification extension, without having to add a finalization pass
that computes each target from its specification,
which would require omniscience about the exact fine-grained nesting structure of the entire program
including all imported modules and generated sub-structures.

The trivial case is that one of the outer “extensions”
introduces a nested object where no previous object existed,
the definition of which is perfect and never itself needs to be extended.
While each inner definition may involve multiple other definitions it inherits from,
each nested definition appears fully formed. Any further extension will happen
by defining a new binding that uses the previous one but does not override it.
This is great when possible, but the entire point of OO is to cover cases
that are not trivial that way (@secref{IoMaE}).

Only slightly less trivial is the case when the original definition is a default placeholder value,
and a further extension just replaces the default by some unrelated value that satisfies
the implicit or explicit type constraint on the binding,
but is somehow more appropriate to the application at hand
(e.g. for performance or compatibility reasons).
The original binding may be to some null value or @c{None} option,
or to some descriptor with methods corresponding to a solid choice
of a default service or algorithm for the given purpose
(in 2026, you might pick
builtin hash-tables for associative arrays,
builtin lists (singly linked?) or balanced trees for sequences,
sqlite for a local relational database,
BLAKE3 for a cryptographic hash function,
zstd for a compression algorithm,
some language-standard library for serialization,
JSON or JSONC for human-readable configuration and data,
etc.)
The “extension” would pick a more appropriate value in context,
as a constant that ignores the previous default.

Now, when the value to extend is a specification, prototype or class,
a good question is how to extend this value in a way that respects its inheritance structure.
The obvious answer is of course for the extended value to inherit from the previous value.
When using single inheritance, one or more modular extensions
can be mixed in at the most-specific end of its inheritance list, specializing the previous value.
When using mixin inheritance, one or more modular extensions can also be mixed in
at the other least-specific end of the inheritance list, providing tentative defaults
rather than definitive overrides to newly defined aspects used by other modules.
When using multiple or optimal inheritance, the previous value may be one of the parents
in the local precedence order of the extended value (list or DAG).
In all these cases, the previous value can be largely opaque and abstract to the inner extension,
which allows multiple inner extensions to be composed in whichever order respects
the dependencies declared through the inheritance structure of the outer extension.

When using mixin inheritance,
you would use @c{(rproto-mix inherited extension)} (@secref{CfR})
to extend those sub-fields you want to edit.
When using multiple or optimal inheritance, you would use
@c{(poi-mix extension inherited)} (mind the opposite order of arguments) defined as follows@xnote[":"]{
  Most implementations of multiple inheritance do not support arbitrary DAGs, only total orders,
  for the local precedence order. This corresponds to using @c{(list parents)}
  instead of @c{(map list parents)} when specifying the local precedence order,
  i.e. the parents @emph{must} be in the specified order or else it’s an error.
  The implementation I propose with @c{(map list parents)} creates a list of singletons,
  meaning the parents are independent, and the linearization will reorder them
  to avoid inconsistency (but will otherwise preserve the order whenever consistent).
}
@Code{
(def (poi-mix/list parents) (make-poi idModExt #f (map list parents)))
(define (poi-mix* . parents) (poi-mix/list parents))
(def (poi-mix extension inherited) (poi-mix* extension inherited))
}

You would use this combinator as part of modular extensions as follows:
@Code{
(def (poi-mix-spec contrib)
  (λ (inherited _self) (poi-mix contrib inherited)))
(def (poi-mix-field-spec key contrib)
  (field-spec key (poi-mix-spec contrib)))
}

There again, you would often use variants that support default cases,
where an absent record or field is automatically populated, or
a yet-undefined or missing modular extension silently ignored or replaced.

Now, you could use a slightly different combinator for inherited and extension prototypes,
one that combines the local precedence orders:
the new prototype, instead of inheriting from the previous ones,
will combine their modular extensions and local precedence orders.
This protocol can express richer sets of behaviors through extension than the above,
but requires that regular extension behavior should be contributed
through the modular extensions of ancestors rather than directly:
the direct modular extension of the prototype is applied last,
after those of the ancestors are mixed in—including those to be added in the future.
Thus, if the direct modular extension is anything other than @c{idModExt},
it should contain only finalizing behavior to be applied after all ancestor contributions,
including contributions from ancestors added by future extensions.
Such an extension protocol must thus be agreed upon in advance,
as it differs from the regular protocol:
the previous protocol with @c{poi-mix} preserves the two prototypes as distinct ancestors
with their own identities, whereas this alternative merges their specifications into one prototype.
Whichever protocol you use, though, you will want to move as much functionality as possible
into parents and their ancestors,
because this is how you ensure that specifications are not applied multiple times,
and are included in a suitable dependency order,
while also avoiding superfluous constraints
that could only cause needless linearization failures.
Also, if agreeing in advance to a non-trivial OO extension protocol is a thing,
a different and related approach is Method Combinations (@secref{MC}).

Finally, there are various non-modular extensions you can make to prototypes.
These extensions are sometimes useful, but require care to avoid clashes.
They are better done in a final wrapper (@secref{RC}), or
in a finalization step that may itself be part of a method combination.
Thus, for instance, you might want not just to compose with an existing modular extension,
but to wrap around it, e.g. to rename methods, or wrap method arguments and results, etc.
If you’re going that way, you might want a wrapper around each and every modular extension
in the ancestry so far, or maybe around the entire object—such global wrapping is more modular
(requires less information about which ancestor did what) but its result is still not modular
(you must make sure to run this wrapping last,
or else every additional extension not in the currently wrapped set must coordinate).
If possible, you are better off avoiding non-modular extensions and instead
defining intermediate objects you can extend independently of the wrapping.
Systematizing this pattern is what method combinations do.
Another non-modular extension, when using optimal inheritance,
is to turn the suffix flag on for performance, or to turn it off for semantic compatibility
when adding ancestors at the least-specific end of the ancestry for the purpose of infrastructure.

@subsection[#:tag "OfC"]{Optics for Classes}

I’ll assume a simple model for first-class classes as per @secref{SFCTD},
wherein classes are just prototypes for type descriptors.

@subsubsection{Optics for Class Instance Methods}

The point of an object is that it will (or at least may) be used more than once
(or else, you might as well directly do the one computation at stake
without constructing the object to begin with).
Thus, I will start by discussing how to use an object before how to create one,
since that is the most frequent operation.

Objects are records, mappings from field identifiers to values.
For the sake of dynamic dispatch (@secref{DD}),
one special field (in my Scheme examples I use the boolean true @c{#t} for its identifier)
holds a type descriptor from which methods can be dynamically extracted.
(If using static dispatch (@secref{SD}) then the type descriptor
is provided separately, and often inlined away as methods are statically resolved.)

Inasmuch as classes are prototypes, the way to deal with methods on a class
is essentially the same as for prototypes, or a refinement thereof.
To define more precise lenses,
I’ll further assume the encoding of @secref{SFCTD},
wherein you use @c{instance-call} to call an instance method,
which extracts the type descriptor using @c{type-of},
which is then invoked with @c{'instance-methods}, the @c{method-id}, and the element.

To modularly extend a class instance method, one first needs to focus on it.
The class descriptor holds instance methods under the field @c{instance-methods},
itself a record from @c{method-id} to method; so the lens onto a specific instance method
is just a nested defaulting field lens (@secref{FL}), and the modular extension that
installs one is the corresponding nested defaulting field spec, @c{field-spec~*}:
@Code{
(def (instance-method-lens method-id)
  (field-lens~* 'instance-methods method-id))
}

Now, when specifying a class instance method, the programmer thinks in terms of
the class instance, i.e. an element of the class’s target type.
But the element does not exist yet—it has to itself be a parameter to some function,
to be passed in the future, while the method has to be attached to the class somehow.

Moreover, since methods may use @c{next-method} to invoke the inherited behavior,
the instance on which the method is called need not be an instance of the class
on which a method is defined: it can be an instance of any subclass thereof
(unless the class is somehow “final”).
Thus the method body must not assume its argument’s type
(reminder: as seen in @secref{TfOO}, subclasses do not induce subtypes,
contrary to widespread belief).
However, if needed the body can dynamically extract (a descriptor for) that type
from the argument’s magic @c{#t} field,
using dynamic dispatch rather than static dispatch (@secref{DvSD}).

Yet again, for an @emph{efficient} @c{next-method},
the implementation must cheaply determine the inherited effective method.
In the definition below, this happens implicitly through the usual inheritance mechanism:
while composing method specifications, each contribution receives the method accumulated so far
as its @c{next-method}.
In general, each method must cheaply access a @c{super} parameter,
and further pass to the next method @emph{its} super, and so on,
such that the entire precedence list (or a method-relevant subset thereof)
must be implicitly or explicitly passed to the method body.
Now, when using single inheritance, you can cheaply compute the method’s super
from the current class only: it is the class’s parent.
But when using multiple or mixin inheritance, this does not apply, and the super must instead
be found in the precedence list, which from the current specification
can be an @c{O(n)} sequential search, or
a hash-table lookup which is @c{O(1)} but involves a large constant factor;
thus a more efficient implementation will somehow pass the rest of the precedence list.
Also note that for the sake of efficiency, the computation of an effective method,
though somewhat expensive, can be cached, and
need not be completed more than once per run of the program.

Here is the code in the simplified case of just modular extensions in mixin inheritance:
the underlying machinery sees @c{self}, i.e. the class,
and @c{super}, the modular definition for the method so far,
as inherited from the tail of the class precedence list,
i.e. ancestors behind the current class in the precedence list
of the type of the original instance used as first argument.
I need a wrapper to bridge these two views, and at this point,
a formal definition is simpler than the words that describe it:
@Code{
(def (instance-method-spec method-id method-body)
  ((field-spec~* 'instance-methods method-id)
    (λ (next-method _self element)
      (method-body next-method element))))
}
Note how the @c{_self} argument (holding the class target) is ignored,
because, using class style OO, the @c{method-body} and any @c{instance-call} it invokes
can extract the type (if and when needed) from the class instance @c{element} itself
(that you must pass as argument to the @c{next-method} when calling it;
also note that given autocurrying as used throughout this book,
this @c{element} parameter can be η-converted away in the above definition).
In typeclass style, the self argument must not be ignored, and
instead must be passed explicitly to the @c{method-body}.

As a simplification, I can also define a @c{base-instance-method-spec}
for methods that never invoke the @c{next-method}.
An example application would be a method @c{area} to be declared
on a class @c{Rectangle} of records with a @c{height} and a @c{width},
with the following definition:
@Code{
(def (base-instance-method-spec method-id method-body)
  (instance-method-spec method-id (K method-body)))

(base-instance-method-spec 'area
  (λ (r) (* (r 'width) (r 'height))))
}

A @c{BorderedRectangle} that inherits from @c{Rectangle} might add border thickness with:
@Code{
(base-instance-method-spec 'area
  (λ (element)
    (let ((border-adjustment (* 2 (element 'border-width))))
      (* (+ (element 'width) border-adjustment)
         (+ (element 'height) border-adjustment)))))
}
Meanwhile, a @c{ScaledShape} mixin might instead use @c{next-method}:
@Code{
(instance-method-spec 'area
  (λ (next-method element)
    (* (element 'scale-factor) (next-method element))))
}

Classes, like prototypes in general, can thus be defined incrementally,
by assembling or composing plenty of such focused specifications;
and using lenses and sensactors to programmatically select each time
how and where they fit in the bigger picture.
You can give types to method specifications independently
from any specific surrounding prototype or class specification.
You can reuse these specifications as part of multiple prototype specifications.
You can transform them, store and transmit them, compose them, decompose them, recompose them,
as first-class objects.
And you can build infrastructure that systematizes any design pattern you follow
in writing these specifications.

@TODO{FOR SECOND EDITION:
Examples:
- defining a method outside a class
- modular extensions used repeatedly to add methods to different classes,
  without being given an identity as a (super)class... just included
  as part of something else with an identity.
- wrapping a set of methods into... a renaming? And how that doesn’t quite commute with inheritance,
  but maybe if your renaming is a functor, and you have a natural transformation to class names,
  you can naturally have a transformed ancestry.
}

@subsubsection{Simple Class Initialization}

One may specify the fields of a class instance
by specifying individual field descriptors
under an @c{instance-field-lens} from the class descriptor:
@Code{
(def (instance-field-lens field-id)
  (field-lens~* 'instance-fields field-id))
}
Note that @c{field-lens~*} is the nested defaulting field lens from the exercise in @secref{FL}:
it focuses on @c{'instance-fields field-id}, a record collecting the per-field
@c{init} modular extension plus, as a further exercise, a @c{check} for validation,
and possibly other metadata (e.g. documentation).

A simple field descriptor would be a record of information about the field:
how to initialize it,
what (static or dynamic type) of value it should hold,
what checkable invariants it should respect,
whether it’s mutable or immutable (in a language with side-effects),
what documentation to show the user about it, etc.
For a minimal model, I will store only a modular extension to initialize it in a field @c{init}
that is a modular extension for the field value in the context of the class instance.
Because I will later need to iterate over fields,
but my records-as-functions lack any introspection on the keys they support,
I will also explicitly remember a list of the names of supported fields,
which also illustrates how a simple form of reflection is implemented by classes;
moreover that list will preserve the order in which fields are defined by superclasses.
This list is not strictly necessary to the semantics of initialization as such:
a class that needed no reflection could incrementally build its @c{base-instance} directly.
But there are many other use cases (such as I/O) for which it is beneficial to have
a stable order in which to enumerate field names.
@Code{
(def (instance-field-spec field-id init-spec)
  (mix*
    (field-spec~ 'instance-field-names
      (λ (inh _self) (field-name-insert field-id (or inh '()))))
    ((field-spec~* 'instance-fields field-id 'init)
      (λ (inh _self) (mix-maybe inh init-spec)))))
(def (field-name-insert x lst)
  (cond ((null? lst) (list x))
        ((eq? x (car lst)) lst)
        (else (cons (car lst) (field-name-insert x (cdr lst))))))
(def (mix-maybe a b) (if a (if b (mix a b) a) b))
}

To initialize a field @c{parts} that is a list of parts,
defaulting to an empty list, I would use something like:
@Code{
(instance-field-spec 'parts (constant-spec '()))
}
To initialize a field @c{price} that defaults as a baseline
to the sum of the prices of the parts times the contents of the field @c{markup},
I would use:
@Code{
(instance-field-spec 'price (λ (_inherited self)
  (* (self 'markup)
     (foldl (λ (part acc) (+ acc (part 'price))) 0 (self 'parts)))))
}
To define a field @c{markup} that has no default initializer and must be provided by users,
I would use:
@Code{
(instance-field-spec 'markup #f)
}
A class could then define a default prototype for new instances as a derived field
@c{base-instance}, contributed by a root class @c{base-class} that every class inherits:
@Code{
(def base-class
  (make-poi
    (field-spec 'base-instance
      (λ (_inh self)
        (make-poi
          (mix (constant-field-spec #t self)
            (mix/list
              (filter identity
                (map (λ (id)
                       (let ((i (self 'instance-fields id 'init)))
                         (and i (field-spec id i))))
                     (or (self 'instance-field-names) '())))))
          #f '())))
    #f '()))
}
@c{base-instance} is a parentless POI: its magic @c{#t} key maps to the class POI itself,
so @c{type-of} and dynamic method dispatch can resolve through it;
and each field with a non-@c{#f} @c{init} becomes a @c{(field-spec id <chained init>)}
read straight from the class’s @c{instance-fields} table.
Fields whose @c{init} is @c{#f} are skipped by the @c{base-instance}—the programmer
must supply a value when constructing an instance,
or as part of a further subclass being instantiated;
otherwise an error may happen when trying to use the uninitialized value.
To instantiate a new element of (the target type of) a @c{class},
define a poi that provides values for at least all these otherwise uninitialized fields,
possibly overrides even the initialized ones,
and inherits from the @c{(class 'base-instance)}.

To define a class, just define a prototype for a type descriptor using
@c{instance-field-spec} and @c{instance-method-spec}
(or the equivalent, or elaborations on them),
and inherit from @c{base-class} so your @c{base-instance} is defined.
Note that if a prototype already inherits from another existing class defined that way,
it would already inherit @c{base-class} transitively through it.
And more functionality could be added to @c{base-class} or extensions thereof,
besides this @c{base-instance}:
such as static typing information, dynamic validation passes,
builtin support for display, serialization or debugging, etc.
Various functions, macros or DSLs (domain-specific languages)
could help further streamline the process.

For example, a class @c{Rectangle-class}
with fields width and height, and a method area:
@Code{
(def Rectangle-class
  (make-poi
    (mix*
      (instance-field-spec 'width #f)
      (instance-field-spec 'height #f)
      (base-instance-method-spec 'area
        (λ (element) (* (element 'width) (element 'height)))))
    #f (list (list base-class))))

(def (make-rectangle width height)
  (make-poi
    (mix (constant-field-spec 'width width)
         (constant-field-spec 'height height))
    #f (list (list (Rectangle-class 'base-instance)))))

(def my-rectangle (make-rectangle 10 20))

(instance-call my-rectangle 'area) ;=> 200
}

Extending the class is just adding more fields or methods to the mix.
Here, I define a field @c{color} with default value @c{"black"}:
@Code{
(def ColoredRectangle-class
  (make-poi
    (instance-field-spec 'color (constant-spec "black"))
    #f (list (list Rectangle-class))))

(def my-colored-rectangle
  (make-poi
    (mix/list (map constant-field-spec
                   '(width height) '(3 5)))
    #f (list (list (ColoredRectangle-class 'base-instance)))))

(my-colored-rectangle 'color) ;=> "black"
(instance-call my-colored-rectangle 'area) ;=> 15
}

The entire class definition reduces to composing focused modular extensions—the same pattern
for method and field definitions.

@section[#:tag "MC"]{Method Combinations}

@subsection{Win-Win}

Another fantastic contribution from Flavors @~cite{Cannon1979} is Method Combinations:
the idea that the many methods declared in partial specifications
are each to contribute partial information that will be harmoniously combined (mixed in),
rather than complete information that has to compete with other conflicting methods
that contradict it, the winners erasing the losers.
@principle{Win-win interactions rather than lose-lose}: that was a revolution
that made multiple inheritance sensible when it otherwise wasn’t.

Flavors notably allowed regular or “primary” methods to be extended in subclasses with
“before” and “after” methods, respectively called before and after the primary method@xnote[","]{
  See @secref{Inner} for a comparison with the original concatenation semantics of Simula and BETA.
}
that could set up and tear down resources, do logging or permission checking or resource accounting,
hold and release locks, etc.
Because these extension points are standard, both subclasses and clients can enjoy them
without the author of the original method of the original class having to foresee,
implement and advertise such an extension protocol, making the design modular.
But that was just the default “method combination”.

Flavors also allowed you to define methods using a “simple method combination”
that used an operator like @c{progn} (sequential execution), @c{and} or @c{or}
(logical conjunction or disjunction, with short-circuit evaluation)
to combine the results of the methods, evaluated either in
most-specific-first or most-specific-last order, as specified by the programmer.
Typical other operators included @c{+ * max min list append nconc}@xnote["—"]{
  @c{nconc} is a historical variant of @c{append} that uses side-effects
  to modify in place each non-empty list but the last, to link to the next one.
  It made sense in the slow and memory-constrained machines of the 1960s to 1980s,
  especially before modern garbage collection.
  But @c{nconc} rarely makes sense in modern times,
  where either the simpler and safer @c{append} is good enough,
  or optimization is better sought from a more sophisticated data representation than linked lists.
  @c{nconc} is seldom more than the opportunity for bugs due to side-effects
  in unexpectedly shared data structures, which sharing, if not present yet,
  might happen after some later refactoring.
}
but you could use any operation that makes sense for your application,
especially if monoidal (associative and with a neutral element).

@principle{The simplest case of method combination is actually
the usual composition of modular extensions},
wherein each extension can refer to its @c{super} argument
along a multiple inheritance specification’s precedence list,
as discussed in @secref{MI}.
But I can do better: I can show how to implement all other method combinations
on top of this foundation@xnote["."]{
  Ironically, that’s the one kind of method combination @emph{not} present as a builtin
  in the original Flavors, while the more elaborate kinds were already provided:
  in the default “daemon” method combination,
  only one primary method (from the most specific class) would be called,
  but @c{before} and @c{after} methods were also supported
  in the style of ADVISE @~cite{Teitelman1966}.
  Although @c{around} methods as such were only added in CLOS @~cite{Bobrow1988},
  in Flavors you could define the equivalent of a single @c{around} method
  by providing a @c{wrapper} macro, or, later, a @c{whopper} function.
  The simple method combinations were supported, again without @c{around} methods,
  and the simple @c{or} method combination covered a pretty common case of next-method-as-fallback.
  You could also provide your own method combination
  that computed the effective method from the ordered list of individual methods.
  Chaining methods through a @c{call-next-method} first argument would definitely
  have been possible.
  Still, such a protocol was not directly provided in Flavors or its successors until
  it appeared in CommonLoops @~cite{Bobrow1986},
  and no one seems to have implemented it on top of Flavors. “Could have” is not “did”.
}

Now, while the original method combinations of Flavors were quite capable,
method combinations were further refined by
New Flavors @~cite{Moon1986},
CommonLoops @~cite{Bobrow1986}, and
most notably by CLOS @~cite{DeMichiel1987 Bobrow1988 Steele1990 Kiczales1991 Pitman1996 Verna2023}.
My presentation will therefore be more directly inspired by CLOS than by Flavors.

@subsection{Uses of Method Combinations}

It would take a lot of space to reproduce and explain @italic{in extenso}
some real-world examples that motivate the use of method combinations:
they would probably have to be in Common Lisp
(the only top-50 popular language that fully supports them),
with a quick introduction to the language, its I/O facilities,
and some existing development framework for network client/server or GUI programming.
A full introduction would then require programs large enough
that the advantage of OO in general and method combinations in particular justifies
use of the feature instead of “just” inlining it away.

Still, I can point to ASDF @~cite{Rideau2010 Rideau2014}, the Common Lisp build system,
as a relatively short program that makes good use of method combinations,
the source code of which is freely available and well-documented.
I know ASDF inside and out because I was once its maintainer and completely rewrote it several times.
CLIM, the Common Lisp Interface Manager,
a large GUI library descended from that of the Lisp Machines,
is also heavily object-oriented, with plenty of uses of method combinations;
but I am not familiar with it.
Other examples abound in Quicklisp.

ASDF defines some general abstract classes that correspond to its build specification protocol,
the main ones being @c{operation} and @c{component}.
These classes are gradually specialized into subclasses and mixins
(such as @c{downward-operation} and @c{source-file})
until concrete classes are defined (such as @c{load-op} and @c{cl-source-file}).
To each class (or pair of operation and component classes, see @secref{MD} below)
is associated one or two methods, sometimes more, that extend the inherited behavior.
In the end, each method call may combine the effects of half a dozen individual inherited methods,
depending on the classes of the arguments.

Thus, ASDF defines @c{:before} methods to validate invariants and issue errors
before the primary methods are called,
to automatically set up some preconditions (like creating destination directories for output files),
or to detect and record dependencies between actions.
ASDF defines @c{:after} methods to track down actions that were successfully completed
so they do not have to be taken again, or to complete system-provided behavior
and check invariants after (re)initialization of some objects.
And ASDF defines @c{:around} methods to fix up the results of special cases,
set up or use caches around computations, adjust dynamic bindings around computations,
or detect circular dependencies between actions.
Other common uses of the standard method combination not illustrated by ASDF include
logging (before, after or around) and permission checks
(usually before to check arguments, sometimes around to also check results or effect logs).

Finally, for historical reasons, the ASDF methods
@c{component-depends-on}, @c{input-files} and @c{output-files}
use the standard method combination yet follow a convention wherein
every method manually appends the contents of @c{call-next-method} to their results.
They would have been better written with the @c{append} method-combination,
which would have automated what is manually done through tedious convention.
But that change would break backward compatibility;
thus it is not justified unless and until this or some other such breakage
is the necessary cost to achieve major benefits
during another rewrite of ASDF.

These methods allow ASDF to be written in a very modular and extensible style, and
to achieve in a few thousand lines of code (and a few more for Quicklisp)
what takes many times more code in other languages@xnote[","]{
  ASDF 3.3.7.4 has 6321 lines of heavily documented and commented code for the build logic,
  plus 7820 lines of similar code for a general-purpose portability layer
  to 17 different implementations.
  ASDF manages build coherence across all source trees of all dependencies,
  across multiple phases of extension of the build system from the build system @~cite{Rideau2014}.
  As an exercise, compare the size of ASDF to that of
  roughly equivalent build systems and toolchains in other languages.
}
all the while exposing an extension interface actually used by many extensions:
support for compiling and linking C, Fortran or Python code,
for automatic dependency detection, for character encodings besides UTF-8,
for deferred code in Lisp files, for file-local variables,
for conditional autoloading of systems, for parallel compilation, etc.

One of the authors of CLOS and its modern method combinations went on to invent
Aspect Oriented Programming (AOP) @~cite{Kiczales1997 Kiczales2001},
which applies the ideas of Teitelman’s advice and Cannon’s method combinations to more languages;
AOP briefly achieved modest popularity on Java and C#. @; TODO cite
Sadly, method combinations have seen very little adoption beyond languages in the Lisp family.

Certainly, for each use of method combinations,
equivalent effects could be achieved manually without method combinations,
by having some master method calling one method for each of the sub-methods,
orchestrated according to a known design pattern for each method combination.
But doing things by hand is less modular precisely because
it constitutes a fourth-class design pattern,
whereas builtin method combinations are second-class, and user-defined ones first-class.
A design pattern means programmers have to insert a lot of boilerplate, correctly, and maintain it.
The boilerplate is costly, and must be inserted by the original author of the protocol,
before it is extended, even though that author doesn’t need it.
Those who would later like to extend the code are interested in the boilerplate,
but if they are not part of the same development team,
they may have to fork the original code to insert that boilerplate.
All this friction is eliminated when all methods
use the standard method combination by default.
And when using a different method combination,
other kinds of boilerplate are also avoided as convention is replaced by automation.

@subsection{Effective Methods and Method Qualifiers}

With method combinations, a target method is called the @emph{effective method}.
It is computed based on individual @emph{method specifications}
declared by each specification along the way.

In CLOS after Flavors, a method specification can be tagged with
one or more @emph{method qualifiers}, usually some kind of symbol,
though, in many Lisp or Scheme dialects,
a “keyword” may be used that is somehow
distinct from regular symbols, often with a syntax involving a colon@xnote["."]{
  Depending on the language or dialect, keywords are typically written with some variant of
  a colon @c{:before} (in Common Lisp, where they are a self-evaluating subset of symbols),
  or @c{after:} (in Gerbil Scheme, where they are self-evaluating separate from symbols), or
  hash-colon @c{#:before} (in Racket, where they are second-class syntax unless quoted,
  and then separate from symbols).
  Many dialects only have symbols, though many programs may still have a @emph{convention}
  of using symbols starting or ending with a colon as keyword specifiers in some protocols.
  Compare also to @c{~labels} or polymorphic @c{`Variants} in OCaml,
  or to a choice of named arguments in many languages.
}
In this book, I will use regular symbols like @c{before} for method qualifiers,
which I will quote to prevent evaluation.
To simplify, I will allow only one qualifier per sub-method.

A regular method specification that is not explicitly qualified by the user
is implicitly qualified as a @emph{primary} method specification by the system.
I will use the symbol @c{primary} for that in my implementation@xnote["."]{
  In Common Lisp, primary method specifications are left unqualified:
  their list of qualifiers is @c{NIL},
  a magic Lisp value that is the empty list, but also
  a magic self-evaluating constant symbol, a boolean, and
  a general-purpose null value, default value and unit value.
  However, while CLOS accepts an ordered @emph{list} of qualifiers for its methods,
  the builtin method combinations of CLOS expect at most one qualifier per method;
  meanwhile, a long-form definition of a method combination in CLOS
  will help you sort methods into supported groups based on matching
  this qualifier list against group patterns or predicates.
  I suppose that the list-of-qualifiers feature exists because someone, at some point,
  used it, or at least envisioned a use for it.
  But that functionality seems little used in practice.
  For the sake of simplicity, I will stick to a single qualifier per method.
  I could have used the less-overloaded but still commonly used
  boolean false value @c{#f} as the single qualifier I assign to primary methods,
  instead of the symbol @c{primary}.
  Each adaptation of CLOS to a different language will choose its own representation.
}
In addition to @c{primary} methods,
the @emph{standard method combination}, used by default (most-specific first),
supports @c{before} methods that will be executed before the primary methods (most-specific first),
@c{after} methods that will be executed after them (most-specific last), and
@c{around} methods that will wrap around the execution of all the above methods (most-specific first).

Simple method combinations accept methods with a method qualifier with the same symbol
as the simple method combination (e.g. @c{+} for adding the results of the methods),
called most-specific first, returning a result as if their bodies were combined
by the function or macro naming the method combination (for the builtin ones;
or whatever the user specifies, for user-specified ones);
simple method combinations also accept @c{around} methods.

CLOS documentation @~cite{Pitman1996} calls “method groups”
groups of methods organized by filtering methods based on their list of qualifiers.
I prefer to call @emph{sub-method} the set of methods with a given qualifier
(of which there is exactly one per method in my design),
and to call @emph{effective sub-method} the notional function that processes
all those methods with that given qualifier.
Thus, the @c{primary} sub-method, the @c{before} sub-method, etc.
Indeed, when manually expressing method combinations as a design pattern,
each sub-method would be expanded into a separate method.

@subsection{Representing Sub-Methods}

The best way to store sub-methods would be if there were “funcallable instances”
(to use CLOS terminology; like instances in T, that are funcallable in general)
that conflate a function and a record in a single entity.
Then the sub-methods would be stored in the “record” part of the method,
and the method would still be its “function” part@xnote["."]{
  Equivalently, the “function” part would be just one special field of the record,
  automatically invoked when the record is used as a function.
}
However, since I have so far implemented records as functions,
then funcallable instances wouldn’t work—the function interface
is already used for field access.
I could exclude symbols (and booleans), that I used as record keys,
from the domain of the “function” as such—but that’s an ugly limitation.
And I crucially relied on records-as-functions
for the semantics of objects as fixpoints (@secref{MFCM}).
Therefore a change of strategy while possible (@secref{RaR}) would complexify
the one aspect that most simplifies my account of the semantics of OO.

Thus instead of storing sub-method information in the same entity
as the function being defined, I will store it in a record next to it.
The upside is I can keep using my simple pure functional fixpoint semantics;
the downside is that information about a method now resides in a pair of bindings
that must be transported together, rather than in a single entity
that can be syntactically bound once.
In practice, I will store sub-method information in a record @c{sub-methods}
next to the methods being combined.

To specify a sub-method, you need:
@itemize[
  @item{A function @c{method-cons} and a base value @c{methods-nil}
        to combine your sub-methods together for the sake of your method combination;
        typically just (a curried variant of) @c{cons} and @c{'()} to build a list,
        for @c{standard-compute-effective-method}.}
  @item{A @c{qualifier} recognized by your method combination;
        typically @c{'primary}, @c{'before}, @c{'after} or @c{'around},
        for @c{standard-compute-effective-method}.}
  @item{The name @c{method-id} of the method to which you attach your sub-method.}
  @item{A function @c{method-fn} that takes as arguments a @c{call-next-method} function
        and the (first) argument to the generic function (assuming single-dispatch for now),
        and returns the method result.}]
@Code{
(def (sub-method-spec method-cons methods-nil
                      qualifier method-id method-fn)
  ((field-spec~* 'sub-methods method-id qualifier)
    (λ (qualified-methods _self)
      (method-cons method-fn (or qualified-methods methods-nil)))))

(def standard-sub-method-spec
  (sub-method-spec (λ (x y) (cons x y)) '()))
}
You may notice the following about the initialization protocol:
@itemize[
  @item{
    I am using the defaulting variant @c{field-spec~*} that will replace the default value
    @c{#f} by a record at each level that a record is expected.
    The first time a sub-method of the given qualifier is declared,
    @c{methods-nil} will be assumed as the previous initial value,
    which is the empty list @c{'()} for standard sub-methods.}
  @item{
    These defaults allow me to simplify the protocol so users do not have to explicitly
    ensure that sub-methods will be initialized with a suitable set of values
    as a dependency before any sub-method is defined.}
  @item{
    A more “static” answer would instead require the object to inherit from a “protocol” specification
    that initializes sub-methods for the methods that are part of the protocol;
    and each such “protocol” specification would itself inherit from a “protocol support” specification
    that initializes the sub-methods record, that in turn inherits from a “record” specification
    that initializes the record being specified.
    Such explicit chain of dependencies would properly ensure that well-typed initial values
    are available before any method declaration.
    In a dynamic language, this would be a lot of verbose drudgery;
    in a static language, meta-level automation could eliminate the verbosity.
    There is some didactic value to the verbose drudgery, and I invite you to try it, as an exercise;
    but for this chapter, I will avoid it, which will save on paper and
    remove potential distractions from my further definitions.}
  @item{
    A yet more sophisticated answer, whether the language is static or dynamic,
    would be for programmable meta-objects to handle the above automation in a library
    rather than as part of the core language itself.
    Those meta-objects must be instantiated (and thus specified)
    before the base object even starts being specified,
    which introduces interesting staging and bootstrap issues.
    See @secref{MOP} for a discussion of meta-objects.}]

Also note that I’m using simple modular extensions here
for sub-methods, as for methods and all nested definitions.
Indeed, the general pattern I’m following is that the unit of inheritance management,
of modular sharing of code, with respect to multiple (or optimal) inheritance,
is the class or prototype.
One could conceivably use prototypes at every level where I’m using records,
but in my case this would bring much complexity for little expressiveness advantage.
Interestingly, some languages (gBeta, Jsonnet) pay the price of this complexity,
hiding it behind a cheap field override syntax.
This approach makes a lot of sense if you design a language around its object system;
but not if you add an object system to an existing language, as I’m doing here.

@subsection{Standard Method Combination}

I can now implement the standard method combination as follows:
running the @c{around} methods, and wrapped inside them the @c{before} methods,
then the @c{primary} methods, and finally the @c{after} methods in reverse order.
The @c{call-chain} function chains methods through an inherited @c{call-next-method}
argument that, when called with any number of arguments,
calls the next method with the @c{self}, the original argument to the generic function.
The @c{progn-methods-most-specific-} (@c{-first} and @c{-last}) functions
chain the execution of methods for @c{before} and @c{after} methods respectively.
The @c{standard-no-applicable-method} is a good default when no method is defined.
And @c{standard-compute-effective-method} finally
computes the effective method from the sub-methods.
The @c{abort} is a poor man’s error mechanism in case the @c{before} or @c{after}
methods try to invoke their @c{super} argument as a @c{call-next-method}@xnote["."]{
  Note that in CLOS itself, a @c{no-applicable-method} generic function is called
  when there is no applicable method at all, which by default will raise an error;
  but the builtin method combinations will also raise an error
  if there are no primary methods, without trying to run secondary methods,
  when my implementation instead evaluates a handler that could raise an error,
  but my simple method combination returns a neutral element.
  CLOS invokes the generic function @c{no-next-method} (that by default raises an error)
  when you try to @c{call-next-method} without a next method,
  when I simply abort (Scheme lacks a condition system like Common Lisp).
  Modifying my code to account for all the subtleties of CLOS is left
  as an exercise to the reader.
}

To keep this presentation minimal, a method here takes just two things:
a @c{call-next-method} function and @c{self}, the object the generic function was
invoked on—@c{CallNextMethod → Self → Result}.
Every combination below is single-dispatch, and a method that needs more than the
receiver reads the object’s other fields straight from @c{self}.
@c{make-call-next-method} builds the @c{call-next-method} argument from the
remaining chain @c{next} and the current @c{self}:
called with however many (uncurried) arguments,
it ignores these arguments and just calls the next method with the receiver as argument.
A sub-method list that was never populated is @c{#f} rather than @c{'()}—so
@c{call-chain} and the @c{progn-methods-} helpers coerce it with @c{(or methods '())},
and @c{standard-compute-effective-method} reads each qualifier through a small @c{sub}
helper that also turns a missing @emph{record} into @c{#f}.
With no primary method the primary chain is just @c{no-applicable-method}.

@Code{
(def (make-call-next-method next self)
  (λ _ (next self)))

(def (call-chain methods on-exhausted)
  (foldr
    (lambda (m next)
      (λ (self)
        ((m (make-call-next-method next self)) self)))
    on-exhausted
    (or methods '())))

(def (progn-methods-most-specific-first methods self)
  (foldl (lambda (m _) ((m abort) self)) #f (or methods '())))

(def (progn-methods-most-specific-last methods self)
  (foldr (lambda (m _) ((m abort) self)) #f (or methods '())))

(define (standard-no-applicable-method method-id . args)
  (error "no applicable method" method-id args))

(define no-applicable-method standard-no-applicable-method)

(def (self-sub-methods self method-id)
  (let ((sm (self 'sub-methods)))
    (and sm (sm method-id))))

(def (standard-compute-effective-method method-id sub-methods)
  (let ((sub (lambda (qualifier) (and sub-methods (sub-methods qualifier)))))
    (call-chain (sub 'around)
      (λ (self)
        (progn-methods-most-specific-first (sub 'before) self)
        (let ((result
                ((call-chain (sub 'primary)
                   (λ (self) (no-applicable-method method-id self)))
                 self)))
          (progn-methods-most-specific-last (sub 'after) self)
          result)))))
}
You then initialize a method to use the standard method combination with this
function; @c{(obj 'method-id)} runs the effective method on the receiver
(@c{self-sub-methods} returns @c{#f} when the method has no sub-methods yet):
@Code{
(def (standard-method-init-spec method-id)
  (field-spec method-id
    (λ (_inherited self)
      ((standard-compute-effective-method
         method-id (self-sub-methods self method-id))
       self))))
}
And you can define sub-methods by using these specifications
with arguments @c{method-id method-fn}:
@Code{
(def primary-method-spec (standard-sub-method-spec 'primary))
(def before-method-spec (standard-sub-method-spec 'before))
(def after-method-spec (standard-sub-method-spec 'after))
(def around-method-spec (standard-sub-method-spec 'around))
}

@subsection{Simple Method Combination}

CLOS allows users to define and use simple method combinations
that combine the results of methods as if they had been passed to some operator.
Predefined such simple method combinations use the operators
@c{progn} (sequential execution of side-effects),
@c{and} (boolean short-circuiting logical and),
@c{or} (boolean short-circuiting logical or),
or the commutative functions
@c{+}, @c{*}, @c{min}, @c{max},
or the @c{list} or @c{append} functions.

Users can also define their own simple method combinations as follows,
where @c{name} is the name of the sub-method
(and also conventionally that of the method combination),
@c{stop?} a predicate on a result value saying whether to short circuit evaluation,
@c{op0} a notional thunk to evaluate if there are no methods
(actually takes one dummy argument for curried style),
@c{op1} a unary operator to run on the first method result to make it into a return value,
@c{op2} the (curried) binary operator with which to combine
the result from the next method and the return value from previous computations
into a new return value,
@c{finish} a unary operator applied to the final accumulator
(the identity for many associative combinations; @c{reverse} for @c{list};
some arbitrary function on the list of inputs as accumulated by cons in the general case),
and @c{sub-methods} a record of the sub-methods.
The methods run @emph{most-specific-first}—which is what every one of Common
Lisp’s builtin short-form combinations does: the effective method is
@c{(operator (most-specific …) … (least-specific …))} and @c{operator} evaluates
its arguments left to right.  So @c{op1}/@c{op2} fold left in that order
(mind the order of their arguments);
@c{list} accumulates with @c{cons} and hands @c{reverse} to @c{finish} so the
result is back in most-specific-first order without an @c{O(n²)} append.
Common Lisp’s short form also lets a combination be used @emph{most-specific-last};
the minimal version below lacks this capability, which is left as an exercise for the reader.
As with the standard combination, each qualified method has the same contract
@c{CallNextMethod → Self → Result}; @c{run} invokes each with @c{abort} as
@c{call-next-method} and folds its @c{Result} with @c{op1} and @c{op2}
(a simple primary method must not call the next; an @c{around} one calls it to
reach the folded inner value).

@Code{
(def (simple-compute-effective-method
       name stop? op0 op1 op2 finish sub-methods)
  (let* ((sub     (lambda (qualifier) (and sub-methods (sub-methods qualifier))))
         (arounds (sub 'around))          ;; #f (⇒ empty) if never initialized
         (methods (sub name)))            ;; most-specific-first, as in CL
   (call-chain arounds
    (λ (self)
      (letrec ((run (λ (m) ((m abort) self)))
               (f   (lambda (acc lst)
                      (if (and (not (stop? acc)) (pair? lst))
                        (let ((v (op2 (run (car lst)) acc)))
                          (if (stop? v) v (f v (cdr lst))))
                        acc))))
        (finish
          (if (pair? methods)
            (f (op1 (run (car methods))) (cdr methods))
            (op0 #f))))))))

(def compute-effective-method/progn
  (simple-compute-effective-method
    'progn (λ (_) #f) (λ (_) #f) (λ (x) x) (λ (r _) r) identity))

(def compute-effective-method/and
  (simple-compute-effective-method
    'and not (λ (_) #t) (λ (x) x) (λ (r _) r) identity))

(def compute-effective-method/+
  (simple-compute-effective-method
    '+ (λ (_) #f) (λ (_) 0) (λ (x) x) (λ (x y) (+ x y)) identity))

(def compute-effective-method/*
  (simple-compute-effective-method
    '* (λ (_) #f) (λ (_) 1) (λ (x) x) (λ (x y) (* x y)) identity))

(def compute-effective-method/list
  (simple-compute-effective-method
    'list (λ (_) #f) (λ (_) '()) (λ (x) (list x)) (λ (x y) (cons x y)) reverse))
}
You then initialize a method that uses a non-standard method combination
with a specification like this one:
@Code{
(def (list-method-init-spec method-id)
  (field-spec method-id
    (λ (_inherited self)
      ((compute-effective-method/list
         (self-sub-methods self method-id))
       self))))
}
After which you can define sub-methods by using specifications like this one,
and/or the @c{around-method-spec} above, with arguments @c{method-id method-fn}:
@Code{
(def list-method-spec (standard-sub-method-spec 'list))
}

@subsection{User-defined Method Combinations}

And users are not limited to predefined method combinations.
In the most general case, users may define their own method combinations,
that support methods declared with whichever qualifiers they want to support,
ordered whichever way they prefer.
Thus for instance, they may define and use method combinations for the following purposes:

@itemize[
  @item{They may reimplement the inheritance semantics of Simula, BETA, Self, C++,
        or of whichever their favorite programming language is.}
  @item{They may define additional layers of @c{around}, @c{before} and @c{after} methods,
        with the same order as usual, or with the opposite order@xnote["."]{
           Interestingly, at some point, ASDF 1 defined a method combination just
           for the purpose of another layer of around methods.
           But ASDF 2, striving for portability even on some less-compliant implementations
           that didn’t support such user-defined method combinations, removed it.
           ASDF 2 instead achieved the same effect manually for the only method
           that used that extra around combination, by introducing
           a new wrapping method that calls the regular one.
           Even without the portability issue, a method combination usually only makes sense
           when it is actively used by more than one method.
        }}
  @item{They may write pure functional monadic variants of the standard method combination,
        lazy or eager, with a constant monad or one given as an argument or a dynamic variable, etc.}
  @item{They may develop interactions wherein qualified method specifications correspond to actions
        by two (or more) participants.}
  @item{They may define method variants that apply at different phases, with different capabilities:
        compile-time vs runtime vs some more refined discipline that includes
        typechecking time, static resource allocation time, error handling time,
        access control checking time, etc.}]

To that end, they would develop some kind of @c{foo-method-init-spec}
to suitably initialize the method and its sub-methods,
based on a @c{compute-effective-foo-method} function
and possibly an initial record of sub-method qualifiers.
They would also define @c{foo-method-spec} or such for each sub-method,
either using @c{standard-sub-method-spec}, or
@c{sub-method-spec} with an appropriate specialized @c{method-cons} argument
to pre-compose the sub-method specifications in advance of @c{compute-effective-foo-method}.

Suitably qualified method specifications can then be used in a structured way to enact any kind of
environment setup or cleanup, argument validation, argument normalization,
resource allocation and deallocation, locking, logging, error handling, access control,
detail accumulation, etc.,
that the primary methods can safely rely on.
Method combinations promote modularity thanks to better factoring of code,
that allows for the extensibility of code along more independent axes.

@subsection[#:tag "Inner"]{Simula’s @c{inner}: Superclass-Controlled Extension}

@subsubsection{Concatenation Semantics}

The very first OO language, Simula @~cite{Dahl1967}, had a peculiar way of combining
the body of classes, which it called “concatenation semantics”.
Its direct successor BETA @~cite{Kristensen1987} unified classes and virtual procedures
in a common notion of “patterns”, with the same semantics to extend their bodies.
In either language, the body of a subclass or subpattern is evaluated
wherever the parent’s body includes the @c{inner} keyword
(which is a no-op when instantiating said parent rather than a descendant thereof).
Additionally, in Simula, an @c{inner} is added at the end of the class’s body if otherwise missing.
By contrast in BETA, a missing inner just means that the body won’t be extended further:
you can still extend the pattern, but whatever body your extension offers will never be evaluated.
A class or pattern has more than a body, though: new fields can be defined, existing
virtual procedures (Simula) or pattern fields (BETA) can be overridden,
and in the case of BETA (but not Simula), these fields can themselves
have bodies and fields of their own, etc. (@secref{NS})

The Simula terminology “class prefix” for superclass betrays the origin of concatenation semantics:
classes literally started as prefixes evaluated in the context of the new class,
defining data fields and procedures and body elements for it,
before the data fields and procedures and body elements of the subclass were in turn processed.
Then, the authors of Simula added a “suffix” part to the body elements,
to allow for cleanups and post-processing.
The semantic elements of the subclass were thus sandwiched between prefix and suffix,
becoming an “inner” part, hence the @c{inner} keyword.
In Simula, @c{inner} had to be a top-level statement.
In BETA, not only can it be present in a control structure,
it can be nested inside another object, allowing for higher-order control structures;
an optional label following the @c{inner} keyword then specifies
which object up the lexical chain the @c{inner} statement refers to.
What started as a naive simplest possible implementation of Hoare’s idea @~cite{Hoare1965}
was generalized to something much more sophisticated...
and yet still quite low-level.

@subsubsection{Inheritance from Before Structured Programming}

When using concatenation semantics, any information sharing or transfer
between concatenated code fragments along the ancestry
requires side-effects to some shared instance variable.
This includes any effect to and from the result value to be returned:
it is up to the users to roll their own protocol for that,
each pattern declaring variables to be used for its results (or other purposes),
and each subpattern having to carefully follow the protocol,
reading or writing these variables@xnote["."]{
  Simula, after ALGOL, automatically names a variable after the current procedure
  for the value to return, that you are supposed to write to, and possibly read.
  However, this only applies to procedures, that in Simula can be overridden,
  but without possibility of combination with old methods
  (and thus in this case, the most specific method wins, completely,
  which is at odds with how concatenation semantics works on Simula classes and BETA patterns).
  In Simula, unlike later BETA, concatenation semantics only applies to class bodies,
  that do not return anything.
  In BETA, where concatenation semantics applies to sub-patterns,
  that conceptually can be virtual procedures or nested virtual classes,
  you have to name your result variable or variables,
  and then explicitly return them in an @c{exit} block.
}

This approach sits well in the context of 1960s spaghetti code with GOTOs,
before @citet{Dijkstra1968} made everyone consider them harmful.
Indeed the original intuition of “concatenation semantics” perfectly fits
as the literal concatenation of the code generated to implement a class,
whether in assembly language, or at some similar low level of abstraction
within an intermediate representation by the compiler.
Unsurprisingly, the one known almost-OO predecessor to Simula, ADVISE @~cite{Teitelman1966},
uses the very same mechanism of communication through variable assignment:
@c{after} advice would assign or read the special variable @c{VALUE}
to affect the function return value or be affected by it
(variable soon enough renamed to @c{!VALUE} to avoid unintentional capture).

But this kind of design already felt quite low-level by the late 1970s.
Being able to call the next method and use its result in a functional style
was soon found to be much more ergonomic;
and it is only more so after concurrency and large distributed systems eventually
made people realize that side-effects can be more confusing than pure information flow.
Thus, while Simula was definitely a breakthrough,
its particular form of inheritance was also a dead-end.
There’s a good reason why almost no one outside Scandinavian academia is interested
in using @c{inner}, at least not as the primary inheritance mechanism in their language.

@subsubsection{Inner as Method Combination}

From my point of view, class and pattern bodies are methods,
and concatenation semantics is a method combination.
And compared to the near-universal practice in OO languages since Smalltalk, it is inside out;
and it is only more obviously inside out compared to ADVISE and Flavors style method combinations.

In most OO languages, the most specific method controls the behavior of method invocations:
the dispatch mechanism identifies that method and calls this most specific method, and
it decides whether and when to chain into the next method, and so on along the chain
from most specific to least specific method.
This allows for radical overrides of the semantics, and enables the modeling
of a wide variety of situations.

With “concatenation semantics”, the behavior is inverted:
the least specific method is called first,
and controls the behavior, and so on from least specific to most specific method.
Radical overrides are possible, but only with explicit permission, and, in the general case,
with an explicit inversion of control:
each method takes an inner continuation argument, and calls the next method
accumulating its own contribution in front of the continuation,
until the last method runs the continuation in the now inverted order.
In other words, you could be manually doing as a convention
what method combinations automate as a service.
Of course, this arrangement is onerous,
especially without adequate syntactic support from the language.

If you have CLOS style method combinations @~cite{Pitman1996},
you can define the prefix and suffix parts
of the method body as @c{before} and @c{after} methods, except that
your Simula style method combination must invert the usual CLOS order for these methods
(inherited from Flavors @~cite{Cannon1979}, inherited from ADVISE @~cite{Teitelman1966}),
so @c{before} methods are most-specific-last instead of most-specific-first,
and the other way around for @c{after} methods.
Alternatively, using only standard method combinations,
you can macro-express @~cite{Felleisen1990}
behavior equivalent to Simula class-body concatenation by defining two methods
that the caller (or a fixed utility method) will call:
a @c{simula-before} method in which you define prefixes as @c{after} methods,
and a @c{simula-after} method in which you define suffixes as @c{before} methods.
This is syntactically confusing if you don’t have macros to automate the details away,
but provides the correct semantics, and proves that Simula style concatenation semantics
is trivially expressible in terms of method combinations@xnote["."]{
  For BETA nested inner blocks, you should be using @c{around} methods instead,
  there again with an inverted order.
  And then again, you could use the same kind of control inversion
  to implement CLOS-style inheritance on top of concatenation semantics—if
  you were crazy enough to use BETA or gBeta.
}

@subsubsection{Usability Matters}
@epigraph{
  It is a profoundly erroneous truism, repeated by all copy-books
  and by eminent people when they are making speeches,
  that we should cultivate the habit of thinking of what we are doing.
  The precise opposite is the case. Civilization advances by extending
  the number of important operations which we can perform without thinking about them.
  Operations of thought are like cavalry charges in a battle—they are strictly limited in number,
  they require fresh horses, and must only be made at decisive moments.
  @|#:- "Alfred North Whitehead"|
}
In the end, Simula style inheritance, even updated by BETA, is a dinosaur with poor usability.
Simula is extremely limited in what you can do and
the side-effects it relies on are the opposite of modular.
BETA is more expressive, yet its strict inner mechanism requires
a lot of forethought from each pattern author to make it extensible in just the right way.
Smalltalk style inheritance, almost universally adopted, is much easier to use,
and everything is extensible by default without having to think too much about it@xnote["."]{
  Preference for one over the other may also reveal a cultural trait @~cite{Petricek2025}.
  Control freaks vs hippies—it is no surprise the Simula team favored static typing,
  while the Smalltalk team favored dynamic typing.
}
CLOS style method combinations are even easier to use and extend,
in many standard ways, without the initial author having to think of it at all.
And with user-defined method combinations, you can do even more,
including expressing Simula style inheritance, if you care about it.
This kind of method combination, while not popular,
nevertheless has users who deem it useful. @; TODO cite Blome1995 Goldberg2004 Foote2005

After all this discussion about inheritance,
I feel that while Dahl and Nygaard’s invention of Simula @~cite{Dahl1967}
has been justly celebrated as a breakthrough,
it is quite unfair to characterize it as “the invention of OO”,
while ignoring the precedent of Teitelman, whose ADVISE @~cite{Teitelman1966}
already had the basic mechanism done better (though in narrower situations),
and the essential followup of Bobrow and Winograd, and of Kay and Ingalls,
whose respective KRL-0 @~cite{Bobrow1976} and Smalltalk-76 @~cite{Ingalls1978}
conceptualized and named inheritance and OO, and made them actually usable.

@subsection{Inherited Interfaces vs Orthogonal Protocols}

In the above design, all specifications that use a method must inherit
from the same @c{foo-method-init-spec method-id} ancestor, or else
they will end up using incompatible method specifications.
Not only is such a requirement for a common ancestor onerous,
it prevents modularity in many ways:
First, redundant specifications that must be manually kept identical
are the epitome of lack of modularity.
Second, and just as importantly, requiring specifications (and prototypes and classes)
to inherit from a common ancestor to be able to define a method,
means that you cannot add new methods to specifications after the fact.

Java and C# are two languages known for their design requiring
a class to explicitly inherit from an interface at the moment it is created
so it may expose methods that satisfy that interface.
This design is terrible, because it requires omniscience from
a class author about all the interfaces that the users of the class may ever want to satisfy,
including, impossibly, about interfaces that do not exist yet,
in other systems that haven’t been written yet.

Alternatives for users would include reimplementing the class from scratch,
or writing an onerous wrapper around the existing class, or a subclass of it.
But all these alternatives introduce subtle and unsubtle incompatibilities,
impedance mismatch, wrappings and unwrappings all over the place, and yet
in the end still have the same problem with respect to yet future interfaces:
those future interfaces will still require yet another layer
of wrapping or reimplementation all over again,
for each team that wants to extend the capabilities of a class.

By contrast, CLOS generic functions and Clojure protocols
enable programmers to define methods for a class (or typeclass) @emph{after the fact},
without wrappings and unwrappings, reimplementations, etc.
This vastly increases modularity.
I will call this design @emph{orthogonal protocol} to contrast it with the
@emph{inherited interface} of Java and C#.
The key feature is some notion of “protocol”, i.e. a set of related functions,
that can have different implementations for various types, classes, specifications, etc.,
while being defined independently (that’s what “orthogonal” means).
Protocol implementations are modular entities, that are also first-class and extensible
in CLOS or Clojure.
Haskell typeclasses, and to a lesser extent Rust traits or Go structural interfaces,
offer second-class solutions that are somewhat more modular than C++, Java or C#,
yet much less than CLOS or Clojure, each of them having many limitations,
and none of them modularly extensible.
@; TODO footnote to explain the limitations

The capability is old: in a live Smalltalk or Lisp image since the 1970s,
adding a method to a class you don’t own is not a feature of its own,
but a consequence of the image being editable.
This ambient capability is also carried in dynamic languages like Ruby and Python,
where it surfaces as global “monkey patching”
(a regular idiom for Rubyists, even after the language added its scoped “refinements” in 2013;
an idiom frowned upon by Pythonistas).
An interesting case is that of Objective-C:
a static language (program structure fixed at compile-time) with a dynamic runtime,
inspired by the dynamic language Smalltalk,
it had to reify this capability as a named construct, “categories”—borrowing
the word from Smalltalk’s purely organizational method categories.

@subsection[#:tag "GF"]{Generic Functions}

In CLOS as opposed to Clojure, protocols are informal (external, fourth-class) groupings
of @emph{generic functions}, that are the only formal (internal, first-class) entities.
A generic function (gf) embodies all the “meta” information about a method:
its signature, its inheritance style and method combination,
its default behavior when no method is defined, etc.

This way of storing information is in sharp contrast with
letting the same information be provided by a specification via
inheriting from an interface specification as in Java,
wherein different classes could inherit from opposite information
about same-named methods, and more importantly you have
the previously discussed issue with lack of modularity.
Protocols as independent entities can also avoid the complexity of mutable inheritance,
wherein you’d modify a specification after the fact so it would inherit from a new interface.

Generic functions were introduced as “generic operations” by T @~cite{Rees1982},
and reprised as “generic functions” by New Flavors, CommonLoops, CLOS,
but also beyond Lisp by Cecil @~cite{Chambers1992}, Fortress @~cite{Allen2011}
and Julia @~cite{Bezanson2014}.

In the “message passing” paradigm adopted by the original Flavors after Smalltalk and Actors,
objects receive and handle messages.
Method combinations are attached to “messages”, i.e. method names.
Method invocations look like @c{(send object method-id args ...)}
where @c{send} is the Flavors function to send a message to an object@xnote["."]{
  Actually, later versions of Flavors called that function @c{send},
  but early versions of Flavors just used @c{funcall}, the Lisp function calling function:
  Flavors was indeed using funcallable instances;
  but the original LISP, and its successors MACLISP and Common Lisp, are “Lisp-2’s”,
  i.e. they specially treat the first position in a SEXP as a different kind of non-terminal,
  and distinguish function (or macro) and variable (or symbol-macro) namespaces;
  thus, to use a variable or expression as a function,
  you can’t just put it in first position but have to use @c{funcall} in first position.
  This is still true in Common Lisp—as opposed to Scheme and its dialects,
  that are “Lisp-1’s”, with only one non-terminal in the language grammar,
  more in line with FP.
}

My “record target” paradigm so far is actually isomorphic to that, in that
an @c{object} is a message-handling function, to which I pass a @c{method-id}
(a Scheme symbol) as message argument, returning the method function to which
I keep passing further curried arguments.
Method invocations look like @c{(object method-id args ...)}.
(In functional style, entities are functions and “sending” is just calling.)

Now in the “generic function” paradigm, the gf (generic function),
identified by @c{method-id}, is the functional entity, and
the @c{object} is its first argument, that I can supplement with further arguments.
Method invocations look like
@c{(method-id object args ...)}.

The three syntactic representations are semantically equivalent,
but the generic function representation makes it obvious that
instead of being associated to objects and their inheritance,
the base information about methods is associated to the gf,
an entity well-integrated with regular functions.

Interestingly, that means that individual method specifications
are no longer to be considered as attached to one single entity,
the record specification, prototype or class;
instead, individual method specifications are to be considered as
attached to a pair of two entities: the gf on the one hand,
and the specification, prototype or class on the other hand.
You can think of it as being in a relational data table indexed by two fields,
the gf and the specification, or hash-table indexed by the pair.

Generic functions in the context of higher-order FP introduce some extra complexity.
Since methods don’t have a single owner but two,
method lifetime management can become tricky:
if either the generic function or the specification becomes unreachable,
then the method can be deleted.
When defining a specification or gf in a nested scope, and it doesn’t escape,
then associated methods can be statically deleted at the scope exit;
if it escapes or may escape, then garbage collection is required, and may involve
a weak hash-table indexed by weak pairs (i.e. the pair becomes unreachable
when either side becomes unreachable, at which point so do indexed entries).

Then again, in CLOS, defining generic functions and classes (as opposed to using them)
is a second-class activity for regular programs that avoid use of reflection,
so the problem is avoided;
however, the MOP enables reflective definition and redefinition of functions
as a first-class activity, for the sake of metaprograms and infrastructure,
at which point programmers are supposed to solve the problem manually if it arises.
More second-class concepts built into the language do increase complexity somewhat though.
And in a very dynamic language with first-class prototypes and generic functions
that get defined locally, avoiding space leaks will involve extra complexity
for garbage collection.

At some point, you may realize that generic functions bring extra complexity
for the same reason they bring extra modularity:
because they associate method specifications to a pair of independent entities,
rather than to a single entity or sub-entity thereof.
Since the entities are independent, new ones can be modularly defined
without modifying existing ones, instead extending those existing ones in incremental ways.
This is the essence of any real solution to the “expression problem” @~cite{Wadler1998}.
And then you may further realize that you could gain even more modularity
by associating methods not to a pair of entities, but to arbitrarily large tuples of entities.
Which leads us to the invention of multiple dispatch.

@section[#:tag "MD"]{Multiple Dispatch}

@subsection{Multimethods}

@; TODO CITE Leavens1988 Mugridge1991 Castagna1992 Millstein2002

So far, the choice of what (effective) method to evaluate when calling a generic function
only depended on its first argument, “the” object on which the function was invoked.
As long as OO was stuck in the “message passing” metaphor in which it was born,
this seemed like the only option: you have an object, you send @emph{it} a message,
you call @emph{its} method, etc. There is one entity, of which you select one sub-entity.
But once Cannon invented method combinations, and the concept of generic function followed,
then it became natural to wonder why effective method selection should only depend on
one argument, especially when there are plenty of “binary methods”
that in fact go to great lengths to work around this limitation:
comparison functions, algebraic operators (addition, multiplication, etc.), and more.

Thus, with multiple dispatch, a method can be specialized based
not just on one argument, but on multiple arguments.
A method specializing on multiple arguments is called a @emph{multimethod},
and a language supporting multiple dispatch is the same as a language supporting multimethods.
The previous behavior of only specializing methods on a single object argument
is renamed “single dispatch”.
The early “message passing” metaphor seemed to make this “single dispatch” special,
but it is retrospectively obvious that it wasn’t.

With multimethods, the notional table of method specifications,
instead of being indexed by a pair of a generic function and a specification, is indexed by
an arbitrary tuple of a generic function and any number of specifications—though
the same number for every method within a given generic function,
the “generic arity” of the function (e.g. 2 for binary methods).

Also, a protocol (a set of related generic functions)
supporting dispatch on multiple arguments
is akin to a typeclass that depends on multiple typeclass constraints.
And indeed, you can desugar the latter into the former:
a typeclass function with @c{n} typeclass constraints is like a generic function
dispatching on @c{n} elements, being the “dictionaries” for each of those @c{n} constraints.
The difference is that Haskell typeclasses do not support methods
chaining into parent methods;
but CLOS protocols do @~cite{Rideau2012}.
If @c{n = 0}, the function is a constructor (if it returns an object) or else an arbitrary function.
If @c{n = 1}, the function is a regular OO method.
If @c{n = 2}, the function is a binary method.
If @c{n > 2}, the function is a more general multimethod.
Multimethods unify all these concepts,
that are often problematic in the naïve view of Class OO,
but pose no problem with prototypes or typeclasses or protocols.

Multimethods notably simplify away “binary methods”, “double dispatch”,
and the “visitor pattern”, offering
a modular alternative to these often nightmarish “design patterns”.
Moreover, multimethods support actual win-win inheritance
whereas the above design patterns typically only support conflict inheritance.

At least CLOS, Clojure and Julia support multimethods,
as well as many Lisp and Scheme object systems (including Gerbil Scheme).
Many past languages including Cecil, Dylan, Fortress or Slate also did.
A few popular languages have libraries that implement some form of them.
@; TODO CITE

@subsection{Binary Methods Done Right}
@epigraph{
  Patterns mean “I have run out of language.”
  @|#:- @elem{Rich Hickey, @emph{Simple Made Easy}}|
}
For decades, developers of object-oriented protocols have stumbled upon problems
regarding “binary methods”, the behavior of which depends not just on one object,
but also on a second object:
comparing objects of various types for equality or for (total or partial) order;
adding two numbers involves different operations depending on whether you try to add two integers,
or two floating-point numbers, or an integer and a floating-point number,
or two strings, or two matrices of the same shape, etc.;
displaying an object on some graphical or text terminal;
inserting an entry into some collection;
planning or performing a build operation on a code component;
etc.

In languages that only have single dispatch, writing such methods is problematic,
and not only due to the contravariance issues I discussed relative to typing (@secref{LotN}):
how do you write such code, and how do you keep it modular and extensible?
You can dispatch on the first object, and have a method in each case; fine.
But what about the second object?
A simple “solution” is to do a runtime typecheck for the second object,
and handle each case in a list of known possibilities.
Problem is, you must know in advance all the possible specifications you will ever want to use
for the second argument, and the result is not extensible.
A more complex solution is to create a separate method
for each possible specification for the first argument,
then call that method on the second argument:
thus the method @c{add} on an integer calls the method @c{add-to-integer} on the second argument,
with the first object as first argument—exchanging the roles of dispatch object
and first non-dispatch argument, but after having specialized the method.
This approach is known as the “double dispatch” design pattern,
and can be generalized to triple dispatch, or any kind of n-ary dispatch.

But double dispatch is awkward and limited in many ways:
since languages that need this do not have generic functions or multiple dispatch,
you need to include the specialized method in the specification itself,
which once again means that all possible specifications for the first argument
must be known in advance when defining the specification for the second argument,
so this fourth-class design pattern is only extensible for the second argument.
Also, in languages with visibility limitations on an object’s internal state,
the refined method might not be able to see the state of the object,
or that state may have to be shared more broadly than desired to enable double dispatch.
Finally, there is no good way to “call the next method” and compose multiple inherited behaviors,
and every programmer of every part of the protocol would have to cooperate with the pattern,
which is a hard discipline for a programmer to follow, and
a near impossible one to enforce upon others.

A more sophisticated approach known as the “visitor pattern” @~cite{Gamma1994}
is both a special case of double dispatch (in how it’s implemented)
and a generalization of it (in what capabilities it offers).
I will present it with classes as is usual, but it generalizes to arbitrary specifications:
@itemize[
  @item{
    A general-purpose method traditionally called @c{accept} takes a “visitor” object as argument,
    and each class @c{Foo} calls the special-purpose method @c{visitFoo} on the visitor,
    with the current object (of class @c{Foo} indeed) as parameter.
    The visitor pattern is thus an instance of double dispatch,
    wherein programmers essentially create a translation
    from the class namespace to the method namespace.}
  @item{
    Each visitor can then provide a method for each of the classes it wants to support.}
  @item{
    By having the visitor object abstract over both the specific operation
    and the further arguments, you can use chains of visitors to implement all kinds of operations.
    Compared to regular double dispatch, the visitor pattern is more general, and
    crucially allows for new visiting operations to be defined after the class is defined.}]

Compared to double dispatch, the visitor pattern involves even more boilerplate:
having to define visitor classes with all the required information.
The design pattern is a third-and-a-half-class entity,
largely done manually by a human (thus a fourth-class entity)
yet with some amount of automation and well-defined uniform naming conventions
in converting class names to visit methods (thus partly a third-class entity).
Importantly, the visitor pattern also requires all visitor-needed state
to be publicly accessible—which, if you want to allow arbitrary future visitors,
might be all state.

The visitor pattern involves more code than double-dispatch, at least the first time it’s used;
but if used more than once, part of the visitor infrastructure can be shared between visitors.
Importantly, and unlike double dispatch, the visitor pattern allows new visitors
to be defined after a class was defined:
its class-to-method namespace translation can be seen as a manual implementation of
a runtime-reflection facility that can enable dynamic behavior even in a static language.
However, in a statically typed second-class Class OO language,
@emph{modularly} extending an existing visitor pattern to support new classes to dispatch on
without modifying the old visitor protocol requires careful use of self-types
for correct static types (F-bounded polymorphism, MyType, CRTP, etc.), and
will also require a new subclass for every operation that needs to apply to the extended visitor.
In other words, extensibility of visitors is possible, but noticeably tedious and costly.

Finally, at least in the commonly described variants
of either double dispatch or the visitor pattern,
there is still no good way to support “call-next-method”:
Either it finds only one method, breaking “linearity” (conservation of information);
like Self’s ill-fated sender path tiebreaker rule, it can’t back out of a bad narrowing decision.
Or it has to explore the entire space of methods, implement some backtracking search,
suffer exponential explosion in the presence of diamonds, or also implement
some clever conflict detection, or some linearization—in a word,
reinvent better inheritance mechanisms as manually enforced design patterns
on top of what the language provides.
A lot of the machinery could then be implemented once and shared by many generalized visitors;
yet at the same time users would have to (correctly) write a lot of boilerplate
to reflect enough of the structure of their classes for the common machinery to take over.
Needless to say, popular literature on visitors remains confined
to the most primitive of these design patterns—the one that breaks linearity.
If the more elaborate design patterns were implemented, the primitive ones
could be tagged as “naive” versions;
but of course, the real elaborate move would be to extend the object system itself
to support multiple dispatch.

Indeed, if the object system itself has builtin support for multiple dispatch, then
double dispatch as a design pattern, and its generalizations to triple and n-ary dispatch,
or to the visitor pattern, disappear completely
as complex yet no-longer-helpful design patterns.
They are replaced by “just use the system-provided multiple dispatch”.
Suddenly, these methods become simple to write, extensible for both arguments,
without the visibility issues of double dispatch and visitors
(assuming a method, if allowed to be written,
is granted the right to see the state of any argument it matches);
moreover, you can achieve full win-win composability of the methods
if using flavorful multiple inheritance or optimal inheritance with multimethods (as in CLOS),
and not something flavorless (as in Cecil).
And you don’t have to write a lot of painful boilerplate—and, worse, keep that boilerplate
up to date as the code evolves.

@; TODO: footnote explaining that pommette.scm contains examples.

@subsection{Multiple Multiple Inheritance}

The set of multimethods that match a given tuple of arguments is necessarily a partial order:
by separately specializing one argument or the other,
you can easily define mutually incomparable method signatures
that match the tuple of arguments,
even if each argument’s ancestry is a total order—as
with Julia’s single inheritance @~cite{Bezanson2014}.
Multimethods can thus naturally leverage and extend the techniques used for multiple inheritance:
linearizing the order of methods,
method combination based on the resulting precedence list, etc.,
as done by CLOS @~cite{Bobrow1988}.
Conversely, some languages like Dylan @~cite{Shalit1996} apply linearization to each argument,
and can linearize tuples of arguments still, yet apply the “conflict” view of inheritance
in case of multiple incomparable tuples of arguments for the next method.
And other languages sadly just adopt conflict all the way
@~cite{Chambers1992 Salzman2005}.

Yet, the same argument in favor of linearization applies
for multiple dispatch as well as for multiple inheritance:
Any side-effects in those methods that will run (that, in the general case, exist)
will necessarily be ordered one way or the other;
the only question is whether the system automates a coherent order,
or puts the onus onto users—at which point it will be both onerous and incoherent.
Linearization is the necessary process that automatically solves this ordering consistently
while also not dropping any method
(see the desirable consistency constraints described in @secref{CiMR}).
Any rejection of linearization leads to a “conflict” view of inheritance (or lack thereof),
that is costly, inexpressive, lossy, and generally counter-productive.

Linearization of tuples typically happens via
the lexicographical order of per-argument linearizations:
this linearization follows the same behavior as the double dispatch and the visitor pattern
in the degenerate case that only the most specific method is called;
it has a clear precedence based on the first argument,
and it nicely extends single dispatch on the first argument,
in that methods specialized on the first argument
will be ordered relative to each other the same as if dispatching on just that argument.
The lexicographic linearization necessarily induces an asymmetry between arguments,
and it is important to choose the correct order of arguments when designing a multimethod protocol:
the argument that most crucially affects the behavior of the method
should appear first in the signature.
For instance, the ASDF API is correct in dispatching on operation first, and component second,
since the behavior varies more with the first (compile some code, or load it, link it, etc.)
than with the second (was the source file plain Lisp, or C FFI code,
code transpiled to Lisp, etc.)@xnote["."]{
  @; TODO move to bibliography and cite
  Interestingly, Daniel Barlow, who started
  @hyperlink["https://asdf.common-lisp.dev/"]{ASDF} in 2001,
  chose this order correctly, when Kent Pitman’s
  @hyperlink["https://nhplace.com/kent/Papers/Large-Systems.html"]{much older design document}
  that inspired him had the arguments in the opposite order—which I believe
  would have made things harder.
}
Of course, if the operation is commutative (e.g. addition),
the side-effects from evaluating the methods that yield the arguments to the operation
do not usually commute, and the method order matters.

Like the multiple inheritance that it extends (@secref{RSaDN}),
multiple dispatch relies on the ability to reify the graph nature of computations
through the generation and equality checking of unique tags,
which programming languages typically implement by exposing pointer equality
(@c{eq?} in Scheme, @c{===} in JavaScript, etc.), and,
for the sake of efficiency, some kind of (hash) tables based on it.
Indeed, to be able to walk through each inheritance DAG’s node once and only once,
one needs to be able to identify and distinguish them; and with multiple dispatch,
one will also want to identify generic functions, their method signatures
(tuples of a generic function and specifications being specialized on),
and partial method signatures (prefixes of the previous tuples).

A simple implementation of multiple dispatch involves a global table of defined multimethods,
which would constrain all specifications to pass around the global context
for the sake of specifying multimethods, though they may narrow it down with (skew) lenses
after extracting a (lens for) the global table.
A more elaborate implementation may involve a multimethod table per generic function,
local to said generic function, so now the context of multimethod specifications
only needs to be broad enough to encompass
the generic function and the specifications in the signature.
By using a double-dispatch-like series of tables and subtables indexed by partial method signature,
stored locally in each prototype target, one may specify multimethods
with just the gf tag and update access to the specifications, without needing to update the gf;
the main advantage is then that the representation is backward compatible with
the previous representation for single dispatch only,
and can leverage the regular inheritance mechanism to recursively handle dispatch
along the lexicographic combination of the precedence lists of the argument prototypes.
Thus, code that previously worked with a narrow context can still work essentially unchanged.

In any case, the specification context for multimethods is usually wider than the specification
context of a single-dispatch method, that needs only contain the prototype or class being specified:
now it needs to view the generic function and each prototype at stake,
and to update whichever of these entities will hold the method information (or worse, a global table).
That is a retrospectively obvious necessity when defining multimethods—but,
one may note that while each single method can do with a narrower context,
duplicating the desired effect of multimethods without system support for multiple dispatch
requires the very same wider context, just with much manual expansion of design patterns:
in all cases, you will need to view or update all the specifications at stake,
and many additional ones, too (“visitors”).
This is an important point: multiple dispatch invites you to broaden the context
of your fixpoints, from a single specification, to, in the limit, the entire OO ecosystem.

@subsection{Base Classes and Specializers}

When using multiple dispatch, it becomes very important to have
a “base” class, prototype or specification, or “top” type or specializer,
that matches any argument, and is automatically included at the end of every precedence list,
even when specifications don’t explicitly inherit from it.
Users can then easily specify universal or default behavior to use
when one or several of many arguments are not specific but others might be.
For instance, in CLOS, the class @c{t} (also the name of the true boolean)
matches any Lisp value of any type, while @c{standard-object} matches any object created with CLOS.

Even when a language does not provide a base class, prototype or specification,
users could by convention adopt one class for all their hierarchies.
But then authors of multiple libraries would have to coordinate to all adopt the same convention.
At this point, it is better if the language itself offers such a base specification.

Now, when using single dispatch, users might have used a separate defaulting mechanism
to specify how their method invocations behave in the absence of explicit specialized methods.
For instance, CLOS’s @c{no-applicable-method} generic function, or
Smalltalk’s @c{doesNotUnderstand:} message, can catch these cases, and more, actually.
But these mechanisms are both more powerful than needed yet not precise enough
to specify default behavior of a generic function when only some of many arguments
must be accepted without any specification to match them against.
When using multiple dispatch, the existence of a base specification is all but necessary;
it is a really important, useful feature.

Also note how I used the word “specializer” above:
CLOS, and some object systems inspired by it, support the specification of methods
attached not to a specific class or prototype, but to a more general notion of @emph{specializer}.
The simplest such kind of specializers in CLOS are @c{eql} specializers,
that work on a single object or value:
for instance, the @c{(eql 42)} specializer will only match the number 42.
The CLOS MOP, not part of the ANSI standard, but largely supported, @; TODO cite
also allows users to define more specializers, and some have implemented “predicate dispatch”,
as once made popular by Cecil @~cite{Chambers1992}, wherein
a specializer can be the conjunction of a class and a predicate (function returning a boolean)
that filters which objects the specializer applies to.

@;{ TODO keeping the same implementation for generic functions that apply to prototypes vs classes.
 Some arguments might dispatch on an object’s class, others on the object itself as prototype;
 what does that mean though to EQL and predicate specializers?
}

@subsection{Implementing Multiple Dispatch}

I have implemented multiple dispatch in the code accompanying this book.
Here are the highlights of this implementation.
You may skip this section if satisfied with its semantics.

@subsection{Who Owns the Methods?}

I started from an implementation of Prototypes with Optimal Inheritance (acronym POI),
mixing the lessons of @secref{ROOfiMC} and @secref{IMSMO}. @; (ch 6 and 7)
The names of functions related to them start or end with “poi”.
A poi, being a prototype, conflates target and specification.
In the style of @c{rproto} (@secref{CfR}),
it stores specification information and other metadata (including a cache of the precedence list)
in a special field (in this case, using @c{#f} as the access key).

Then came the issue of where to store method information,
which was especially important since I chose to stick to a pure functional implementation
(modulo the ability to compare entities for identity without having to @emph{manually} assign them
unique identifiers).
Indeed, in a stateful implementation, one can “just” side-effect an entity to add a new multimethod;
but in a pure implementation, one has to explicitly update “the” entity as identified by a lens,
@emph{before} one computes the fixpoint of all the entities at stake—and
then this opens the door to non-termination due to
bad initialization ordering or circularity, that could often be solved by making everything lazy,
yet for the sake of simplicity I wanted my code to still work on any eager functional language.

I considered storing method information in the prototypes themselves,
basically automating the way double-dispatch is manually implemented:
when defining a multimethod @c{m} on a tuple of arguments with prototypes @c{(p1… pn)},
define a method on @c{m} for prototype @c{p1},
that chains into a specialized method @c{m_p1} defined for prototype @c{p2},
where @c{m_p1} is a fresh method name or identity generated from those of @c{m} and @c{p1}.
Then if it is more than double dispatch, have a @c{m_p1_p2} generated from @c{m_p1} and @c{p2}
and defined for @c{p3}, and so on, until a specialized method @c{m_p1_…_pn-1} is defined on @c{pn}
that contains the user-specified code.
For the sake of method combinations, each generated method explicitly
invokes the regular @c{call-next-method} mechanism to accumulate all declared user methods
in a lexicographically ordered list: each @c{m_...pk} method first recurses
into dispatching to the next argument, then uses per-prototype inheritance
to recurse into ancestors of the current prototype@xnote["."]{
  Note that if one only cares for the most specific matching method,
  one doesn’t need to accumulate methods in lists,
  and the method listing infrastructure becomes wasteful and inefficient.
  On the other hand, one might not want to write the code twice,
  for the case where one only needs one answer vs the case where one needs all of them.
  Thanks to delimited control @~cite{Felleisen1988 Danvy1990},
  one can efficiently support both styles while writing the code only once;
  the same effect can also be achieved monadically @~cite{Dybvig2007}
  in languages lacking native support for partial continuations.
  Of course, being able to specify the algorithm once and only once,
  thereby ensuring consistency and facilitating proofs of correctness,
  doesn’t mean the runtime implementation should have a single code path:
  it might indeed be advantageous for a compiler to take that one abstract specification
  and pick at runtime the best of many specialized variants of it for each task at hand,
  e.g. based on inlining a suitably limited monad in the monadic specification,
  as well as specific data representations, then optimizing
  the hell out of the known specialized context.
  Note that this approach is valid even without multiple dispatch,
  or with @c{call-next-method} as the only form of method combination;
  but the more elaborate forms exacerbate the issue
  by involving increasingly complex method resolution algorithms.
}
However, this strategy requires either modifying the prototypes in place
with stateful side-effects, or being able to lazily refer to incomplete prototypes
before, during and after the fixpoint process
as further declarations add new methods to prototypes.
It is not compatible with the pure yet eager approach I chose to illustrate
how to implement OO in the most portable way.

Therefore, I chose to instead store method information in the generic functions,
wherein, with a little bit of trickery and minor limitations,
generic functions can be both callable functions and extensible objects containing
information about generic arity, available multimethods, etc.
The method representation is equivalent to the above, but without the charade
of lots of intermediate methods @c{m_p1...pk} as with manual dispatch above,
or a combination of reflecting classes @c{c} into methods @c{visit_c} then
specialized contexts @c{m_p1...pk} into visitor classes of their own, as with the visitor pattern.
Instead, there is just an index made of nested records, where the @c{p1...pk} correspond
to a path looking up @c{p1} in the top record, returning a record into which you
lookup @c{p2}, etc., until @c{pk}; if a record is not found along the way,
there is no method specialized on those prototypes@xnote["."]{
  Obviously the index of methods would have to be somewhat more complex when supporting
  more general specializers than classes or prototypes.
  For instance, instead of one method per tuple of classes,
  you might have lists or tables of methods with
  @c{eql} or predicate specializers that further refine that tuple of classes.
  I leave that as an exercise to the reader.
}

Now, whichever way you represent and index multimethods,
the overall search in the worst case is still in @c{O(pⁿ)}
where @c{p} is the maximum depth of an argument’s precedence list,
and @c{n} is the number of arguments (saturated set of methods for all tuples of ancestors).
But if the size of an index record along the way is notably smaller than
that of the precedence lists for the argument at stake,
iterating over method entries can be faster than iterating over argument ancestors;
beware though that you may then have to sort those method entries
if there was no global linearization order on which to pre-sort them@xnote["."]{
  As another performance concern, one should also be careful to not recursively
  @c{append} and re-@c{append} lists of methods at each level of recursion:
  @c{append} is an @c{O(l)} operation on usual single-linked lists,
  where @c{l} is the number of methods found;
  and this reappending would cost @c{O(nl²)}
  where @c{n} is the number of arguments, instead of @c{O(nl)} as should be.
  Instead, one should adopt a representation of lists that has a constant-time appending operation,
  even if just to flatten it to the usual single-linked lists at the end.
  The simplest such representation is by representing list-builders as functions that
  prepend elements to a list, a.k.a. difference lists. @; TODO cite Hughes1986
  Composing two such functions is the same as appending the lists of their elements.
  By applying the final list-prepending function to the empty list,
  the binary tree of composed element-prependers is indeed flattened.
  In a stateful language, you could just push elements at the front of a list
  that you reverse in the end.
  In a pure language, you could also locally use a state monad for the same purpose;
  but the composed list-prependers have the advantage of remaining pure without a monad.
}
In any case, effective method computation can be quite slow, and
the results are better cached for efficiency, @;TODO @secref{ch10}
after which further calls are cheaper than would be a naive implementation of visitors.
Thus, in a proper implementation of multiple dispatch, you only pay the full price
once per call “shape” (tuple of specifications),
which if you are using static classes only happens a finite and relatively small
number of times in the program.
And in fact, in the rare case that call shapes are indeed very dynamic,
so that the cache keeps missing, then you are precisely in the situation
where you most need the expressiveness of multiple dispatch, since
you are dynamically defining what would be extremely tedious
with the visitor pattern or double-dispatch.

The naive way of using double-dispatch or the visitor pattern
would both require a lot of boilerplate and be less performant,
while introducing a heavy maintenance burden.
A non-naive way of using double-dispatch or the visitor pattern
to achieve the same semantic features and optimizations as properly implemented multiple dispatch
would require even more boilerplate, still be less performant,
and soon become a maintenance nightmare:
having programmers manually do the job of a compiler and having to maintain
a lot of invariants by hand as the system evolves, without the help of automated enforcement.

Another issue with multiple dispatch, that only gets more “interesting”
when using curried functions for handling arguments, is that the generic function
must accept all the arguments of each invocation before it may evaluate any individual method:
this includes non-dispatch mandatory arguments, and, if the language supports them,
optional positional arguments, rest arguments and keyword arguments.
To support arbitrary shapes of argument lists, I can factor the multiple dispatch
of a generic function into three parts—an accepter, a combiner, and an invoker:
@itemize[
  @item{The accepter function accepts all these arguments, makes a record of them
    (e.g. in Scheme, a list), and invokes a continuation function on them
    (taken as argument to the accepter).}
  @item{The combiner applies the method combination, by
    using the linearization algorithm on the methods of each method qualifier it supports,
    based on multiple dispatch with the supported arity.
    The combiner then invokes these methods in order,
    each with a suitable additional (and in many languages, implicit) @c{call-next-method} argument,
    by calling the invoker function.}
  @item{The invoker function passes along the recorded arguments to the current method@xnote["."]{
      For instance, a curried accepter for three arguments, without syntactic shortcuts,
      would be @c{(λ (f) (λ (x) (λ (y) (λ (z) (f (cons x (cons y (cons z '()))))))))}.
      And the corresponding curried invoker for three arguments would be
      @c{(λ (f args) (((f (car args)) (cadr args)) (caddr args)))}
      where the Lisp functions @c{car}, @c{cadr} and @c{caddr} extract respectively
      the first, second and third argument of a list.}}]

Furthermore, to match CLOS, one may have to allow a method to update
the record of arguments passed along the rest of its call chain
by optionally specifying them when calling @c{call-next-method}.
A dynamic language might do all these computations at runtime, whereas
a static language might inline as much of it as possible at compile-time.

@subsection{Subjective Dispatch}

Some object systems with multiple dispatch offer an extension for
“subjective dispatch” or “subjective multimethods” @~cite{Salzman2005},
wherein some context provides an @emph{implicit} extra argument to a method call.
In particular, Slate allows for a @emph{dynamically bound} subject,
to be similarly added as a hidden argument
at the end of the argument list of every method call @~cite{Salzman2005}.

Now if using “conflict” for incomparable tuples of specifications, as in Cecil or Dylan,
then argument positions are symmetrical, and putting the extra argument
at the beginning or end of the argument list, or anywhere in between,
is a matter of syntactic convention without any semantic weight.
On the other hand, if methods are ordered lexicographically, as in Slate or CLOS,
then argument positions are very much not symmetrical.
Assuming the common strategy as used by Slate and CLOS,
wherein earlier arguments have higher-priority than later arguments in the dispatch process,
then the first position has the highest priority, whereas the last position has the lowest.

Subjective dispatch in first position can then enable context-dependent methods
to completely override the meaning of programs in arbitrary ways, overriding any other method;
whereas subjective dispatch in last position can only minimally alter program behavior,
making it a weakly expressive mechanism that might not be worth the complexity it brings.
a high-priority subject can provide methods that intercept and control behavior
early in the effective method, when a low-priority cannot.
That is how @citet{Gonzalez2005} found that subjective dispatch was more usable
with the subjective argument first,
and giving the feature more semantic weight, so it might be worth the trouble.
@; TODO also cite Gonzalez2007 Gonzalez2008 Hirschfeld2008

@subsection{Global Dispatch Tables}
The implementation I offered was minimal in terms of effects and scope:
the only effect is tagging for identity, which is pure enough
in a calculus that deals with terms as graphs rather than as trees;
and the scope of the context necessary to register a method handler is the smallest
that contains all the specifications at stake.

But there are other possible representations for multiple dispatch:
you could have a global table of methods, or one table per arity, or one table per gf,
with a bigger state monad as side-effect, but no scope limitation requiring update to specifications;
specifications are then emptied of everything but their identity,
all actual code information being moved to these tables.
In this global table paradigm, it becomes simpler to think uniformly in terms of an API:
browsing it, searching it, controlling access to it, iterating over it, transforming it, etc.

Conceptualizing method dispatch through a global table also suggests a change of perspective,
from extending one specification to extending entire ecosystems @~cite{Ossher1992}:
Programmers don’t extend just one class, yielding an extended class—they
extend one class hierarchy, yielding an extended class hierarchy;
or even one ecosystem, yielding an extended ecosystem.
This perspective also solves apparent problems like the need for “orphan instances”
for Haskell typeclasses, retroactive “friend” classes in C++ or interfaces in Java,
the “expression problem”, etc@xnote["."]{
  Orphan instances are what happens when some Haskell package defines new kinds of typeclasses
  (which in OO would be protocols, i.e. sets of generic functions),
  while another independent package defines new kinds of data structures
  (which in OO would be classes, or more generally, prototypes),
  and a third package defines how the latter are “orphaned” instances of the former
  (which in OO would be methods for the generic functions specialized on prototypes)—orphaned
  because owned neither by the typeclasses nor by the data structures.
  Orphaned instances act as compile-time side-effects on the semantics of the language—monkey patching.
  Yet they are necessary because indeed, neither the typeclass nor the data structure
  must depend on the other.

  By contrast, not only are Common Lispers not afraid of “orphaned methods”,
  Gary King invented @c{asdf-system-connections} to automatically load systems with those methods
  when both the system with the generic functions and the system with classes were loaded.
  Of course this is only actually useful in the context of an @emph{interactive} system
  where which modules will be loaded depends on interaction with the user.
  Indeed, in a non-interactive system, the programmer managing the build will have an easier time
  explicitly specifying the system with the combined methods as a dependency.
  But the point is that the independent definition of classes, protocols and methods is
  a huge feature, and not a bug, though it appears wrong if you look at it the wrong way.
}
The impossible extension ownership conflicts dissolve, when one realizes
the unit of coherent extension was never the individual class or object or function,
but the entire program or library being modified.
Indeed, logical “laws” interrelating functions and data structures
always prevented the unilateral extension of a single entity
from ever being valid, except in the simplest and least meaningful of cases.
Once the locus of coherent semantics is established,
properly situated solutions become possible to address coherence extension.

Extending a specification at a given “location” to declare additional methods
is essentially “monkey patching”, i.e. modifying code in place,
even if modeled with pure semantics using lenses:
the specification at a given location is extended.
But this equation goes both ways: while some may cast aspersions on multiple dispatch
by associating it with “dirty” low-level tricks some use to implement features in some languages,
others will realize that those tricks can be seen as mere implementation details
of legitimate high-level semantics to cleanly and purely extend programs in modular ways.

In the end, this view of extensibility reminds us that
a name path or location properly identifies
an extensible “intention” rather than an immutable “extension”,
a meeting point rather than fixed code.
That was always the case since code evolves with bug fixes and new features,
and the entire point of modularity is that names always were meeting points
for changing code the user doesn’t want to look into,
rather than identifiers for exact code the user wants to always be bit-for-bit as specified.
If users wanted the latter, they would be using cryptographic hashes, not names.
But thanks to OO, i.e. internal modular extensibility,
this phenomenon of a constant name for changing content
happens @emph{inside} the language rather than only outside it.

Nota Bene: By using modular extensions as first-class functions,
I do not need to introduce a hierarchy of complex new syntactic constructs
each time I generalize modular extensibility from individual methods,
to prototypes or classes, to hierarchies of prototypes, to entire software ecosystems, etc.
The very same universal notion applies to types of arbitrary complexity—or simplicity.

@section[#:tag "DvSD"]{Dynamic vs Static Dispatch}

@subsection{Two Different Semantics for Class Method Call}

When describing the semantics of Class OO in @secref{SFCTD},
I used the semantics commonly adopted by all popular Class OO languages:
calling a method on an object will consult a type descriptor associated to the object,
then will extract a function associated to the method-id from that type descriptor,
and finally will call that function with the object as first argument,
followed by any remaining arguments.

This semantics is called @emph{dynamic dispatch},
and though every popular Class OO language supports it,
it is actually not the default in Simula, C++ and C#,
which instead favor @emph{static dispatch} by default:
programs include type declarations, and the type descriptors from which functions are extracted
are “statically” based on those declarations at compile-time,
rather than on object values at runtime.

Still, the usual dynamic dispatch of OO is available for these languages
by specifying methods as “virtual” (or “open” in Kotlin);
and many Class OO languages also have static dispatch, if only for “static” methods
that do not take any implicit object argument (as in C++, Java, C#, etc.).

The existence of these two kinds of dispatch raises interesting questions:
What is the relationship between dispatch strategy and OO?
Does OO as such mandate either dispatch strategy?
Either way, what does that tell us about OO?

@subsection{Method Dictionaries}

To clarify where exactly the difference between the two dispatch strategies lies,
both semantics can be factored through the use of
a “typeclass dictionary” or “dispatch table”:
@Code{
(def (call-method-via-dictionary dictionary object method-id)
  (dictionary method-id object))
}
The difference is that in dynamic dispatch,
the dictionary is extracted at runtime from the first-class object itself,
as its virtual dispatch table—which in @secref{SFCTD}
I implemented as @c{(object #t 'instance-methods)},
whereas in static dispatch, the dictionary is extracted at compile-time
from second-class type annotations about the object, as e.g.
@c{(static-type 'instance-methods)}.

To further clarify the concepts at stake in a language
with @emph{both} static types and dynamic dispatch,
@citet{Allen2011} cleverly introduces the word
@emph{Ilk} to denote the runtime object “type” used for dynamic dispatch,
as contrasted to @emph{Type} as traditionally denoting the compile-time object type
when doing type analysis and optimization, and in this case also static dispatch.
Now, in a consistent typesystem, a Type will correctly determine
the set of possible Ilks that an expression of that Type can have at runtime.
And if a narrow enough set of Ilks is determined through the type analysis,
especially through the use of class sealing or “final” annotations, then
the method could be determined at compile-time even though the language does dynamic dispatch,
and the dispatch could be optimized away at compile-time.
If the set of Ilks allowed by the Type is too large or open, then the type analysis
does not permit optimization of dynamic dispatch into static dispatch.

@subsection{First-class vs second-class modularity}

Whichever dispatch strategy or set of available strategies you choose
for your second-class Class OO language,
OO as such in that language still happens only as second-class computations at compile-time.
The static or dynamic dispatch strategy describes how the runtime system
uses the structures and algorithms built by the compile-time OO.
They are features that complement OO as such, rather than alter it.

Crucially note how the two dispatch strategies also apply to
non-OO ways to build data structures and algorithms:
second-class vs first-class modules in ML dialects;
or second-class typeclasses vs existentially quantified first-class values
with typeclass constraints in Haskell (usually using GADT syntax);
or even second-class libraries of functions vs structs containing function pointers in C
(that could even be used with COM or DCOM); @; TODO cite
Scheme libraries vs objects-as-closures without inheritance as in SICP @~cite{Abelson1996},
etc.

@principle{Dynamic dispatch embodies first-class modularity},
wherein a first-class entity, the object, or its “virtual dispatch table”,
serves as a module that specifies at runtime what code to run.
From this point of view, it is a feature @emph{in addition to}
the second-class modularity (and modular extensibility) of second-class Class OO.
By contrast, static dispatch embodies second-class modularity only,
which is necessarily implied as a subset of
the second-class modular extensibility of second-class Class OO.

In Prototype OO, every prototype’s target is a first-class record
that exists at runtime, the fields of which are looked up to determine the behavior of a program.
Indeed, Prototype OO embodies first-class modular extensibility,
which implies first-class modularity.
Of course, Prototype OO can also be used to generate algorithms that subsequently
manipulate data without consulting the data’s type to determine behavior,
just checking or plainly assuming that data has the expected type;
in that case, using Prototype OO to generate some algorithm
involves first-class modular extensibility, whereas using the algorithm afterwards
might not use it. But the two are not separated by formal execution “stages”.

On the other hand, second-class Class OO embodies second-class modular extensibility,
and indeed introduces a formal separation between a compile-time execution stage
during which classes are defined, and a runtime execution stage during which they are used.
And second-class modular extensibility does not imply first-class modularity.

Now, even languages with first-class Class OO usually expose it through an API
that looks like second-class Class OO, and that regular users use as if it were,
even when they could do more: they write a finite set of classes known at compile-time, and
these classes are constant at runtime after initialization,
with a constant inheritance hierarchy, a constant set of fields and methods, etc.
The dynamic features are mostly used for interactive development and debugging.

In the opposite direction, you could very well have Class OO
without first-class modularity, and indeed that is what happens
when you stick to static methods and static dispatch in C++, Java or C#, etc.:
the method selection is completely determined at compile-time,
code paths not depending on runtime lookup of object type descriptors.
It is easy to imagine a Class OO language that only has static dispatch
and does not even feature dynamic dispatch as a “virtual” option;
it might be somewhat rigid to work with,
and programmers used to dynamic languages and dynamic dispatch might be quite dismayed
to find that a lot of their usual design patterns do not apply anymore.
It would fail many people’s misdefinition of “OO”
as being or implying first-class modularity (@secref{OOinE}).
But it would still be Class OO, and a lot of C++ programs
already stick to only using such a dialect.

@subsection{Choosing between Static and Dynamic Dispatch}

@subsubsection[#:tag "DD"]{Dynamic Dispatch}

Dynamic dispatch is easy to implement in a few lines of code (again, as in @secref{SFCTD}).
It requires much less mental and software scaffolding to implement and use than static dispatch,
since it does away with the entire static type infrastructure.
Indeed, dynamic dispatch is readily available to all programming languages,
whether they are statically typed or not,
and, importantly, to all programming language users and implementers,
whether or not they are willing to invest upfront in the expensive type scaffolding.

Thus, only dynamic dispatch offers a semantics
that all OO programmers can understand, implement, use, and agree on,
whereas static dispatch is not even applicable to dynamic OO languages.
Inasmuch as one is looking for a dispatch strategy that is “natural” to Class OO,
and that can be universally adopted by both dynamic and static languages,
then necessarily this dispatch strategy must be dynamic dispatch.
That is why in practice all Class OO languages offer dynamic dispatch at least as an option
(even though, as I mentioned, one could imagine a Class OO language with only static dispatch).

The dynamic approach also allows for much more experimentation,
and amplification of successes and correction of failures,
than the much more rigid static approach.
Furthermore, there is now plenty of precedent for
layering a typesystem on top of a dynamic language,
as TypeScript demonstrated for JavaScript, bringing
much of the safety and tooling of types on top of dynamic systems,
though also much of their complexity.

On the other hand, dynamic dispatch necessarily adds overhead at runtime
to each method call that uses it. This overhead can be kept small in the common case,
thanks to caching and sealing; but it remains irreducible in the worst case,
especially in an interactively extensible system.
Marking classes as sealed, final, or suffix, as well as a modicum of
type declarations or type inference, can reduce dynamic dispatch to static dispatch,
in many parts of a program where performance matters.
And even when that is not possible, repetitive usage patterns mean that
the results of dynamic method dispatch can be cached, so that most of the time,
one only needs to access the same effective method as the last time or one of the last few times,
which can be quickly checked.

@subsubsection[#:tag "SD"]{Static Dispatch}

With static dispatch, the dispatch itself (resolution to effective method)
can be fully resolved at compile-time, based on static type information.
In many cases, a compiler is able to infer at compile-time based on static analysis
what would be the result of a runtime dispatch,
or speculatively handle known dispatch patterns,
an optimization known as devirtualization.

Sticking to static dispatch is the same as using OO’s modular extensibility at compile-time only,
to generate code that is neither extensible nor modular at runtime.
Compared to the looser dynamic dispatch,
static dispatch not only brings more safety and performance,
but does so in a @emph{predictable} way.
Such safety, performance and predictability are paramount for some applications
and for “system programming”;
they also satisfy programmers with a mindset very different
from those who like dynamic dispatch @~cite{Petricek2017}.

On the flip side, static dispatch requires a typesystem
that associates compile-time “types” to objects, expressions and variables.
Developing with such a typesystem involves either a lot of programmer annotations,
or a lot of sophistication in type inference infrastructure
that both human and machine understand and agree upon.
Static dispatch is thus intrinsically more complex and costlier than dynamic dispatch
to use and implement, once you factor the cost of this infrastructure.
Static dispatch is also intimately tied to the specific details of whichever typesystem is used,
which vary in myriad ways big and small from language to language, and version to version.

Certainly, it is hard to write a static language, much less a @emph{good} static language,
one with a typesystem coherent enough to make computational sense,
expressive enough that it is not merely a burden to programmers, yet
not so complex that it boggles the mind.

@subsubsection{Some Historical Perspective}

Historically, the first language with classes, Simula,
was a language with static types based on Algol,
and offered both static and dynamic dispatch—years before OO was even conceptualized.
But the first languages that fully conceptualized OO,
Smalltalk and KRL (an extension to Lisp)
were dynamic languages that only offered dynamic dispatch (KRL had Prototype OO);
the dynamic aspect was instrumental in enabling the experimentation
that led to the invention of modern OO and a lot of its features.

C++, partly inspired by Simula, and interested in system programming,
leaned heavily into the static dispatch approach, but kept dynamic dispatch as an option.
C#, heavily inspired by C++ as well as by Java, followed.
These languages, with large corporate backing,
evolved over many decades to slowly acquire better features,
until their typesystems became quite expressive.
As of late, they are even capable of expressing functional programming in somewhat ergonomic ways,
when they were initially incapable of it.
These languages also raise the bar quite high for any new object-oriented language
that would try to bet on static dispatch:
One must put up big antes just to develop a typesystem that can match those of C++ or C#,
in addition to which one must somehow innovate to become
ten times better along some meaningful dimension
to surpass them and have a chance at adoption worth the investment.
Such requirements mean that there is less experimentation and less diversity
among languages with static dispatch, and therefore more ecosystem fragility
when suboptimal decisions are made, and when corporate support eventually dries up.
On the other hand, the power concentration also means that more resources are poured
(albeit inefficiently) into improving those few languages with static dispatch
than into any given language with dynamic dispatch only.

It remains to be seen whether AI, by massively lowering the cost of implementing known features,
will increase inertia in favor of these static-dispatch Class OO incumbents,
or will level the playing field in favor of new languages, static or dynamic.

@section{Maximal OO}

I started my formalization of OO with Minimal OO (@secref{MOO}):
a minimal first-class functional model of OO.
It was minimal in the sense of being the simplest that explains OO to a functional programmer.
A language can be even less capable and still meet the minimal requirements for supporting OO:
it would have second-class Class OO with static dispatch only, not even dynamic dispatch.
Thus, just because a language supports a minimally capable form of OO
does not make its users somehow magically enjoy the benefits of the Minimal OO model,
much less those of the most advanced forms of OO.

With this chapter, I have instead tried to suggest Maximal OO:
I have presented many of the most advanced forms of OO,
and included a wide variety of concepts that stretch OO to its known limits,
hinting at their diversity.
I have shown how they work well together to increase modular extensibility.
And I have offered simple yet general formalization techniques for these features.

I do not claim to have built a @italic{ne plus ultra} of OO—after all,
I have made many simplifications as well as generalizations
compared to earlier systems like CLOS.
Actually, I don’t believe there is a @italic{ne plus ultra} of OO;
there are only diminishing returns to increasing the sophistication of OO infrastructure:
the more advanced features only pull their own weight if successfully used
to rein in the complexity of large enough code bases;
yet with even larger and more complex code bases,
still further helpful OO features could be devised.

My main contribution in this chapter was to show how
@principle{you can algebraically decompose and recompose, nest and extract
ever more powerful OO features out of small building blocks}:
(a) open modular extensions (ModExts) that you can refocus with skew lenses,
enabling you to edit your semantics in arbitrarily fine ways, and
(b) prototypes with optimal inheritance (POIs), coarser-grained units
that are lensable without skew and offer manageable handles
onto which to attach richer inheritance structures@xnote["."]{
  A prototype’s underlying modular extension has a fixpoint
  precisely because the target is simultaneously the focus value and modular context,
  thus appropriate for a monomorphic lens;
  extending the prototype non-trivially may involve a polymorphic lens.
  These two prototypical prototype operations would not be possible
  if the lenses involved non-trivial skew.
  And prototypes are also the locus at which
  efficient implementation techniques apply (@secref{EOI}).
}
Thus, my presentation of OO is maximal not because it includes every imaginable feature,
but because it offers a framework to keep expanding the scope of OO itself.

@exercise[#:difficulty "Easy"]{
  Read and make sense of the example code I developed for this chapter,
  that you may find e.g. at
  @url{https://github.com/metareflection/poof/blob/main/pommette/pommette.scm}.
  Or to make things harder, first try as many of the exercises as possible
  without reading my code.
  Note that pommette sometimes already includes variants that diverge slightly from the book,
  with some additional features.
}

@exercise[#:difficulty "Easy"]{
  Implement the @emph{product} of two (or three, or more) lenses,
  that allows view and update of a pair (or list) of data each based on its lens.
  Implement @c{car-lens}, @c{cdr-lens} to access the two parts of a @c{cons},
  or the equivalent lenses for a pair in your language,
  then @c{list-first-lens}, @c{list-second-lens}, @c{list-third-lens} as used in @secref{PS},
  or the equivalent in your language to access the components of a triplet,
  whichever way you encode specifications for optimal inheritance.
}

@exercise[#:difficulty "Easy"]{
  Implement semantic and syntactic helpers for defining classes and their instances:
}
@itemize[
  @item{
    A function @c{poi-add-parent} that takes a parent and adds it at the very end
    of a poi’s local precedence order.}
  @item{
    A function @c{class←poi} that ensures a poi inherits from @c{base-class}.}
  @item{
    A macro @c{defclass} that defines a variable as a poi
    that always implicitly inherits from @c{base-class}.}
  @item{
    A function @c{instance←class} that given a class and a poi
    returns a new poi wherein the given poi extends the base-instance of the given class.}
  @item{
    A function @c{make-instance} that, given a class and the rest of its arguments as a plist
    of field name and field value, creates an element of the class wherein the respective
    given fields are bound to the given values, and other fields are as specified by
    the class’s @c{base-instance}, e.g. @c{(make-instance class 'x 1 'y 2)}.}
  @item{
    An extension to @c{base-class} and @c{make-instance}
    that includes user-specified validation checks.}
  @item{
    A function @c{mandatory-fields} that given a class, returns a list of the mandatory
    fields for the class—those without initialization data.}
  @item{
    A function @c{class-constructor} that given a class and as many arguments as there are
    mandatory fields (curried the usual way), in order, constructs an element of the class
    where each mandatory field has the value given from the arguments, in order.}
  @item{
    A memoization mechanism, so that base-instance, or
    instances generated by participating functions, do not generate
    lots of new equivalent objects at every invocation with empty or otherwise equal keys.}
  @item{
    A dispatch-on-type mechanism so you can have multiple dispatch in Class OO,
    rather than only in Prototype OO.}]

@exercise[#:difficulty "Easy"]{
  Write efficient implementations of the missing simple CLOS method combinations,
  for those of @c{+ * max min progn list append nconc or and} that weren’t implemented yet.
  Hints: @c{progn} is just the Lisp operator for sequential evaluation of expressions,
  returning the value of the last one. @c{nconc} is a variant of @c{append} that uses side-effects;
  and @c{append} and @c{nconc} done wrong
  will be quadratic rather than linear so be careful@xnote["."]{
    For @c{min} and @c{max}, note the existence
    of IEEE floating point numbers @c{+inf.0} and @c{-inf.0},
    as plausible values to return when no method is defined, though beware that
    using them may on some Scheme implementations cause undesired coercion to flonum,
    and so does not work quite as it should with integers.
    I therefore do not recommend using them implicitly
    over issuing an error as CLOS does in this case.
    Users can always explicitly include such a value as a base method
    if it works for them, e.g. because they are using flonums, anyway.
    Or they can define their own variant of @c{min} or @c{max}
    and corresponding method combinations that will avoid coercing the result.
}}

@exercise[#:difficulty "Easy"]{
  Implement generic algebra, wherein the objects you manipulate can be integers,
  rationals, floating point numbers, or vectors or matrices of the same.
  For each relevant pair of suitably wrapped objects, use multiple dispatch to define
  addition and multiplication.
  Do the exercise in class style, and again in typeclass style.
  Which style is more amenable to generating high performance code
  (assuming enough inlining and optimization in the compiler)?
}

@exercise[#:difficulty "Medium"]{
  Traversals are a generalization of lenses that can focus on any number of elements,
  whereas a lens focuses on one and only one element.
  Read about traversals, then implement a traversal that focuses
  on all of a prototype’s ancestors.
  Bonus: assuming you implemented some reflection on which modular extension implements
  or overrides which methods (or implementing it),
  use the traversal to apply a transformation that
  systematically intercepts all method calls, and maintains a table
  of which methods of which extensions are effectively called how many times
  during a program execution.
}

@exercise[#:difficulty "Medium"]{
  Implement optional validation and normalization for class elements.
  The default method will run validation checks on each field value.
  For each field with a defined validation check, check the field.
  The @c{make-instance} function will run those checks.
}

@exercise[#:difficulty "Medium"]{
  In a few lines of code, define a method combination that implements
  the concatenation semantics of Simula, and its “inner” keyword.
  Implement it purely with functions, and optionally use macros so the syntax
  is closer to the original.
  Optionally, implement the semantics of BETA instead of that of Simula,
  or in addition to it@xnote["."]{
    Note that in the case of BETA, much of the difficulty is in
    understanding its semantics to begin with,
    based on the @italic{sui generis} nature of the little available documentation.
    See my notes on @citet{Kristensen1987}, or
    ask AI for help understanding BETA and its documentation.
}}

@exercise[#:difficulty "Medium"]{
  Implement method caching for generic functions:
  the generic function maintains an LRU cache of the last 8 times it was called,
  on what (tuples of) specifications it was called,
  and what effective method resulted.

  Harder: use macros to instead (or additionally) implement
  a 4-deep LRU cache of effective methods @emph{per (dynamic) call site}
  of a generic function.
}

@exercise[#:difficulty "Medium"]{
  Discuss how you would use flavorful multiple dispatch to implement
  a generic protocol to display objects onto a terminal.
  Which arguments would you have in which order, and why?
  Consider: the object to display itself, output display port being used
  that can be of many kinds (window system, text terminal, text file stream, binary stream, etc.),
  some “descriptor” for the many options with which to interpret or decode the object encoding
  (unit and bounds for a number, language used, entering digits vs sliding a ruler
  or turning a knob, tying the number to some other visible effect, etc.).
}

@exercise[#:difficulty "Medium"]{
  Implement the richer kinds of skew lenses fit for optimal inheritance, as per @secref{FME},
  and use them to define more interesting prototypes than possible with mere modular extensions.
}

@exercise[#:difficulty "Medium, Recommended"]{
  If you did exercise @exercise-ref{08to09}, compare
  your attempt at explaining these advanced OO topics with how I did.
  What aspects did you anticipate? What surprised you?
  What did you do better or worse?
}

@exercise[#:difficulty "Hard, Recommended" #:tag "09to10"]{
  Think about how to @emph{efficiently} implement objects.
  Also think about how to implement them in a @emph{flexible} way,
  so you can offer options to your users as to what semantics they want to use exactly,
  including as many of the features mentioned in this book as possible (or omitted!).
  Are efficiency and flexibility in harmony or in conflict?
  What mechanisms will you need to expose to maximize both expressiveness and performance?
  Write down your answers before you read the next chapter.
}

@exercise[#:difficulty "Hard"]{
  Implement a pure functional monadic variant of the standard method dispatch.
  How does the object system need to be extended (if at all)
  to support pure monadic operations that can fully replace stateful OO?
}

@exercise[#:difficulty "Hard"]{
  Start from my implementation of multiple dispatch, or one you wrote yourself, and
  implement @c{eql} specializers and predicate dispatch on top of it.
}

@exercise[#:difficulty "Hard"]{
   Give methods a full @emph{calling convention} that abstracts over
   how a function can take arguments beyond the (first) receiver:
   This calling convention records an @emph{arity} for curried or uncurried
   mandatory arguments, for optional arguments,
   flags for whether rest arguments are supported,
   or how optionals and currying interact with each other
   (for extra difficulty, also support some form of keyword arguments).

   Then, any given arity should support a pair of methods:
   First, an @emph{accepter} that gathers all the call’s arguments into a list—one at a time,
   if curried, all at once if uncurried, and anywhere in between if you dare—then
   invokes a continuation on said list.
   Second, an @emph{invoker} that applies a function supporting that arity
   with a list of arguments as previously returned by the accepter.
   Reimplement generic functions to support both method combinations and multiple dispatch
   based on such abstract arity.

   As a bonus, also implement the CLOS feature whereby @c{call-next-method}
   called with one or more arguments (as opposed to without argument)
   will accept arguments as per the accepter, then replace the current list of arguments
   with the given list. For extra points, validate the new list,
   by checking that its elements have the same classes as those of the old list,
   and/or lead to the same set of methods.
   For yet extra brownies, accept the arguments when
   only the “tail” of the list of methods is the same,
   with that notion of “tail” depending crucially on the method combination.
}

@exercise[#:difficulty "Hard"]{
  Can you retroactively add methods to a generic function?
  In a pure functional way? With eager evaluation? With lazy evaluation?
}

@exercise[#:difficulty "Hard"]{
  Determine whether the scheme for nested POI inheritance suggested in @secref{IoNaI}
  matches BETA’s polymorphic families with single inheritance,
  in the case where POIs are all suffix specifications.
  Determine whether it matches whatever Newspeak does for nested classes
  with the mixin extension pattern
  when the inheritance DAG is a tree you can flatten into a list of mixins.
  Exhibit either formal proofs of equivalence, or counter-examples.
}

@exercise[#:difficulty "Research"]{
  Implement a programming language with static types that supports
  optimal inheritance, method combinations and multiple dispatch.
  You may start from @~cite{Allen2011} for the typesystem.
}
