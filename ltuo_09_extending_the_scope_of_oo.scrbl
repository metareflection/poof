#lang scribble/base
@; -*- Scheme -*-
@(require "util/ltuo_lib.rkt")
@(set-chapter-number 9)

@title[#:tag "EtSoO"]{Extending the Scope of OO}
@epigraph{
  The psychological profile [of a programmer] is mostly
  the ability to shift levels of abstraction, from low level to high level.
  To see something in the small and to see something in the large.
  When you’re writing a program, you’re saying, ‘Add one to the counter,’
  but you know why you’re adding one to the counter.
  You can step back and see a picture of the way a process is moving.
  Computer scientists see things simultaneously at the low level and the high level.
    @|#:- "Donald Knuth"|
}
@section{Optics for OO}
@; https://golem.ph.utexas.edu/category/2020/01/profunctor_optics_the_categori.html
@; Profunctor Optics: The Categorical View

@subsection{Focused Specifications}

Before I may revisit familiar features of advanced OO systems such as
accessors, method combinations or multimethods, I must once again
introduce some new elementary concept that will much simplify their formalization:
@emph{focused} specifications.

A specification, whether modular definitions, modular extensions,
multiple inheritance specifications, optimal inheritance specifications, etc.,
can be @emph{focused} by enriching it (via conflation or explicit product)
with two access paths:
a path from the top of the program state to the module context being referenced, and
a path from the top of the program state to the method being extended.
Thus, instead of being “free floating”, your specification will be “located”
within the context of the greater program state.
Furthermore, to keep formalizing OO features in terms of pure functional semantics,
these access paths I will formalize as functional @emph{lenses} (see below).

This approach I propose to specifying OO software is a potential game-changer
in making OO even more modular than it used to be, because now
method specifications can now be considered individually,
then grouped incrementally into larger algebraically coherent chunks,
and at each step, parsed, defined, typed, analyzed, proven correct, and generally reasoned about,
at whichever granularity they make sense—whereas before then,
you couldn’t even start to parse a definition, much less reason about it,
until after it was part of a potentially very large class, involving more semantic context
than can safely fit in anyone’s ability to reason without mistake.

Before I discuss new features, I will start with showing how focused specifications
can simplify the formalization of individual classes or prototypes within an ecosystem,
or of regular methods within a prototype.
Having to introduce lenses means the notion of focused specification
was overkill and a distraction in the formalism of previous chapters,
when handwaving the relationship between open and closed modular extensions was good enough.
But since I am going to pay the price anyway for the sake of explaining advanced features,
I may as well enjoy the benefits for basic features as well.

@subsection{Short Recap on Lenses}

A lens @~cite{bananas1991 Foster2007CombinatorsFB oconnor2012lenses Pickering2017Optics}
is the pure Functional Programming take on what in stateful languages would typically be
a C pointer, ML reference, Lisp Machine locative, Common Lisp place, etc.:
a way to pin-point some location to inspect and modify within the wider program’s state.
A lens is determined by a “view”, function from a “source” to a “focus”;
and an “update”, function from a change to the focused data to a change
of the wider state from “source” to an updated “target”@xnote["."]{
  In more stateful languages, a more popular view is that of
  a pair of a “getter” and a “setter”;
  this maps well to lower-level primitives of stateful languages.
  But these variants do not compose well in a pure functional setting,
  where composability is a good sign of good design,
  while lack of composability is a strong “smell” of bad design.
  In a pure functional setting, the “getter” part is fine (same as view),
  but the “setter” part drops crucial information about the flow of information,
  and does not compose well, instead requiring the flow of information to be
  be awkwardly moved to other parts of the program to be reinjected into the setter.
  By contrast, the “update” API trivially composes.
  In the Haskell lens libraries, the “update” function is instead called “over”;
  maybe because it “applies an update @emph{over} a change in focus”;
  maybe also because the word “update” was taken by other functions;
  it’s not a great name. We’ll stick to “update”.
}
As a function from source to focus and back, it can thus also be seen as generalizing
paths of fields and accessors, e.g. field @c{bar} of the 3rd element of field @c{foo}
of the entry with key @c{(42, "baz")} within a table.

@Paragraph{Monomorphic Lens}
A monomorphic lens (or simple lens), can be seen as
a pair of a view function @c{s → a}, and an update function @c{(a → a) → s → s}.
The view function allows you to get a current “inner” value under focus, of type @c{a},
from the “outer” context, of type @c{s}.
The update function allows you to see how a local change in the “inner” value under focus
transforms the “outer” context being focused:
@Code{
type MonoLens s a =
       { view : s → a ; update : (a → a) → s → s }
}

@Paragraph{Polymorphic Lens}
A polymorphic lens (or “stabby” lens), of type @c{PolyLens s t a b}, generalizes the above:
you still have a view function @c{s → a} to extract an inner value from the outer context,
but your update function now has type @c{(a → b) → s → t},
so that the inner change in value can involve different input and output types,
and so can the outer change in context.
But the starting points of the update function are the same as
the start and end points of the the view function,
so that you are updating the same thing you are viewing.
Monomorphic lenses are a special case or polymorphic lenses.
@Code{
type PolyLens s t a b =
       { view : s → a ; update : (a → b) → s → t }
type MonoLens s a = PolyLens s s a a
}

@Paragraph{Skew Lens}
A skew lens (or “irpjsq” lens, pronounced “earp jusq”), further generalizes the above:
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

@Paragraph{View and Update}
I can also give separate types for View and Update:
@Code{
type View r s = s → r
type Update i p j q = (i → p) → j → q
type SkewLens r i p s j q =
  { view : View r s ; update : Update i p j q }
}

@Paragraph{Getter and Setter}
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

@Paragraph{Composing Lenses}
I can compose view, update and lenses as follows,
with the obvious identity lens:
@Code{
compose-view : View s t → View r s → View r t
(def (compose-view v w)
  (compose w v))

compose-update : Update i p j q → Update j q k r → Update i p k r
(def compose-update compose)

make-lens : View r s → Update i p j q → SkewLens r i p s j q
(def (make-lens v u)
  (extend-record 'view v
    (extend-record 'update u
      empty-record)))

compose-lens : SkewLens s j q ss jj qq → SkewLens r i p s j q →
                 SkewLens r i p ss jj qq
(def (compose-lens l k)
  (make-lens
    (compose-view (l 'view) (k 'view))
    (compose-update (l 'update) (k 'update))))

id-lens : SkewLens r i p r i p
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
Views, Updates, Lenses are categories, wherein composition is associative,
and identities are neutral elements.


@Paragraph{Field Lens}
Given some record representation, a view for a field of identifier key @c{k}
is just a function that returns the field value @c{r.k} for given as argument the record @c{r},
whereas an update gives you a change in record given a change for that field.
More sophisticated representations will have more sophisticated lenses,
but here is what it looks like in my trivial representation of records
as functions from identifiers to value, where @c{r.k = (r 'k)}:
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
you can apply @c{(field-lens* 'foo 'bar)} to @c{x}.
Note that the order of lenses in @c{field-lens*} is covariant with
the usual notation @c{x.foo.bar}.
Some syntactic sugar could help you achieve a similar notation, too,
but that would require implementation-dependent extensions to Scheme.

A @c{(field-lens key)} can be a simple lens of type @c{MonoLens s a}
when applied to a record of type @c{s} that has a field @c{key} of type @c{a}.
But it can also be used as a polymorphic lens or skew lens,
where you view the field @c{key} of your context and
also modify the field @c{key} of the value you extend,
but the two need not be the same, and the modification need not preserve types.

@subsection{Focusing a Modular Extension}

@Paragraph{Skewing a Modular Extension}

You may have notice that I used the same letters @c{r i p}
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

@Paragraph{Metaphors for Modular Extensions and Skew Lenses}

A modular extension can be conceived as a @emph{sensactor}:
it has a sensor, the input from the module context,
and an actuator, the output of an extension to the value under focus.

A single skew lens can change both the module context, and the extension focus.
A @c{SkewLens r i p s j q} can transform an inner @c{ModExt r i p}
into an outer @c{ModExt s j q}.
As always, note that in general, @c{r} (required, the module context)
is largely independent from @c{i p} (inherited and provided, the extension focus).
They only coincide just before the end of the specification,
to obtain a closed modular definition you can resolve with a single use of the Y combinator.

One way to think of a skew lens is as similar to the pair of a periscope
through with a submarine pilot may look outside,
and the wheel or yoke through which they may pilot their boat:
the submarine pilot is the sensactor, and the skew lens
transforms the I/O loop of the pilot into the I/O loop of the submarine.
Similarly, one may consider the laparoscope that a surgeon may use,
and the separate instrument with which he will operate the patient:
the “skew lens” transforms the surgeon I/O into the instrument I/O.

It is a common case that the actuator is within the frame of the sensor,
such that the sensactor may observe not only what they are modifying, but also a wider context.
In formal words, there are two polymorphic lenses @c{l} and @c{k}
such that the skew lens view is @c{(l 'view)}, and the skew lens update is
@c{(compose-lens k l 'update)}, further specializing the previous view.
But that is not necessary in the general case.
Just like competent painters can put paint on canvas while looking at their model,
not at their hand, the “skew lens” does not imply that the hand is seen by the eye,
that the actuator is somehow visible in the frame of the sensor.
The actuator may involve a bigger extension focus (heating the whole room, not just the thermometer),
or an extension focus completely disjoint from the context being observed
(like a caricaturist reproducing features observed in his model,
but in an exaggerated style).

Now, inasmuch as you consider those function types as a model for stateful mutation
with in-memory pointers, that means that the “read pointer” @c{r}
and the “(re(ad)-write) pointer” @c{p} (with its “initial value” @c{i}) are independent;
in a mutable variant of a skew lens, you will have two pointers,
not just one as with monomorphic or polymorphic lens.

@Paragraph{Focused Specification}

A @emph{focused specification} will be the datum of a skew lens and a specification.
Above, the specification was a modular extension;
but in general, it may as well be a modular definition,
a multiple inheritance specification, an optimal inheritance specification, etc.,
depending on the kind of specification you’re interested in.

The skew lens says what the specification is modifying exactly,
relative to a known place—typically the entire ecosystem, but
potentially any other place you may currently be looking at.

@;{
I will call @emph{specification focus} the datum of a skew lens
to the complete or somehow outermost ecosystem of type @c{ES}:
@Code{
type SpecFocus r i p = ModExt r i p → ModExt ES ⊤ ES
}
More generally, you could use any answer type @c{A}, or even @c{⊥},
and consider that a SpecFocus is just a continuation that consume a specification
@Code{
type SpecFocus r i p = ModExt r i p → A
}

A specification focus is the context for a specification or focused specification:
fit a specification into it, and you get your program.
The general case above is an @emph{open} specification focus;
a @emph{closed} specification focus is of the form:
@Code{
type ClosedSpecFocus a = ModExt p ⊤ p → A
}}

@subsection{Adjusting Context and Focus}

@Paragraph{Adjusting both together}

A monomorphic lens, or simple lens, can refocus a closed specification focus
into another closed specification focus, such that a local closed specification
@c{ClosedSpec a} can be turned into a global closed specification for the complete ecosystem,
@c{ClosedSpec ES}. Thus, when specifying a value @c{foo.bar} in the ecosystem,
you will use @c{(compose-lens (field-lens 'foo) (field-lens 'bar))} as your @c{SpecFocus}.

As is a theme in this book, though,
and which was never discussed before in the OO literature,
the interesting entities are the @emph{open} specifications, not just the closed ones.
This is what makes skew lenses interesting.
If you considered only closed specifications, you would only consider monomorphic lenses
(or maybe polymorphic lenses when you specifically want to distinguish types
for the ecosystem before and after extension).
But then, you wouldn’t be able to formalize the advanced notions
I am going to discuss in the rest of this chapter.

@Paragraph{Adjusting the Extension Focus}
Given a focus on a specification
one can focus on a specific method of that specification
by further adjusting the extension focus using @c{u = (field-update key)}
where @c{key} is the identifier for the method.
Thus, @c{(compose-lens (field-lens* 'foo 'bar) (update-only-lens (field-update 'baz)))}
or equivalently @c{(update-lens (field-lens* 'foo 'bar) (field-update 'baz))}
will let you specify a method @c{baz}
for the specification under @c{foo.bar} in the ecosystem, where:
@Code{
update-only-lens : Update i p j q → SkewLens r i p r j q
(def (update-only-lens u)
  (make-lens identity u))

update-lens : SkewLens r i p s j q → Update j q jj qq →
     SkewLens r i p r jj qq
(def (update-lens l u)
  (make-lens (l 'view) (compose (l 'update) u)))
}

More generally, given a lens @c{l} to focus on the specification,
and a lens update @c{u} to refocus on just the extension focus,
the lens @c{(compose-lens l (update-only-lens u))} will up focus on the method at @c{u}
of the specification at @c{l}.
This is a common case when specifying
a sub-method in a methods (more on that coming),
a method in a specification,
a specification in a library,
a library in the ecosystem—all while keeping the broader entity
as context when specifying the narrower one.

@Paragraph{Broadening the Focus}

At times, you may want to make the focus broader than the module context.
Then, you can use a lens with “negative focal length”:
instead of narrowing the focus to some subset of it,
it broadens the focus.
Thus you can zoom out rather than only zoom in.
Zooming out can take you back to were you were previously,
or to a completely different place.

For instance, given a broader context @c{c : s},
and a lens @c{l : MonoLens s a}, then the following “reverse lens” broadens the focus,
by completing the “rest” of the reverse focus with data from the context.
Beware though that if you use the update more than once, you will always get answers
completed with data from the same non updated context.
If you want to update the context each time, you have to reverse the lens
with the updated context every time.
@Code{
reverse-view : s → MonoLens s a → View a s
reverse-update : s → MonoLens s a → Update a s a s
reverse-lens : s → MonoLens s a → MonoLens a s

(def (reverse-view s l a)
  (setter←lens l a s))
(def (reverse-update s l f a)
  (l 'view (f (reverse-view s l a))))
(def (reverse-lens s l)
  (make-lens (reverse-view s l) (reverse-update s l)))
}

@Paragraph{Adjusting the Context}

The module context contains the data based on which a modular extension may compute its extension.
Sometimes, you may want to narrow the context, to match the already narrowed extension focus;
or to broaden the context to the next broader entity;
or to locally override some configuration in the context;
or to locally instrument some of the context entities
(e.g. for debugging, testing, profiling performance, etc.);
or just to switch the context to something different for any reason.
You can also use the getter to narrow the context in terms
of visibility, access rights, or some other system of permissions or capabilities,
so the extension may only invoke safe primitives,
or primitives that were suitably wrapped in a “security proxy” for safety.
Or you can focus the context on one object to specify extensions for another object
that somehow “mirrors” or reacts to the object in context,
or logs its history, backs up or persists its data, does resource accounting, etc.

To adjust the context without adjusting the extension focus, use:
@Code{
view-only-lens : View r s → SkewLens r i p s i p
(def (view-only-lens v)
  (make-lens v identity))

view-lens : SkewLens r i p s j q → View rr r →
    SkewLens rr i p r j q
(def (view-lens l v)
  (make-lens (compose-view (l 'view) v) (l 'update)))
}

@subsection{Optics for Specifications and Prototypes}

So far the only primitive lens I showed was the field lens.
Here are two kinds of lenses that are essential to deal with prototypes and classes.

@Paragraph{Specification Methods}

As told in the previous section,
given a Lens @c{l} to focus on a specification from the environment,
and an Update @c{u} to focus the extension on a method or submethod within that specification,
one can extend that method of that specification with a modular extension @c{m}, with:
@Code{
(skew-ext (update-lens l u) m)
}
For instance, to move 50 pixels to the right a widget registered under the name “foo”,
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

@Paragraph{Prototype Specification}

I’ll assume for now that prototypes are records implemented with the @c{rproto} encoding
from @secref{CfR}. Then, if you have a lens @c{l} to focus onto a prototype,
you may further focus on the prototype’s specification by further composing @c{l}
with the following lens, after which you can further use lenses
to modularly extend the specification methods as above:
@Code{
(def rproto-spec-view spec←rproto)
(def rproto-spec-setter rproto←spec)
(def rproto-spec-lens
  (lens←getter*setter rproto-spec-view rproto-spec-setter))}

The entire point of @c{rproto} is that the target view is @c{identity}.
However, what the target update should be is an interesting question.
There are many options; none of them seems universally correct;
any of them can be a feature or a bug, depending on user intent, but is often a bug;
and so the error behavior is probably the safest one to use by default:
@itemlist[
  @item{If you update some fields of the target, then the target will not be in sync
    with the specification anymore unless the user keeps it so the hard way.
    If some further program extends that prototype, it will restart from the specification,
    and ignore any update otherwise made to fields.}
  @item{If you try to have an update function that arbitrary changes the specification
    to be @c{constant-spec} that constantly returns the current state of the record,
    then the result remains extensible, but in a way that forgets the formulas,
    only remembers the current values.}
  @item{If you update the target then the magic specification field will be erased by default,
    and the object will not be extensible through inheritance anymore,
    unless you make it so again the hard way by explicitly providing a new magic specification field.}
  @item{If you try to update the target, an error will be thrown,
    and you won’t have to debug very hard surprises later.}]
To a first approximation, this corresponds to using variants of these Update functions:
@Code{
(def rproto-target-update/OutOfSync
  identity)
(def rproto-target-update/OverwriteSpec
  rproto←record)
(def rproto-target-update/NoMoreSpec
  (extend-record #f #f))
(define rproto-target-update/Error
  abort)
}

@subsection{Optics for Class Instance Methods}

Inasmuch as classes are prototypes, the way to deal with methods on a class
are sensibly the same as for prototypes, or a refinement thereof.
To define more precise lenses,
I’ll further assume the encoding of @secref{SFCTD},
wherein you use @c{instance-call} to call an instance method,
that extracts the type descriptor using @c{type-of},
that it then invoked with @c{'instance-methods}, the @c{method-id}, and the element.

To modularly extend it a class instance method, one needs first
focus on it, by composing a lens focusing on a prototype for a type descriptor
with an @c{instance-method-lens} below, to obtain
a skew lens for a specific instance method:
@Code{
(def (instance-method-lens method-id)
  (update-lens rproto-spec-lens
    (compose-update (field-update 'instance-methods)
                    (field-update method-id))))
}

Now, when specifying a class instance method, the programmer thinks in terms of
the class instance, i.e. an element of the class’s target type.
And his method may use @c{call-next-method} to invoke the inherited behavior,
from the tail of class precedence list of the instance’s type.
However, the underlying modular extension machinery sees @c{self}, i.e. the class, and
and @c{super}, the modular definition for the method so far,
as inherited from the tail of class precedence list, i.e. ancestors behind the current class
in the precedence list of the type of the original instance used as first argument.
I need a wrapper to bridge these two views, and at this point,
a formal definition is simpler than the words that describe it:
@Code{
(def (instance-method-spec method-id method-body)
  (skew-ext (instance-method-lens method-id)
    (λ (inherited-method _self element)
      (λ args
        (method-body element
          (make-call-next-method
            inherited-method element args))))))
(def (make-call-next-method inherited-method element args)
   (case-lambda
     (() (apply (inherited-method element) args))
     ((new-element . new-args)
        (apply (inherited-method new-element) new-args))))
}
The @c{_self} argument is ignored,
because the @c{method-body} can presumably extract the type from the class instance.
In typeclass-style, it might instead be passed to the @c{method-body}.
Note how the second clause of the @c{case-lambda} above implements
an advanced use of @c{call-next-method} not available in all languages,
wherein you can provide an updated instance and updated arguments.
In a pure language, it may be more important than in an effectful language
to be able to call the next method with an updated instance and updated arguments.
However, there are semantic constraints on what new element and new arguments
can be safely used with the inherited method;
some object system authors might not be ready to either enforce those constraints
or deal with programs that break them at runtime, and may choose
not to offer this advanced use case to their users.

As an example application, a method @c{area} may be declared on a class @c{Rectangle}
with the following definition:
@Code{
(def (base-instance-method-spec method-id method-body)
  (instance-method-spec method-id
    (λ (element _call-next-method) (method-body element))))

(base-instance-method-spec 'area
  (λ (element)
    (* (element 'width) (element 'height))))
}
The area method does not need a @c{call-next-method},
since it computes @c{area} directly,
so I use a simpler @c{base-instance-method-spec} that wholly omits that argument.

A @c{BorderedRectangle} that inherits from @c{Rectangle} might add border thickness with:
@Code{
(base-instance-method-spec 'area
  (λ (element)
      (let ((border (* 2 (element 'border-width))))
        (* (+ (element 'width) border)
           (+ (element 'height) border)))))
}
Meanwhile, a @c{ScaledShape} mixin might instead use @c{call-next-method}:
@Code{
(instance-method-spec 'area
  (λ (element call-next-method)
    (* (element 'scale-factor) (call-next-method))))
}

Classes, like prototypes in general, can thus be defined incrementally,
by assembling or composing plenty of such focused specifications;
and using lenses and sensactors to programmatically select each time
how and where they fit in the bigger picture.
You can give types to method specifications independently
from any specific surrounding prototype or class specification.
You can reuse these specification as part of multiple prototype specifications.
You can transform them, store and transmit them, compose them, decompose them, recompose them,
as first class objects.
And can build infrastructure that systematizes any design pattern you follow
in writing these specifications.

@; TODO EXAMPLES:
@; DEFINING A METHOD OUTSIDE A CLASS
@; COMBINING METHODS INTO DIFFERENT CLASSES
@; WRAPPING A METHOD INTO... A RENAMING?

@subsubsection{Simple Class Initialization}

One may specify the fields of a class instance,
by specifying individual field descriptors under a @c{instance-field-lens}:
@Code{
(def (instance-field-lens field-id)
  (update-lens rproto-spec-lens
    (compose-update (field-update 'instance-fields)
                   (field-update field-id))))
}
A simple field descriptor would be a record of an @c{init} modular extension to initialize it,
and other information such as whether the slot is mutable (in a language with side-effects),
type information (that could be static if the class is second-class, but will be dynamic here), etc.
I will stick to just the @c{init} protocol for now:
@Code{
(def (simple-instance-field-spec field-id init-mod-ext)
  (skew-ext (instance-field-lens field-id)
    (rproto←record (record (init init-mod-ext)))))
}
To initialize a field @c{parts} that is a list of parts,
defaulting to an empty list, you would use something like:
@Code{
(simple-instance-field-spec 'parts (constant-spec '()))
}
To initialize a field @c{price} that defaults as a baseline
to the sum of the prices of the parts times the contents of the field @c{markup},
you would use:
@Code{
(simple-instance-field-spec 'parts (λ (_inherited self)
  (* (self 'markup)
     (foldl + 0 (λ (part) (part 'price)) (self 'parts)))))
}
A field @c{markup} that has no default initializer must be provided by users could be defined as:
@Code{
(simple-instance-field-spec 'parts (λ (_inherited _self)
  (abort "missing field markup")))
}
A class could then define a default prototype for new instances as:
@Code{
(skew-ext (update-lens rproto-spec-lens (field-update 'new-instance-prototype))
  (λ (_inherited self)
    (rproto-mix*
      (apply mix*
        (map (λ (slot)
             (compose (field-update (slot 'name))
                      (slot 'init-spec)))
           slots)))))
}

A class descriptor holds a list of slot descriptors
and builds the instance prototype by focusing and mixing:
@Code{
(def (class-proto slots)
  (rproto←spec
    (mix*
      (map (λ (slot)
             (compose (field-update (slot 'name))
                      (slot 'init-spec)))
           slots))))

(def (default-slot name value)
  (record (name name)
          (init-spec (constant-spec value))))

(def (computed-slot name thunk)
  (record (name name)
          (init-spec (λ (super self) (thunk self)))))

(def (required-slot name)
  (record (name name)
          (init-spec (λ (super self)
                       (error "Missing required slot" name)))))
}

Each slot's @c{init-spec} is focused onto its field
by composing with @c{field-update},
then all are mixed together.
The @c{fix-record} closes the recursion,
yielding a prototype where each slot is initialized
according to its descriptor.

For example, a @c{Rectangle} class with width, height, and computed area:
@Code{
(def rectangle-slots
  (list
    (default-slot 'width 10)
    (default-slot 'height 20)
    (computed-slot 'area
      (λ (self) (* (self 'width) (self 'height))))))

(def rectangle-proto (class-proto rectangle-slots))
}
@Code{
(expect
  (rectangle-proto 'width) => 10
  (rectangle-proto 'height) => 20
  (rectangle-proto 'area) => 200)
}

Extending the class is just adding more slots to the mix:
@Code{
(def colored-rectangle-slots
  (cons (default-slot 'color "black") rectangle-slots))

(def colored-rectangle-proto (class-proto colored-rectangle-slots))
}
@Code{
  (colored-rectangle-proto 'color) => "black"
  (colored-rectangle-proto 'area) => 200)
}

The entire class definition reduces to composing focused modular extensions—
the same pattern used for methods, now lifted to slot initialization.Claude is AI and can make mistakes. Please double-check responses. Opus 4.5Claude is AI and can make mistakes. Please double-check responses.Share


@; TODO section on multiple and optimal inheritance in this context

@section[#:tag "MC"]{Method Combinations}

@subsection{Win-Win}

Another fantastic contribution from Flavors @~cite{Cannon1979} is Method Combinations:
the idea that the many methods declared in partial specifications
are each to contribute partial information that will be harmoniously combined (mixed in),
rather than complete information that have to compete with other conflicting methods
that contradict it, the winners erasing the losers.
Win-win interactions rather than win-lose, that was a revolution
that made multiple inheritance sensible when it otherwise wasn’t.

Flavors notably allowed regular or “primary” methods to be extended in subclasses with
“before” and “after” methods, respectively called before and after the primary method@xnote[","]{
  The semantics of before and after methods is quite similar to the concatenation semantics
  of Simula and Beta, except that the most-specific-first order of before methods
  and most-specific-last order of after methods is the opposite of the concatenation semantics
  of Simula. Of course, one could easily define a new method combination
  that uses the order of Simula, opposite of this default order for before and after methods.
  Note how the default order used by Lisp works better with the normal OO extension protocol
  adopted by everyone after Smalltalk.
}
that could setup and tear down resources, do logging or permission checking or resource accounting,
hold and release locks, etc.
Because these extension points are standard, both subclasses and clients can enjoy them
without the author of the original method of the original class having to foresee,
implement and advertise such an extension protocol, making the design modular.
But that was just the default “method combination”.

Flavors also allowed you to define methods using a “simple method combination”
that used an operator like @c{progn} (sequential execution), @c{and} or @c{or}
(logical conjunction or disjunction, with short-circuit evaluation)
to combine the results of each method, evaluated either in
most-specific-first or most-specific-last order, as specified by the programmer.
Typical other operators included @c{+ * max min progn list append nconc}@xnote["."]{
  @c{nconc} is a historical variant of @c{append} that uses side-effects
  to modify in place each non-empty list but the last, to link to the next one.
  It made sense in the slow and memory-constrained machines of the 1960s to 1980s,
  especially so before modern garbage-collection.
  But @c{nconc} rarely makes sense in modern days,
  where either the simpler and safer @c{append} is good enough,
  or optimization is better sought from a more sophisticated data representation than linked lists.
}

The simplest case of method combination is actually
the usual composition of modular extensions,
wherein each extension can refer to its @c{super} argument
along a multiple inheritance specification’s precedence list,
as discussed in @secref{MI}.
But I can do better, show how to implement all other method combinations
on top of this foundation@xnote["."]{
  Ironically, that’s the one kind of method combination @emph{not} present as a builtin
  in the original Flavors, while the more elaborate kind were already provided:
  in the default “daemon” method combination,
  only one primary method (from the most specific class) would be called,
  but @c{before} and @c{after} methods were also supported
  in the style of ADVISE @~cite{teitelman1966};
  @c{around} methods were only added in CLOS @~cite{Bobrow1988CLOS}.
  The simple method combinations were supported, again without @c{around} methods.
  But you could define arbitrary method combinations by providing a @c{wrapper} macro
  that computed the effective method from the ordered list of individual methods.
  And chaining methods through a @c{call-next-method} first argument would definitely
  have been possible.
  Still such a protocol was not provided in Flavors or its successors until
  it appeared in CommonLoops @~cite{Bobrow1986CommonLoops}.
}

Now, while the original method combinations of Flavors were quite capable,
method combinations were further refined by New Flavors, CommonLoops, and
most notably by CLOS @~cite{CLtL2 clhs AMOP Verna2023}.
My presentation will therefore be more directly inspired by CLOS than by Flavors.

@subsection{Uses of Method Combinations}

It would take a lot of space to reproduce and explain @italic{in extenso}
some real-world examples that motivate the use of method combinations:
they would probably have to be in Common Lisp (the only language that fully supports them),
with a quick introduction to the language, its I/O facilities,
and some existing development framework for network client/server or GUI programming.
A full introduction would then require programs large enough
that the advantage of OO in general and method combinations in particular justify
use of the feature instead of “just” inlining it away.

Still, I can point to ASDF @~cite{ASDF2 ASDF3}, the Common Lisp build system,
as a program that makes good use of method combinations,
and the source code of which is freely available, and well-documented.
I know ASDF because I was once its maintainer and completely re-wrote it several times.
CLIM, the Common Lisp Interface Manager,
a large GUI library descended from that of the Lisp Machines,
is also heavily object-oriented, with plenty of uses of method combinations;
but I am not as familiar with it.
Plenty of other examples abound in Quicklisp.

ASDF will define some general abstract classes that correspond to some protocol
(such as @c{component} and @c{operation}) that are gradually specialized
into subclasses and mixins (such as @c{source-file} and @c{downward-operation})
until concrete classes are defined (such as @c{cl-source-file} and @c{load-op}).
To each class (or pair of component and operation classes, see @secref{MD} below)
is associated one or two methods, sometimes more, that extend the inherited behavior.
In the end, each method call may combine the effects of half a dozen individual inherited methods,
depending on the classes of the arguments.

Thus, ASDF defines @c{:before} methods to validate invariants and issue errors
before the primary methods are called,
automatically setup some preconditions (like creating destination directories for output files),
or detect and record dependencies between actions.
ASDF defines @c{:after} methods to track down actions that were successfully completed
so they do not have to be taken again, or complete system-provided behavior
and check invariants after (re)initialization of some objects.
And ASDF defines @c{:around} methods to fixup the results of special cases,
setup or use caches around computations, adjust dynamic bindings around computations,
or detect circular dependencies between actions.
Other common uses of the standard method combination not present in ASDF include
logging (before, after or around) and permission checks
(usually before, sometimes around to check results are authorized).

Finally, for historical then backward-compatibility reasons, the ASDF methods
@c{component-depends-on}, @c{input-files} and @c{output-files}
use the standard method combination and manually append
the contents of @c{call-next-method} to their results;
but they would have better been written with the @c{append} method-combination,
which would have automated what is manually done through tedious convention.

These methods allow ASDF to be written in a very modular and extensible style, and
achieve in a few thousand lines of code (and a few more for Quicklisp)
what takes ten times more code in other languages,
all the while exposing an extension interface actually used by many extensions:
support for compiling and linking C, FORTRAN or Python code,
for automatic dependency detection, for character encodings beside UTF-8,
for deferred code in Lisp files, for file-local variables,
for conditional autoloading of systems, for parallel compilation, etc.

One of the authors of method combinations went on to invent
Aspect Oriented Programming (AOP) @~cite{Kiczales1997 Kiczales2001},
that applies the ideas of Teitelman’s advice and Cannon’s method combinations
to more languages.

Certainly, for each use method combinations,
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

In CLOS after Flavors, a method specification can be tagged with a @emph{method qualifier},
usually some kind of symbol, though, in many Lisp or Scheme dialects,
a “keyword” may be used that is somehow
distinct from regular symbols, often with a syntax involving a colon@xnote["."]{
  Depending on the language or dialect, keywords are typically written with a some variant of
  a colon @c{:before} (in Common Lisp, where they are self-evaluating subset of symbols),
  or @c{after:} (in Gerbil Scheme, where they are self-evaluating separate from symbols), or
  hash-colon @c{#:before} (in Racket, where they second-class syntax unless quoted,
  and then separate from symbols).
  Many dialects only have symbols, though many programs may still have a @emph{convention}
  of using symbols starting or ending with a colon as keyword specifiers in some protocols.
  Compare also to @c{~labels} or polymorphic @c{`Variants} in OCaml,
  or a choice of named arguments in many languages.
}
In this book, I will use regular symbols like @c{before} for method qualifiers,
for portability, that I will quote to prevent evaluation.

A regular method specification that is not explicitly tagged by the user
is implicitly qualified as a @emph{primary} method specification by the system.
I will use the symbol @c{primary} for that in my implementation@xnote["."]{
  In Common Lisp, primary method specifications are tagged with the unit value @c{NIL}
  that is also a magic self-evaluating constant symbol, a boolean,
  the conventional end-of-list marker, and general-purpose
  null value, default value and unit value.
  I could similarly have used the less-overloaded but still commonly used
  boolean false value @c{#f} for the same purpose.
  Each adaptation of CLOS to a different language will choose its own.
}
But beside @c{primary} methods,
the @emph{standard method combination}, used by default,
supports @c{before} methods that will be executed before the primary methods, and
@c{after} methods that will be executed after them, and
@c{around} methods that will wrap around the execution of each super method.

Simple method combinations accept methods with a method qualifier with the same symbol
as the simple method combination (e.g. @c{+} for adding the results of the methods),
and also with the @c{around} method qualifier.
User-defined method combinations may accept methods with any kind of method qualifier.

I will call @emph{sub-method} the group of method specifications with a given qualifier,
and @emph{effective sub-method} the code that will run when invoking those method specifications:
e.g. the @c{primary} sub-method, the @c{before} sub-method, etc.
Indeed, when manually expressing method combinations as a design pattern,
each sub-method would be expanded into a separate method.

@subsection{Representing sub-methods}

The best way to store sub-methods would be if there were “funcallable instances”
(to use CLOS terminology; like instances in T, that are funcallable in general)
that conflate a function and a record in a single entity.
Then the sub-methods would be stored in the “record” part of the method,
and the method would still be its “function” part.
However, if records are already applied as functions to symbols to extract values,
as I implemented them so far, then funcallable instances wouldn’t work—the function
call interface is already used for field access
(unless symbols are excluded from the function’s co-domain, but that’s ugly).
And since getting a record value cannot be a function call, you also cannot directly use
the Y combinator on a record the way we did previously (see @secref{MFCM}),
but must instead use a slightly different strategy (see @secref{RaR}).

Lacking such funcallable instances, we can store sub-method information
in a record @c{sub-methods} next to the methods being combined.
For the sake of generality, a method-spec can be any kind of specification
(modular extension, multiple or optimal inheritance specification, etc.),
and the @c{method-cons} says how to combine it with the specification data so far;
it could be an actual @c{mix} of modular extensions, or
just something that @c{cons}es the new specification into a list
to be folded or otherwise processed later
(e.g. for conflict resolution in flavorless multiple inheritance):
@Code{
(def (sub-method-lens method-id tag)
  (compose-lens* (field-lens 'sub-methods)
                 (field-lens method-id)
                 (field-lens tag)))
(def (sub-method-spec method-cons tag method-id method-spec)
  (sub-method-lens method-id tag 'update
    (λ (method-specs) (method-cons method-spec method-specs))))
(def (standard-method-cons spec specs)
  (cons spec specs))
(def standard-sub-method-spec
  (sub-method-spec standard-method-cons))
(def (sub-methods-support-spec)
  (field-spec 'sub-methods record-spec))
(def (method-combination-init-spec
       method-id method-combination-init)
  (field-spec 'sub-methods
    (constant-field-spec method-id method-combination-init)))
(def (simple-method-combination-init name)
  (extend-record 'around '()
   (extend-record name '()
    empty-record)))
(def standard-method-combination-init
   (extend-record 'before '()
    (extend-record 'after '()
     (simple-method-combination-init 'primary))))
}

Then there is the question of who is responsible for initializing
the sub-methods record and each of the sub-methods, what the default value should be, etc.
The simplest, “dynamic”, answer would be that the field lens treat an absent field as
a field yielding the top value @c{#f}, and
would treat @c{#f} as an empty record when extending it;
and finally @c{method-cons} would recognize @c{#f} and treat it specially.

A more “static” answer would require the object to inherit
from a “protocol” specification that initializes sub-methods
for the methods that are part of the protocol;
and each such “protocol” specification itself inherits from a “protocol support” specification
that initializes the sub-methods record, that in turn inherits from a “record” specification
that initializes the record being specified.
No dynamic handling of uninitialized values, instead strict discipline in specifications.
This discipline works best with types, or at least with multiple (or optimal) inheritance,
so that programmers do not have to manually chain the many modular extensions involved
in the correct dependency order without feedback beyond a runtime type mismatch.

A yet more sophisticated answer would have meta-objects automatically handle
the initialization of specifications and targets, prototypes, and classes.
Those meta-objects must be instantiated (and thus specified)
before the base object even starts being specified,
which introduces interesting staging and bootstrap issues.
See @secref{MOP} for a discussion of meta-objects.

For this chapter, I will adopt the “static” approach
while manually maintaining the initialization discipline
without the help from either types or multiple inheritance.
The approach is somewhat more verbose and harder to get right than the others,
but it is more explicit and thus hopefully more didactic.

@subsection{Standard Method Combination}

We can then implement the standard method combination as follows:
running the @c{around} methods, and wrapped inside them the @c{before} methods,
then the @c{primary} methods, and finally the @c{after} methods in reverse order.
The @c{call-chain} function chains methods through an inherited @c{call-next-method}
argument that, if called with no argument, calls the next method with the same arguments,
but, if called with arguments, passes them to the subsequent methods.
The @c{progn-method-most-specific-} (-first and -last) methods chain the execution of methods
for @c{before} and @c{after} methods respectively.
The @c{standard-no-applicable-method} is a good default when no method is defined.
And @c{standard-compute-effective-method} finally
computes the effective method from the sub-methods.
The @c{abort} are a poor man’s error mechanism in case the @c{before} or @c{after}
methods try to invoke their @c{super} argument as a @c{call-next-method}.

@Code{
(def (call-chain methods on-exhausted)
  (foldr
    (lambda (m next)
      (λ (self . args)
        (apply m (make-call-next-method next self args) self args)))
    on-exhausted
    methods))

(def (progn-methods-most-specific-first methods self args)
  (foldl (lambda (m _) (apply m abort self args)) #f methods))

(def (progn-methods-most-specific-last methods self args)
  (foldr (lambda (m _) (apply m abort self args)) #f methods))

(def (standard-no-applicable-method method-id . self-args)
  (error "no applicable method" method-id self-args))

(def no-applicable-method standard-no-applicable-method)

(def (standard-compute-effective-method method-id sub-methods)
  (call-chain (sub-methods 'around)
    (λ (self . args)
      (progn-methods-most-specific-first
        (sub-methods 'before) self args)
      (let ((result
              (apply (call-chain (sub-methods 'primary)
                       (λ (self . args)
                         (apply no-applicable-method
                           method-id self args)))
                       self args)))
          (progn-methods-most-specific-last
            (sub-methods 'after) self args)
          result))))
}
You can then initialize a method to use the standard method combination by calling this function:
@Code{
(def (standard-method-init-spec method-id)
  (mix
    (method-spec method-id
       (λ (_super self)
          (standard-compute-effective-method method-id (self 'sub-methods))))
    (method-combination-init-spec
       method-id standard-method-combination-init)))
}
And you can define sub-methods by using these specifications
with arguments @c{method-id method-spec}:
@Code{
(def primary-method-spec (standard-sub-method-spec 'primary))
(def before-method-spec (standard-sub-method-spec 'before))
(def after-method-spec (standard-sub-method-spec 'after))
(def around-method-spec (standard-sub-method-spec 'around))
}

@subsection{Simple Method Combination}

CLOS allows users to define and use simple method combinations
that compose the results of methods according to some n-ary associative
(and sometimes also commutative) operator.
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
@c{op0} a thunk to evaluate if there are no methods (takes one dummy argument),
@c{op1} an unary operator to run on the first method result to make it into a return value,
@c{op2} the (curried) binary operator with which to combine
the result from the next method and the return value from previous computations
into a new return value,
@c{order} one of the two symbols @c{most-specific-first} or @c{most-specific-last}
telling in which order to run the methods,
and @c{sub-methods} a record of the sub-methods.
Again, the @c{abort} are a poor man’s error mechanism in case the regular methods
try to invoke their @c{super} argument as a @c{call-next-method}.

@Code{
(def (simple-compute-effective-method
       name stop? op0 op1 op2 order sub-methods)
  (let* ((arounds (sub-methods 'around))
         (methods (sub-methods name))
         (ordered (case order
                    ((most-specific-first) methods)
                    ((most-specific-last) (reverse methods)))))
   (call-chain arounds
    (λ (self . args)
      (letrec ((run (λ (m) (apply m abort self args)))
               (f (λ (acc lst)
                    (if (and (not (stop? acc)) (pair? lst))
                      (let ((v (op2 (run (car lst)) acc)))
                        (if (stop? v) v (f v (cdr lst))))
                      acc))))
        (if (pair? ordered)
          (f (op1 (run (car ordered))) (cdr ordered))
          (op0 #f)))))))

(def (compute-effective-method/progn sub-methods)
  (simple-compute-effective-method
    'progn (λ (_) #f) (λ (_) #f) (λ (x) x) (λ (r _) r)
    'most-specific-first sub-methods))

(def (compute-effective-method/and sub-methods)
  (simple-compute-effective-method
    'and not (λ (_) #t) (λ (x) x) (λ (r _) r)
    'most-specific-first sub-methods))

(def (compute-effective-method/+ sub-methods)
  (simple-compute-effective-method
    '+ (λ (_) #f) (λ (_) 0) (λ (x) x) +
    'most-specific-first sub-methods))

(def (compute-effective-method/* sub-methods)
  (simple-compute-effective-method
    '* (λ (_) #f) (λ (_) 1) (λ (x) x) *
    'most-specific-first sub-methods))

(def (compute-effective-method/list sub-methods)
  (simple-compute-effective-method
    'list (λ (_) #f) (λ (_) '()) list cons
    'most-specific-last sub-methods))
}
You can define then initialize a function with specifications like this one:
@Code{
(def (list-method-init-spec method-id)
  (mix
    (method-spec method-id
       (λ (_super self)
          (compute-effective-method/list (self 'sub-methods))))
    (method-combination-init-spec
       method-id (simple-method-combination-init 'list))))
}
And you can define sub-methods by using specifications like this one,
and/or the @c{around-method-spec} above, with arguments @c{method-id method-spec}:
@Code{
(def list-method-spec (standard-sub-method-spec 'list))
}

@subsection{User-defined Method Combinations}

And users are not limited to predefined method combinations.
In the most general case, users may define their own method combination,
that support methods declared with whichever qualifiers they want to support,
ordered whichever way they prefer.
Thus for instance, they may define and use method combinations for the following purposes:

@itemize[
  @item{They may reimplement the inheritance semantics of Simula, of C++,
        or of whichever their favorite programming language is.}
  @item{They may define additional layers of @c{around}, @c{before} and @c{after} methods,
        with the same order as usual, or the opposite order@xnote["."]{
           Interestingly, at some point, ASDF 1 defined a method combination just
           for the purpose of another layer of around methods.
           But ASDF 2, vying for portability even on some implementations that didn’t support it,
           removed it, and instead achieved the same effect manually for the only method
           that used it, by introducing a new wrapping method that calls the regular one.
           Even without the portability issue, a method combination usually only makes sense
           when it is actively used by more than one method.
        }}
  @item{They may write pure functional monadic variants of the standard method combination,
        lazy or eager, with a constant monad or one given as an argument or a dynamic variable, etc.}
  @item{They may develop interactions wherein tagged method specifications correspond to actions
        by two (or more) participants.}
  @item{They may define method variants that apply at different phases, with different capabilities:
        compile-time vs runtime vs some more refined discipline that includes
        typechecking time, static resource allocation time, error handling time,
        access control checking time, etc.}]

To that effect, they would develop some kind of @c{foo-method-init-spec}
to suitably initialize the method and its sub-methods,
based on a @c{compute-effective-foo-method} function, and a @c{foo-method-combination-init} record;
they would also define @c{foo-method-spec} or such for each sub-method,
either using @c{standard-sub-method-spec}, or
@c{sub-method-spec} with an appropriate a specialized @c{method-cons} argument
to pre-compose the sub-method specifications in advance of @c{compute-effective-foo-method}.

Suitably tagged method specifications can then be used in a structured way to enact any kind of
environment setup or cleanup, argument validation, argument normalization,
resource allocation and deallocation, locking, logging, error handling, access control,
detail accumulation, etc.,
that the primary methods can safely rely on.
Method combinations promote modularity thanks to better factoring of code,
that allows for the extensibility of code along more independent axes.

@subsection{Inherited Interfaces vs Orthogonal Protocols}

In the above design, all specifications that use a method must inherit
from the same @c{foo-method-init-spec method-id} ancestor, or else
they will end up using incompatible method specifications.
Not only is such requirement for a common ancestor onerous,
it prevents modularity in many ways:
First, redundant specifications that must be manually maintained identical
is the very definition of not modular.
Second, and just as importantly, by requiring specifications (and prototypes and classes)
to inherit from a common ancestor to be able to define a method,
means that you cannot add new methods to specifications after the fact.

Java and C# are two languages known for their terrible design requiring
a class to explicitly inherit from an interface at the moment it is created
so it may expose methods that satisfy that interface.
This design is terrible, because it requires omniscience from
a class author about all the interfaces that the users of the class may ever want to satisfy,
including, impossibly, about interfaces that do not exist yet,
in other systems that haven’t been written yet.
Alternatives for users would include to reimplement the class from scratch,
or write an onerous wrapper around the existing class,
or a subclass of it. But all these alternatives introduce
subtle and unsubtle incompatibilities, impedance mismatch,
wrappings and unwrappings all over the place, and yet
in the end still have the same problem with respect to yet future interfaces,
then requiring another layer of wrapping all over again
for each team that wants to extend the capabilities of a class.

By contrast, CLOS generic functions, Clojure protocols, Haskell typeclasses, Rust traits, etc.,
enable programmers to define methods for a class (or typeclass) @emph{after the fact},
without wrappings and unwrappings, reimplementations, etc.
This vastly increases modularity.
I will call this design @emph{orthogonal protocol} to contrast it with the
@emph{inherited interface} of Java and C#.
The key feature is some notion of “protocol”, i.e. a set of related functions,
that can have different implementations for various types, classes, specifications, etc.,
while being defined independently (that’s what “orthogonal” means).
Protocol implementations are modular entities, that are also first-class and extensible
in CLOS or Clojure, but sadly only second-class and not extensible in Haskell or Rust.

@subsection{Generic Functions}

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
Protocols as independent entities also can avoid the complexity of mutable inheritance,
wherein you’d modify a specification after the fact so it would inherit from a new interface.

Generic functions were introduced as “generic operations” by T @~cite{Rees1982T},
and reprised as “generic functions” by New Flavors, LOOPS, CommonLoops, CLOS,
but also beyond Lisp by Cecil @~cite{CecilMultimethods} and Fortress @~cite{Allen2011Type}.

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
the @c{object} is its first argument, that I can complement with further arguments.
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

Generic function in the context of higher-order FP introduces some extra complexity.
Since methods don’t have a single owner but two,
method lifetime management can become tricky:
if either the generic function or the specification becomes unreachable,
then the method can be deleted.
When defining a specification or gf in a nested scope, and it doesn’t escape,
then associated methods can be statically deleted at the scope exit;
if it escapes or may escape, then garbage collection is required, and may involve
a weak hash-table indexed by weak pairs (i.e. the pair becomes unreachable
when either side becomes unreachable, at which point so do indexed entries).

Then again, in CLOS, generic functions and classes are considered
second-class global objects for regular programs, so the problem is avoided
(though reflection through the MOP make them first-class
for the sake of metaprograms and infrastructure, at which point
programmers are supposed to solve the problem manually if it arises).
More second-class concepts built into the language do increase complexity somewhat though.
And in a very dynamic language with first-class prototypes and generic functions
that get defined locally, avoiding space leaks will involve extra complexity
for garbage collection.

At some point, you may realize that generic functions bring extra complexity
for the same reason they bring extra modularity:
because they associate method specifications to a pair of independent entities,
rather than to a single entity or sub-entity thereof.
Since the entities are independent, new ones can be modularly defined
without modifying existing ones, the behavior of which is extended in incremental ways.
This is the essence of any real solution to the “expression problem” @~cite{Wadler1998}.
And then you may further realize that you could gain even more modularity
by associating methods not to a pair of entities, but to arbitrary large tuples of entities.
Which leads us to the invention of multiple dispatch.

@section[#:tag "MD"]{Multiple Dispatch}

@subsection{Multimethods}

So far, the choice of what (effective) method to evaluate when calling a generic function
only depended on its first argument, “the” object on which the function was invoked.
And as long as OO was stuck in the “message passing” metaphor in which it was born,
this seemed like the only option: you have an object, you send @emph{it} a message,
you call @emph{its} method, etc. There is one entity, of which you select one sub-entity.
But once you invented method combinations, and the concept of generic function followed,
then it becomes natural to wonder why effective method selection should only depend on
one argument, especially when there are plenty of “binary methods”
that in fact go through great lengths to work around this limitation:
comparison functions, algebraic operators (addition, multiplication, etc.), and more.

Thus, with multiple dispatch, a method can be specialized based
not just on one argument, but multiple arguments.
A method specializing on multiple arguments is called a @emph{multimethod},
and a language supporting multiple dispatch is the same as a language supporting multimethods.
And the previous behavior of only specializing methods on a single object argument
is renamed “single dispatch”.

With multimethods, the notional table of method specifications,
instead of being indexed by a pair of a generic function and a specification, is indexed by
an arbitrary tuple of a generic function and any number of specifications—though
the same number for every method within a given generic function,
the “generic arity” of the function (e.g. 2 for binary methods).

Also, a protocol (sets of related generic functions)
supporting dispatch on multiple arguments
is akin to a typeclass that depends on multiple typeclass constraints.
And indeed, you can desugar the latter into the former:
a typeclass function with @c{n} typeclass constraints is like a generic function
dispatching on @c{n} elements, being the “dictionaries” for each of those @c{n} constraints.
The difference being that Haskell typeclasses do not support inheritance,
but CLOS protocols do @~cite{LIL2012}.

Multimethods notably simplify away “binary methods”, “double dispatch”,
and the “visitor pattern”.
They offer a modular alternative to these often nightmarish “design patterns”,
and supporting actual win-win inheritance when they typically only support conflict inheritance.

@subsection{Binary Methods Done Right}

For decades, developers of object-oriented protocols have stumbled upon problems
regarding “binary methods”, the behavior of which depends not just on one object,
but also on a second object:
comparing objects of various types for equality or for (total or partial) order;
adding two numbers involves different operations whether you try to add two integers,
or two floating-point numbers, or an integer and a floating-point number,
or two strings, or two matrices of the same shape, etc.;
displaying an object on some graphical or text terminal;
inserting an entry into some collection;
planning or performing a build operation on a code component;
etc.

In languages that only have single dispatch, writing such methods is problematic,
and not only due to the contravariance issues we discussed relative to typing (@secref{LotN}):
how do you write such code, and how do you keep it modular and extensible?
You can dispatch on the first object, and have method in each case; fine.
But what about the second object?
A simple “solution” is to do a runtime typecheck for the second object,
and handle each case in a list of known possibilities.
Problem is, you must know in advance all the possible types you will ever want to use
fro the second argument, and the result is not extensible.
A more complex solution is to create a separate method
for each possible specification for the first argument,
then call that method on the second argument:
thus the method @c{add} on an integer calls the method @c{add-to-integer} on the second argument,
with the first object as first argument—exchanging the roles of dispatch object
and first non-dispatch argument, but after having specialized the method.
This approach is known as the “double dispatch” design pattern,
and can be generalized to triple dispatch, or any kind of n-uple dispatch.

But double dispatch is awkward and limited in many ways:
since languages that need this do not have generic functions or multiple dispatch,
you need to include the specialized method in the specification itself,
which once again means that all possible specifications for the first argument
must be known in advance when defining the specification for the second argument,
so this fourth-class design pattern is only extensible for the second argument.
Also, in languages with visibility limitations on an object’s internal state,
the refined method might not be able to see the state of the object,
or that state may have to be shared more broadly than desired.
Finally, there is no good way to “call the next method” and compose multiple inherited behavior,
and every programmer of every part of the protocol would have to cooperate with the pattern,
which is hard to enforce.

A more sophisticated approach known as the “visitor pattern” @~cite{GoF1994},
is both a special case of double dispatch and a generalization of it.
A general-purpose method @c{accept} or @c{visit} takes a “visitor” object as argument,
and each class @c{Foo} calls the special-purpose method @c{visitFoo} on the visitor,
with the current object (of class @c{Foo} indeed) as parameter.
The visitor pattern is thus an instance of double dispatch,
that essentially provides a translation from the class namespace to the method namespace.
Each visitor can then provide a method for each the specifications it wants to support;
and visitors can themselves be extended with further methods to support further specifications,
making them more extensible than e.g. pattern-matching on argument classes.
By having the visitor object abstract over both the specific operation
and the further arguments, you can use chains of visitors to implement all kinds of operations.
Compared to regular double dispatch, the visitor pattern is more general, and
crucially allows for operations being defined after the class is defined.
But the visitor pattern also involves more boilerplate,
having to define visitor classes with all the required information.
Importantly, the visitor pattern also requires all state to be public,
or otherwise shared will all possible present and future visitors.
Finally, while there is still no good way to support “call-next-method”
with the visitor pattern, at least the problem is factored in a way that
the support could conceivably be implemented once and then shared with many visitors.

Now, if the object system itself has builtin support multiple dispatch, then
double dispatch as a design pattern, and its generalizations to triple and n-uple dispatch,
or to the visitor pattern, disappear completely
as complex yet insufficiently helpful design patterns.
They are replaced by “just use the system-provided multiple dispatch”.
Suddenly, these methods become simple to write, extensible for both arguments,
without visibility issues (any method can see the state of any argument it matches),
and with full win-win composability of the methods.

@subsection{Multiple Multiple Inheritance}

The set of multimethods that match a given tuple of arguments is necessarily a partial order:
by separately specializing one argument or the other,
you can easily define mutually incomparable method signatures
that match the tuple of arguments, even if each argument’s ancestry are a total order.
Multimethods will thus naturally leverage and extend the techniques used for multiple inheritance:
linearizing the order of methods,
method combination based on the resulting precedence list, etc.
Typically, linearization happens via lexicographical order of per-argument linearization:
this linearization corresponds to the same behavior as the double dispatch and the visitor pattern
in the degenerate case that only the first found method is called;
it has a clear precedence based on the first argument,
and it nicely extends the order of a single dispatch order on the first argument,
in that methods specialized on the first argument
will be placed relatively to each other the same as if dispatching on just that argument.

Like the multiple inheritance that it extends (see @secref{RSaDN}),
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
only needs be broad enough to encompass the generic function and the specifications in the signature.
By using a double-dispatch-like series of tables and subtables indexed by partial method signature,
stored locally in each prototype target, one may specify multimethods
with just the gf tag and update access to the specifications, without needing to update the gf;
the main advantage is then that the representation is backward compatible with
the previous representation for single dispatch only,
so that code that previously worked with a narrow context can still work essentially unchanged.

In any case, the specification context for multimethods is usually wider than the specification
context of a single-dispatch method, that needs only contain the prototype or class being specified.
That is a retrospectively obvious necessity when defining multimethods—but,
one may note that while each single method can do with a narrower context,
duplicating the effect of multimethods requires the very same wider context
even without system support for multiple dispatch, and
manually using double-dispatch as a design pattern:
in all cases, you will need to modify all the specifications at stake,
and many additional ones, too (“visitors”).
This is an important point: multiple dispatch invites you to broaden the context
of your fixpoints, from a single specification, to, in the limit, the entire OO ecosystem.

@XXXX{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX HERE XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

@;TODO implementation


@section[#:tag "DD"]{Dynamic Dispatch}

First-class modularity vs second-class.
Note that in most second-class OO languages, you often also have this as first-class modularity
yet without first-class modular extensibility. (Subtle distinction.)

Dynamic dispatch as such is not OO: it is first-class modularity, that can happen with or without OO (which is first-class or second-class modular extensibility). Proof: Microsoft COM, first-class modules in ML, "objects" without inheritance in SICP, etc.

Now if you have first-class OO, then you necessarily have dynamic dispatch.

But you could have second-class OO without dynamic dispatch: C++ without "virtual" methods; Java with only static methods; Interface passing style with constant interfaces; etc.

Kin vs type. @~cite{Allen2011Type}.

Also works in typeclass-style vs class-style.

How that interacts with multiple dispatch tables, global or local.

Subjective dispatch: dynamically bound implicit first argument to all methods
(higher priority than other arguments, can majorly redefine the meaning of programs).
Objective dispatch: dynamically bound implicit last argument to all methods
(lower priority than other arguments, can minorly adjust the meaning of all programs).

@exercise[#:difficulty "easy"]{
  Define the missing simple CLOS method combinations in an efficient way, for
  @c{+ * max min progn list append nconc or and}.
  Hints: @c{progn} is just the Lisp operator for sequential evaluation of expressions,
  returning the value of the last one. @c{nconc} is a variant of @c{append} that uses side-effects;
  and @c{append} and @c{nconc} done wrong will be quadratic rather than linear so be careful.
  Also note the existence of IEEE floating point number @c{+inf.0} and @c{-inf.0}.
}

@exercise[#:difficulty "medium"]{
  In a few lines of code, define a method combination that implements
  the concatenation semantics of Simula, and its “inner” keyword.
  Implement it purely with functions, and optionally use macros so the syntax
  is closer to the original.
  Optionally, implement the semantics of Beta instead of that of Simula,
  or in addition to it@xnote["."]{
    Note that in the case of Beta, much of the difficulty is in
    understanding its semantics to begin with, based on the available documentation.
    See my notes on @citet{Kristensen1987Beta}, or
    ask AI for help understanding the Beta documentation.
}}

@exercise[#:difficulty "Medium"]{
  If you did exercise @exercise-ref{08to09}, compare
  your attempt at explaining these advanced topics OO with how I did.
  What aspects did you anticipate? What surprised you?
  What did you do better or worse?
}
