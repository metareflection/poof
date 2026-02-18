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
A skew lens (or “ripsjq” lens, pronounced “rip sick”), yet generalized the above:
now the types for the update are not required to be the same as those for the view,
so you don’t have to be looking exactly at the change you’re experiencing.
The view @c{s → r} goes from an outer context @c{s} to an inner context @c{r}
(where “r” is for required, and “s” is just the next letter),
and the update goes from an extension @c{i → p} to @c{j → q}
(where “i” is for inherited, “p” is for provided, and “j” and “q” are just the next letters).
Polymorphic lenses are a special case of skew lenses.
@Code{
type SkewLens r i p s j q =
       { view : s → r ; update : (i → p) → j → q }
type PolyLens s t a b = SkewLens a a b s s t
}

@Paragraph{View and Update}
I can also give separate types fo View and Update:
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
  (make-lens get (λ (f s) (set (f (get s))))))
(def (setter←lens l)
  (λ (b s) (l 'update (λ (_a) b))))
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
@c{composeLens} just composes each component with the proper function@xnote["."]{
  As usual, you can represent your lenses such that you can compose them with the
  regular @c{compose} function, by pre-applying the @c{composeLens} function to them.
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
  (extend-record s key (f (s key))))
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

@Paragraph{From Sick to Ripped}

You may have notice that I used the same letters @c{r i p}
to parameterize a @c{SkewLens} (plus their successors)
as to parameterize a @c{ModExt}. This is not a coincidence.
You can focus a modular extension by looking at it through a matching skew lens:
@Code{
skew-ext : SkewLens r i p s j q → ModExt r i p → ModExt s j q
(def (skew-ext l m)
  (compose* (l 'update) m (l 'view)))

(define (compose* . l) (foldl (uncurry2 compose) identity l))
}

Thus with a @c{SkewLens r i p s j q},
I can change the continuation for a modular extension
from expecting @c{s j q} (pronounced “sick”)
to expecting @c{r i p} (pronounced “rip”).

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
@c{((compose-lens k l) 'update)}, further specializing the previous view.
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
you will use @c{(composeLens (field-lens 'Foo) (field-lens 'Bar))} as your @c{SpecFocus}.

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
Thus, @c{(composeLens (field-lens* 'foo 'bar) (updateOnlyLens (field-update 'baz)))}
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
the lens @c{(composeLens l (updateOnlyLens u))} will up focus on the method at @c{u}
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

(def (reverse-view s l)
  (setter←lens l s))
(def (reverse-update s l a f)
  (l 'view (f (reverse-view s l) a)))
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
         (λ (_self super) (+ super 50)))
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
    (λ (_self inherited-method element)
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
(def (simple-instance-field-spec field-id initModExt)
  (skew-ext (instance-field-lens field-id)
    (rproto←record (record (init initModExt)))))
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
(simple-instance-field-spec 'parts (λ (self _inherited)
  (* (self 'markup)
     (foldl + 0 (λ (part) (part 'price)) (self 'parts)))))
}
A field @c{markup} that has no default initializer must be provided by users could be defined as:
@Code{
(simple-instance-field-spec 'parts (λ (_self _inherited)
  (abort "missing field markup")))
}
A class could then define a default prototype for new instances as:
@Code{
(skew-ext (update-lens rproto-spec-lens (field-update 'new-instance-prototype))
  (λ (self _inherited)
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

@section{Method Combinations}

@subsection{Win-Win}

Another fantastic contribution from Flavors @~cite{Cannon1979} is Method Combinations:
the idea that the many methods declared in partial specifications
are each to contribute partial information that will be harmoniously combined (mixed in),
rather than complete information that have to compete with other conflicting methods
that contradict it, the winners erasing the losers.
Win-win interactions rather win-lose, that was a revolution
that made multiple inheritance sensible when it otherwise wasn’t.

I will present the more refined generalization of this principle as the
method combinations from CLOS @~cite{CLtL2 clhs},
rather than the more limited method combinations of the original Flavors.

The simplest case of method combination is actually
the usual composition of modular extensions,
wherein each extension can refer to its super argument
along a multiple inheritance specification’s precedence list,
as discussed in @secref{MI}.
But I can do better, as an API on top of this foundation.

@subsection{Effective Methods and Method Qualifiers}

With method combinations, a target method is called the @emph{effective method}.
It is computed based on individual @emph{method specifications}
declared by each specification along the way.

In CLOS@~cite{CLtL2 CLHS AMOP} after Flavors,
a method can tagged with a @emph{method qualifier},
usually some kind of symbols, though, in many Lisp or Scheme dialects,
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
In our source code, I will quote them, since most Scheme dialects will not consider
that special syntax for self-evaluating keywords as in Common Lisp.

A regular method that is not explicitly tagged by the user
is implicitly qualified as a @emph{primary method} by the system.
I will use the @c{:primary} keyword for that in our implementation@xnote["."]{
  In Common Lisp, primary method are tagged with the unit value @c{NIL}
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

I will call @emph{sub-method} the group of methods with a given qualifier,
e.g. the @c{primary} sub-method, the @c{before} sub-method, etc.

@subsection{Representing sub-methods}

The best way to store sub-methods would be if there were “funcallable instances”
(to use CLOS terminology, but like instances in T in general)
that conflate a function and a record in a single entity.
Obviously, the interface for extracting a value from a record cannot then be
applying the record as a function to a symbol, or the function wouldn’t be available
(unless symbols are excluded from the function’s co-domain, but that’s ugly).
Since getting a record value isn’t a function call, you also cannot directly use
the Y combinator on a record (see @secref{RaR}).
Then the submethods would be stored in the “record” part of the method,
and the method would still be its “function” part.

Lacking such funcallable instances, we can store submethod information
in a record submethods next to the methods being combined.
For the sake of generality, a method-spec can be any kind of specification
(modular extension, multiple or optimal inheritance specification, etc.),
and the @c{method-cons} says how to combine it with the specification data so far;
it could be an actual @c{mix} of modular extensions, or
just something that @c{cons}es the new specification into a list
to be folded or otherwise processed later
(e.g. for conflict resolution in flavorless multiple inheritance):
@Code{
(def (sub-method-lens method-id tag)
  (composeLens* (field-lens 'sub-methods)
                (field-lens method-id)
                (field-lens tag)))
(def (sub-method-spec method-id tag method-cons method-spec)
  (sub-method-lens method tag 'update
    (λ (method-specs) (method-cons method-spec method-specs))))
(def (standard-method-cons spec specs)
  (cons spec specs))
(def (sub-methods-support-spec)
  (field-spec 'sub-methods record-spec))
(def (method-combination-init-spec method-id method-combination-init)
  (field-spec 'sub-methods (constant-field-spec 'method method-combination-init)))
(def primary-method-combination-init
  (extend-record 'primary '() empty-record))
(def standard-method-combination-init
  (extend-record 'around '()
   (extend-record 'before '()
    (extend-record 'after '()
     primary-method-combination-init))))
}

Then there is the question of who is responsible for initializing
the submethods record and each of the submethods, what the default value should be, etc.
The simplest, “dynamic”, answer would be that the field lens treat an absent field as
a field yielding the top value @c{#f}, and
would treat @c{#f} as an empty record when extending it;
and finally @c{method-cons} would recognize @c{#f} and treat it specially.

A more “static” answer would require the object to inherit
from a “protocol” specification that initializes sub-methods
for the methods that are part of the protocol;
and each such “protocol” specification itself inherits from a “protocol support” specification
that initializes the submethods record, that in turn inherits from a “record” specification
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

@XXXX{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX HERE XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

@Code{
(def (compute-effective-around-method method specification)
  (compute-effective-sub-method
   method 'around specification
   ))
(def (compute-effective-before-method method specification)
  (compute-effective-sub-method method 'around specification))

(def (standard-compute-effective-method method specification)
  (let* ((around
          (compute-effective-around-method method specification))
         (before
          (compute-effective-before-method method specification))
         (primary
          (compute-effective-primary-method method specification))
         (after
          (compute-effective-after-method method specification)))
    (around
      (lambda ()
        (before)
        (let ((result (primary)))
          (after)
          result)))))

}

@subsection{Simple Method Combination}

@subsection{User-defined Method Combinations}




These methods can then enact any kind of setup or cleanup,
resource allocation and deallocation, locking, error handling, access control,
argument normalization, etc.,
that the primary methods can safely rely on.
Of course, methods thus named only make sense in the context of side-effects;
but in a pure functional language, they may still be useful,
just translated into monadic functions that provide equivalent functionality
in the user’s favorite monad.

But that is not all. CLOS, after Flavors, allows users to select different
method combinations from the @emph{standard method combination} that is the default.
With the @c{list} combination, each defined method provides a single answer,
and the effective method will collect those answers into a list.
With the @c{append} combination, each defined method may provide a list of answers,
and the effective method will append those lists into a single one.
Similar method combinations are available for other monoidal operations:
@c{+}, @c{*}, @c{min}, @c{max}, @c{and} (boolean short-circuiting logical and),
@c{or} (boolean short-circuiting logical and), @c{progn} (sequential execution of side-effects).
Users can define their own simple method combinations with their own monoidal operation,
that can chain methods defined along the precedence list
from @c{:most-specific-first} to least specific,
or @c{:least-specific-first} to most specific.

And users are not limited to predefined method combinations:
they can also define their own,
that will compute an @emph{effective method} in whichever arbitrary way they want,
from whichever methods were declared with whatever qualifiers they want to support,
ordered whichever way they prefer.
A method combination can trivially implement
Simula- and Beta-style “prefix” inheritance in seven lines of code;
and flavorless conflict-style multiple inheritance could be implemented
in a few hundred lines of code@xnote["."]{
  Claude Code in a few minutes created and debugged a working implementation
  of flavorless inheritance,
  in under 800 lines of code and 200 of tests, for SBCL and CCL, using the CLOS MOP.
  Yet, the fact that, in over 46 years that both kinds of multiple inheritance existed,
  and that it was relatively easy to implement flavorless inheritance
  on top of flavorful inheritance using method combinations,
  no human seems to have ever bothered to implement this mechanism, much less use it,
  is a strong symptom that in fact it is a silly thing to do.
  The Claude AI assistant comments:
  “once you’ve grasped that methods can @emph{combine} rather than @emph{collide},
  deliberately implementing collision semantics would feel like building
  a car that refuses to start if you have both a driver and a passenger.”
}

@subsection{Generic Functions}

The original Flavors adopted the “message passing” paradigm,
and associated method combinations to messages:
to pass a message @c{m} to object @c{x}, you invoke @c{(x m args ...)}.
But its successor New Flavors @~cite{Moon1986Flavors},
introduced the notion of a @emph{generic function},
that preserves the usual syntax and semantics of a function,
and encapsulates the same notion of an identified entity you invoke with arguments,
the behavior of which can be specified in modular extensible ways:
to call a generic function @c{g} on object @c{x}, you invoke @c{(g x args ...)}.
The object @c{x} is now an argument instead of the function.
Method combination information is then associated to this generic functions.
The notion of generic function was thereafter adopted by
CommonLOOPS, CLOS, Cecil, Fortress, etc. @; TODO @~cite{Bobrow1986CommonLoops}

@; TODO quickly mention multi-methods with secref

Note how program information is stored in two independent set of entities:
one the one hand the specifications, and the generic functions
(or the generic functions grouped into “protocols”).


@subsection{Implementing Method Combination}

Define a method-specification as a derived field next to the effective method
(or maybe inside it, if conflation or function and object is allowed),
and the effective method as “just”
calling effective-method resolution on the method-specification.
overriding a method now composes a modular extension on a field of the method-specification object:
the primary field, before field, etc.

@section{Multiple Dispatch}

Together with multiple dispatch, requires some shared (and usually global)
dispatch table.
Can still be pure, with global modular extension.
How does that work for classes defined or extended in a narrow scope?

@section{Dynamic Dispatch}

First-class modularity vs second-class.
Note that in most second-class OO languages, you often also have this as first-class modularity
yet without first-class modular extensibility. (Subtle distinction.)

Dynamic dispatch as such is not OO: it is first-class modularity, that can happen with or without OO (which is first-class or second-class modular extensibility). Proof: Microsoft COM, first-class modules in ML, "objects" without inheritance in SICP, etc.

Now if you have first-class OO, then you necessarily have dynamic dispatch.

But you could have second-class OO without dynamic dispatch: C++ without "virtual" methods; Java with only static methods; Interface passing style with constant interfaces; etc.

Kin vs type. @~cite{Allen2011Type}

Also works in typeclass-style vs class-style.

How that interacts with multiple dispatch tables, global or local.
