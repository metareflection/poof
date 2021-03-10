#lang scribble/acmart @acmsmall @review

@(require (only-in scribble/manual racket racketblock litchar itemize item)
          (only-in scribble/example examples make-base-eval)
          "util/examples-module.rkt"
          (for-label racket))

@(declare-examples/module poof racket
   (provide (all-defined-out)))
@examples/module[poof #:hidden
(require (only-in racket/base [define def])
         "util/eval-check.rkt")
]

@title{Prototype Object-Orientation Functionally}

@author[#:affiliation "Mutual Knowledge Systems, Inc."]{Francois-Rene Rideau}
@author[#:affiliation "Mutual Knowledge Systems, Inc."]{Alex Knauth}
@author[#:affiliation "Harvard University"]{Nada Amin}

@section{Prototypes, bottom up}

@subsection{Object Orientation in 109 characters of standard Scheme}

@examples/module[poof #:no-result
(define (fix p b) (define f (p (lambda i (apply f i)) b)) f)
(define (mix c p) (lambda (f b) (c f (p f b))))
]

We contend that the above two definitions summarize
the essence of object-oriented programming, and that
all the usual "object oriented" concepts can be easily recovered from them.

The rest of this essay will make the case.

@subsubsection{The Essence of OOP in words}

Object-Oriented Programming consists in specifying code in modular increments
that each compute their part from the combined whole and the computation so far.

Here we implement these increments as *prototypes*: functions of two arguments,
@racket[self] and @racket[super]
(or, above, @racket[f] and @racket[b], for fixed-point and base value).
Function @racket[fix] @emph{instantiates} a prototype @racket[p] given a
super/base value @racket[b].
Function @racket[mix] has @emph{child} prototype @racket[c] @emph{inherit} from
@emph{parent} prototype @racket[p]
so they operate on a same fixed-point @racket[f] while chaining their effects on
@racket[b].

Given some arbitrary instance type @racket[Self], and a super-type
@racket[Super] of @racket[Self],
a prototype for @racket[Self] from @racket[Super] will thus be
a function from @racket[Self] and @racket[Super] to @racket[Self].

The first argument @racket[self] of type @racket[Self] will hold the instance
resulting as a fixed point from the entire computation.
When composing multiple prototypes, every prototype will receive
the @emph{same} value as their @racket[self] argument:
the complete instance that results from applying the every prototype in order.
This allows prototypes to "cooperate" with each other
on @emph{different} aspects of the computation,
wherein one prototype defines some aspect (e.g. a "method" in some dictionary)
while relying on aspects to be defined by other prototypes (e.g. other methods),
accessed through the @racket[self] argument in what is called "late binding".

The second argument @racket[super] by contrast holds the partial result of the
fixed-point computation after applying only the "next" prototypes.
When composing multiple prototypes, each prototype will (presumably) receive
a different value. The last prototype in the list (rightmost, most ancestral
parent) will receive the "base" or "bottom" value from the fix function
(often literally the bottom value or function in the language), then the
"previous" prototype (its child, to the left) will receive ("inherit")
the result of that "next" computation (its parent, to the right), and so on
until the first prototype (leftmost, most recent child) inherits
its @racket[super] value from the rest and computes the final instance.
This allows prototypes to cooperate with other prototypes on a @emph{same} aspect
of the instance computation, wherein children prototypes can accumulate, modify
or override the method values inherited from the parent prototypes.

@subsubsection{Applicability to arbitrary Programming Languages}

The above definitions are readily available to any language with closures
and either dynamic types or dependent types. However, their potential is not
fully realized in languages with mere parametric polymorphism
(see chapter X on typing prototypes).
We will also require lazy evaluation (or side effects to implement them)
as a language feature to @emph{efficiently} implement objects with our formulas,
but do not otherwise require side-effects --- though they can be used
for the usual optimizations in the common "linear" case.

@subsection{A minimal object system}

How do the two above functions relate to objects?

@subsubsection{Records as functions}

First, let's use the following trivial encoding of "records" as functions
from symbol (the name of a slot) to value (bound to the slot).

Thus, the function @racket[x1-y2] below encodes a record with two fields
@racket[x] and @racket[y] bound respectively to @racket[1] and @racket[2].

@examples[#:eval poof #:no-result
(code:comment "x1-y2 : (Fun 'x -> Nat | 'y -> Nat)")
(define (x1-y2 msg)
  (case msg
    ((x) 1)
    ((y) 2)
    (else (error "unbound slot" msg))))
]

We can check that we can indeed access the record slots
and get the expected values:

@examples[#:eval poof #:label #f
(eval:check (x1-y2 'x) 1)
(eval:check (x1-y2 'y) 2)
]

Note that we use Lisp symbols for legibility, but in poorer languages,
"symbols" could be any large enough type with decidable equality,
e.g. integers or strings, etc.

In accordance with Lisp tradition, we will say "slot" instead of "field"
or "member" or "method", and say that the slot is bound to the given value
rather than it "containing" the value or any such thing.

@subsubsection{Prototypes for Records}

A @emph{prototype} for a record is a function of two arguments @racket[self] and
@racket[super] (both records) to an @racket[extended-self] record.
@racketblock[
(deftype (Proto Self Super) (Fun Self Super -> Self st: (<: Self Super)))
fix : (Fun (Proto Self Super) Super -> Self st: (<: Self Super))
mix : (Fun (Proto Self Super) (Proto Super Super2) -> (Proto Self Super2) st: (<: Self Super Super2))
]

Thus, this prototype extends or overrides its super-record with
a slot @racket[x] unconditionally bound to @racket[3]:
@examples[#:eval poof #:no-result
(code:comment "$x3 : (Proto (Fun 'x -> Nat | A) A)")
(define ($x3 self super)
  (lambda (msg) (if (eq? msg 'x) 3 (super msg))))
]

This prototype computes a complex number slot @racket[z] based on real and
imaginary values bound to the respective slots @racket[x] and @racket[y] of its
@racket[self]:
@examples[#:eval poof #:no-result
(code:comment "$z<-xy : (Proto (Fun (Or 'x 'y) -> Real | 'z -> Complex | A) (Fun (Or 'x 'y) -> Complex | A))")
(define ($z<-xy self super)
  (lambda (msg)
    (case msg
      ((z) (+ (self 'x) (* 0+1i (self 'y))))
      (else (super msg)))))
]

That prototype doubles the number in slot @racket[x] from its @racket[super] record.
We say that it @emph{inherits} the value for slot @racket[x]:
@examples[#:eval poof #:no-result
(code:comment "$double-x : (Proto (Fun 'x -> Number | A) (Fun 'x -> Number | A))")
(define ($double-x self super)
  (lambda (msg)
    (if (eq? msg 'x) (* 2 (super 'x)) (super msg))))
]

More generally a record prototype extends its @racket[super] record with new slots
and/or overrides the values bound to its existing slots, and may in the
process refer to both the records @racket[self] and @racket[super] and their slots,
with some obvious restrictions to avoid infinite loops from circular definitions.

Note that we use the name prefix @litchar{$} for a prototype.

But how do we test the above prototypes?

We can use the above record @racket[x1-y2] as a base value and use the
@racket[fix] operator:

@examples[#:eval poof #:label #f
(code:comment "x3-y2 : (Fun 'x -> Nat | 'y -> Nat)")
(define x3-y2 (fix $x3 x1-y2))
(eval:check (x3-y2 'x) 3)
(eval:check (x3-y2 'y) 2)
]

@examples[#:eval poof #:label #f
(code:comment "z1+2i : (Fun 'x -> Nat | 'y -> Nat | 'z -> Complex)")
(define z1+2i (fix $z<-xy x1-y2))
(eval:check (z1+2i 'x) 1)
(eval:check (z1+2i 'y) 2)
(eval:check (z1+2i 'z) 1+2i)
]

@examples[#:eval poof #:label #f
(code:comment "x2-y2 : (Fun 'x -> Nat | 'y -> Nat)")
(define x2-y2 (fix $double-x x1-y2))
(eval:check (x2-y2 'x) 2)
(eval:check (x2-y2 'y) 2)
]

We can also @racket[mix] these prototypes together before to compute the
@racket[fix]:
@examples[#:eval poof #:label #f
(code:comment "z6+2i : (Fun 'x -> Nat | 'y -> Nat | 'z -> Complex)")
(define z6+2i (fix (mix $z<-xy (mix $double-x $x3)) x1-y2))
(eval:check (z6+2i 'x) 6)
(eval:check (z6+2i 'y) 2)
(eval:check (z6+2i 'z) 6+2i)
]

And since the @racket[$z<-xy] prototype got the @racket[x] and @racket[y] values
from the @racket[self]
and not the @racket[super], we can freely commute it with the other two prototypes
that do not affect either override slot @racket['z] or inherit from it:
@examples[#:eval poof #:label #f
(eval:check ((fix (mix $z<-xy (mix $double-x $x3)) x1-y2) 'z) 6+2i)
(eval:check ((fix (mix $double-x (mix $z<-xy $x3)) x1-y2) 'z) 6+2i)
(eval:check ((fix (mix $double-x (mix $x3 $z<-xy)) x1-y2) 'z) 6+2i)
]

@racket[mix] is associative, and therefore we also have
@examples[#:eval poof #:label #f
(eval:check ((fix (mix (mix $z<-xy $double-x) $x3) x1-y2) 'z) 6+2i)
(eval:check ((fix (mix (mix $double-x $z<-xy) $x3) x1-y2) 'z) 6+2i)
(eval:check ((fix (mix (mix $double-x $x3) $z<-xy) x1-y2) 'z) 6+2i)
]

But the result of @racket[mix] is slightly more efficient in the former form
(fold right) than the present form (fold left).

However, since @racket[$double-x] inherits slot @racket[x] that @racket[$x3]
overrides, there is
clearly a dependency between the two that prevents them from commuting:
@examples[#:eval poof #:label #f
(code:comment "x6-y2 : (Fun 'x -> Nat | 'y -> Nat)")
(define x6-y2 (fix (mix $double-x $x3) x1-y2))
(code:comment "x3-y2 : (Fun 'x -> Nat | 'y -> Nat)")
(define x3-y2 (fix (mix $x3 $double-x) x1-y2))
(eval:check (x6-y2 'x) 6)
(eval:check (x3-y2 'x) 3)
]

@subsubsection{Record prototype generators}

Now that we understand record prototypes, we can look at various utility
functions to build them.

To define an object with a field @racket[k] mapped to a value @racket[v], use:
@examples[#:eval poof #:no-result
(code:comment "$field : (Fun k:Symbol V -> (Proto (Fun 'k -> V | A) A))")
(define ($field k v) (code:comment "k v: constant key and value for this defined field")
  (lambda (self super) (code:comment "self super: usual prototype variables")
    (lambda (msg) (code:comment "msg: message received by the object, a.k.a. method name.")
      (if (equal? msg k) v (code:comment "if the message matches the key, return the value")
        (super msg))))) (code:comment "otherwise, recur to the object's super object")
]

What of inheritance? Well, we can modify an inherited field using:
@examples[#:eval poof #:no-result
(code:comment "$field-modify : (Fun k:Symbol (Fun V -> W) -> (Proto (Fun 'k -> W | A) (Fun 'k -> V | A) A) st: (<: V W))")
(define ($field-modify k modify) (code:comment "k: constant key; modify: function from super-value to sub-value.")
  (lambda (self super)
    (lambda (msg)
      (if (equal? msg k) (modify (super msg))
        (super msg)))))
]

What if a field depends on other fields? We can use this function
@examples[#:eval poof #:no-result
(code:comment "$field-compute : (Fun k:Symbol (Fun A -> V) -> (Proto (Fun 'k -> V | A) A))")
(define ($field-compute k fun) (code:comment "k: constant key; fun: function from self to value.")
  (lambda (self super)
    (lambda (msg)
      (if (equal? msg k) (fun self)
        (super msg)))))
]

A very general form of slot compute-and-override would take as parameters both
the @racket[self] argument and a @racket[next-method] function to inherit the
slot value from the @racket[super] argument
@examples/module[poof #:no-result
(code:comment "$field-gen : (Fun k:Symbol (Fun A (Fun -> V) -> W) -> (Proto (Fun 'k -> W | A) (Fun 'k -> V | A) A) st: (<: V W))")
(define ($field-gen k fun) (code:comment "k: constant key; fun: function from self to value.")
  (lambda (self super)
    (lambda (msg)
      (if (equal? msg k) (fun self (lambda () (super msg)))
          (super msg)))))
]

We could redefine the former ones in terms of that latter:
@examples/module[poof #:no-result
(define ($field k v)
  ($field-gen k (lambda (_self _next-method) v)))
(define ($field-modify k modify)
  ($field-gen k (lambda (_self next-method) (modify (next-method)))))
(define ($field-compute k fun)
  ($field-gen k (lambda (self _next-method) (fun self))))
]

Thus you can re-define the above prototypes as:
@examples/module[poof #:no-result
(define $x3 ($field 'x 3))
(define $double-x ($field-modify 'x (lambda (x) (* 2 x))))
(define $z<-xy ($field-compute 'z (lambda (self) (+ (self 'x) (* 0+1i (self 'y))))))
]

Here is a universal bottom function to use as the base for fix:
@examples/module[poof #:no-result
(code:comment "bottom-record : (Fun Symbol -> _)")
(define (bottom-record msg) (error "unbound slot" msg))
]

To define a record with a single field @racket[foo] bound to @racket[0], we can
use:
@examples/module[poof #:label #f
(code:comment "x3 : (Fun 'x -> Nat)")
(define x3 (fix $x3 bottom-record))
(eval:check (x3 'x) 3)
]

To define a record with two fields @racket[x] and @racket[y] bound to @racket[1]
and @racket[2] respectively, we can use:
@examples/module[poof #:label #f
(code:comment "x1-y2 : (Fun 'x -> Nat | 'y -> Nat)")
(define x1-y2 (fix (mix ($field 'x 1) ($field 'y 2)) bottom-record))
(eval:check (x1-y2 'x) 1)
(eval:check (x1-y2 'y) 2)
]

@subsection{Prototype Basics}

@subsubsection{Composing prototypes}

The identity prototype, neutral element for mix, is as follows:
@examples/module[poof #:no-result
(code:comment "$id : (Proto X X)")
(define ($id f b) b)
]

It doesn't override any information from the super/base object,
but only passes it through. It also doesn't consult information in
the final fixed-point nor refers to it.

But maybe this prototype business is easier to understood when written in
"long form", with long identifiers, and traditional arguments "self" and
"super". Thus, @racket[$id] becomes:
@examples/module[poof #:no-result
(code:comment "identity-prototype : (Proto Instance Instance)")
(define (identity-prototype self super) super)
]

Thus, @racket[(fix p b)] becomes:
@examples/module[poof #:no-result
(code:comment "instantiate-prototype : (Fun (Proto Self Super) Super -> Self)")
(define (instantiate-prototype prototype base-super)
  (define self (prototype (lambda i (apply self i)) base-super))
  self)
]

A more thorough explanation of this fixed-point function is in @seclink["Appendix_B"]{Appendix B}.

And @racket[(mix p q)] becomes:
@examples/module[poof #:no-result
(code:comment "compose-prototypes : (Fun (Proto Self Super) (Proto Super Super2) -> (Proto Self Super2) st: (<: Self Super Super2))")
(define (compose-prototypes child parent)
  (lambda (self super) (child self (parent self super))))
]

Note the types of the variables and intermediate expressions:
@racketblock[
child : (Proto Self Super)
parent : (Proto Super Super2)
self : Self
super : Super2
(parent self super) : Super
(this self (parent self super)) : Self
]

When writing long-form functions instead of vying for conciseness, we will
use the same naming conventions as in the function above:
@itemize[
 @item{@racket[child] (or @racket[this]) for a @emph{prototype} at hand, in leftmost position;}
 @item{@racket[parent] for a @emph{prototype} it is being mixed with, in latter position;}
 @item{@racket[self] for the @emph{instance} that is a fixed point of the computation;}
 @item{@racket[super] for the base (or so-far accumulated) @emph{instance} of the computation.}
]

Note the important distinction between @emph{prototypes} and @emph{instances}.
Instances are elements of some function type, and are themselves
the results of the computation whereby prototypes are instantiated.
Prototypes are increments of computation, functions from instance (of a
subtype @racket[Self]) and instance (of a supertype @racket[Super]) to instance
of the subtype @racket[Self].

Now, prototypes are interesting because @racket[mix]
(a.k.a. @racket[compose-prototypes])
is an associative operator with neutral element @racket[$id]
(a.k.a. @racket[identity-prototype]).
Thus prototypes form a monoid, and you can compose
or instantiate a list of prototypes:
@examples[#:eval poof #:no-result
(code:comment "compose-protototype-list : (Fun (IndexedList I (lambda (i) (Proto (A_ i) (A_ (1+ i))))) -> (Proto (A_ 0) (A_ (Card I))))")
(define (compose-prototype-list l)
  (cond
   ((null? l) identity-proto)
   ((null? (cdr l)) (car l))
   ((null? (cddr l)) (compose-prototypes (car l) (cadr l)))
   (else (compose-prototypes (car l) (compose-prototype-list (cdr l))))))
]

A more succint way to write the same function is:
@examples/module[poof #:no-result
(define (compose-prototype-list prototype-list)
  (foldr compose-prototypes identity-prototype prototype-list))
]

@examples/module[poof #:no-result
(code:comment "instantiate-prototype-list : (Fun (IndexedList I (lambda (i) (Proto (A_ i) (A_ (1+ i))))) (A_ (Card I)) -> (A_ 0))")
(define (instantiate-prototype-list prototype-list base-super)
  (instantiate-prototype (compose-prototype-list prototype-list) base-super))
]

Prototype composition is notably more expressive than "single inheritance"
as commonly used in many "simple" object systems:
Object systems with "single inheritance" require programmers to @racket[cons]
objects (or classes) one component at a time in the front of a rigid list of
"parent" objects (or classes), where the base object (or class) is set.
Prototype object systems enable programmers to @racket[append] list of prototypes
independently from any base object, to compose and recompose prototypes
in different orders and combinations.
Prototypes are thus more akin to the "mixins" or "traits" of more advanced
objects systems.

Prototype composition however, does not by itself subsume multiple
inheritance. We will show in @seclink["Better_objects"]{chapter 4} how to
combine the two.

@subsubsection{The Bottom of it}

In a language with partial functions, such as Scheme, there is a practical
choice for a universal function to use as the @racket[base-super] argument to
@racket[instantiate-prototype]: the @racket[bottom] function, that never returns,
but instead, for enhanced usability, throws an error that can be caught.
@examples/module[poof #:no-result
(code:comment "bottom : (Fun I ... -> O ...)")
(define (bottom . args)
  (error "bottom" args))
]

Thus, in dialects with optional arguments, we could make @racket[bottom] the default
value for @racket[base-super]. Furthermore, in any variant of Scheme, we can define
the following function @racket[instance] that takes the rest of its arguments as
a list of prototypes, and instantiates the composition of them:
@examples/module[poof #:no-result
(code:comment "instance : (Fun (IndexedList I (lambda (i) (Proto (A_ i) (A_ (1+ i)))))... -> (A_ 0)))")
(define (instance . prototype-list)
  (instantiate-prototype-list prototype-list bottom))
]

What if you @emph{really} wanted to instantiate your list of prototypes with some
value @racket[b] as the base super instance? You can "just" tuck
@racket[(constant-prototype b)] at the tail end of your protototype list:
@examples/module[poof #:no-result
(code:comment "constant-prototype : (Fun A -> (Proto A _))")
(define (constant-prototype base-super)
  (lambda (_self _super) base-super))
]

Or the same with a shorter name and a familiar definition as a combinator
@examples/module[poof #:no-result
(code:comment "$const : (Fun A -> (Proto A _))")
(define ($const b) (lambda _ b))
]

Small puzzle for the points-free Haskellers reading this essay:
what change of representation will allow you to compose prototypes
with regular function composition instead of applying the
binary function @racket[mix]?

ROT13'ed answer:
gur pbzcbfnoyr cebgbglcr sbe cebgbglcr c vf (p c) = (ynzoqn (d) (zvk c d)),
naq gb erpbire gur hfhny cebgbglcr sebz vg, lbh whfg unir gb nccyl vg gb $vq.

@subsubsection{Note for code minimalists}

We described the @racket[fix] and @racket[mix] functions in
only 109 characters of Scheme.
We can do even shorter with various extensions.
MIT Scheme and after it Racket, Gerbil Scheme, and more, allow you to write:
@examples[#:eval poof #:no-result
(define ((mix p q) f b) (p f (q f b)))
]
And then we'd have Object Orientation in 100 characters only.

Then again, in Gerbil Scheme, we could get it down to only 86 (counting newline):
@examples[#:eval poof #:no-result
(def (fix p b) (def f (p (lambda i (apply f i)) b)) f)
]

Of, compressing spaces, to 78 (not counting newline, since we don't count spaces):
@examples[#:eval poof #:no-result
(def(fix p b)(def f(p(lambda i(apply f i))b))f)(def((mix p q)f b)(p f(q f b)))
]

@section{Pure Objective Fun}

@subsection{Using prototypes to incrementally define simple data structures}

@section{Prototypes beyond Objects}

@subsection{Prototypes for Numeric Functions}

@subsubsection{Prototypes to incrementally specify numeric functions}

@subsubsection{Number Thunks}

@section[#:tag "Better_objects"]{Better objects, still pure}

@section{Classes}

@section{Stateful Objects}

@section[#:tag "Appendix_A"]{Appendix A: A Digression about type notation}

@section[#:tag "Appendix_B"]{Appendix B: Fixed-Point functions}

@section{Chapter X: On Typing Prototypes}

@(finalize-examples/module poof)
