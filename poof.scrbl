#lang scribble/acmart @acmsmall @review

@; To be submitted to OOPSLA 2021? https://2021.splashcon.org/track/splash-2021-oopsla

@(require scriblib/bibtex
          (only-in scribble/manual racket racketblock litchar itemize item)
          (only-in scribble/example examples make-base-eval)
          (only-in scriblib/footnote note)
          syntax/parse/define
          "util/examples-module.rkt"
          (for-label racket))

@(declare-examples/module poof racket
   (provide (all-defined-out)))
@examples/module[poof #:hidden
(require (only-in racket/base [define def])
         "util/eval-check.rkt")]

@(define-simple-macro (r a ...) (racket a ...))
@(define-simple-macro (Definitions a ...) (examples/module poof #:no-result a ...))
@(define-simple-macro (Examples a ...) (examples #:eval poof #:no-result a ...))
@(define-simple-macro (Checks a ...) (examples #:eval poof #:label #f a ...))

@title{Prototype Object-Orientation Functionally}

@author[
  #:email (email "fare@mukn.io")
  #:affiliation (affiliation #:institution @institution{@emph{Mutual Knowledge Systems, Inc.}})
]{Francois-Rene Rideau}
@author[
  #:email (email "alexknauth@mukn.io")
  #:affiliation (affiliation #:institution @institution{@emph{Mutual Knowledge Systems, Inc.}})
]{Alex Knauth}
@author[
  #:email (email "namin@seas.harvard.edu")
  #:affiliation (affiliation #:institution @institution{@emph{Harvard University}})
]{Nada Amin}

@(define-bibtex-cite "poof.bib" ~cite citet generate-bibliography)

@;TAPL by @citet{tapl} is relevant.
@;TAPL@~cite{tapl} has a relevant chapter (§32).

@section[#:tag "Prototypes_bottom_up"]{Prototypes, bottom up}

@subsection{Object Orientation in 109 characters of standard Scheme}

@Definitions[
(define (fix p b) (define f (p (lambda i (apply f i)) b)) f)
(define (mix c p) (lambda (f b) (c f (p f b))))
]

We contend that the above two definitions summarize
the essence of object-oriented programming, and that
all the usual “object oriented” concepts can be easily recovered from them.
The rest of this essay will make the case.

@subsubsection{The Essence of OOP}

Object-Oriented Programming consists in specifying code in modular increments
that each compute their part from the combined whole and the computation so far.

We will realize these increments as @emph{prototypes}:
functions of two arguments, @r[self] and @r[super]
(or, above, @r[f] and @r[b], for fixed-point and base value).
Function @r[fix] @emph{instantiates} a prototype @r[p]
given a super/base value @r[b].
Function @r[mix] has @emph{child} prototype @r[c] @emph{inherit}
from @emph{parent} prototype @r[p]
so they operate on a same fixed-point @r[f]
while chaining their effects on @r[b].

Given some arbitrary instance type @r[Self],
and a super-type @r[Super] of @r[Self],
a prototype for @r[Self] from @r[Super] will thus be
a function from @r[Self] and @r[Super] to @r[Self].
@racketblock[
(deftype (Proto Self Super) (Fun Self Super -> Self st: (<: Self Super)))
fix : (Fun (Proto Self Super) Super -> Self st: (<: Self Super))
mix : (Fun (Proto Self Super) (Proto Super Sup2) -> (Proto Self Sup2)
        st: (<: Self Super Sup2))
]

The first argument @r[self] of type @r[Self] will hold the instance
resulting as a fixed point from the entire computation.
When composing multiple prototypes, every prototype will receive
the @emph{same} value as their @r[self] argument:
the complete instance that results from applying the every prototype in order.
This allows prototypes to “cooperate” with each other
on @emph{different} aspects of the computation,
wherein one prototype defines some aspect (e.g. a “method” in some dictionary)
while relying on aspects to be defined by other prototypes (e.g. other methods),
accessed through the @r[self] argument in what is called “late binding”.

The second argument @r[super] by contrast holds the partial result of the
fixed-point computation after applying only the “next” prototypes.
When composing multiple prototypes, each prototype will (presumably) receive
a different value. The last prototype in the list (rightmost, most ancestral
parent) will receive the “base” or “bottom” value from the fix function
(often literally the bottom value or function in the language), then the
"previous" prototype (its child, to the left) will receive ("inherit")
the result of that “next” computation (its parent, to the right), and so on
until the first prototype (leftmost, most recent child) inherits
its @r[super] value from the rest and computes the final instance.
This allows prototypes to cooperate with other prototypes on a @emph{same} aspect
of the instance computation, wherein children prototypes can accumulate, modify
or override the method values inherited from the parent prototypes.

@subsubsection{Applicability to other Programming Languages}

The two definitions above can be easily translated to any language with closures
and either dynamic types or dependent types. However, their potential is not
fully realized in languages with mere parametric polymorphism.
@; TODO: insert above reference to type discussion?
Furthermore, for an @emph{efficient} implementation of objects with our formulas,
we will also require lazy evaluation (or side effects to implement them)
as a optional or ubiquitous language feature.
We do not otherwise require side-effects---though they can be used
for the usual optimizations in the common “linear” case.

@subsection{A minimal object system}

@subsubsection{Records as functions}

Let us relate the above two functions to objects,
by first encoding “records” of multiple named values as functions
from symbol (the name of a slot) to value (bound to the slot).
In accordance with Lisp tradition,
we will say “slot” where others may say “field” or “member” or “method”,
and say that the slot is bound to the given value
rather than it “containing” the value or any such thing.

Thus, the function @r[x1-y2] below encodes a record with two slots
@r[x] and @r[y] bound respectively to @r[1] and @r[2].

@Examples[
(code:comment "x1-y2 : (Fun 'x -> Nat | 'y -> Nat)")
(define (x1-y2 msg) (case msg ((x) 1)
                              ((y) 2)
                              (else (error "unbound slot" msg))))
]

We can check that we can indeed access the record slots
and get the expected values:

@Checks[
(eval:check (x1-y2 'x) 1)
(eval:check (x1-y2 'y) 2)
]

Note that we use Lisp symbols for legibility, but in poorer languages,
"symbols" could be any large enough type with decidable equality,
e.g. integers or strings, etc.

@subsubsection{Prototypes for Records}

A @emph{prototype} for a record is a function of two arguments @r[self] and
@r[super] (both records) to an @r[extended-self] record.
To distinguish prototypes from instances and other values,
we will follow the convention of prefixing with @litchar{$} the names of prototypes.
Thus, the following prototype extends or overrides its super-record with
a slot @r[x] unconditionally bound to @r[3]:
@Examples[
(code:comment "$x3 : (Proto (Fun 'x -> Nat | A) A)")
(define ($x3 self super) (λ (msg) (if (eq? msg 'x) 3 (super msg))))
]

This prototype computes a complex number slot @r[z] based on real and
imaginary values bound to the respective slots @r[x] and @r[y] of its
@r[self]:
@Examples[
(code:comment "$z<-xy : (Proto (Fun (Or 'x 'y) -> Real | 'z -> Complex | A) (Fun (Or 'x 'y) -> Real | A))")
(define ($z<-xy self super)
  (λ (msg)
    (case msg
      ((z) (+ (self 'x) (* 0+1i (self 'y))))
      (else (super msg)))))
]

That prototype doubles the number in slot @r[x] from its @r[super] record.
We say that it @emph{inherits} the value for slot @r[x]:
@Examples[
(code:comment "$double-x : (Proto (Fun 'x -> Number | A) (Fun 'x -> Number | A))")
(define ($double-x self super)
  (λ (msg) (if (eq? msg 'x) (* 2 (super 'x)) (super msg))))
]

More generally a record prototype extends its @r[super] record with new slots
and/or overrides the values bound to its existing slots, and may in the
process refer to both the records @r[self] and @r[super] and their slots,
with some obvious restrictions to avoid infinite loops from circular definitions.

@subsubsection{Basic testing} How to test these prototypes?
With the @r[fix] operator using the above record @r[x1-y2] as base value:

@Checks[
(code:comment "x3-y2 : (Fun 'x -> Nat | 'y -> Nat)")
(define x3-y2 (fix $x3 x1-y2))
(eval:check (list (x3-y2 'x) (x3-y2 'y)) '(3 2))

(code:comment "z1+2i : (Fun 'x -> Nat | 'y -> Nat | 'z -> Complex)")
(define z1+2i (fix $z<-xy x1-y2))
(eval:check (list (z1+2i 'x) (z1+2i 'y) (z1+2i 'z)) '(1 2 1+2i))

(code:comment "x2-y2 : (Fun 'x -> Nat | 'y -> Nat)")
(define x2-y2 (fix $double-x x1-y2))
(eval:check (list (x2-y2 'x) (x2-y2 'y)) '(2 2))
]

We can also @r[mix] these prototypes together before to compute the @r[fix]:
@Checks[
(code:comment "z6+2i : (Fun 'x -> Nat | 'y -> Nat | 'z -> Complex)")
(define z6+2i (fix (mix $z<-xy (mix $double-x $x3)) x1-y2))
(eval:check (map z6+2i '(x y z)) '(6 2 6+2i))
]

And since the @r[$z<-xy] prototype got the @r[x] and @r[y] values
from the @r[self]
and not the @r[super], we can freely commute it with the other two prototypes
that do not affect either override slot @r['z] or inherit from it:
@Checks[
(eval:check ((fix (mix $z<-xy (mix $double-x $x3)) x1-y2) 'z) 6+2i)
(eval:check ((fix (mix $double-x (mix $z<-xy $x3)) x1-y2) 'z) 6+2i)
(eval:check ((fix (mix $double-x (mix $x3 $z<-xy)) x1-y2) 'z) 6+2i)
]

@r[mix] is associative, and therefore the following forms are equivalent to the previous ones,
though the forms above (fold right) are slightly more efficient than the forms below (fold left):
@Checks[
(eval:check ((fix (mix (mix $z<-xy $double-x) $x3) x1-y2) 'z) 6+2i)
(eval:check ((fix (mix (mix $double-x $z<-xy) $x3) x1-y2) 'z) 6+2i)
(eval:check ((fix (mix (mix $double-x $x3) $z<-xy) x1-y2) 'z) 6+2i)
]

Now, since @r[$double-x] inherits slot @r[x] that @r[$x3] overrides,
there is clearly a dependency between the two that prevents them from commuting:
@Checks[
(code:comment "x6-y2 : (Fun 'x -> Nat | 'y -> Nat)")
(define x6-y2 (fix (mix $double-x $x3) x1-y2))
(code:comment "x3-y2 : (Fun 'x -> Nat | 'y -> Nat)")
(define x3-y2 (fix (mix $x3 $double-x) x1-y2))
(eval:check (list (x6-y2 'x) (x3-y2 'x)) '(6 3))
]

@subsubsection{Record prototype generators}

Now that we understand record prototypes, we can look at various utility
functions to build them.

To define an object with a slot @r[k] mapped to a value @r[v], use:
@Definitions[
(code:comment "$slot : (Fun k:Symbol V -> (Proto (Fun 'k -> V | A) A))")
(define ($slot k v) (code:comment "k v: constant key and value for this defined slot")
  (λ (self super) (code:comment "self super: usual prototype variables")
    (λ (msg) (code:comment "msg: message received by the object, a.k.a. method name.")
      (if (equal? msg k) v (code:comment "if the message matches the key, return the value")
        (super msg))))) (code:comment "otherwise, recur to the object's super object")
]

What of inheritance? Well, we can modify an inherited slot using:
@Definitions[
(code:comment "$slot-modify : (Fun k:Symbol (Fun V -> W) -> (Proto (Fun 'k -> W | A) (Fun 'k -> V | A) A) st: (<: V W))")
(define ($slot-modify k modify) (code:comment "modify: function from super-value to value")
  (λ (self super) (λ (msg)
    (if (equal? msg k) (modify (super msg))
      (super msg)))))
]

What if a slot depends on other slots? We can use this function
@Definitions[
(code:comment "$slot-compute : (Fun k:Symbol (Fun A -> V) -> (Proto (Fun 'k -> V | A) A))")
(define ($slot-compute k fun) (code:comment "fun: function from self to value.")
  (λ (self super)
    (λ (msg)
      (if (equal? msg k) (fun self)
        (super msg)))))
]

A very general form of slot compute-and-override would take as parameters both
the @r[self] argument and a @r[next-method] function to inherit the
slot value from the @r[super] argument
@Definitions[
(code:comment "$slot-gen : (Fun k:Symbol (Fun A (Fun -> V) -> W) -> (Proto (Fun 'k -> W | A) (Fun 'k -> V | A) A) st: (<: V W))")
(define ($slot-gen k fun) (code:comment "fun: from self and super-value thunk to value.")
  (λ (self super)
    (λ (msg)
      (if (equal? msg k) (fun self (λ () (super msg)))
        (super msg)))))
]

We could redefine the former ones in terms of that latter:
@Examples[
(define ($slot k v)
  ($slot-gen k (λ (_self _next-method) v)))
(define ($slot-modify k modify)
  ($slot-gen k (λ (_self next-method) (modify (next-method)))))
(define ($slot-compute k fun)
  ($slot-gen k (λ (self _next-method) (fun self))))
]

Thus you can re-define the above prototypes as:
@Examples[
(define $x3 ($slot 'x 3))
(define $double-x ($slot-modify 'x (λ (x) (* 2 x))))
(define $z<-xy ($slot-compute 'z (λ (self) (+ (self 'x) (* 0+1i (self 'y))))))
]

Here is a universal bottom function to use as the base for fix:
@Definitions[
(code:comment "bottom-record : (Fun Symbol -> _)")
(define (bottom-record msg) (error "unbound slot" msg))
]

To define a record with a single slot @r[foo] bound to @r[0], we can use:
@Checks[
(code:comment "x3 : (Fun 'x -> Nat)")
(define x3 (fix $x3 bottom-record))
(eval:check (x3 'x) 3)
]

To define a record with two slots @r[x] and @r[y] bound to @r[1]
and @r[2] respectively, we can use:
@Checks[
(code:comment "x1-y2 : (Fun 'x -> Nat | 'y -> Nat)")
(define x1-y2 (fix (mix ($slot 'x 1) ($slot 'y 2)) bottom-record))
(eval:check (map x1-y2 '(x y)) '(1 2))
]

@subsubsection{Back to 1981!}

Interestingly, the above approach to objects is essentially equivalent,
though not exactly isomorphic, to what Yale T had in 1981 @~cite{Adams89object-orientedprogramming},
that was notably reprised by YASOS in 1992 @~cite{dickey1992scheming}
and distributed with SLIB since.
There are minor differences: what we call “instance”, T calls “object” or “instance”,
but instead of “prototypes” it has “components” with a slightly different API.
Also T assumes methods are all function-valued, to be immediately called
when methods are looked up, as if all accesses to an object went through the @r[operate] function below.
Methods in T can be directly represented as slots with a function value in our approach.
Non-function-valued slots in our approach above can be represented in T
by nullary methods returning the constant value.
@Definitions[
(define (operate instance selector . args) (apply (instance selector) args))
]

@subsection{Prototype Basics}

@subsubsection{Long-form Basics}

Let's rewrite @r[fix] and @r[mix] in long-form
with variables @r[self] and @r[super] instead of @r[f] and @r[b], etc.
Thus, the instantiation function @r[(fix p b)] becomes:
@Definitions[
(code:comment "instantiate-prototype : (Fun (Proto Self Super) Super -> Self)")
(define (instantiate-prototype prototype base-super)
  (define self (prototype (λ i (apply self i)) base-super))
  self)
]

A more thorough explanation of the above fixed-point function is in @seclink["Appendix_B"]{Appendix B}.
Meanwhile, the composition function @r[(mix p q)] becomes:
@Definitions[
(code:comment "compose-prototypes : (Fun (Proto Self Super) (Proto Super Super2) -> (Proto Self Super2) st: (<: Self Super Super2))")
(define (compose-prototypes child parent)
  (λ (self super2) (child self (parent self super2))))
]

Note the types of the variables and intermediate expressions:
@racketblock[
child : (Proto Self Super)
parent : (Proto Super Super2)
self : Self
super2 : Super2
(parent self super2) : Super
(this self (parent self super2)) : Self
]

When writing long-form functions, instead of vying for conciseness, we will
use the same naming conventions as in the function above:
@itemize[
 @item{@r[child] (or @r[this]) for a @emph{prototype} at hand, in leftmost position;}
 @item{@r[parent] for a @emph{prototype} that is being mixed with, in latter position;}
 @item{@r[self] for the @emph{instance} that is a fixed point of the computation;}
 @item{@r[super] for the base (or so-far accumulated) @emph{instance} of the computation.}
]

Note the important distinction between @emph{prototypes} and @emph{instances}.
Instances are elements of some function type, and are themselves
the outputs of the instantiation wherein prototypes are inputs.
Prototypes are increments of computation, functions from instance (of a
subtype @r[Self]) and instance (of a supertype @r[Super]) to instance
of the subtype @r[Self], both inputs and outputs of composition.

@subsubsection{Composing prototypes}

The identity prototype as follows is neutral element for mix:
@Definitions[
(code:comment "$id : (Proto X X)")
(define ($id f b) b)
]

It doesn't override any information from the super/base object,
but only passes it through. It also doesn't consult information in
the final fixed-point nor refers to it. In “long form”, it becomes:
@Definitions[
(code:comment "identity-prototype : (Proto Instance Instance)")
(define (identity-prototype self super) super)
]

Now, prototypes are interesting because @r[mix] (a.k.a. @r[compose-prototypes])
is an associative operator with neutral element @r[$id] (a.k.a. @r[identity-prototype]).
Thus prototypes form a monoid, and you can compose
or instantiate a list of prototypes:
@Definitions[
(code:comment "compose-prototype-list : (Fun (IndexedList I (λ (i) (Proto (A_ i) (A_ (1+ i))))) -> (Proto (A_ 0) (A_ (Card I))))")
(define (compose-prototype-list l)
  (cond
   ((null? l) identity-prototype)
   ((null? (cdr l)) (car l))
   ((null? (cddr l)) (compose-prototypes (car l) (cadr l)))
   (else (compose-prototypes (car l) (compose-prototype-list (cdr l))))))
]

A more succint way to write the same function is:
@Examples[
(define (compose-prototype-list prototype-list)
  (foldr compose-prototypes identity-prototype prototype-list))
]

@Definitions[
(code:comment "instantiate-prototype-list : (Fun (IndexedList I (λ (i) (Proto (A_ i) (A_ (1+ i))))) (A_ (Card I)) -> (A_ 0))")
(define (instantiate-prototype-list prototype-list base-super)
  (instantiate-prototype (compose-prototype-list prototype-list) base-super))
]

Prototype composition is notably more expressive than “single inheritance”
as commonly used in many “simple” object systems:
Object systems with “single inheritance” require programmers to @r[cons]
objects (or classes) one component at a time in the front of a rigid list of
"parent" objects (or classes), where the base object (or class) is set.
Prototype object systems enable programmers to @r[append] lists of prototypes
independently from any base object, to compose and recompose prototypes
in different orders and combinations.
Prototypes are thus more akin to the “mixins” or “traits” of more advanced
objects systems.
Prototype composition however, does not by itself subsume multiple
inheritance. We will show in @seclink["Better_objects"]{chapter 4} how to
combine the two.

@subsubsection{The Bottom of it}

In a language with partial functions, such as Scheme, there is a practical
choice for a universal function to use as the @r[base-super] argument to
@r[instantiate-prototype]: the @r[bottom] function, that never returns,
but instead, for enhanced usability, throws an error that can be caught.
@Definitions[
(code:comment "bottom : (Fun I ... -> O ...)")
(define (bottom . args) (error "bottom" args))
]

Thus, in dialects with optional arguments, we could make @r[bottom] the default
value for @r[base-super]. Furthermore, in any variant of Scheme, we can define
the following function @r[instance] that takes the rest of its arguments as
a list of prototypes, and instantiates the composition of them:
@Definitions[
(code:comment "instance : (Fun (IndexedList I (λ (i) (Proto (A_ i) (A_ (1+ i)))))... -> (A_ 0)))")
(define (instance . prototype-list)
  (instantiate-prototype-list prototype-list bottom))
]

What if you @emph{really} wanted to instantiate your list of prototypes with some
value @r[b] as the base super instance? You can “just” tuck
@r[(constant-prototype b)] at the tail end of your prototype list:
@Definitions[
(code:comment "constant-prototype : (Fun A -> (Proto A _))")
(define (constant-prototype base-super) (λ (_self _super) base-super))
]

Or the same with a shorter name and a familiar definition as a combinator
@Definitions[
(code:comment "$const : (Fun A -> (Proto A _))")
(define ($const b) (λ _ b))
]

Small puzzle for the points-free Haskellers reading this essay:
what change of representation will enable prototypes to be composed like regular functions
without having to apply a binary function like @r[mix]? Solution in footnote.@note{
Represent prototype @r[p] as
@r[(λ (q) (mix p q)) : (Fun (Proto Super S2) -> (Proto Self S2))].
To recover @r[p] from that, just apply to @r[$id].
}

@section{Pure Objective Fun}

@subsection{Using prototypes to incrementally define simple data structures}

@subsubsection{Prototypes for Order}

Let's use prototypes to build some simple data structures.
First, we'll write prototypes that offer an abstraction for the ability
to compare elements of a same type at hand, in this case,
either numbers or strings.
@Definitions[
(define ($number-order self super)
  (λ (msg) (case msg
             ((<) (λ (x y) (< x y)))
             ((=) (λ (x y) (= x y)))
             ((>) (λ (x y) (> x y)))
             (else (super msg)))))
(define ($string-order self super)
  (λ (msg) (case msg
             ((<) (λ (x y) (string<? x y)))
             ((=) (λ (x y) (string=? x y)))
             ((>) (λ (x y) (string>? x y)))
             (else (super msg)))))
]

We can add a “mixin” for a @r[compare] operator that summarizes in one call
the result of comparing two elements of the type being described.
A mixin is a prototype meant to extend other prototypes.
See how this mixin can be used to extend either of the prototypes above.
Also notice how, to refer to other slots in the eventual instance,
we call @r[(self '<)] and suches.
@Definitions[
(define ($compare<-order self super)
  (λ (msg) (case msg
             ((compare) (λ (x y)
                          (cond (((self '<) x y) '<)
                                (((self '>) x y) '>)
                                (((self '=) x y) '=)
                                (else (error "incomparable" x y)))))
             (else (super msg)))))
(define number-order (instance $number-order $compare<-order))
(define string-order (instance $string-order $compare<-order))]
@Checks[
(eval:check ((number-order '<) 23 42) #t)
(eval:check ((number-order 'compare) 8 4) '>)
(eval:check ((string-order '<) "Hello" "World") #t)
(eval:check ((string-order 'compare) "Foo" "FOO") '>)
(eval:check ((string-order 'compare) "42" "42") '=)
]

We can define a order on symbols by delegating to strings!
@Definitions[
(define ($symbol-order self super)
  (λ (msg) (case msg
             ((< = > compare)
              (λ (x y) ((string-order msg) (symbol->string x) (symbol->string y))))
             (else (super msg)))))
(define symbol-order (instance $symbol-order))]
@Checks[
(eval:check ((symbol-order '<) 'aardvark 'aaron) #t)
(eval:check ((symbol-order '=) 'zzz 'zzz) #t)
(eval:check ((symbol-order '>) 'aa 'a) #t)
(eval:check ((symbol-order 'compare) 'alice 'bob) '<)
(eval:check ((symbol-order 'compare) 'b 'c) '<)
(eval:check ((symbol-order 'compare) 'a 'c) '<)]

@subsubsection{Prototypes for Binary Trees}

We can use the above @r[order] prototypes to build binary trees
over a suitable ordered key type @r[Key].
We'll represent a tree as a list of left-branch,
list of key-value pair and ancillary data, and right-branch,
which preserves the order of keys when printed:
@Definitions[
(define ($binary-tree-map self super)
  (λ (msg)
    (define (node l kv r) ((self 'node) l kv r))
    (case msg
      ((empty) '())
      ((empty?) null?)
      ((node) (λ (l kv r) (list l (list kv) r)))
      ((singleton) (λ (k v) (node '() (cons k v) '())))
      ((acons)
       (λ (k v t)
         (if ((self 'empty?) t) ((self 'singleton) k v)
             (let* ((tl (car t)) (tkv (caadr t)) (tk (car tkv)) (tr (caddr t)))
               (case (((self 'Key) 'compare) k tk)
                 ((=) (node tl (cons k v) tr))
                 ((<) (node ((self 'acons) k v tl) tkv tr))
                 ((>) (node tl tkv ((self 'acons) k v tr))))))))
      ((ref)
       (λ (t k e)
         (if ((self 'empty?) t) (e)
             (let ((tl (car t)) (tk (caaadr t)) (tv (cdaadr t)) (tr (caddr t)))
               (case (((self 'Key) 'compare) k tk)
                 ((=) tv)
                 ((<) ((self 'ref) tl k e))
                 ((>) ((self 'ref) tr k e)))))))
      ((afoldr)
       (λ (acons empty t)
         (if ((self 'empty?) t) empty
           (let ((tl (car t)) (tk (caaadr t)) (tv (cdaadr t)) (tr (caddr t)))
             ((self 'afoldr)
              acons (acons tk tv ((self 'afoldr) acons empty tl)) tr)))))
      (else (super msg)))))]

With this scaffolding, we can define a dictionary data structure
that we can use later to differently represent objects:
@Definitions[
(define symbol-tree-map (instance ($slot 'Key symbol-order) $binary-tree-map))
]
However, when we use it, we immediately find an issue:
trees will too often be skewed, leading to long access times,
especially so when building them from an already ordered list:
@Checks[
(define my-binary-dict (code:comment "heavily skewed right, height 5")
  (foldl (λ (kv t) ((symbol-tree-map 'acons) (car kv) (cdr kv) t))
         (symbol-tree-map 'empty)
         '((a . "I") (b . "II") (c . "III") (d . "IV") (e . "V"))))
(eval:check my-binary-dict '(() ((a . "I")) (() ((b . "II")) (() ((c . "III")) (() ((d . "IV")) (() ((e . "V")) ()))))))]

But binary trees otherwise work:
@Checks[
(eval:check (map (λ (k) ((symbol-tree-map 'ref) my-binary-dict k (λ () #f))) '(a b c d e z))
            '("I" "II" "III" "IV" "V" #f))]

@subsubsection{Prototypes for @emph{Balanced} Binary Trees}

We can incrementally define a balanced tree data structure
(in this case, using the AVL balancing algorithm)
by overriding a single method of the original binary tree prototype:
@Definitions[
(define ($avl-tree-rebalance self super)
  (λ (msg)
    (define (left t) (car t))
    (define (kv t) (caadr t))
    (define (height t) (if (null? t) 0 (cdadr t)))
    (define (right t) (caddr t))
    (define (balance t) (if (null? t) 0 (- (height (right t)) (height (left t)))))
    (define (mk l kv r)
      (let ((lh (height l)) (rh (height r)))
        (or (member (- rh lh) '(-1 0 1)) (error "tree unbalanced!"))
        (list l (cons kv (+ 1 (max lh rh))) r)))
    (define (node l ckv r)
      (case (- (height r) (height l))
        ((-1 0 1) (mk l ckv r))
        ((-2) (case (balance l)
                ((-1 0) (mk (left l) (kv l) (mk (right l) ckv r))) ;; LL rebalance
                ((1) (mk (mk (left l) (kv l) (left (right l))) ;; LR rebalance
                         (kv (right l)) (mk (right (right l)) ckv r)))))
        ((2) (case (balance r)
               ((-1) (mk (mk l ckv (left (left r))) ;; RL rebalance
                         (kv (left r)) (mk (right (left r)) (kv r) (right r))))
               ((0 1) (mk (mk l ckv (left r)) (kv r) (right r))))))) ;; RR rebalance
    (case msg ((node) node) (else (super msg)))))
(define symbol-avl-map
  (instance $avl-tree-rebalance $binary-tree-map ($slot 'Key symbol-order)))]

Our dictionary is now well-balanced, height 3, and the tests still pass:
@Checks[
(define my-avl-dict
  (foldl (λ (kv t) ((symbol-avl-map 'acons) (car kv) (cdr kv) t))
         (symbol-avl-map 'empty)
         '((a . "I") (b . "II") (c . "III") (d . "IV") (e . "V"))))
(eval:check my-avl-dict
                '((() ((a . "I") . 1) ()) ((b . "II") . 3)
                  ((() ((c . "III") . 1) ()) ((d . "IV") . 2) (() ((e . "V") . 1) ()))))
(eval:check (map (λ (k) ((symbol-avl-map 'ref) my-avl-dict k (λ () #f))) '(a b c d e z))
            '("I" "II" "III" "IV" "V" #f))
]

@section{Beyond Objects and back}

Prototypes are not just for records as functions from symbol to value:
they can be used to incrementally specify any kind of functions!

@subsection{Prototypes for Numeric Functions}

Let's see how prototypes can be used to build functions from real numbers to real numbers.
The following prototype is a mixin for an even function,
that delegates to other prototypes the images of positive values
and returns the image of the opposite for negative values:
@Examples[
(define ($even self super)
  (λ (x) (if (< x 0) (self (- x)) (super x))))
]
The following prototype is a mixin for squaring the parent value:
@Examples[
(define ($cube self super)
  (λ (x) (let ((y (super x))) (* y y y))))
]
We can instantiate a function out of those of prototypes, and test it:
@Checks[
(define absx3 (instance $even $cube ($const (λ (x) x))))
(eval:check (map absx3 '(3 -2 0 -1)) '(27 8 0 1))
]

@subsubsection{Number Thunks}

Now, the simplest numeric functions are thunks: nullary functions that yield a number.
@Checks[
(define (b0) 0)
(define (p1+ _ b) (λ () (+ 1 (b))))
(define (p2* _ b) (λ () (* 2 (b))))
(eval:check ((fix (mix p1+ p2*) b0)) 1)
(eval:check ((fix (mix p2* p1+) b0)) 2)
]

@subsection{Prototypes are for Computations not Values}

Prototypes for number thunks can be generalized to prototypes for any kind of thunks:
you may incrementally specify instances of arbitrary types using prototypes,
by first wrapping values of that type into a thunk.
An alternative to thunks would be to use Scheme's @r[delay] special form,
or whatever form of @emph{lazy evaluation} is available in the language at hand.

To reprise the Call-By-Push-Value paradigm @~cite{conf/tlca/Levy99},
prototypes incrementally specify @emph{computations} rather than @emph{values}.
in applicative languages we reify these computations as values one way or the other,
as functions or delayed values.

Now, most interesting prototypes will only lead to error or divergence if you try
to instantiate them by themselves---they are “mixins”,
just like $r[$compare<-order] or $r[$avl-tree-rebalance] above,
designed to be combined with other prototypes.
Indeed, the whole entire point of incrementally specifying computations is
that you want to manipulate fragments that are not complete specifications.
To use these fragments in a total language where all computations terminates
will require attaching to each prototype some side-condition as to
which aspects of a computation it provides that other prototypes may rely on,
and which aspects of a computation it requires that other prototypes must provide.
This side-condition may well be a type in one of the many existing type systems
for objects @~cite{Abadi97atheory} @~cite{tapl}.

@Definitions[
(code:comment "(deftype (DelayedProto Self Super) (Fun (Delayed Self) (Delayed Super) -> Self))")
(code:comment "delayed-fix : (Fun (DelayedProto Self Super) (Delayed Super) -> (Delayed Self))")
(define (delayed-fix p b) (define f (delay (p f b))) f)
(code:comment "delayed-mix : (Fun (DelayedProto Self Super) (DelayedProto Super Super2) -> (DelayedProto Self Super2))")
(define (delayed-mix c p) (λ (f b) (c f (delay (p f b)))))
(code:comment "delayed-$id : (DelayedProto X X)")
(define (delayed-$id f b) (λ (_self super) (force super)))
]

@subsection{Prototypes in Lazy Pure Functional Dynamic Languages}

Interestingly, this is exactly how objects are encoded in
the pure lazy functional dynamically typed language Nix @~cite{dolstra2008nixos}.

Since 2015, the Nix standard library contains variants of the @r[fix] et @r[mix] functions,
wherein instances of its “attribute sets” mapping from strings to values
can be defined in terms of some base definition and one or many “extensions”
that are functions from attribute sets @r[self] and @r[super] to
an attribute set that is meant to extend and override @r[super]
as incremental contribution to the computation of the @r[self] fixed-point.
Apart from minor details and their construction being specialized for “attribute sets”,
this is the very same construction that we are presenting,
even though Nix's extension system was not even consciously meant
as an object system when it was designed.
This is not a coincidence, since the present essay started with
an effort to formalize the essence of objects as understood from Nix and Jsonnet.

Indeed, Jsonnet @~cite{jsonnet}, another lazy pure functional dynamic language,
sports an object system that is semantically equivalent to that of Nix.
Jsonnet itself was invented as a simplified semantic reconstruction of the essential ideas
behind GCL, the decade-older configuration language used everywhere inside Google.
GCL also wasn't explicitly designed as an object system, yet ended up having discovered
an excellent point in the design space, despite the overall clunkiness of the language.
That across several decades, closely matching designs were independently reinvented many times
without the intention to do so, is a good sign that this design is a “fixed point in design space”,
akin to the notion of “fixed point in time” in Dr Who @~cite{DrWhoFPIT}:
however you may try to reach a different conclusion, you still end-up with that fixed-point eventually.

Still, there are ways in which Jsonnet and Nix improved upon
the prototype object systems as initially designed in ThingLab @~cite{Borning77} or T @~cite{Rees82t:a},
or later made popular by SELF or JavaScript @; cite
As in these previous inventions, Jsonnet and Nix use first-class functions to construct objects.
They also rely on dynamic types to avoid the need for dependent types and internal type theories
required to statically type first-class prototypes in the most general case.
But unlike previous incarnations, they also rely on lazy evaluation as a way to express
first-class computations that can be manipulated whether or not they terminate,
without a clumsy explicit wrapping in a thunk or a lazy constructor.
They also do without having to resort to side-effects;
the pure functional semantics are thus not only especially clean and simple,
fitting in a few hundreds of characters,
but reveal an underlying mathematical structure of universal interest.

The success of Jsonnet and Nix at modularly managing complex configurations
through object extensions suggest that pure lazy functional programming is useful
way beyond the popular package offered by Haskell, of this programming model
with ML-like static typesystem that sports parametric polymorphism but no dependent types.
That typesystem rejects most interesting uses of prototypes,
thereby neglecting an important application domain for pure lazy functional programming.
Thus, though some claim the ML typesystem is at a "sweet spot",
wherein it provides good expressiveness while still being computable
and providing understandable error messages @~cite{minsky08},
maybe the spot is too sweet to be healthy, and
is lacking in proteins, or at least in prototypes.

Now, there is another special way in which Jsonnet and Nix improve upon T's objects,
that provides insight into Object-Oriented Programming:
they unify instances and prototypes as objects.
We'll come back to that in section 4. @; TODO: use @seclink

@subsection{Prototypes are Useful Even Without Subtyping}

The above example of prototypes for numeric functions also illustrate that
even if you language's only subtyping relationship is
the identity whereby each type is the one and only subtype or itself
(or something slightly richer with syntactic variable substitution in parametric polymorphism),
then you can use prototypes to incrementally specify computations,
with the following monomorphic types:
@racketblock[
(deftype (MProto A) (Fun A A -> A))
fix : (Fun (MProto A) A -> A)
mix : (Fun (MProto A) (MProto A) -> (MProto A))
]
As per the previous discussion, if your language doesn't use lazy evaluation,
you may have to constrain the @r[A]'s to be functions, or to
wrap the @r[A]'s in naked input position inside a @r[Lazy] type constructor.

Lack of subtyping greatly reduces the expressive power of prototypes;
yet, as we'll see, @; TODO: add suitable @seclink
the most popular use of prototypes in Object-Oriented Programming is completely monomorphic:
prototypes for type descriptors, a.k.a. classes;
only this common use of prototypes happens at the meta-level,
classes being second-class objects (pun not intended by the clueless practitioners).

@section[#:tag "Better_objects"]{Better objects, still pure}

In @seclink["Prototypes_bottom_up"]{section 1},
we implemented a rudimentary object system on top of prototypes.
Compared to mainstream systems, it already featured powerful notions:
@itemize[
@item{Mixins, more general than single-inheritance, yet requires much more
maintenance than multiple inheritance; using isn't modular.}
]

Now, let's try to build a more featureful one.
@itemize[
@item{
   One downside of the previous object system is that
   there was no introspection as to what keys were provided by a prototype.
}@item{
   Another downside was that we had to separately manipulate two different
   kinds of entities, prototypes and instances, even though, often,
   we want to deal with only one entity that can play either/both roles.
}@item{
   Finally, the object system was barebones and doesn't support additional
   features like multiple inheritance, type annotations, method combinations,
   multiple-dispatch, ...
}]

A few possible solutions to the first issue:
@itemize[
@item{
  Have prototype functions respond to a special magic message @r['keys].
  It is the responsibility of the programmer to make sure this message
  is and remains in synch with the actual list of messages supported---or
  we could provide some macros for it.
}@item{
  Instead of passing around prototype functions alone, we could pass around
  a product of a function and a list of keys, as a list or other structure.
  There again, macros can do that for us, and even abstract over which
  encoding is used, whether the above or any.
}@item{
  Directly pass around a structure that embodies a dictionary mapping
  symbols to functions, e.g. a hash-table from SRFI-125, or even just
  the pure symbol-avl-map we just implemented in chapter II.
}]

Possible solutions to the second issue:
@itemize[
@item{
  Use the prototypes as a way to compute the instance through fixed-point,
  but use a wrapper to present a better interface to the result, that
  among other things will keep the prototype information available together
  with the instance information, in a single entity.
  Separating instance computation and instance usage also allows us to
  provide two distinct representations for the same information, one
  optimized for construction, the other for consumption.
}]

Possible solutions to the third issue:
@itemize[
@item{
  Consider that the composable "prototype information" isn't just
  a function prototype, but also other meta-information like
  a list of direct super prototypes (for multiple inheritance),
  a separate prototype for type declarations, (or a merged prototype
  with separate calling conventions for type information), yet another
  prototype for method combinations, etc.
}@item{
  Instead of a long litany of features hardwired in an ad hoc way in
  a giant object system, a composable Meta-Object Protocol that enables
  all the features in a modular fashion.
}]

@section{Classes}

@section{Stateful Objects}

Note how all the functions defined in the previous chapter were pure:
They didn't use any side-effect whatsoever.
No @r[set!], no tricky use of @r[call/cc].

But what if we are OK with using side-effects? How much does that simplify things?

We can do away with one of the two self super arguments, and instead pass a single mutable value
as argument, where the identity provides the self, and the current state of the value provides the super.

Thus, we could for instance use @r[(deftype Proto (Fun MutableHashTable ->))],
where the hash-table contains the effective methods to compute each slot as thunks
or lazy computations already closed over the hash-table.
Overriding a slot would replace the effective method based on the new method, the self and super method.
Thus, individual methods would still be parameterized over self and super,
even though the object prototype only has the self as parameter.

Alternatively, each individual slot contains an object, and the methods are prototypes
for these "objects", with the same mutable signature. But that means that only mutable objects
are allowed as values in the language. Yikes.

@section[#:tag "Appendix_A"]{Appendix A: A Digression about type notation}

@section[#:tag "Appendix_B"]{Appendix B: Fixed-Point functions}

@section[#:tag "Appendix_C"]{Appendix C: Note for code minimalists}

In our introduction, we described the @r[fix] and @r[mix] functions
in only 109 characters of Scheme.
We can do even shorter with various extensions.
MIT Scheme and after it Racket, Gerbil Scheme, and more, allow you to write:
@Examples[
(define ((mix p q) f b) (p f (q f b)))
]
And then we'd have Object Orientation in 100 characters only.

Then again, in Gerbil Scheme, we could get it down to only 86, counting newline:
@racketblock[
(def (fix p b) (def f (p (lambda i (apply f i)) b)) f)
]

Or, compressing spaces, to 78,
not counting newline, since we elide spaces:
@racketblock[
(def(fix p b)(def f(p(lambda i(apply f i))b))f)(def((mix p q)f b)(p f(q f b)))
]

@(generate-bibliography)
@(finalize-examples/module poof)
