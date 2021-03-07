;;;; Chapter 1. Prototypes, bottom up

(displayln "1. Prototypes, bottom up")

(displayln "1.1. Object Orientation in 109 characters of standard Scheme")
(define (fix p b) (define f (p (lambda i (apply f i)) b)) f)
(define (mix c p) (lambda (f b) (c f (p f b))))
;; We contend that the above two definitions summarize
;; the essence of object-oriented programming, and that
;; all the usual "object oriented" concepts can be easily recovered from them.
;;
;; The rest of this essay will make the case.

(displayln "1.1.1. The Essence of OOP in words")
'("
Object-Oriented Programming consists in specifying code in modular increments
that each compute their part from the combined whole and the computation so far.

Here we implement these increments as *prototypes*: functions of two arguments,
`self` and `super` (or, above, `f` and `b`, for fixed-point and base value).
Function `fix` *instantiates* a prototype `p` given a super/base value `b`.
Function `mix` has *child* prototype `c` *inherit* from *parent* prototype `p`
so they operate on a same fixed-point `f` while chaining their effects on `b`.

Given some arbitrary instance type Self, and a super-type Super of Self,
a prototype for Self from Super will thus be
a function from Self and Super to Self.

The first argument `self` of type Self will hold the instance
resulting as a fixed point from the entire computation.
When composing multiple prototypes, every prototype will receive
the *same* value as their self argument:
the complete instance that results from applying the every prototype in order.
This allows prototypes to "cooperate" with each other
on *different* aspects of the computation,
wherein one prototype defines some aspect (e.g. a "method" in some dictionary)
while relying on aspects to be defined by other prototypes (e.g. other methods),
accessed through the `self` argument in what is called "late binding".

The second argument `super` by contrast holds the partial result of the
fixed-point computation after applying only the "next" prototypes.
When composing multiple prototypes, each prototype will (presumably) receive
a different value. The last prototype in the list (rightmost, most ancestral
parent) will receive the "base" or "bottom" value from the fix function
(often literally the bottom value or function in the language), then the
"previous" prototype (its child, to the left) will receive ("inherit")
the result of that "next" computation (its parent, to the right), and so on
until the first prototype (leftmost, most recent child) inherits
its `super` value from the rest and computes the final instance.
This allows prototypes to cooperate with other prototypes on a *same* aspect
of the instance computation, wherein children prototypes can accumulate, modify
or override the method values inherited from the parent prototypes.
")

(displayln "1.1.2. Applicability to arbitrary Programming Languages")

;; The above definitions are readily available to any language with closures
;; and either dynamic types or dependent types. However, their potential is not
;; fully realized in languages with mere parametric polymorphism
;; (see chapter X on typing prototypes).
;; We will also require lazy evaluation (or side effects to implement them)
;; as a language feature to *efficiently* implement objects with our formulas,
;; but do not otherwise require side-effects --- though they can be used
;; for the usual optimizations in the common "linear" case.

(displayln "1.2. A minimal object system")
;; How do the two above functions relate to objects?

(displayln "1.2.1. Records as functions")
;; First, let's use the following trivial encoding of "records" as functions
;; from symbol (the name of a slot) to value (bound to the slot).

;; Thus, the function `x1-y2` below encodes a record with two fields
;; `x` and `y` bound respectively to `1` and `2`.
;; x1-y2 : (Fun 'x -> Nat | 'y -> Nat)
(define (x1-y2 msg)
  (case msg
    ((x) 1)
    ((y) 2)
    (else (error "unbound slot" msg))))

;; We can check that we can indeed access the record slots
;; and get the expected values:
(check! (= (x1-y2 'x) 1))
(check! (= (x1-y2 'y) 2))

;; Note that we use Lisp symbols for legibility, but in poorer languages,
;; "symbols" could be any large enough type with decidable equality,
;; e.g. integers or strings, etc.
;;
;; In accordance with Lisp tradition, we will say "slot" instead of "field"
;; or "member" or "method", and say that the slot is bound to the given value
;; rather than it "containing" the value or any such thing.

(displayln "1.2.2. Prototypes for Records")
;; A *prototype* for a record is a function of two arguments `self` and `super`
;; (both records) to an `extended-self` record.
;; (deftype (Proto Self Super) (Fun Self Super -> Self st: (<: Self Super)))
;; fix : (Fun (Proto Self Super) Super -> Self st: (<: Self Super))
;; mix : (Fun (Proto Self Super) (Proto Super Super2) -> (Proto Self Super2) st: (<: Self Super Super2))

;; Thus, this prototype extends or overrides its super-record with
;; a slot `x` unconditionally bound to `3`:
;; $x3 : (Proto (Fun 'x -> Nat | A) A)
(define ($x3 self super)
  (lambda (msg) (if (eq? msg 'x) 3 (super msg))))

;; This prototype computes a complex number slot `z` based on real and
;; imaginary values bound to the respective slots `x` and `y` of its `self`:
;; $z<-xy : (Proto (Fun (Or 'x 'y) -> Real | 'z -> Complex | A) (Fun (Or 'x 'y) -> Complex | A))
(define ($z<-xy self super)
  (lambda (msg)
    (case msg
      ((z) (+ (self 'x) (* 0+1i (self 'y))))
      (else (super msg)))))

;; That prototype doubles the number in slot `x` from its `super` record.
;; We say that it *inherits* the value for slot `x`:
;; $double-x : (Proto (Fun 'x -> Number | A) (Fun 'x -> Number | A))
(define ($double-x self super)
  (lambda (msg)
    (if (eq? msg 'x) (* 2 (super 'x)) (super msg))))

;; More generally a record prototype extends its `super` record with new slots
;; and/or overrides the values bound to its existing slots, and may in the
;; process refer to both the records `self` and `super` and their slots, with
;; some obvious restrictions to avoid infinite loops from circular definitions.

;; Note that we use the name prefix $ for a prototype.

;; But how do we test the above prototypes?

;; We can use the above record x1-y2 as a base value and use the fix operator:

;; x3-y2 : (Fun 'x -> Nat | 'y -> Nat)
(define x3-y2 (fix $x3 x1-y2))
(check! (= (x3-y2 'x) 3))
(check! (= (x3-y2 'y) 2))

;; z1+2i : (Fun 'x -> Nat | 'y -> Nat | 'z -> Complex)
(define z1+2i (fix $z<-xy x1-y2))
(check! (= (z1+2i 'x) 1))
(check! (= (z1+2i 'y) 2))
(check! (= (z1+2i 'z) 1+2i))

;; x2-y2 : (Fun 'x -> Nat | 'y -> Nat)
(define x2-y2 (fix $double-x x1-y2))
(check! (= (x2-y2 'x) 2))
(check! (= (x2-y2 'y) 2))

;; We can also `mix` these prototypes together before to compute the `fix`:
;; z6+2i : (Fun 'x -> Nat | 'y -> Nat | 'z -> Complex)
(define z6+2i (fix (mix $z<-xy (mix $double-x $x3)) x1-y2))
(check! (= (z6+2i 'x) 6))
(check! (= (z6+2i 'y) 2))
(check! (= (z6+2i 'z) 6+2i))

;; And since the `$z<-xy` prototype got the x and y values from the `self`
;; and not the `super`, we can freely commute it with the other two prototypes
;; that do not affect either override slot 'z or inherit from it:
(check! (= 6+2i ((fix (mix $z<-xy (mix $double-x $x3)) x1-y2) 'z)))
(check! (= 6+2i ((fix (mix $double-x (mix $z<-xy $x3)) x1-y2) 'z)))
(check! (= 6+2i ((fix (mix $double-x (mix $x3 $z<-xy)) x1-y2) 'z)))

;; `mix` is associative, and therefore we also have
(check! (= 6+2i ((fix (mix (mix $z<-xy $double-x) $x3) x1-y2) 'z)))
(check! (= 6+2i ((fix (mix (mix $double-x $z<-xy) $x3) x1-y2) 'z)))
(check! (= 6+2i ((fix (mix (mix $double-x $x3) $z<-xy) x1-y2) 'z)))
;; But the result of mix is slightly more efficient in the former form
;; (fold right) than the present form (fold left).

;; However, since `$double-x` inherits slot `x` that `$x3` overrides, there is
;; clearly a dependency between the two that prevents them from commuting:
;; x6-y2 : (Fun 'x -> Nat | 'y -> Nat)
(define x6-y2 (fix (mix $double-x $x3) x1-y2))
;; x3-y2 : (Fun 'x -> Nat | 'y -> Nat)
(define x3-y2 (fix (mix $x3 $double-x) x1-y2))
(check! (= 6 (x6-y2 'x)))
(check! (= 3 (x3-y2 'x)))

(displayln "1.2.3. Record prototype generators")
;; Now that we understand record prototypes, we can look at various utility
;; functions to build them.

;; To define an object with a field `k` mapped to a value `v`, use:
;; $field : (Fun k:Symbol V -> (Proto (Fun 'k -> V | A) A))
(define ($field k v) ;; k v: constant key and value for this defined field
  (lambda (self super) ;; self super: usual prototype variables
    (lambda (msg) ;; msg: message received by the object, a.k.a. method name.
      (if (equal? msg k) v ;; if the message matches the key, return the value
        (super msg))))) ;; otherwise, recurse to the object's super object

;; What of inheritance? Well, we can modify an inherited field using:
;; $field-modify : (Fun k:Symbol (Fun V -> W) -> (Proto (Fun 'k -> W | A) (Fun 'k -> V | A) A) st: (<: V W))
(define ($field-modify k modify) ;; k: constant key; modify: function from super-value to sub-value.
  (lambda (self super)
    (lambda (msg)
      (if (equal? msg k) (modify (super msg))
        (super msg)))))

;; What if a field depends on other fields? We can use this function
;; $field-compute : (Fun k:Symbol (Fun A -> V) -> (Proto (Fun 'k -> V | A) A))
(define ($field-compute k fun) ;; k: constant key; fun: function from self to value.
  (lambda (self super)
    (lambda (msg)
      (if (equal? msg k) (fun self)
        (super msg)))))

;; A very general form of slot compute-and-override would take as parameters both
;; the `self` argument and a `next-method` function to inherit the slot value
;; from the `super` argument
;; $field-gen : (Fun k:Symbol (Fun A (Fun -> V) -> W) -> (Proto (Fun 'k -> W | A) (Fun 'k -> V | A) A) st: (<: V W))
(define ($field-gen k fun) ;; k: constant key; fun: function from self to value.
  (lambda (self super)
    (lambda (msg)
      (if (equal? msg k) (fun self (lambda () (super msg)))
          (super msg)))))

;; We could redefine the former ones in terms of that latter:
(define ($field k v)
  ($field-gen k (lambda (_self _next-method) v)))
(define ($field-modify k modify)
  ($field-gen k (lambda (_self next-method) (modify (next-method)))))
(define ($field-compute k fun)
  ($field-gen k (lambda (self _next-method) (fun self))))

;; Thus you can re-define the above prototypes as:
(define $x3 ($field 'x 3))
(define $double-x ($field-modify 'x (lambda (x) (* 2 x))))
(define $z<-xy ($field-compute 'z (lambda (self) (+ (self 'x) (* 0+1i (self 'y))))))

;; Here is a universal bottom function to use as the base for fix:
;; bottom-record : (Fun Symbol -> _)
(define (bottom-record msg) (error "unbound slot" msg))

;; To define a record with a single field foo bound to 0, we can use:
;; x3 : (Fun 'x -> Nat)
(define x3 (fix $x3 bottom-record))
(check! (= (x3 'x) 3))

;; To define a record with two fields `x` and `y` bound to `1` and `2`
;; respectively, we can use:
;; x1-y2 : (Fun 'x -> Nat | 'y -> Nat)
(define x1-y2 (fix (mix ($field 'x 1) ($field 'y 2)) bottom-record))
(check! (= (x1-y2 'x) 1))
(check! (= (x1-y2 'y) 2))

(displayln "1.3. Prototype Basics")

(displayln "1.3.1. Composing prototypes")

;; The identity prototype, neutral element for mix, is as follows:
;; $id : (Proto X X)
(define ($id f b) b)
;; It doesn't override any information from the super/base object,
;; but only passes it through. It also doesn't consult information in
;; the final fixed-point nor refers to it.

;; But maybe this prototype business is easier to understood when written in
;; "long form", with long identifiers, and traditional arguments "self" and
;; "super". Thus, $id becomes:
;; identity-prototype : (Proto Instance Instance)
(define (identity-prototype self super) super)

;; Thus, (fix p b) becomes:
;; instantiate-prototype : (Fun (Proto Self Super) Super -> Self)
(define (instantiate-prototype prototype base-super)
  (define self (prototype (lambda i (apply self i)) base-super))
  self)
"A more thorough explanation of this fixed-point function is in Appendix A."

"And (mix p q) becomes:"
;; compose-prototypes : (Fun (Proto Self Super) (Proto Super Super2) -> (Proto Self Super2) st: (<: Self Super Super2))
(define (compose-prototypes child parent)
  (lambda (self super) (child self (parent self super))))
;; Note the types of the variables and intermediate expressions:
;; child : (Proto Self Super)
;; parent : (Proto Super Super2)
;; self : Self
;; super : Super2
;; (parent self super) : Super
;; (this self (parent self super)) : Self

"When writing long-form functions instead of vying for conciseness, we will
use the same naming conventions as in the function above:
- `child` (or `this`) for a *prototype* at hand, in leftmost position;
- `parent` for a *prototype* it is being mixed with, in latter position;
- `self` for the *instance* that is a fixed point of the computation;
- `super` for the base (or so-far accumulated) *instance* of the computation.

Note the important distinction between *prototypes* and *instances*.
Instances are elements of some function type, and are themselves
the results of the computation whereby prototypes are instantiated.
Prototypes are increments of computation, functions from instance (of a
subtype `Self`) and instance (of a supertype `Super`) to instance of the
subtype `Self`."

"Now, prototypes are interesting because `mix` (a.k.a. `compose-prototypes`)
is an associative operator with neutral element `$id` (a.k.a.
`identity-prototype`). Thus prototypes form a monoid, and you can compose
or instantiate a list of prototypes:"
;; compose-protototype-list : (Fun (IndexedList I (lambda (i) (Proto (A_ i) (A_ (1+ i))))) -> (Proto (A_ 0) (A_ (Card I))))
(define (compose-prototype-list l)
  (cond
   ((null? l) identity-proto)
   ((null? (cdr l)) (car l))
   ((null? (cddr l)) (compose-prototypes (car l) (cadr l)))
   (else (compose-prototypes (car l) (compose-prototype-list (cdr l))))))

"A more succint way to write the same function is:"
;; compose-protototype-list : (Fun (IndexedList I (lambda (i) (Proto (A_ i) (A_ (1+ i))))) (A_ (Card I)) -> (Proto (A_ 0) (A_ (1+ i))))
(define (compose-prototype-list prototype-list)
  (foldr compose-prototypes identity-prototype prototype-list))

;; instantiate-protototype-list : (Fun (IndexedList I (lambda (i) (Proto (A_ i) (A_ (1+ i))))) (A_ (Card I)) -> (A_ 0))
(define (instantiate-prototype-list prototype-list base-super)
  (instantiate-prototype (compose-prototype-list prototype-list) base-super))

'("Prototype composition is notably more expressive than "single inheritance"
as commonly used in many "simple" object systems:
Object systems with "single inheritance" require programmers to `cons`
objects (or classes) one component at a time in the front of a rigid list of
"parent" objects (or classes), where the base object (or class) is set.
Prototype object systems enable programmers to `append` list of prototypes
independently from any base object, to compose and recompose prototypes
in different orders and combinations.
Prototypes are thus more akin to the "mixins" or "traits" of more advanced
objects systems.

Prototype composition however, does not by itself subsume multiple
inheritance. We will show in chapter 4 how to combine the two.")

(displayln "1.3.2. The Bottom of it")

"In a language with partial functions, such as Scheme, there is a practical
choice for a universal function to use as the `base-super` argument to
`instantiate-prototype`: the `bottom` function, that never returns,
but instead, for enhanced usability, throws an error that can be caught."
;; bottom : (Fun I ... -> O ...)
(define (bottom . args)
  (error "bottom" args))

"Thus, in dialects with optional arguments, we could make `bottom` the default
value for `base-super`. Furthermore, in any variant of Scheme, we can define
the following function `instantiate` that takes the rest of its arguments as
a list of prototypes, and instantiates the composition of them:"
;; instance : (Fun (IndexedList I (lambda (i) (Proto (A_ i) (A_ (1+ i)))))... -> (A_ 0)))
(define (instance . prototype-list)
  (instantiate-prototype-list prototype-list bottom))

"What if you *really* wanted to instantiate your list of prototypes with some
value `b` as the base super instance? You can "just" tuck
`(base-prototype b)` at the tail end of your protototype list:"
;; constant-prototype : (Fun A -> (Proto A _))
(define (constant-prototype base-super)
  (lambda (_self _super) base-super))

"Or the same with a shorter name and a familiar definition as a combinator"
;; $const : (Fun A -> (Proto A _))
(define ($const b) (lambda _ b))

"Small puzzle for the points-free Haskellers reading this essay:
what change of representation will allow you to compose prototypes
with regular function composition instead of applying binary function mix?"

"ROT13'ed answer:
gur pbzcbfnoyr cebgbglcr sbe cebgbglcr c vf (p c) = (ynzoqn (d) (zvk c d)),
naq gb erpbire gur hfhny cebgbglcr sebz vg, lbh whfg unir gb nccyl vg gb $vq."

(displayln "1.3.3. Note for code minimalists")

"We described the fix and mix functions in only 109 characters of Scheme.
We can do even shorter with various extensions.
MIT Scheme and after it Racket, Gerbil Scheme, and more, allow you to write:"
(define ((mix p q) f b) (p f (q f b)))
"And then we'd have Object Orientation in 100 characters only."

"Then again, in Gerbil Scheme, we could get it down to only 86 (counting newline):"
(def (fix p b) (def f (p (lambda i (apply f i)) b)) f)

"Of, compressing spaces, to 78 (not counting newline, since we don't count spaces):"
(def(fix p b)(def f(p(lambda i(apply f i))b))f)(def((mix p q)f b)(p f(q f b)))
