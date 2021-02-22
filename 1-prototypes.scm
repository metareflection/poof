;;;; Chapter 1. Function prototypes, in the most universal sense

(displayln "1. Defining function prototypes")

(displayln "1.1. Object Orientation in 109 characters of standard Scheme")
(define (fix p b) (define f (p (lambda i (apply f i)) b)) f)
(define (mix p q) (lambda (f b) (p f (q f b))))
;; We contend that the above two definitions summarize
;; the essence of object-oriented programming, and that
;; all the usual "object oriented" concepts can be easily recovered from them.
;;
;; The rest of this essay will make this case.

(displayln "1.1.1. The Essence of OOP in words")

;; OOP is about specifying code in modular increments, that we shall call
;; "prototypes". Prototypes are functions of two arguments, `self` and `super`,
;; or, above `f` (fixed-point, self) and `b` (base value, super).
;; Function `fix` *instantiates* a prototype `p` given a super/base value `b`.
;; Function `mix` composes two prototypes into one by inheritance, wherein they
;; operate on the same fixed-point `f` while chaining their effects on `b`.

(displayln "1.1.2. Application to arbitrary programming languages")

;; The above definitions are readily available to any language with closures
;; and either dynamic types or dependent types. However, their potential is not
;; fully realized in languages with mere parametric polymorphism.
;; We will also require lazy evaluation (or side effects to implement them)
;; as a language feature to *efficiently* implement objects with our formulas,
;; but do not otherwise require side-effects --- though they can be used
;; for the usual optimizations in the common "linear" case.

(displayln "1.2. Trivial objects")
; How do the two above functions relate to objects?

(displayln "1.2.1. Records as functions")
;; First, let's use the following trivial encoding of "records" as functions
;; from symbol (the name of a slot) to value (bound to the slot).
;;
;; Thus, the function `x1-y2` below encodes a record with two fields
;; `x` and `y` bound respectively to `1` and `2`.
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

;; Thus, this prototype extends or overrides its super-record with
;; a slot `x` unconditionally bound to `3`:
(define ($x3 self super)
  (lambda (msg) (if (eq? msg 'x) 3 (super msg))))

;; This prototype computes a complex number slot `z` based on real and
;; imaginary values bound to the respective slots `x` and `y` of its `self`:
(define ($z<-xy self super)
  (lambda (msg)
    (case msg
      ((z) (+ (self 'x) (* 0+1i (self 'y))))
      (else (super msg)))))

;; That prototype doubles the number in slot `x` from its `super` record.
;; We say that it *inherits* the value for slot `x`:
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

(define x3-y2 (fix $x3 x1-y2))
(check! (= (x3-y2 'x) 3))
(check! (= (x3-y2 'y) 2))

(define z1+2i (fix $z<-xy x1-y2))
(check! (= (z1+2i 'x) 1))
(check! (= (z1+2i 'y) 2))
(check! (= (z1+2i 'z) 1+2i))

(define x2-y2 (fix $double-x x1-y2))
(check! (= (x2-y2 'x) 2))
(check! (= (x2-y2 'y) 2))

;; We can also `mix` these prototypes together before to compute the `fix`:
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
(define x6-y2 (fix (mix $double-x $x3) x1-y2))
(define x3-y2 (fix (mix $x3 $double-x) x1-y2))
(check! (= 6 (x6-y2 'x)))
(check! (= 6 (x3-y2 'x)))

(displayln "1.2.3. Record prototype generators")
;; Now that we understand record prototypes, we can look at various utility
;; functions to build them.

;; To define an object with a field `k` mapped to a value `v`, use:
(define ($field k v) ;; k v: constant key and value for this defined field
  (lambda (self super) ;; self super: usual prototype variables
    (lambda (msg) ;; msg: message received by the object, a.k.a. method name.
      (if (equal? msg k) v ;; if the message matches the key, return the value
        (super msg))))) ;; otherwise, recurse to the object's super object

;; What of inheritance? Well, we can modify an inherited field using:
(define ($field-modify k modify) ;; k: constant key; modify: function from super-value to sub-value.
  (lambda (self super)
    (lambda (msg)
      (if (equal? msg k) (modify (super msg))
        (super msg)))))

;; What if a field depends on other fields? We can use this function
(define ($field-compute k fun) ;; k: constant key; fun: function from self to value.
  (lambda (self super)
    (lambda (msg)
      (if (equal? msg k) (fun self)
        (super msg)))))

;; A very general form of slot compute-and-override would take as parameters both
;; the `self` argument and a `next-method` function to inherit the slot value
;; from the `super` argument
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
(define (bottom-record msg) (error "unbound slot" msg))

;; To define a record with a single field foo bound to 0, we can use:
(define x3 (fix $x3 bottom-record))
(check! (= (x3 'x) 3))

;; To define a record with two fields `x` and `y` bound to `1` and `2`
;; respectively, we can use:
(define x1-y2 (fix (mix ($field 'x 1) ($field 'y 2)) bottom-record))
(check! (= (x1-y2 'x) 1))
(check! (= (x1-y2 'y) 2))

