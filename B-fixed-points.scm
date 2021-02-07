;;; Appendix B: Fixed-Point functions

;; In chapter 1, we quickly went over the fixed-point function fix
;; that we used to instantiate prototypes.
;;
;; Reminder:
;; A function prototype (Proto A B) is function p : A <- A B
;; where A and B and function types, and A is a subtype of B.
;; Thus, p takes a function f of type A, and a function B of type B,
;; and returns a new function f' of same type A.
;;
;; To instantiate a prototype is get a function of type A
;; from a function of type B.
;; Given these premises, there is one and only one construction that
;; allows us to and only one way to get an A: it's by calling p.
;; But to call p, we need to already have an A!
;; Where do we get this function of type A to begin with?
;; That's where the magic of fixed-point functions comes in:
;; they will somehow *tie a knot*, and get a reference to the
;; function being defined, even before the function is defined.

;; Here is how we want a fixed point to look like:
(define (well-typed-but-invalid p b)
  (define f (p f b))
  f)
;; Unhappily, this doesn't work in Scheme, because Scheme is eager:
;; the call to p needs to fully evaluate its arguments,
;; but f hasn't been fully defined yet, so the call is invalid.
;; Some Scheme implementations may detect that this definition tries to use
;; f before it is defined and raise an error at compile-time.
;; Some implementations will initially bind f to a magic "unbound" marker,
;; and trying to use f before it is defined will result in an error at runtime.
;; In yet other implementations, f will initially be bound to some default
;; value such as #!void that will be used without otherwise raising an error
;; until you try to call f while expecting it to be a function, and then
;; it raises a runtime error and you wonder why the program is trying to call
;; this useless default value. And finally, some reckless implementations will
;; try to use f before the data frame was even properly initialized at all,
;; and some random low-level value is used that might not make sense with
;; respect to the GC, and you'll eventually dance fandango on core.

;; Yet, in a lazy languages, the above definition works!
;; Indeed in Nix, you can write the equivalent definition:
;;   let fix = p: b: let f = p f b; in f
;;
;; In Scheme, we can similarly write:
(define (delayed-fix p b)
  (define df (delay (p f b)))
  f)
;; But in this only works if p accepts and returns delayed computations,
;; rather than direct function values. Then we have will have
;;   (DelayedProto A B) == (Delayed A) <- (Delayed A) (Delay B)
;; delayed-fix : (Delayed A) <- (DelayedProto A B) (Delayed B)
;; On the other hand, this works for arbitrary types A and B,
;; and not just for function types!

;; So, how do we get around this issue without delay?
;; One solution would be as follows -- can you tell why it works,
;; and why it isn't fully satisfactory?
(define (fix--0 p b)
  (define (f . i) (apply (p f b) i))
  f)
;; First, why it works: by making f a function, we can recursively refer to f
;; from within the body of function f itself, and by the time this reference
;; is used, f was called, and by the time f was called, f was defined.
;; Thus we can give f to p to compute the fixed-point value (p f b).
;; But by that time we're trying to call the fixed point, so
;; we take all the input arguments to f in a list i, and we pass them all to
;; the fixed-point expression using the builtin function apply. All is well.
;; Try it, it works.
;;
;; However, there is an issue:
;; fix--0 calls p again at every invocation of the fixed-point f.
;; Therefore, if p makes expensive computations, it will pay to recompute them
;; every time from scratch. Worse, if p wants to build data structures
;; meant to be shared between invocations, such as a cache,
;; this sharing will be lost between calls to f.
;; There can be no sharing of information between calls to f.
;; No pre-computation, no cacheing, no memoization, no shared mutable state.

;; Therefore a better solution, that does allow for sharing computations
;; and state between invocations of the fixed-point result, is:
(define (fix--1 p b)
  (define f (p (lambda i (apply f i)) b))
  f)
;; That's the same as the fix function from chapter 1.
;; Note how the anonymous lambda closure does part of the "protection"
;; or "delay" whereby the recursive data structure will only be called
;; after f is defined, but relies on p not causing its first argument
;; to be called during its evaluation, only stored in a data structure
;; or in a closure to be called later after p has returned.

;; If you don't like internal defines, you can write the same function
;; equivalently using letrec, as:
(define (fix--2 p b)
  (letrec ((f (p (lambda i (apply f i)) b)))
    f))

;; And if you don't even like letrec, you can use a Y-combinator variant:
(define (fix--3 p b)
  ((lambda (yf) (yf yf))
   (lambda (yf) (p (lambda i (apply (yf yf) i)) b))))
