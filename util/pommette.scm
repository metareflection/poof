;;;;;; pommette - a cheeky, barebones implementation of a meta-object protocol.
;;;;;; LtUO demonstration mini object systems written in Scheme
#|
With Gerbil Scheme: gxi pommette.scm
With Chez Scheme: chezscheme pommette-chez.scm
With Racket: racket pommette.rkt
|#
#|
   Semicolons are comments to the end of line.
   Blocks between #| ... |# are multi-line comments.
|#

;;; Flag for verbose execution:
(define verbose #t)

;;;;; Chapter 5: Minimal Object System
;;;; Prelude: general purpose utilities

;;; Support for Functional Programming with lots of curried (chained unary) functions

;; Macro for uncurried calls to curried functions
#| This definition illustrates use of simple Scheme hygienic macros
   To learn more about Scheme syntax and semantics in general,
   please consult the R7RS-small, some Scheme tutorial,
   your implementation’s reference manual,
   or the docs.racket-lang.org website.
   Beware that various implementations have their own extensions and limitations.
|#
;; Uncurried calls to curried functions
(define-syntax @
  (syntax-rules ()
    ((_) identity)
    ((_ f) f)
    ((_ f a . rest) (@ (f a) . rest))))

(define-syntax define-identifier-macro
  (syntax-rules ()
    ((_ name symbol-expr list-expr)
     (cond-expand
       (gerbil
        (define-syntax name
          (syntax-rules ()
            ((_ . x) (list-expr . x))
            (_ symbol-expr))))
       ((or racket chezscheme)
        (define-syntax (name stx)
          (syntax-case stx ()
            ((_ . x) #'(list-expr . x))
            (_ #'symbol-expr))))))
    ((_ name expr)
     (define-identifier-macro name expr expr))))

(define-syntax λ
  (syntax-rules ()
    ((_ () . body) (begin . body))
    ((_ (x) . body) (lambda (v) (def x v) . body))
    ((_ (x . y) . body) (λ (x) (λ y . body)))
    ((_ v . body) (lambda v . body))))

(define-syntax def
  (syntax-rules ()
    ((_ (pat . vars) . body)
     (def pat (λ vars . body)))
    ((_ v . body)
     (begin
       ;; Allow autocurrying self-reference in the definition body
       ;; (name) with 0 args calls tmp; (name a ...) curries via @
       (define-syntax @tmp (syntax-rules () ((_) (tmp)) ((_ . a) (@ tmp . a))))
       (define tmp (let () . body))
       (define-identifier-macro v tmp @tmp)))))

;;; Expectations -- trivial test suite
(define (check-expectation check-expr good-expr checked-thunk good-thunk)
  (let ((actual (checked-thunk))
        (expected (good-thunk)))
    (if (equal? actual expected)
        (and verbose
             (begin (display "Checked that ") (write check-expr)
                    (display " and ") (write good-expr)
                    (display " both evaluated to ") (write actual) (newline)))
        (error "expectation failure" " expected " check-expr " to evaluate the same as " good-expr
               " but got " actual " instead of " expected))))
(define (check-failure expr thunk msg)
  (let ((failed?
         (cond-expand
           (gerbil
            (with-catch true (lambda () (thunk) #f)))
           (chezscheme
            (guard (e (#t #t)) (thunk) #f))
           (guile
            (catch #t (lambda () (thunk) #f) (lambda args #t)))
           (gambit
            (with-exception-catcher (lambda (_) #t) (lambda () (thunk) #f)))
           (else
            'unsupported))))
    (case failed?
      ((#t) (and verbose
                 (begin (display "Checked that ")
                        (write expr)
                        (display " fails as expected")
                        (newline))))
      ((#f) (error msg expr "did not fail"))
      ((unsupported)
       (and verbose
            (begin (display "Unsupported Scheme implementation, skipping failure check for ")
                   (write expr)
                   (newline)))))))

(define-syntax expect
  (syntax-rules (=> =>fail!)
    ((expect) #t)
    ((expect expr => result . r)
     (begin
       (check-expectation 'expr 'result (lambda () expr) (lambda () result))
       (expect . r)))
    ((expect expr =>fail! . r)
     (begin
       (check-failure 'expr (lambda () expr) "Expected failure, but ")
       (expect . r)))))

;; Test our expectation and failure infrastructure
(expect (+ 2 3) => 5
        (+ 20 3) => (+ 3 20)
        (* 6 7) => 42
        (/ 1 0) =>fail!
        (car '()) =>fail!)

;; Let's not forget to minimally check the previously defined λ
(expect ((λ (x) (+ x 3)) 2) => 5)

;;; Aborting
#| Note the use of variable-length arguments:
   The unparenthesized args variable catches all arguments;
   apply passes them at the end of the arguments to the function error. |#
(define abort (λ args (apply error "Aborting" args)))

(expect (abort "intentional") =>fail!)

(expect
 ;; explicitly wrapped uncurried call to explicitly curried function:
 (@ (λ (x) (λ (y) (λ (z) (λ (t) (+ x y z t))))) 1 2 3 4) => 10 ;;
 ;; explicitly wrapped uncurried call to implicitly curried function:
 (@ (λ (x y z t) (+ x y z t)) 1 2 3 4) => 10
 ;; failed uncurried call to curried function:
 ((λ (x y z t) (+ x y z t)) 1 2 3 4) =>fail!)

;;; 5.2.2 Records (moved ahead, because we use it in 5.1.2 already)
(def (empty-record _)
  #f)
(def (extend-record key value rec i)
  (if (equal? i key) value (rec i)))
(def (record-ref key rec)
  (rec key))
(define-syntax record
  (syntax-rules ()
    ((record) empty-record)
    ((record (k v) . r) (((extend-record 'k) v) (record . r)))))

(expect (empty-record 'foo) => #f
        ((((extend-record 'foo) 1) empty-record) 'foo) => 1 ;; explicitly curried call
        (@ extend-record 'foo 1 empty-record 'bar) => #f ;; explicitly wrapped curried call
        (extend-record 'foo 1 empty-record 'bar) => #f) ;; implicitly wrapped curried call

;;; 5.1.2 Coloring a point
(def point-a (record (x 2) (y 4))) ;; Using the syntax above
(def (paint-blue p) (extend-record 'color "blue" p)) ;; Using regular functions
(def p1 (paint-blue point-a))
(def p2 (record (x 2) (y 4) (color "blue")))

;; Not that 'x is a constant expression returning the symbol x,
;; as opposed to plain x which is an expression that dereferences variable x.
(expect (point-a 'x) => 2
        (point-a 'y) => 4
        (point-a 'z) => #f
        (point-a 'color) => #f
        ((record-ref 'x) point-a) => 2
        (@ record-ref 'y point-a) => 4)

;; To speed up those tests, we use map function point-p over various values
(expect (map point-a '(x y z color)) => '(2 4 #f #f)
        (map p1 '(x y z color)) => '(2 4 #f "blue")
        (map p2 '(x y z color)) => '(2 4 #f "blue"))

;;; 5.1.4
;; Simple function composition
(def (compose ext1 ext2 val)
  (ext1 (ext2 val)))
(def (identity val) val)

;; Example of the short form syntax for defining functions without writing a lambda
(def (mul10 x) (* x 10))
(def (add1 x) (+ x 1))
(def (sub2 x) (- x 2))

;; Check that our composition works as expected:
(expect (((compose mul10) add1) 4) => 50
        (@ compose add1 mul10 4) => 41
        (compose mul10 mul10 3) => 300)

;; Generalizing compose to n-ary composition.
(define compose*
  (case-lambda
    (() identity)
    ((x) x)
    ((x y) (compose x y))
    ((x . r) (compose x (apply compose* r))))) ;; or (foldl compose* x r)

(define (uncurry2 f) (lambda (x y) ((f x) y)))

(expect
  ((compose*) 5) => 5
  ((compose* add1) 99) => 100
  ((compose* add1 add1) 67) => 69
  ((compose* add1 add1 add1) 20) => 23
  ((compose* add1 add1 mul10) 4) => 42
  ((compose* add1 mul10 sub2) 0) => -19
  ((uncurry2 (λ (x y) (+ x y))) 4 5) => 9
  (((λ (x y) (+ x y)) 4) 5) => 9
  ((λ (x y) (+ x y)) 4 5) =>fail!) ;; wrong number of arguments to curried function

;;; 5.1.5
(define top #f)

(define point-c
  (record (x 3) (y 4) (color "blue")))

(expect (map point-c '(x y z color)) => '(3 4 #f "blue"))

#;(define ls-sorted (λ (ctx) (compose* (ctx 'sort) (ctx 'ls))))

;;; Y combinator
;; https://www.hjorthjort.xyz/2018/11/08/2018-11-08-really_getting_the_y-combinator.html

;; eta-conversion of a function
;; Often used to protect it from over-eager evaluation, in applicative context.
;; A macro, not a function, precisely to protect against overly eager evaluation of f.
(define-syntax η (syntax-rules () ((_ f) (λ (x) (f x)))))

;; As a warm up, S K I combinators (that can also be useful later)
(def (S x y z) (x z (y z)))
(def (K x _y) x)
(def (I x) x)

;; B combinator, composition
;; a.k.a. Z for Schönfinkel (Zusammensetzungsfunktion) (compoZition function)
;; : (Y→X)→(Z→Y)→Z→X
(def (B x y z)
  (x (y z)))

;; Ue: U, eager -- self-application combinator
;; a.k.a. duplication combinator Δ \Delta, or ω, half of Ω = (ω ω)
;; "Half of Y"
;; https://en.wikipedia.org/wiki/SKI_combinator_calculus
;; https://www.tfeb.org/fragments/2020/03/09/the-u-combinator/
;; same as (def (Ue x) (eta (x x)))
;; : µX.(X→A)→A
(def (Ue x y)
  (x x y))

;; Y, eager -- fixpoint combinator
;; a.k.a. Z https://en.wikipedia.org/wiki/Fixed-point_combinator#Z_combinator
(def (Ye f)
  (Ue (B f Ue)))

;; Y, eager, expanded. Same as Ye without intermediate definitions,
;; so you can just copy/paste a one-liner
(def Yex (λ (f) ((λ (x y) (x x y)) (λ (x) (f (λ (y) (x x y)))))))

;; Turing 1937's Θ formula (Theta) -- only work in lazy context,
;; and I don’t feel lize rewriting it with force and delay. Exercise: do it.
;; (def Θ ((λ (v u) (u (v v u))) (λ (v u) (u (v v u)))))
#| nix repl
let Y = f: (x: x x) (x: f (x x));
    Theta = (v: u: (u (v v u))) (v: u: (u (v v u)));
    pre_fact = f: n: if n <= 1 then n else n * f (n - 1); in
    [(Y pre_fact 6) (Theta pre_fact 6)]
|#

;; Y, eager, stateful -- the statefulness is hidden in letrec.
;; Reminder: (η p) = (λ (x) (p x))
(def (Yes f) (letrec ((p (f (η p)))) p))

(def Y Yes)

;; lazy convention: arguments are delayed, results are forced.
;; Note that we are optimizing away some unnecessary delays and forces
;; to optimize this lazy representation in Scheme.
;; Another approach would be to be more systematic in delaying all arguments,
;; which would introduce lots of unnecessary "administrative" forcings for no gain;
;; and then introduce automated compiler optimizations. That's a project for another time.
;; I will annotate these functions with their type, where ^X is the type for delayed X.

;; Y, lazy, written with a letrec
;; : (^X→X)→X
(def (Yl f)
  (letrec ((p (f (delay p)))) p))

;; B, lazy -- composition, but the first argument’s argument is delayed.
;; : (^Y→X)→^(Z→Y)→Z→X
(def (Bl f g x)
  (f (delay ((force g) x))))

;; U, lazy -- self-application / duplication, for delayed functions
;; : µX.^(X→A)→A
(def (Ul x)
  ((force x) x))

;; Y, lazy, written with combinators
;; essentially, Y f = U (B f U); a form also known as X,
;; versus stricto sensu Y f = (B f U) (B f U) that (X f) β-expands into.
(def (Ylc f) ;; : ^(^X→X)→X
  (Ul (delay (Bl f (delay Ul)))))

;; Y, lazy, expanded from Yl without intermediate definitions, for a one-linear
;; : ^(^X→X)→X
(def (Ylx f) ((λ (x) ((force x) x)) (delay (λ (x) (f (delay ((force x) x)))))))

;; Compute Factorial 6 with Y
(def (eager-pre-fact f n) ;; precursor for Ye encoding
  (if (<= n 1) n (* n (f (- n 1)))))
(def lazy-pre-fact (λ (f n) ;; precursor for Yl encoding
  (if (<= n 1) n (* n ((force f) (- n 1))))))
(def (half-pre-fact f n) ;; precursor for Ue encoding
  (if (<= n 1) n (* n (Ue f (- n 1)))))

(expect ((Ye eager-pre-fact) 6) => 720
        ((Ye eager-pre-fact) 6) => 720
        ((Yes eager-pre-fact) 6) => 720
        ((Yl lazy-pre-fact) 6) => 720
        ((Ylc lazy-pre-fact) 6) => 720
        ((Ylx lazy-pre-fact) 6) => 720
        ((Ue half-pre-fact) 6) => 720)

;;; Poor man's implementation of lazy as a function that always returns the results
;;; of the first successful evaluation.
;;; Tries to survive escaping continuations by consistently returning the first successful result.
;;; Not remotely thread safe though.
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
(define-syntax once
  (syntax-rules ()
    ((_ body ...) (compute-once (lambda () body ...)))))

(define (! x) (x)) ;; force, even if from def or λ

;; Y, once; B, once; U, once; Y, once with combinators; Y, once, expanded
;; direct adaptations of Yl Bl Ul Ylc Ylx to once and ! instead of delay and force.
(def (Yo f) ;; : ^(^X→X)→X
  (letrec ((p (f (once p)))) p))
(def (Bo x y z) ;; : (^Y→X)→^(Z→Y)→Z→X
  (x (once ((! y) z))))
(def (Uo x) ;; : µX.^(X→A)→A
  ((! x) x))
(def (Yoc f) ;; : ^(^X→X)→X
  (Uo (once (Bo f (once Uo)))))
;; : ^(^X→X)→X
(def (Yox f) ((lambda (x) ((x) x)) (once (λ (x) (f (once ((! x) x)))))))

(def once-pre-fact (λ (f n)
  (if (<= n 1) n (* n ((! f) (- n 1))))))

(expect
 ((Yo once-pre-fact) 6) => 720
 ((Yoc once-pre-fact) 6) => 720
 ((Yox once-pre-fact) 6) => 720)

;; Trivial implementation of lazy from once
;;(define-syntax delay (syntax-rules () ((_ . body) (once (λ () . body)))))
;;(define force (λ (p) (p)))
;;(define once-thunk (λ (thunk) (lazy (thunk)))) ;; only for nullary thunks
(define foo
  (let ((twice #f))
    (once (if twice (error "called twice") (begin (set! twice #t) 42)))))
(expect (foo 41) => 42
        (foo 4) => 42
        (foo) => 42
        (foo 1 2 3) => 42)


;;; 5.3.2 Composing Modular Extensions
(def (mix p c t s) ;; parent child super self
  (c (p t s) s)) ;; chain p into c for their super argument, use the same self for both.

(def (idModExt t _s) ;; super self, ignore self return super
  t) ;; neutral element for mix

;;; 5.3.3 Closing Modular Extensions
(def (fix t m) ;; top (supermost) element, and modular extension
  (Y (m t))) ;; Apply m to t, take the fixpoint for the self argument

;; Generalizing compose*
;; Take a monoid two-argument operation op2, return the n-ary variant.
(define op*←op2 (lambda (op2 id)
  ;; Simpler, though less efficient: (λ args (foldl op2 id args))
  (letrec ((op* (case-lambda
                 (() id)
                 ((x) x)
                 ((x y) (op2 x y))
                 ((x . r) (op2 x (apply op* r)))))) ;; or (foldl op2 x r)
    op*)))
;; Variant of op*←op2, but for a curried operator that takes one argument then the next.
(define op*←op1.1 (lambda (op1.1 id)
  (op*←op2 (lambda (x y) (@ op1.1 x y)) id)))

(define mix* (op*←op1.1 mix idModExt))

;; Specification that calls a unary operation on the super value
(def (op-super-spec op super _self)
  (op super))

(expect
  (fix 4 (mix*)) => 4
  (fix 4 (mix* (op-super-spec add1) (op-super-spec add1))) => 6
  (fix 4 (apply mix* (map op-super-spec (list add1 add1 mul10)))) => 60
  (fix 4 (apply mix* (map op-super-spec (list add1 add1 mul10 add1)))) => 61)

;;; 5.3.4 Default and non-default Top Type
(def fixt (fix top))

(def (fixt/inlined m)
  (Y (m top)))

(def (record-spec _super _self)
  empty-record)

;; With single inheritance, you inherit from record-spec.
;; With mixin inheritance and with some dynamic typing, you may want your mixin
;; to inherit (even repeatedly) from record!-spec,
;; so it doesn't matter who does or doesn't initialize the record.
;; use dynamic typing to make something a record if not previously?
;; OR use static typing to get an appropriate top?
;; Or have a universal null? test for the default top object?
;; With multiple inheritance, you can instead depend on record-spec;
;; with optimal inheritance, it can further be a suffix specification.
(def (record!-spec super _self)
  (or super empty-record))

(def fix-record (fix empty-record))
(def (fix-record/inlined m)
  (Y (m empty-record)))
(def (fix-record/fixt m)
  (fixt (mix record-spec m)))

;;; 5.3.5 Minimal OO Indeed
(def (field-spec key compute-value super self method-id)
  (let ((inherited (super method-id)))
    (if (equal? key method-id)
        (compute-value inherited self)
        inherited)))

;;; field-spec~ : like field-spec, but treats #f super as empty-record.
;;; Useful when the top is #f and we are not sure whether a sub-record was initialized yet.
(def (field-spec~ key compute-value super self method-id)
  (field-spec key compute-value (or super empty-record) self method-id))


;;; 5.3.6 Minimal Colored Point
(def coord-spec
  (mix* (field-spec 'x (λ (_inherited _self) 2))
        (field-spec 'y (λ (_inherited _self) 4))))

(def color-spec
  (field-spec 'color (λ (_inherited _self) "blue")))

(def point-p (fix-record (mix* coord-spec color-spec)))

(expect (point-p 'x) => 2
        (point-p 'color) => "blue"
        (map point-p '(x y z color)) => '(2 4 #f "blue")
        (map (fix-record/inlined (mix coord-spec color-spec)) '(x y z color)) => '(2 4 #f "blue")
        (map (fix-record/fixt (mix coord-spec color-spec)) '(x y z color)) => '(2 4 #f "blue"))

(def (constant-spec value _super _self)
  value)
(def (constant-field-spec key value)
  (field-spec key (constant-spec value)))

;;; 5.3.7 Minimal Extensibility and Modularity Examples
(def (add-x-spec dx)
  (field-spec 'x (λ (inherited _self) (+ dx inherited))))

(def (sqr x)
  (* x x))

(def area-spec
  (field-spec 'area (λ (_inherited self)
    (* (self 'x) (self 'y)))))

(def rho-spec
  (field-spec 'rho (λ (_inherited self)
    (sqrt (+ (sqr (self 'x)) (sqr (self 'y)))))))

(def point-r
  (fix-record (mix* rho-spec coord-spec (add-x-spec 1))))

(expect (point-r 'x) => 3
        (point-r 'rho) => 5
        (map point-r '(x y rho color)) => '(3 4 5 #f))

;;; 5.3.8 Interaction of Modularity and Extensibility

(def (my-modular-def self method-id)
  (case method-id
    ((start) 5)
    ((length) (λ (l) (if (null? l) 0 (+ 1 (self 'length (cdr l))))))
    ((size) (- (self 'length (self 'contents)) (self 'start)))
    (else #f)))

(def my-saying
  '(Designing a computer programming system that "doesn’t" address transactional
    persistence means that "you’re" proud of having no data worth keeping.))

(def my-contents-spec
  (constant-field-spec 'contents my-saying))

(def my-contents
  (Yes (λ (self) (my-contents-spec (my-modular-def self) self))))

(expect (my-contents 'contents) => my-saying
        (my-contents 'length my-saying) => 20
        (my-contents 'size) => 15)

(def my-modular-def-without-global-recursion
  (let ((start% 5))
    (letrec ((length% (λ (l) (if (null? l) 0 (+ 1 (length% (cdr l)))))))
      (λ (self method-id)
        (case method-id
          ((start) start%)
          ((length) length%)
          ((size) (- (length% (self 'contents)) start%))
          (else #f))))))

(def my-contents-2
  (Yes (λ (self) (my-contents-spec (my-modular-def-without-global-recursion self) self))))

(expect (my-contents-2 'contents) => my-saying
        (my-contents-2 'length my-saying) => 20
        (my-contents-2 'size) => 15)

(def (base-bill-of-parts super self method-id)
  (case method-id
    ((parts) '())
    ((part-count) (length (self 'parts)))
    (else (super method-id))))

(def (part-spec part)
  (field-spec 'parts (λ (inherited _self) (cons part inherited))))

(def torso-spec (part-spec 'torso))
(def head-spec (part-spec 'head))
(def arms-spec (part-spec 'arms))
(def legs-spec (part-spec 'legs))

(def body-rec (fix-record (mix* base-bill-of-parts torso-spec legs-spec arms-spec head-spec)))

(expect (map body-rec '(parts part-count)) => '((head arms legs torso) 4))

;;;;; 5.x Finalization

(def (finalize-spec super self)
  (let ((finalizer ((field-view~* #f '__finalizer) super)))
    (if finalizer (finalizer super self) super)))

(def (fix-record* m)
  (fix-record (mix* finalize-spec m)))

(def (register-finalizer-spec finalizer super _self)
  ((field-update~* #f '__finalizer)
    (λ (previous)
      (if previous (mix* finalizer previous) finalizer))
    super))

;; generate-tag: ... → Tag
(define generate-tag
  (let ((counter 101)) ;; start high enough that a tag is obvious while debugging.
    (lambda _ (begin0 counter (set! counter (+ 1 counter))))))

(def (sub-record-spec key spec)
  (mix*
    (constant-field-spec key empty-record)
    (skew-ext (field-lens key) spec)
    (register-finalizer-spec
      (skew-ext (field-lens key) finalize-spec))))

;;;;; 5.x Order, Binary Tree Map, AVL Tree Map, Alist+AVL Hybrid Map

;;;; 5.x.1 Orderings

;; compare<-order-spec : derives 'compare from '<, '=, '> methods.
(def (compare<-order-spec super self method-id)
  (case method-id
    ((compare)
     (λ (x y)
       (cond ((self '< x y) '<)
             ((self '> x y) '>)
             ((self '= x y) '=)
             (else (error "incomparable" x y)))))
    (else (super method-id))))

;; number-order-spec : '<, '=, '> for numbers.
(def (number-order-spec super _self method-id)
  (case method-id
    ((<) (λ (x y) (< x y)))
    ((=) (λ (x y) (= x y)))
    ((>) (λ (x y) (> x y)))
    (else (super method-id))))

;; string-order-spec : '<, '=, '> for strings.
(def (string-order-spec super _self method-id)
  (case method-id
    ((<) (λ (x y) (string<? x y)))
    ((=) (λ (x y) (string=? x y)))
    ((>) (λ (x y) (string>? x y)))
    (else (super method-id))))

(def number-order (fix-record (mix* compare<-order-spec number-order-spec)))
(def string-order (fix-record (mix* compare<-order-spec string-order-spec)))

;; symbol-order-spec : delegates '<, '=, '>, 'compare to string-order on symbol->string.
(def (symbol-order-spec super _self method-id)
  (case method-id
    ((< = > compare)
     (λ (x y) (string-order method-id (symbol->string x) (symbol->string y))))
    (else (super method-id))))

(def symbol-order (fix-record symbol-order-spec))

(expect
  (number-order '< 23 42) => #t
  (number-order 'compare 8 4) => '>
  (string-order '< "Hello" "World") => #t
  (string-order 'compare "Foo" "FOO") => '>
  (string-order 'compare "42" "42") => '=
  (symbol-order '< 'aardvark 'aaron) => #t
  (symbol-order '= 'zzz 'zzz) => #t
  (symbol-order '> 'aa 'a) => #t
  (symbol-order 'compare 'alice 'bob) => '<
  (symbol-order 'compare 'b 'c) => '<
  (symbol-order 'compare 'c 'a) => '>)

;;;; 5.x.2 Binary Tree Map

;; binary-tree-map-spec : a sorted associative map over (self 'Key).
;; Representation:
;;   empty = '()
;;   node  = (left-subtree ((k . v)) right-subtree)
;; Methods: 'empty, 'empty?, 'node, 'singleton, 'acons, 'ref, 'afoldr.
(def (binary-tree-map-spec super self method-id)
  (case method-id
    ((empty)  '())
    ((empty?) null?)
    ((node)   (λ (l kv r) (list l (list kv) r)))
    ((singleton) (λ (k v) (self 'node '() (cons k v) '())))
    ((acons)
     (λ (k v t)
       (if (self 'empty? t) (self 'singleton k v)
         (let* ((tl (car t)) (tkv (caadr t)) (tk (car tkv)) (tr (caddr t)))
           (case (self 'Key 'compare k tk)
             ((=) (self 'node tl (cons k v) tr))
             ((<) (self 'node (self 'acons k v tl) tkv tr))
             ((>) (self 'node tl tkv (self 'acons k v tr))))))))
    ((ref)
     (λ (t k e)
       (if (self 'empty? t) (e)
         (let ((tl (car t)) (tk (caaadr t)) (tv (cdaadr t)) (tr (caddr t)))
           (case (self 'Key 'compare k tk)
             ((=) tv)
             ((<) (self 'ref tl k e))
             ((>) (self 'ref tr k e)))))))
    ((afoldr)
     (λ (f acc t)
       (if (self 'empty? t) acc
         (let ((tl (car t)) (tk (caaadr t)) (tv (cdaadr t)) (tr (caddr t)))
           (self 'afoldr f (f tk tv (self 'afoldr f acc tl)) tr)))))
    (else (super method-id))))

(def symbol-tree-map
  (fix-record (mix* (constant-field-spec 'Key symbol-order)
                    binary-tree-map-spec)))

(define my-binary-dict
  (foldl (lambda (kv t) (symbol-tree-map 'acons (car kv) (cdr kv) t))
         (symbol-tree-map 'empty)
         '((a . "I") (b . "II") (c . "III") (d . "IV") (e . "V"))))

(expect
  my-binary-dict =>
  '(() ((a . "I")) (() ((b . "II")) (() ((c . "III")) (() ((d . "IV")) (() ((e . "V")) ())))))
  (map (lambda (k) (symbol-tree-map 'ref my-binary-dict k (lambda () #f)))
       '(a b c d e z))
  => '("I" "II" "III" "IV" "V" #f))

;;;; 5.x.3 AVL Tree Map

;; avl-tree-rebalance-spec : overrides 'node to rebalance after insertion.
;; AVL node representation:
;;   node = (left-subtree ((k . v) . height) right-subtree)
;; Height is stored alongside the kv pair for O(1) balance-factor checks.
(def (avl-tree-rebalance-spec super _self method-id)
  (define (left t)    (car t))
  (define (kv t)      (caadr t))
  (define (height t)  (if (null? t) 0 (cdadr t)))
  (define (right t)   (caddr t))
  (define (balance t) (if (null? t) 0 (- (height (right t)) (height (left t)))))
  (define (mk l ckv r)
    (let ((lh (height l)) (rh (height r)))
      (or (member (- rh lh) '(-1 0 1)) (error "tree unbalanced!"))
      (list l (cons ckv (+ 1 (max lh rh))) r)))
  (def (node l ckv r)
    (case (- (height r) (height l))
      ((-1 0 1) (mk l ckv r))
      ((-2) (case (balance l)
              ((-1 0) (mk (left l) (kv l) (mk (right l) ckv r)))         ;; LL
              ((1)    (mk (mk (left l) (kv l) (left (right l)))           ;; LR
                          (kv (right l)) (mk (right (right l)) ckv r)))))
      ((2)  (case (balance r)
              ((-1)   (mk (mk l ckv (left (left r)))                      ;; RL
                          (kv (left r)) (mk (right (left r)) (kv r) (right r))))
              ((0 1)  (mk (mk l ckv (left r)) (kv r) (right r)))))))      ;; RR
  (case method-id
    ((node) node)
    (else (super method-id))))

;; Dict : AVL tree map with symbol keys.
(def Dict
  (fix-record (mix* (constant-field-spec 'Key symbol-order)
                    binary-tree-map-spec
                    avl-tree-rebalance-spec)))

(define my-avl-dict
  (foldl (lambda (kv t) (Dict 'acons (car kv) (cdr kv) t))
         (Dict 'empty)
         '((a . "I") (b . "II") (c . "III") (d . "IV") (e . "V"))))

(expect
  my-avl-dict =>
  '((() ((a . "I") . 1) ()) ((b . "II") . 3)
    ((() ((c . "III") . 1) ()) ((d . "IV") . 2) (() ((e . "V") . 1) ())))
  (map (lambda (k) (Dict 'ref my-avl-dict k (lambda () #f)))
       '(a b c d e z))
  => '("I" "II" "III" "IV" "V" #f))

;;;; 5.x.4 Alist+AVL Hybrid Map

;; alist+avl-map-spec : keeps the (self 'threshold) most-recently-added entries in an
;; alist for O(1) insertion and fast sequential access; older entries go to a Dict.
;;
;; Representation: (pair alist avl)
;;   alist = ((k . v)...) most-recently-added first, at most threshold entries
;;   avl   = AVL tree (Dict format) for older/evicted entries (symbol keys)
;;
;; On 'acons k v: remove k from alist (update), prepend (k . v); if |alist| > threshold,
;;   evict the oldest (last) alist entry into the AVL tree.
;; On 'ref t k e: scan alist first, then AVL tree.
;; On 'afoldr f acc t: fold AVL (skipping alist keys), then fold alist right-to-left;
;;   each key appears once, alist value shadows any stale AVL entry.
(def (alist+avl-map-spec super self method-id)
  (case method-id
    ((threshold) 8)
    ((empty)     (cons '() '()))
    ((empty?)    (λ (t) (and (null? (car t)) (null? (cdr t)))))
    ((singleton) (λ (k v) (cons (list (cons k v)) '())))
    ((acons)
     (λ (k v t)
       (let* ((al  (car t))
              (avl (cdr t))
              ;; Remove any existing entry for k from the alist (update semantics)
              (al2 (let lp ((a al))
                     (cond ((null? a) a)
                           ((equal? (caar a) k) (cdr a))
                           (else (cons (car a) (lp (cdr a)))))))
              ;; Prepend new entry at the front (most recently added)
              (al3 (cons (cons k v) al2)))
         (if (> (length al3) (self 'threshold))
           ;; Evict the oldest (last) alist entry into the AVL tree
           (let* ((rl    (reverse al3))
                  (evict (car rl))
                  (al4   (reverse (cdr rl)))
                  (avl2  (Dict 'acons (car evict) (cdr evict) avl)))
             (cons al4 avl2))
           (cons al3 avl)))))
    ((ref)
     (λ (t k e)
       (let ((found (assoc k (car t))))
         (if found (cdr found)
           (Dict 'ref (cdr t) k e)))))
    ((afoldr)
     (λ (f acc t)
       (let* ((al  (car t))
              (avl (cdr t))
              ;; Fold AVL tree, skipping keys already in the alist cache
              (acc1 (Dict 'afoldr
                       (λ (k v a) (if (assoc k al) a (f k v a)))
                       acc avl))
              ;; Fold alist right-to-left: oldest processed first, most-recent last
              (acc2 (foldr (lambda (kv a) (f (car kv) (cdr kv) a)) acc1 al)))
         acc2)))
    (else (super method-id))))

(def alist+avl-map (fix-record alist+avl-map-spec))

;; Tests: build a map with 10 entries (threshold=8).
;; After inserting a..j in order: a and b (oldest) are evicted to AVL;
;; c..j (8 entries, most-recent-first) remain in the alist.
(define my-hybrid-dict
  (foldl (lambda (kv t) (alist+avl-map 'acons (car kv) (cdr kv) t))
         (alist+avl-map 'empty)
         '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5)
           (f . 6) (g . 7) (h . 8) (i . 9) (j . 10))))

(expect
  (alist+avl-map 'ref my-hybrid-dict 'a (lambda () #f)) => 1   ;; in AVL part
  (alist+avl-map 'ref my-hybrid-dict 'j (lambda () #f)) => 10  ;; in alist
  (alist+avl-map 'ref my-hybrid-dict 'z (lambda () #f)) => #f) ;; absent

;; afoldr collects all 10 entries; alist values shadow any stale AVL entries.
(define my-hybrid-alist
  (alist+avl-map 'afoldr (λ (k v acc) (cons (cons k v) acc))
                 '() my-hybrid-dict))

(expect
  (length my-hybrid-alist)   => 10
  (assoc 'a my-hybrid-alist) => '(a . 1)
  (assoc 'j my-hybrid-alist) => '(j . 10))

;;;;; 6 Rebuilding OO from its Minimal Core

;;;; 6.1.2 Conflation: Crouching Typecast, Hidden Product

(def (pproto←spec spec)
  (cons spec (delay (fix-record spec))))
(def spec←pproto car)
(def (target←pproto pproto)
  (force (cdr pproto)))
(def pproto-id (pproto←spec idModExt))
(def (pproto-mix parent child)
  (pproto←spec (mix (spec←pproto parent) (spec←pproto child))))
;;(define (pproto-mix* . l) (foldl (uncurry2 pproto-mix) pproto-id l))
(define pproto-mix* (op*←op1.1 pproto-mix pproto-id))

(def coord-pproto (pproto←spec coord-spec))
(def color-pproto (pproto←spec color-spec))
(def point-p-pproto (pproto-mix coord-pproto color-pproto))

(expect (map (target←pproto coord-pproto) '(x y z color)) => '(2 4 #f #f)
        (map (target←pproto color-pproto) '(x y z color)) => '(#f #f #f "blue")
        (map (target←pproto point-p-pproto) '(x y z color)) => '(2 4 #f "blue"))

(def (add-x-pproto dx)
  (pproto←spec (add-x-spec dx)))
(def rho-pproto (pproto←spec rho-spec))

(def point-r-pproto (pproto-mix* rho-pproto coord-pproto (add-x-pproto 1)))
(def point-rc-pproto (pproto-mix* point-r-pproto color-pproto))

(expect (map (target←pproto point-r-pproto) '(x y rho color)) => '(3 4 5 #f)
        (map (target←pproto point-rc-pproto) '(x y rho color)) => '(3 4 5 "blue"))

;;; TODO: find a simple yet meaningful example for recursive protos...
;;; and their further specialization, nested or not

#|
(define web-config-spec
  (mix*
   (field-spec 'database
      (mix*
        (constant-field-spec 'port 80)
        (field-spec 'allowed
        (record!-spec)))
   record!-spec))
   (override-
  (λ (self) (λ (super) (λ (method-id)
    (case method-id
      ((port) 80)
      ((database) (length (self 'parts)))
      (else (super method-id)))))))
|#

;;;; 6.1.3 Recursive Conflation
(def (qproto-wrapper spec super _self)
  (cons spec super))
(def (qproto←spec spec)
  (delay (fix-record (mix spec (qproto-wrapper spec)))))
(def spec←qproto car)
(def (target←qproto qproto)
  (force (cdr qproto)))
(def (qproto-mix parent child)
  (qproto←spec (mix (spec←qproto parent) (spec←qproto child))))
(define (qproto-mix* . l) (foldl (uncurry2 qproto-mix) pproto-id l))


;;;; 6.1.4 Conflation for Records

(def (rproto-wrapper spec super self method-id)
  (if method-id (super method-id) spec))
(def (rproto←spec spec)
  (fix-record (mix spec (rproto-wrapper spec))))
(def rproto-id (rproto←spec idModExt))
(def (spec←rproto rproto)
  (rproto #f))
(def target←rproto identity)
(def (rproto-mix parent child)
  (rproto←spec (mix (spec←rproto parent) (spec←rproto child))))
(define rproto-mix* (op*←op1.1 rproto-mix rproto-id))
(def (rproto←record r)
  (rproto←spec (constant-spec r)))

(def coord-rproto (rproto←spec coord-spec))
(def color-rproto (rproto←spec color-spec))
(def point-p-rproto (rproto-mix coord-rproto color-rproto))

(expect (map (target←rproto coord-rproto) '(x y z color)) => '(2 4 #f #f)
        (map (target←rproto color-rproto) '(x y z color)) => '(#f #f #f "blue")
        (map (target←rproto point-p-rproto) '(x y z color)) => '(2 4 #f "blue"))

(def (add-x-rproto dx)
  (rproto←spec (add-x-spec dx)))
(def rho-rproto (rproto←spec rho-spec))

(def point-r-rproto (rproto-mix* rho-rproto coord-rproto (add-x-rproto 1)))
(def point-rc-rproto (rproto-mix* point-r-rproto color-rproto))

(expect (map (target←rproto point-r-rproto) '(x y rho color)) => '(3 4 5 #f)
        (map (target←rproto point-rc-rproto) '(x y rho color)) => '(3 4 5 "blue"))

;;;; 6.2.2 Simple First-Class Type Descriptors
;;;; TODO: examples of SCFTP.

(def (type-of instance)
  (instance #t))
(def (instance-call instance method-id)
  (type-of instance 'instance-methods method-id instance))

;;;; 6.2.3 Parametric First-Class Type Descriptors
;;;; TODO: examples in both monomorphic and polymorphic styles

;;;; 6.2.4 Class-style vs Typeclass-style
;;;; TODO: examples in both class-style and typeclass-style

;;;; 6.3 Types for OO
;;;; TODO: implement a type system???

;;;; 6.4 Stateful OO
;;;; TODO: show stateful examples???

;;;;; 7 Inheritance: Mixin, Single, Multiple, or Optimal

;;;; 7.2 Single Inheritance

;;; type ModDef r p = ∀ s : Type . s ⊂ r s ⇒ s → p s
;;; fixModDef : ModDef p p → Y p
;;; extendModDef : ModExt r1 p2 p1 → ModDef r2 p2 → ModDef r1∩r2 p1∩p2
;;; baseModDef : ModDef (λ (_) Top) (λ (_) Top)

(def fixModDef Y)
(def (extendModDef mext parent self)
  (mext (parent self) self))
(def (baseModDef _) top)

;;;; 7.3.7 Mixin Inheritance plus Precedence List

;; compute-precedence-list : MISpec ? ? ? → DependentList ? (MISpec ? ? ?)
;; effectiveModExt : MISpec r i p → ModExt r i p
;; fixMISpec : top → MISpec p top p → p

#|
(def (effectiveModExt mispec)
  (foldl (uncurry2 mix) idModExt (map getModExt (compute-precedence-list mispec))))
(def (fixMISpec top mispec)
  (fix top (effectiveModExt mispec)))
|#

;;;;; 9 Extending the Scope of OO

;;;; 9.1.2 Short Recap on Lenses

;; type View r s = s → r
;; type Update i p j q = (i → p) → j → q
;; type SkewLens r i p s j q = { view : View r s ; update : Update i p j q }

;;; Composing Lenses
;; compose-view : View s t → View r s → View r t
(def (compose-view v w)
  (compose w v))

;; compose-update : Update i p j q → Update j q k r → Update i p k r
(def (compose-update f g)
  (compose f g))

;; make-lens : View r s → Update i p j q → SkewLens r i p s j q
(def (make-lens v u)
  (extend-record 'view v
    (extend-record 'update u
      empty-record)))

;; compose-lens : SkewLens s j q ss jj qq → SkewLens r i p s j q →
;;                SkewLens r i p ss jj qq
(def (compose-lens l k)
  (make-lens
    (compose-view (l 'view) (k 'view))
    (compose-update (l 'update) (k 'update))))

;; id-lens : SkewLens r i p r i p
(def id-lens
  (make-lens identity identity))

(define compose-lens* (op*←op1.1 compose-lens id-lens))

;;; Getter and Setter (moved after make-lens)
(def (lens←getter*setter get set)
  (make-lens get (λ (f s) (set (f (get s)) s))))
(def (setter←lens l)
  (λ (b) (l 'update (λ (_a) b))))

;;; Field Lens
(def (field-view key r)
  (r key))
(def (field-update key f r)
  (extend-record key (f (r key)) r))
(def (field-lens key)
  (make-lens (field-view key) (field-update key)))

(define (field-lens* . keys)
  (apply compose-lens* (map field-lens keys)))

;; Same but #f interpreted as empty-record
;; field-view~ : Key → Record → Value
(def (field-view~ key r)
  (and r (r key)))
;; field-update~ : Key → (Value → Value) → Record → Record
(def (field-update~ key f rec)
  (field-update key f (or rec empty-record)))
;; field-lens~ : Key → Lens
(def (field-lens~ key)
  (make-lens (field-view~ key) (field-update~ key)))

(define (field-view~* . keys)
  (apply compose* (map field-view~ (reverse keys))))

;; field-lens~* : Key... → Lens
;; Like field-lens* but each intermediate node is initialized to empty-record if #f.
(define (field-lens~* . keys)
  (apply compose-lens* (map field-lens~ keys)))

;; field-update~* : Key... → (Value → Value) → Record → Record
;; Like field-update~ but nested over multiple keys.
(define (field-update~* . keys)
  (apply compose* (map field-update~ keys)))


(def test-rec (record (a (record (b (record (c 42)))))))
(def test-point (record (x 10) (y 20)))
(def x-lens (field-lens 'x))
(def set-x (setter←lens x-lens))
(def x-lens-2 (lens←getter*setter (field-view 'x) (extend-record 'x)))
(expect
  (@ (compose-lens*) 'view test-rec) => test-rec
  (@ (field-lens* 'a 'b 'c) 'view test-rec) => 42
  (@ (field-lens*) 'view test-rec) => test-rec
  (field-view 'x test-point) => 10
  (field-view 'y test-point) => 20
  (field-lens 'x 'view test-point) => 10
  (field-lens 'y 'view test-point) => 20
  (field-lens 'x 'update add1 test-point 'x) => 11
  (field-lens 'x 'update mul10 test-point 'x) => 100
  (field-lens 'x 'update mul10 test-point 'y) => 20  ;; y unchanged
  (id-lens 'view test-point) => test-point
  (id-lens 'update add1 5) => 6
  (compose-lens (field-lens 'a) (field-lens 'b) 'view
    (record (a (record (b 99))))) => 99
  (set-x 999 test-point 'x) => 999
  (set-x 999 test-point 'y) => 20
  (x-lens-2 'view test-point) => 10
  (x-lens-2 'update add1 test-point 'x) => 11)

;;;; 9.1.3 Focusing a Modular Extension
;;; From Sick to Ripped
;; skew-ext : SkewLens i r p j s q → ModExt i r p → ModExt j s q
(def (skew-ext l m super self)
  (l 'update (λ (inner-super) (m inner-super (l 'view self))) super))

;;;; 9.1.4 Adjusting Context and Focus
;;; Adjusting the Extension Focus
;; update-only-lens : Update i p j q → SkewLens r i p r j q
(def (update-only-lens u)
  (make-lens identity u))

;; update-lens : SkewLens r i p s j q → Update j q jj qq → SkewLens r i p r jj qq
(def (update-lens l u)
  (make-lens (l 'view) (compose (l 'update) u)))

(def outer-rec (record (inner (record (val 5)))))
(def inner-val-lens (field-lens* 'inner 'val))
(def (double-ext super _self) (* 2 super))
(def focused-ext (skew-ext (update-only-lens (inner-val-lens 'update)) double-ext))
(expect
  (fix outer-rec focused-ext 'inner 'val) => 10)
(expect
  ;; update-only-lens: view is identity, update applies transformation
  (update-only-lens (compose mul10) 'view 7) => 7
  (update-only-lens (compose mul10) 'update add1 7) => 80)  ;; mul10 (add1 7)

;;; Broadening the Focus
;; reverse-view : s → MonoLens s a → View a s
;; reverse-update : s → MonoLens s a → Update a s a s
;; reverse-lens : s → MonoLens s a → MonoLens a s
(def (reverse-view s l a)
  (setter←lens l a s))
(def (reverse-update s l f a)
  (l 'view (f (reverse-view s l a))))
(def (reverse-lens s l)
  (make-lens (reverse-view s l) (reverse-update s l)))

(def rev-x (reverse-lens test-point x-lens))

(expect
  (x-lens 'view test-point) => 10
  (rev-x 'view 42 'x) => 42
  (rev-x 'view 42 'y) => 20 ;; y unchanged from test-point

  ;; update transforms the record, then extracts the value
  (rev-x 'update (field-lens 'x 'update mul10) 10) => 100
  (rev-x 'update (field-lens 'y 'update mul10) 10) => 10) ;; y unchanged from test-point

;;; Adjusting the Context
;; view-only-lens : View r s → SkewLens r i p s i p
(def (view-only-lens v)
  (make-lens v identity))

;; view-lens : SkewLens r i p s j q → View rr r → SkewLens rr i p r j q
(def (view-lens l v)
  (make-lens (compose-view (l 'view) v) (l 'update)))

(expect
   ;; view-only-lens: view transforms, update is identity
  (view-only-lens mul10 'view 7) => 70
  (view-only-lens mul10 'update add1 7) => 8)

;;;; 9.1.5 Optics for Specifications, Prototypes and Classes

;;; Specification Methods
(def widget-shop
  (record (widgets (record (foo (record (x-pos 100) (y-pos 500)))))))
(expect
 (skew-ext
  (update-lens (field-lens* 'widgets 'foo) (field-update 'x-pos))
  (λ (super _self) (+ super 50))
  widget-shop
  widget-shop
  'widgets 'foo 'x-pos) => 150)

;;; Prototype Specification
(def rproto-spec-view spec←rproto)
(def (rproto-spec-setter new-spec _old-rproto) (rproto←spec new-spec))
(def rproto-spec-lens (lens←getter*setter rproto-spec-view rproto-spec-setter))

;;; Prototype Target Update options (what to do when updating the target of an rproto)
(def rproto-target-update/OutOfSync     ;; just update fields, spec no longer matches
  identity)
(def rproto-target-update/OverwriteSpec ;; replace spec with constant-spec of current state
  rproto←record)
(def rproto-target-update/NoMoreSpec    ;; erase the magic spec field, no longer extensible
  (extend-record #f #f))
(define rproto-target-update/Error      ;; signal an error — safest default
  abort)

;;;; 9.1.6 Optics for Class Instance Methods

;; instance-method-lens : MethodId → SkewLens for a class instance method
(def (instance-method-lens method-id)
  (update-lens rproto-spec-lens
    (compose-update (field-update 'instance-methods)
                    (field-update method-id))))

;; make-call-next-method : inherited-method → element → args → call-next-method-fn
(def (make-call-next-method inherited-method element args)
  (case-lambda
    (()              (apply (inherited-method element) args))
    ((new-element . new-args) (apply (inherited-method new-element) new-args))))

;; instance-method-spec : MethodId → (element → call-next-method → result) → ModExt
(def (instance-method-spec method-id method-body)
  (skew-ext (instance-method-lens method-id)
    (λ (inherited-method _self element)
      (λ args
        (method-body element
          (make-call-next-method inherited-method element args))))))

;; base-instance-method-spec : omits call-next-method for leaf methods
(def (base-instance-method-spec method-id method-body)
  (instance-method-spec method-id
    (λ (element _call-next-method) (method-body element))))

;; instance-field-lens : FieldId → SkewLens for a class instance field descriptor
(def (instance-field-lens field-id)
  (update-lens rproto-spec-lens
    (compose-update (field-update 'instance-fields)
                    (field-update field-id))))

;; simple-instance-field-spec : FieldId → ModExt → ModExt
(def (simple-instance-field-spec field-id init-mod-ext)
  (skew-ext (instance-field-lens field-id)
    (rproto←record (record (init init-mod-ext)))))

;;;; 9.1.7 Simple Class Initialization

;; class-proto : List(SlotDescriptor) → rproto
;; A slot descriptor is a record with 'name and 'init-spec fields.
;; Each init-spec is a modular extension: (inherited self) → value
(def (class-proto slots)
  (rproto←spec
    (apply mix*
      (map (λ (slot) (field-spec (slot 'name) (slot 'init-spec))) slots))))

(def (constant-slot name value)
  (record (name name)
          (init-spec (constant-spec value))))

(def (computed-slot name thunk)
  (record (name name)
          (init-spec (λ (_super self) (thunk self)))))

(def (required-slot name)
  (record (name name)
          (init-spec (λ (_super _self)
                       (error "Missing required slot" name)))))

;; Tests for class-proto
(def rectangle-slots
  (list
    (constant-slot 'width 10)
    (constant-slot 'height 20)
    (computed-slot 'area (λ (self) (* (self 'width) (self 'height))))))

(def rectangle-proto (class-proto rectangle-slots))

(expect
  (rectangle-proto 'width) => 10
  (rectangle-proto 'height) => 20
  (rectangle-proto 'area) => 200)

(def colored-rectangle-slots
  (cons (constant-slot 'color "black") rectangle-slots))

(def colored-rectangle-proto (class-proto colored-rectangle-slots))

(expect
  (colored-rectangle-proto 'color) => "black"
  (colored-rectangle-proto 'area) => 200)

;;; HPROTO encoding
;;; (pass half before method-id, not after as in YASOS
;;; also take a late-bound hyper/htop for mixin semantics)

(define rop*←op2 (lambda (op2 id)
  ;; Simpler, though less efficient: (λ args (foldl op2 id args))
  (letrec ((op* (case-lambda
                 (() id)
                 ((x) x)
                 ((x y) (op2 x y))
                 ;; weird: (foldl (lambda (x y) (op2 y x)) x r) doesn't work with Chez (?)
                 ((x y . r) (apply op* (op2 x y) r)))))
    op*)))
;; Variant of rop*←op2, but for a curried operator that takes one argument then the next.
(define rop*←op1.1 (lambda (op1.1 id)
  (rop*←op2 (lambda (x y) (@ op1.1 x y)) id)))

(def (id-hspec hyper half) hyper)
(def (half-top half) #f)
(def (half-empty-record half msg-id) #f)
(def (hspec-half hyper hspec) (hspec hyper))
(def (hspec-fix hyper hspec) (hspec hyper (hspec hyper)))
(def (half-ref half) (half half))
(def (hspec-rmix hparent hchild hyper half)
  (hchild (hparent hyper) half))
(define hspec-rmix* (rop*←op1.1 hspec-rmix id-hspec))
(def (hspec-half-top) (hspec-half half-top))
(def (hspec-half-record) (hspec-half half-empty-record))
(def (field-hspec key hcompute-value hyper half method-id)
  (let ((inherited (hyper half method-id)))
    (if (equal? key method-id)
        (hcompute-value inherited half)
        inherited)))
(def (constant-field-hspec key val)
  (field-hspec key (constant-spec val)))

;;; Reproducing earlier examples in this encoding
(def coord-hspec
  (hspec-rmix* (constant-field-hspec 'x 2)
               (constant-field-hspec 'y 4)))
(def color-hspec
  (field-hspec 'color (λ (_half _hinherited) "blue")))
(def point-24h (hspec-half-record (hspec-rmix coord-hspec color-hspec)))
(def (add-x-hspec dx) (field-hspec 'x (λ (inherited _half) (+ dx inherited))))
(def area-hspec (field-hspec 'area (λ (_inherited half) (* (half half 'x) (half half 'y)))))

(def point-34ah (hspec-half-record (hspec-rmix* coord-hspec color-hspec (add-x-hspec 1) area-hspec)))
(def blue-h (hspec-half-record color-hspec))

(expect (half-ref half-top) => #f
        (half-ref blue-h 'color) => "blue"
        (map (half-ref blue-h) '(x y z color area)) => '(#f #f #f "blue" #f)
        (map (half-ref point-24h) '(x y z color area)) => '(2 4 #f "blue" #f)
        (map (half-ref point-34ah) '(x y z color area)) => '(3 4 #f "blue" 12))


;; TODO: write and test wrapper to Y-style spec from a U-style hspec, and back
(def (hspec→spec hspec super self)
   (letrec ((half (λ (_) (hspec (λ (_) super) half))))
     (half #f)))
(expect (map (fix-record (hspec→spec (hspec-rmix* coord-hspec color-hspec (add-x-hspec 1) area-hspec)))
             '(x y z color area)) => '(3 4 #f "blue" 12))

;; TODO: fix this
(def (spec→hspec spec hyper half)
  ;; eta-conversions necessary in eager context
  (letrec ((self (λ (x) (half half x))) ;; (η (half half))
           (super (λ (x) (hyper half x)))) ;; (η (hyper half))
    (spec super self)))

(define u-comp (spec→hspec (mix* coord-spec area-spec (add-x-spec 1) color-spec)))

(expect (map (half-ref (hspec-half-record u-comp)) '(x y z color area)) => '(3 4 #f "blue" 12))



;;;;; 9.2 Method Combinations

;;;; 9.2.1 Representing Sub-Methods

;; standard-method-cons : MethodFn → List(MethodFn) → List(MethodFn)
;; Prepends a method fn to the existing list (standard cons).
(def (standard-method-cons spec specs)
  (cons spec specs))

;; sub-method-spec : MethodCons → Tag → MethodId → MethodFn → ModExt
;;   MethodCons = MethodFn → List(MethodFn) → List(MethodFn)
;;   Tag        = Symbol  (qualifier: 'primary 'before 'after 'around, or simple-comb name)
;;   MethodId   = Symbol  (method name in the record, e.g. 'compute 'greet)
;;   MethodFn   = CallNextMethod → Self → (Arg... → Result)  (see 9.2.2 for details)
;;   ModExt     = ? → ? → ?  (modular extension; see field-spec)
;;
;; Creates a ModExt that prepends method-fn to sub-methods[method-id][tag].
(def (sub-method-spec method-cons tag method-id method-fn)
  (field-spec 'sub-methods
    (λ (inherited _self)
      (let* ((subs       (or inherited empty-record))
             (per-method (or (subs method-id) empty-record))
             (tag-list   (or (per-method tag) '())))
        (extend-record method-id
          (extend-record tag (method-cons method-fn tag-list) per-method)
          subs)))))

;; standard-sub-method-spec : Tag → MethodId → MethodFn → ModExt
;;   (sub-method-spec with standard-method-cons; 1st arg is Tag, 2nd is MethodId)
(def standard-sub-method-spec (sub-method-spec standard-method-cons))

;; sub-method-lens : MethodId → Tag → SkewLens into the sub-methods record
;; (Useful for rproto encoding; in Y encoding prefer sub-method-spec directly.)
(def (sub-method-lens method-id tag)
  (compose-lens* (field-lens 'sub-methods)
                 (field-lens method-id)
                 (field-lens tag)))

;; method-combination-init-spec : MethodId → InitRecord → ModExt
;;   InitRecord = record {tag: List(MethodFn), ...}
;; Initializes sub-methods[method-id] with init-record if not already present.
(def (method-combination-init-spec method-id method-combination-init)
  (field-spec 'sub-methods
    (λ (inherited _self)
      (let ((subs (or inherited empty-record)))
        (if (subs method-id)
          subs
          (extend-record method-id method-combination-init subs))))))

;; simple-method-combination-init : Name → InitRecord  {around: (), name: ()}
(def (simple-method-combination-init name)
  (extend-record 'around '()
   (extend-record name '()
    empty-record)))

;; standard-method-combination-init : InitRecord  {before:() after:() around:() primary:()}
(def standard-method-combination-init
  (extend-record 'before '()
   (extend-record 'after '()
    (simple-method-combination-init 'primary))))

;;;; 9.2.2 Standard Method Combination

;; MethodFn = CallNextMethod → Self → (Arg... → Result)   (curried)
;;   CallNextMethod = case-lambda: () → Result | new-arg... → Result
;;     calling with no args forwards the original args to the next method;
;;     calling with new-args uses those instead.
;;   Self   = the current object (the fixpoint record)
;;   Arg... = the method's own user arguments (applied after Self)
;;
;; This mirrors the ModExt triple: CallNextMethod ≈ Inherited, Self ≈ Required.
;; Methods that ignore CallNextMethod and Self may use (constant-spec f)
;;   where f is a function of Arg... only.
;;
;; call-chain invokes m as: first (m cnm) → fn-of-self,
;;   then (fn-of-self self) → fn-of-args, then (apply fn-of-args args).

;; make-call-next-method : Next → Args → CallNextMethod
;;   Next = ...Args → Result  (the remaining chain)
;; When called with no args, forwards the original args to next.
;; When called with new-args, forwards them instead.
(def (make-call-next-combined-method next args)
  (case-lambda
    (()       (apply next args))
    (new-args (apply next new-args))))

;; call-chain : List(MethodFn) → OnExhausted → Self → EffectiveMethod
;;   EffectiveMethod = Arg... → Result   (self already captured via closure)
;;   Each MethodFn m is invoked curried: (m cnm) → fn-of-self,
;;   then (fn-of-self self) → fn-of-args, then (apply fn-of-args args).
(def (call-chain methods on-exhausted self)
  (foldr
    (lambda (m next)
      (λ args
        (apply ((m (make-call-next-combined-method next args)) self) args)))
    on-exhausted
    methods))

;; progn-methods-most-specific-first : List(MethodFn) → Self → Args → #f
;; Runs each method in order for side-effects; call-next-method = abort.
(def (progn-methods-most-specific-first methods self args)
  (foldl (lambda (m _) (apply ((m abort) self) args)) #f methods))

;; progn-methods-most-specific-last : List(MethodFn) → Self → Args → #f
(def (progn-methods-most-specific-last methods self args)
  (foldr (lambda (m _) (apply ((m abort) self) args)) #f methods))

;; standard-no-applicable-method : MethodId → ...Args → Error
(define (standard-no-applicable-method method-id . args)
  (error "no applicable method" method-id args))

(define no-applicable-method standard-no-applicable-method)

;; standard-compute-effective-method : MethodId → SubMethods → Self → EffectiveMethod
;;   SubMethods = record {before: List(MethodFn), after: ..., around: ..., primary: ...}
(def (standard-compute-effective-method method-id sub-methods self)
  (call-chain (sub-methods 'around)
    (λ args
      (progn-methods-most-specific-first (sub-methods 'before) self args)
      (let ((result
              (apply (call-chain (sub-methods 'primary)
                       (λ args (apply no-applicable-method method-id args))
                       self)
                     args)))
        (progn-methods-most-specific-last (sub-methods 'after) self args)
        result))
    self))

;; standard-method-init-spec : MethodId → ModExt
;; Initializes method-id to use the standard method combination.
;; The stored value is an EffectiveMethod; self is captured from the field-spec closure.
(def (standard-method-init-spec method-id)
  (mix
    (field-spec method-id
       (λ (_inherited self)
         (standard-compute-effective-method method-id (self 'sub-methods method-id) self)))
    (method-combination-init-spec method-id standard-method-combination-init)))

;; Convenience specs for each standard qualifier (Tag → MethodId → MethodFn → ModExt)
(def primary-method-spec (standard-sub-method-spec 'primary))
(def before-method-spec  (standard-sub-method-spec 'before))
(def after-method-spec   (standard-sub-method-spec 'after))
(def around-method-spec  (standard-sub-method-spec 'around))

;; Tests for standard method combination

;; Single primary method: (obj 'compute x) → (f x)
(def smc-obj-mul10
  (fix-record
    (mix*
      (standard-method-init-spec 'compute)
      (primary-method-spec 'compute (λ (_call-next-method _self x) (* x 10))))))

(expect
  (smc-obj-mul10 'compute 3) => 30
  (smc-obj-mul10 'compute 5) => 50)

;; Around method wraps primary; (call-next-method) invokes primary with original args
(def smc-obj-around
  (fix-record
    (mix*
      (standard-method-init-spec 'compute)
      (primary-method-spec 'compute (constant-spec (λ (x) (* x 10))))
      (around-method-spec 'compute (λ (call-next-method _self _x) (+ (call-next-method) 1))))))

(expect (smc-obj-around 'compute 3) => 31) ;; (* 3 10) = 30; around adds 1

;; Before/after run for side-effects; call-next-method = abort (must not be called)
(define smc-log '())
(def smc-obj-logged
  (fix-record
    (mix*
      (standard-method-init-spec 'op)
      (primary-method-spec 'op (constant-spec (λ (x) (* x x))))
      (before-method-spec  'op (constant-spec (λ (x)
                                 (set! smc-log (cons (list 'before x) smc-log)))))
      (after-method-spec   'op (constant-spec (λ (x)
                                 (set! smc-log (cons (list 'after x) smc-log))))))))

(expect (smc-obj-logged 'op 4) => 16)
(expect smc-log => '((after 4) (before 4)))

;;;; 9.2.3 Simple Method Combination

;; simple-compute-effective-method :
;;   Name → Stop? → Op0 → Op1 → Op2 → Order → SubMethods → Self → EffectiveMethod
;;   Name  = Symbol  (tag for the sub-method list)
;;   Stop? = Result → Bool        (short-circuit: stop folding when true)
;;   Op0   = #f → Result          (result when no methods; takes dummy arg)
;;   Op1   = Result → Acc         (transforms first method result into initial accumulator)
;;   Op2   = Result → Acc → Acc   (fold step; must be curried)
;;   Order = 'most-specific-first | 'most-specific-last
;;
;; Simple MethodFn = CallNextMethod → Self → Result  (no user args; result is
;;   the method's direct contribution, folded by Op1/Op2 across all methods).
;; Each simple method m is called as ((m abort) self) with abort as cnm
;;   (call-next-method must not be invoked in simple methods).
;; Use (constant-spec v) for a method that contributes the constant value v.
(def (simple-compute-effective-method
       name stop? op0 op1 op2 order sub-methods self)
  (let* ((arounds (sub-methods 'around))
         (methods (sub-methods name))
         (ordered (case order
                    ((most-specific-first) methods)
                    ((most-specific-last) (reverse methods)))))
   (call-chain arounds
    (λ args
      (letrec ((run (λ (m) (m abort self)))
               (f   (lambda (acc lst)
                      (if (and (not (stop? acc)) (pair? lst))
                        (let ((v (op2 (run (car lst)) acc)))
                          (if (stop? v) v (f v (cdr lst))))
                        acc))))
        (if (pair? ordered)
          (f (op1 (run (car ordered))) (cdr ordered))
          (op0 #f))))
    self)))

(def (compute-effective-method/progn)
  (simple-compute-effective-method
    'progn (λ (_) #f) (λ (_) #f) (λ (x) x) (λ (r _) r)
    'most-specific-first))

(def (compute-effective-method/and)
  (simple-compute-effective-method
    'and not (λ (_) #t) (λ (x) x) (λ (r _) r)
    'most-specific-first))

(def (compute-effective-method/+)
  (simple-compute-effective-method
    '+ (λ (_) #f) (λ (_) 0) (λ (x) x) (λ (x y) (+ x y))
    'most-specific-first))

(def (compute-effective-method/*)
  (simple-compute-effective-method
    '* (λ (_) #f) (λ (_) 1) (λ (x) x) (λ (x y) (* x y))
    'most-specific-first))

(def (compute-effective-method/list)
  (simple-compute-effective-method
    'list (λ (_) #f) (λ (_) '()) (λ (x) (list x)) (λ (x y) (cons x y))
    'most-specific-last))

;; list-method-init-spec : MethodId → ModExt
;; Initializes method-id to collect contributions from all methods into a list.
;; Most-specific method's contribution appears first in the result list.
(def (list-method-init-spec method-id)
  (mix
    (field-spec method-id
       (λ (_inherited self)
         (compute-effective-method/list (self 'sub-methods method-id) self)))
    (method-combination-init-spec method-id (simple-method-combination-init 'list))))

;; list-method-spec : MethodId → MethodFn → ModExt  (tag = 'list)
(def list-method-spec (standard-sub-method-spec 'list))

;; Tests for simple method combination (list)
;; Methods taking no user args use (constant-spec value) as MethodFn
;; ((constant-spec v) call-next-method self) = v
(def list-parts-obj
  (fix-record
    (mix*
      (list-method-init-spec 'parts)
      (list-method-spec 'parts (constant-spec 'wheel))
      (list-method-spec 'parts (constant-spec 'engine)))))

;; most-specific-last evaluation, most-specific-first in result list
;; engine was added last (most specific) → appears first in result
(expect ((list-parts-obj 'parts)) => '(engine wheel))

;; Tests for + combination
(def sum-obj
  (fix-record
    (mix*
      (mix
        (field-spec 'total
          (λ (_inherited self)
            (compute-effective-method/+ (self 'sub-methods 'total) self)))
        (method-combination-init-spec 'total (simple-method-combination-init '+)))
      (standard-sub-method-spec '+ 'total (constant-spec 3))
      (standard-sub-method-spec '+ 'total (constant-spec 4)))))

(expect ((sum-obj 'total)) => 7)

;;;;; C4 Linearization: Multiple Inheritance with Suffix Support
;;;; Ported from gerbil/src/gerbil/runtime/c3.ss
;;;; See gerbil/doc/reference/gerbil/runtime/c3.md for the theory.
;;;; See gerbil/src/gerbil/test/c3-test.ss for the tests.

;;;; Portable hash tables (using eq? as the key equality predicate)

(cond-expand
  (gerbil
   (begin
     (import :std/debug/DBG)
     (define (make-eqht) (make-table test: eq?))
     (define (eqht-ref t k default) (table-ref t k default))
     (define (eqht-set! t k v) (table-set! t k v))))
  (racket
   (begin
     (define (make-eqht) (make-hasheq))
     (define (eqht-ref t k default) (hash-ref t k default))
     (define (eqht-set! t k v) (hash-set! t k v))))
  (chezscheme
   (begin
     (define (make-eqht) (make-eq-hashtable))
     (define (eqht-ref t k default) (hashtable-ref t k default))
     (define (eqht-set! t k v) (hashtable-set! t k v))))
  (else
   (begin
     (define (make-eqht) (make-hash-table equal?)))
     (define (eqht-ref t k default) (hash-table-ref/default t k default))
     (define (eqht-set! t k v) (hash-table-set! t k v))))

(def (memo f)
  (let ((t (make-eqht)))
    (lambda (x)
      (let ((y (eqht-ref t x t)))
        (if (eq? y t)
            (let* ((z (f x))
                   (y2 (eqht-ref t x t))) ;; second check in case non-local exits did something funky
              (if (eq? y2 t)
                  (begin (eqht-set! t x z) z)
                  y2))
            y)))))

;;;; List utilities needed by C4

;; Reverse lst and prepend to tail.
(define (append-reverse lst tail)
  (let loop ((l lst) (t tail))
    (if (null? l) t (loop (cdr l) (cons (car l) t)))))

;; Walk rhead left-to-right, reverse-prepending elements onto tail,
;; until (pred elem) is true.  Returns (values remaining new-tail).
(define (append-reverse-until pred rhead tail)
  (let loop ((rhead rhead) (tail tail))
    (cond
      ((null? rhead) (values '() tail))
      ((pred (car rhead)) (values rhead tail))
      (else (loop (cdr rhead) (cons (car rhead) tail))))))

;; Destructively remove empty sublists from a list of lists; return modified list.
(define (remove-nulls lists)
  (filter pair? lists))

;; Return the first element of lst satisfying pred, or #f.
(define (find pred lst)
  (let loop ((l lst))
    (cond ((null? l) #f)
          ((pred (car l)) (car l))
          (else (loop (cdr l))))))

;;;; The C4 Linearization Algorithm

;; filter-map: map f over lst, keeping only truthy results (not in R7RS-small).
(define (filter-map f lst)
  (let loop ((l lst) (acc '()))
    (if (null? l)
        (reverse acc)
        (let ((r (f (car l))))
          (loop (cdr l) (if r (cons r acc) acc))))))

;; c4-linearize head parents get-precedence-list suffix? [eq [get-name]]
;;   -> (values precedence-list super-suffix-or-#f)
;;
;; Compute the precedence list for a specification.
;;   head               - prefix list to prepend (typically (list x) or '())
;;   parents            - list of totally-ordered parent chains (each chain is a list);
;;                        supports an arbitrary DAG for the local precedence order,
;;                        e.g. '((A B C)) for a single chain, or '((A B) (C A)) for a DAG.
;;   get-precedence-list - procedure: x -> its full precedence list (incl. x at front)
;;   suffix?            - predicate: is x a "suffix" (single-inh struct)?
;;   eq                 - equality on specs (optional, default: equal?)
;;   get-name           - name extractor for error messages (optional, default: identity)
;;
;; Returns two values:
;;   - the linearized precedence list (most specific first)
;;   - the most specific suffix ancestor, or #f
(define (c4-linearize head parents get-precedence-list suffix? . opts)
  (let* ((eq       (if (pair? opts) (car opts) eq?))
         (get-name (if (and (pair? opts) (pair? (cdr opts)))
                       (cadr opts) (lambda (x) x)))
         (super-suffix
          (lambda (x)
            (find suffix? (cdr (get-precedence-list x))))))
    (set! parents (remove-nulls parents))
    (cond
      ;; 0 non-empty parent-lists: base class
      ((null? parents)
       (values head #f))

      ;; 1 parent-list with 1 parent: single inheritance
      ((and (null? (cdr parents)) (null? (cdar parents)))
       (let* ((parent (caar parents))
              (pl (get-precedence-list parent)))
         (values (append head pl)
                 (if (suffix? parent) parent (super-suffix parent)))))

      ;; Multiple inheritance
      (else
       (let ((rcandidates '())  ;; reversed candidate lists accumulated during scan
             (ss  #f)           ;; most specific suffix ancestor found so far
             (ss-tail '()))     ;; PL of ss (suffix-tail), or '() if none

         (define (get-names lst) (map get-name lst))
         (define (err . args)
           (apply error "Inconsistent precedence graph"
                  `(head: ,(get-names head)
                    common-suffix-tail: ,(get-names ss-tail)
                    rcandidates: ,(map get-names rcandidates)
                    ,@args)))

         ;; Is s2 reachable via super-suffix chain from s1?
         (define (super-suffix? s1 s2)
           (or (not s2)
               (let loop ((s s1))
                 (and s (or (eq s s2) (loop (super-suffix s)))))))

         ;; Merge two suffix specs; return the more specific one, or error.
         (define (merge-suffix s1 s2)
           (cond
             ((not s2) s1)
             ((not s1) s2)
             (else
              (let loop ((t1 s1) (t2 s2))
                (cond
                  ((eq t1 s2) s1)
                  ((eq t2 s1) s2)
                  ((not t1) (if (super-suffix? t2 s1) s2
                                (err 'suffix-incompatibility: (list (get-name s1) (get-name s2)))))
                  ((not t2) (if (super-suffix? t1 s2) s1
                                (err 'suffix-incompatibility: (list (get-name s1) (get-name s2)))))
                  (else (loop (super-suffix t1) (super-suffix t2))))))))

         ;; Ancestor counts: tracks non-head appearances across all candidate lists.
         ;; Also used for deduplication: get-count=0 means "not yet processed".
         (define ancestor-counts (make-eqht))
         (define (get-count c) (eqht-ref ancestor-counts c 0))
         (define (inc-count! c) (eqht-set! ancestor-counts c (+ 1 (get-count c))))
         (define (dec-count! c) (eqht-set! ancestor-counts c (- (get-count c) 1)))

         ;; Initial scan: for each parent-list, for each parent, walk its PL.
         ;; get-count=0 detects parents not yet processed (deduplication across chains).
         (define ___init__rcandidates__ss__ss-tail__ancestor-counts ; make Chez happy
         (for-each
          (lambda (parent-list)
            (for-each
             (lambda (parent)
               (when (zero? (get-count parent))
                 ;; New parent: walk its PL until we hit a suffix ancestor.
                 (let loop ((al (get-precedence-list parent)) (r '()))
                   (cond
                     ((null? al)
                      ;; No suffix found; add reversed non-suffix prefix to candidates.
                      (unless (null? r)
                        (set! rcandidates (cons r rcandidates))))
                     ((suffix? (car al))
                      ;; Found suffix; try to merge with current ss.
                      (let ((ms (merge-suffix (car al) ss)))
                        (unless (eq ms ss)
                          ;; New longer suffix: count new suffix-tail elements.
                          ;; (stops at the old ss, which was already counted)
                          (let count-loop ((tl al))
                            (unless (null? tl)
                              (unless (eq (car tl) ss)
                                (inc-count! (car tl))
                                (count-loop (cdr tl)))))
                          (set! ss ms)
                          (set! ss-tail al))
                        ;; Done with this PL; add the reversed non-suffix prefix.
                        (unless (null? r)
                          (set! rcandidates (cons r rcandidates)))))
                     (else
                      (inc-count! (car al))
                      (loop (cdr al) (cons (car al) r)))))))
             parent-list))
          parents))

         ;; Build suffix-tail-index: element -> position.
         ;; Most specific element gets highest index (= length of suffix-tail),
         ;; least specific gets index 1.
         (define suffix-tail-index (make-eqht))
         (define __init_suffix-tail-index ; make Chez happy
         (let loop ((i (length ss-tail)) (t ss-tail))
           (unless (null? t)
             (eqht-set! suffix-tail-index (car t) i)
             (loop (- i 1) (cdr t)))))

         ;; Build r-local-order: reverse of each non-singleton parent-list.
         ;; These enforce the local precedence order constraints.
         (define r-local-order
           (filter-map (lambda (pl) (and (pair? (cdr pl)) (reverse pl)))
                       parents))
         (define ___init_r-local-order__and_update__rcandidates (begin ; make Chez happy
         (for-each (lambda (cl) (for-each inc-count! cl)) r-local-order)
         (set! rcandidates (append r-local-order rcandidates))))

         ;; Re-reverse each reversed candidate list, removing suffix-tail elements.
         ;; Suffix-tail elements are skipped; they must appear in increasing index order
         ;; (highest = most specific first, as we traverse the reversed list from
         ;; less-specific to more-specific).
         (define (remove-suffix-tail-and-reverse rcl)
           (let u ((cl-rhead rcl) (suffix-pos -1))
             (cond
               ((null? cl-rhead) '())
               (else
                (let* ((c    (car cl-rhead))
                       (clrh (cdr cl-rhead))
                       (p    (eqht-ref suffix-tail-index c #f)))
                  (cond
                    ((not p)
                     ;; c not in suffix-tail: collect consecutive non-suffix-tail elements.
                     (let-values (((clrh2 h)
                                   (append-reverse-until
                                    (lambda (x) (eqht-ref suffix-tail-index x #f))
                                    clrh (list c))))
                       (if (null? clrh2)
                           h
                           (err 'precedence-list-head: (get-names (reverse clrh2))
                                'ancestor-out-of-order-vs-suffix-tail: (get-name (car clrh2))))))
                    ((> p suffix-pos)
                     ;; c in suffix-tail, in correct order; skip it.
                     (u clrh p))
                    (else
                     ;; c in suffix-tail, out of order.
                     (err 'ancestor-out-of-order-vs-suffix-tail: (get-name c)
                          'suffix-pos: suffix-pos))))))))

         ;; Build candidate lists (suffix-tail removed, in proper PL order).
         (define candidates
           (reverse (remove-nulls (map remove-suffix-tail-and-reverse rcandidates))))

         ;; Promote heads: decrement count for head of each candidate list.
         ;; A head with count=0 is a valid next element for the precedence list.
         (define ___adjust_counts ; make Chez happy
         (for-each (lambda (cl) (dec-count! (car cl))) candidates))

         ;; c3-select-next: find first candidate-list head with count=0.
         (define (c3-select-next tails)
           (let loop ((ts tails))
             (cond
               ((null? ts) (err 'c3-select-next: 'fail))
               ((zero? (get-count (caar ts))) (caar ts))
               (else (loop (cdr ts))))))

         ;; remove-next: remove chosen element from all candidate lists.
         ;; Decrement the count of each newly exposed head.
         (define (remove-next next tails)
           (map (lambda (tail)
                  (cond
                   ((eq (car tail) next)
                    (and (pair? (cdr tail))
                         (dec-count! (cadr tail)))
                    (cdr tail))
                   (else
                    tail)))
                tails))

         ;; Main C3 merge loop: repeatedly select and remove the next element.
         (define precedence-list
           (let c3loop ((rhead (append-reverse head '())) (tails candidates))
             (cond
               ((null? tails)
                (append-reverse rhead ss-tail))
               ((null? (cdr tails))
                (append-reverse rhead (append (car tails) ss-tail)))
               (else
                (let ((next (c3-select-next tails)))
                  (c3loop (cons next rhead)
                          (remove-nulls (remove-next next tails))))))))

         (values precedence-list ss))))))

;;;; Tests for C4 Linearization

;; Names starting with a lowercase letter are "suffix" specs (like Gerbil structs).
(define (test-struct? sym)
  (char-lower-case? (string-ref (symbol->string sym) 0)))

;; Test hierarchy (same as gerbil/c4/src/gerbil/test/c3-test.ss)
(define test-supers
  '((O)
    (A O) (B O) (C O) (D O) (E O)
    (K1 A B C) (K2 D B E) (K3 D A) (Z K1 K2 K3)
    (J1 C A B) (J2 B D E) (J3 A D) (Y J1 J3 J2)
    (DB B) (WB B) (EL DB) (SM DB) (PWB EL WB) (SC SM) (P PWB SC)
    (GL O) (HG GL) (VG GL) (HVG HG VG) (VHG VG HG)
    (HH) (GG HH) (II GG) (FF HH) (EE HH) (DD FF)
    (CC EE FF GG) (BB) (AA BB CC DD)
    (o O) (a o) (b a) (c b o) (d D c) (M A B b a) (N C c) (L M N) (k D L) (j E k A) (I N M)
    (x1) (x2 x1) (x3 x2) (x4 x3) (x5 x4 x1)
    (SBA) (SBB) (SBS SBA) (sBs SBA) (SBC SBS SBB)))

(define (test-get-supers x)
  (let ((p (assq x test-supers))) (if p (cdr p) '())))

;; Memoized precedence-list computation
(define pl-cache (make-eqht))
(define (compute-pl x)
  (let ((cached (eqht-ref pl-cache x #f)))
    (or cached
        (let-values (((pl _ss)
                      (c4-linearize (list x) (list (test-get-supers x))
                                    compute-pl test-struct? eq?)))
          (eqht-set! pl-cache x pl)
          pl))))

(define test-objects
  '(O A B C D E K1 K2 K3 Z J1 J2 J3 Y DB WB EL SM PWB SC P
    GL HG VG HVG VHG HH GG II FF EE DD CC BB AA
    o a b c d M N L k j I x1 x2 x3 x4 x5 SBA SBB SBS sBs SBC))

(define expected-pls
  '((O) (A O) (B O) (C O) (D O) (E O)
    (K1 A B C O) (K2 D B E O) (K3 D A O) (Z K1 K2 K3 D A B C E O)
    (J1 C A B O) (J2 B D E O) (J3 A D O) (Y J1 C J3 A J2 B D E O)
    (DB B O) (WB B O) (EL DB B O) (SM DB B O) (PWB EL DB WB B O) (SC SM DB B O)
    (P PWB EL SC SM DB WB B O)
    (GL O) (HG GL O) (VG GL O) (HVG HG VG GL O) (VHG VG HG GL O)
    (HH) (GG HH) (II GG HH) (FF HH) (EE HH) (DD FF HH)
    (CC EE FF GG HH) (BB) (AA BB CC EE DD FF GG HH)
    (o O) (a o O) (b a o O) (c b a o O) (d D c b a o O) (M A B b a o O)
    (N C c b a o O) (L M A B N C c b a o O) (k D L M A B N C c b a o O)
    (j E k D L M A B N C c b a o O) (I N C M A B c b a o O)
    (x1) (x2 x1) (x3 x2 x1) (x4 x3 x2 x1) (x5 x4 x3 x2 x1)
    (SBA) (SBB) (SBS SBA) (sBs SBA) (SBC SBS SBA SBB)))

(expect (map compute-pl test-objects) => expected-pls)

;; Spot-checks from c3-test.ss
(expect (compute-pl 'Z)  => '(Z K1 K2 K3 D A B C E O)
        (compute-pl 'Y)  => '(Y J1 C J3 A J2 B D E O)
        (compute-pl 'P)  => '(P PWB EL SC SM DB WB B O)
        (compute-pl 'AA) => '(AA BB CC EE DD FF GG HH)
        (compute-pl 'a)  => '(a o O))

;; CG has inconsistent inheritance (HVG and VHG contradict each other).
(let ()
  (define cg-supers
    (lambda (x) (if (eq? x 'CG) '(HVG VHG) (test-get-supers x))))
  (define cg-cache (make-eqht))
  (define (cg-pl x)
    (let ((cached (eqht-ref cg-cache x #f)))
      (or cached
          (let-values (((pl _) (c4-linearize (list x) (list (cg-supers x)) cg-pl test-struct? eq?)))
            (eqht-set! cg-cache x pl) pl))))
  (expect (cg-pl 'CG) =>fail!))

;; SBc has incompatible suffix parents (suffix constraint violation).
(let ()
  (define sbc-supers
    (lambda (x) (if (eq? x 'SBc) '(sBs SBB) (test-get-supers x))))
  (define sbc-cache (make-eqht))
  (define (sbc-pl x)
    (let ((cached (eqht-ref sbc-cache x #f)))
      (or cached
          (let-values (((pl _) (c4-linearize (list x) (list (sbc-supers x)) sbc-pl test-struct? eq?)))
            (eqht-set! sbc-cache x pl) pl))))
  (expect (sbc-pl 'SBc) =>fail!))

;; Test c4-linearize* with DAG local precedence order (list-of-lists parents).
;; Each sub-list is a totally-ordered chain; together they express a partial order DAG.
(let ()
  (define (my-c4* local-order)
    (let-values (((pl _) (c4-linearize '() local-order compute-pl test-struct? eq?)))
      pl))
  (expect
   (my-c4* '((A) (B) (C)))    => '(A B C O)   ;; three unordered singletons
   (my-c4* '((A B) (C A)))    => '(C A B O)   ;; C before A, A before B
   (my-c4* '((C A) (C B)))    => '(C A B O)   ;; C before both A and B
   (my-c4* '((C B) (C A)))    => '(C B A O)   ;; C before both, B before A
   (my-c4* '((A B) (B C) (C A))) =>fail!))    ;; cycle: A<B<C<A

;;;;; 7.4 Prototype with Optimal Inheritance (POI)

;; POI is a prototype in the style of rproto, the spec accessible via #f
;;   'mod-ext         -> ModExt             -- this spec's own modular extension
;;   'parents         -> List(List(POI))    -- local precedence chains of direct parents
;;   'suffix?         -> Bool               -- requires the suffix property (single-inh chain)
;;   'precedence-list -> List(POI)          -- linearized ancestors, most-specific first (lazy)
;;
;; parents is a list of totally-ordered chains, the same format as c4-linearize's parents:
;;   e.g. (list (list A B C)) for a single chain, (list (list A B) (list C A)) for a DAG.

(def (poi-spec poi) (poi #f))
(def (poi-target poi) poi)
(def (poi-id poi) (poi #f 'id))
(def (poi-precedence-list poi) (cons poi (poi #f 'precedence-list)))
(def (poi-suffix poi) (poi #f 'suffix))
(def (poi-mod-ext poi) (poi #f 'mod-ext))
(def (poi-suffix? poi) (poi #f 'suffix?))
(def (poi-parents poi) (poi #f 'parents))

(define (make-poi mod-ext suffix? parents)
  (letrec
      ((id (generate-tag))
       (precedence-list-and-suffix
        (compute-once (lambda () (call-with-values (lambda ()
          (c4-linearize '() parents poi-precedence-list poi-suffix? eq? poi-id)) cons))))
       (precedence-list (lambda () (car (precedence-list-and-suffix))))
       (suffix (lambda () (cdr (precedence-list-and-suffix))))
       (self (η (fix (record (#f spec))
                     (apply mix* finalize-spec
                            (reverse (cons mod-ext (map poi-mod-ext (precedence-list))))))))
       (spec
        (lambda (msg)
          (case msg
            ((target)          self)
            ((precedence-list) (precedence-list))
            ((suffix)          (suffix))
            ((id)              id)
            ((mod-ext)         mod-ext)
            ((suffix?)         suffix?)
            ((parents)         parents)
            (else #f)))))
    self))

(define-syntax poi
  (syntax-rules ()
    ((_ args ...) (poi-internal (args ...) idModExt #f '()))))
(define-syntax poi-internal
  (syntax-rules (:e :s :p :pp :p*)
    ((_ () mod-ext suffix? parents) (make-poi mod-ext suffix? parents))
    ((_ (:e e args ...) _ s p) (poi-internal (args ...) e s p))
    ((_ (:s s args ...) e _ p) (poi-internal (args ...) e s p))
    ((_ (:p p ...) e s _) (poi-internal () e s (list (list p ...))))
    ((_ (:pp pp ...) e s _) (poi-internal () e s (list pp ...)))
    ((_ (:p* p* args ...) e s _) (poi-internal (args ...) e s p*))))

;; memo-poi: poi that memoizes all method accesses
;; It is a suffix poi, because memoization must happen in the very beginning,
;; and thus the finalizer must be registered at the very end.
(def memo-poi
     (poi :e (register-finalizer-spec (λ (super _self) (memo super)))
          :s #t))

;;;; Tests for POI

;; Simple diamond: O <- A, O <- B, {A,B} <- Z
;; Note: compute-value lambdas must use pommette's λ (auto-curried), not plain lambda,
;; because def-bound parameters become identifier macros that expand
;; (compute-value inherited self) to ((compute-value inherited) self).
(let ()
  (define O (poi))
  (define A (poi :e (constant-field-spec 'a 1) :p O))
  (define B (poi :e (constant-field-spec 'b 2) :p O))
  (define Z (poi :e (constant-field-spec 'z 3) :p A B))

  ;; Precedence lists
  (expect
   (poi-precedence-list Z) => (list Z A B O)
   ;; Instantiate: all fields accessible, each ancestor contributes once
   (map Z '(z b a o)) => '(3 2 1 #f)))

;; Suffix (single-inheritance) chain: s <- C  where s is a suffix spec
(let ()
  (define s (poi :e (constant-field-spec 's 0) :s #t))
  (define C (poi :e (constant-field-spec 'C 99) :p s))

  ;; s-oisp is the last (least-specific) in C's PL, as required by the suffix property
  (expect
   (poi-precedence-list C) => (list C s)
   (C 'C) => 99
   (C 's) => 0))

;; Overriding: child adds 10 to parent's field
(let ()
  (define base (poi :e (constant-field-spec 'val 5)))
  (define child (poi :e (field-spec 'val (λ (inh _self) (+ inh 10))) :p base))
  (expect
   (base 'val) => 5
   (child 'val) => 15))   ;; child's +10 applied on top of base's 5

;;;; OISpec C4 hierarchy examples
;; The following tests replicate each major C4/C3 example hierarchy
;; but using OISpec instances instead of symbols.
;; We verify: (1) the precedence-list order matches the C4 expected result,
;;            (2) diamond ancestors appear exactly once,
;;            (3) for suffix hierarchies, the suffix property holds.

;; --- Wikipedia 2021: Z hierarchy ---
;; Classes: O, A B C D E O, K1=(A B C), K2=(D B E), K3=(D A), Z=(K1 K2 K3)
;; Expected PL: Z K1 K2 K3 D A B C E O
(let ()
  (define-syntax m (syntax-rules () ((_ . p) (poi :p . p))))
  (define O  (m))
  (define A  (m O))
  (define B  (m O))
  (define C  (m O))
  (define D  (m O))
  (define E  (m O))
  (define K1 (m A B C))
  (define K2 (m D B E))
  (define K3 (m D A))
  (define Z  (m K1 K2 K3))
  (expect
   (poi-precedence-list Z) => (list Z K1 K2 K3 D A B C E O)
   (poi-precedence-list K1) => (list K1 A B C O)
   (poi-precedence-list K2) => (list K2 D B E O)
   (poi-precedence-list K3) => (list K3 D A O)))

;; --- Wikipedia 2023: Y hierarchy ---
;; J1=(C A B), J2=(B D E), J3=(A D), Y=(J1 J3 J2)
;; Expected PL: Y J1 C J3 A J2 B D E O
(let ()
  (define-syntax m (syntax-rules () ((_ . p) (poi :p . p))))
  (define O  (m))
  (define A  (m O))
  (define B  (m O))
  (define C  (m O))
  (define D  (m O))
  (define E  (m O))
  (define J1 (m C A B))
  (define J2 (m B D E))
  (define J3 (m A D))
  (define Y (m J1 J3 J2))
  (expect (poi-precedence-list Y) => (list Y J1 C J3 A J2 B D E O)))

;; --- C3 paper: Boat hierarchy ---
;; boat(B), day-boat(DB=B), wheel-boat(WB=B), engine-less(EL=DB),
;; small-multihull(SM=DB), pedal-wheel-boat(PWB=EL WB),
;; small-catamaran(SC=SM), pedalo(P=PWB SC)
;; Expected PL: P PWB EL SC SM DB WB B O
(let ()
  (define-syntax m (syntax-rules () ((_ . p) (poi :p . p))))
  (define O   (m))
  (define B   (m O))
  (define DB  (m B))
  (define WB  (m B))
  (define EL  (m DB))
  (define SM  (m DB))
  (define PWB (m EL WB))
  (define SC  (m SM))
  (define P   (m PWB SC))
  (expect (poi-precedence-list P) => (list P PWB EL SC SM DB WB B O)))

;; --- C4 suffix hierarchy: lowercase = suffix (single-inheritance chain) ---
;; O, o=(O suffix), a=(o), b=(a), c=(b o), d=(D c) where D is a class
;; Expected PLs: o→(o O), a→(a o O), b→(b a o O), c→(c b a o O), d→(d D c b a o O)
(let ()
  (define-syntax m (syntax-rules () ((_ . p) (poi :p . p))))
  (define-syntax s (syntax-rules () ((_ . p) (poi :s #t :p . p))))
  (define O (m))
  (define D (m O))
  (define o (s O))
  (define a (s o))
  (define b (s a))
  (define c (s b o))
  (define d (s D c))
  (expect
   (poi-precedence-list o) => (list o O)
   (poi-precedence-list a) => (list a o O)
   (poi-precedence-list b) => (list b a o O)
   (poi-precedence-list c) => (list c b a o O)
   (poi-precedence-list d) => (list d D c b a o O)))

;; --- C4 regression: x5=(x4 x1) where x4=(x3), x3=(x2), x2=(x1), x1 base ---
;; Expected PL: x5 x4 x3 x2 x1
(let ()
  (define-syntax s (syntax-rules () ((_ . p) (poi :s #t :p . p))))
  (define x1 (s))
  (define x2 (s x1))
  (define x3 (s x2))
  (define x4 (s x3))
  (define x5 (s x4 x1))
  (expect
   (poi-precedence-list x5) => (list x5 x4 x3 x2 x1)))

;; --- Instantiation with C4 merged fields across the Z hierarchy ---
;; Each class contributes a unique field; Z's instance can access all of them.
(let ()
  (define-syntax x
    (syntax-rules ()
      ((x name . p) (define name (poi :e (constant-field-spec 'name 'name) :p . p)))))
  (x O)
  (x A O)
  (x B O)
  (x C O)
  (x D O)
  (x E O)
  (x K1 A B C)
  (x K2 D B E)
  (x K3 D A)
  (x Z K1 K2 K3)
  (expect
   (map Z '(Z K1 K2 K3 A B C D E O missing)) => '(Z K1 K2 K3 A B C D E O #f)))

;; --- Full test-objects/test-supers/expected-pls coverage ---
;; Build POI instances for all test objects using the same test vectors
;; already validated for c4-linearize directly.  test-objects is in topological
;; order (each object's supers appear earlier in the list), so parents always
;; exist in the alist when we create a child.
(let ()
  (define sym-poi-alist '())  ;; (sym . oisp) pairs, most-recently-added first
  (define (sym->poi sym)
    (let ((p (assq sym sym-poi-alist)))
      (if p (cdr p) (error "POI not found for symbol" sym))))

  ;; Reverse-lookup: OISpec -> symbol (for comparing PLs with expected-pls)
  (define (poi->sym poi)
    (let ((p (find (lambda (pair) (eq? (cdr pair) poi)) sym-poi-alist)))
      (if p (car p) (error "No symbol for POI" poi))))

  ;; Create one OISpec per test object
  (for-each
   (lambda (sym)
     (let* ((supers  (test-get-supers sym))
            ;; Single chain of direct parents (same as c4-linearize call-site above)
            (parents (if (null? supers) '() (list (map sym->poi supers))))
            (poi     (make-poi idModExt (test-struct? sym) parents)))
       (set! sym-poi-alist (cons (cons sym poi) sym-poi-alist))))
   test-objects)

  ;; Check every object's precedence-list matches the expected one
  (expect
   (map (lambda (sym) (map poi->sym (poi-precedence-list (sym->poi sym))))
        test-objects)
   => expected-pls))

;;;;; 9.3.2 Double Dispatch and Visitor Pattern

;;; Manual double dispatch (design pattern):
;;   shape1's collide! dispatches a type-specialized callback on shape2,
;;   passing shape1 as argument. Extensible only for the second argument—
;;   every spec for the first argument must be enumerated in the second's spec.

(let ()
  (def circle-dd
    (mix*
      (constant-field-spec 'radius 5)
      ;; First dispatch: call type-specialized method on second arg, passing self
      (field-spec 'collide! (λ (_inh self other)
                               (other 'collide-with-circle! self)))
      ;; Second-dispatch receivers: what to return when I am the second argument
      (field-spec 'collide-with-circle! (constant-spec (K 'circle-circle)))
      (field-spec 'collide-with-square! (constant-spec (K 'square-circle)))))

  (def square-dd
    (mix*
      (constant-field-spec 'side 4)
      (field-spec 'collide! (λ (_inh self other)
                               (other 'collide-with-square! self)))
      (field-spec 'collide-with-circle! (constant-spec (K 'circle-square)))
      (field-spec 'collide-with-square! (constant-spec (K 'square-square)))))

  (let ()
    (def c (fix-record circle-dd))
    (def s (fix-record square-dd))
    (expect
     (c 'collide! c) => 'circle-circle
     (c 'collide! s) => 'circle-square
     (s 'collide! c) => 'square-circle
     (s 'collide! s) => 'square-square)))

;;; Visitor pattern:
;;   Each shape acts both as an element (accept! dispatches to visitor's visit-MYTYPE!)
;;   and as a visitor (visit-X! handles what to do when colliding with a shape of type X).
;;   Advantage: new operations (visitors) can be added without modifying element specs.
;;   Limitation: still requires knowing all element types when defining each visitor.

(let ()
  (def circle-vis
    (mix*
      ;; As element: route visitor to visit-circle!
      (field-spec 'accept! (λ (_inh self visitor) (visitor 'visit-circle! self)))
      ;; As visitor: what to return when I collide with each shape type
      (field-spec 'visit-circle! (constant-spec (K 'circle-circle)))
      (field-spec 'visit-square! (constant-spec (K 'circle-square)))
      ;; collide! = let other accept self as a visitor
      (field-spec 'collide! (λ (_inh self other) (other 'accept! self)))))

  (def square-vis
    (mix*
      (field-spec 'accept! (λ (_inh self visitor) (visitor 'visit-square! self)))
      (field-spec 'visit-circle! (constant-spec (K 'square-circle)))
      (field-spec 'visit-square! (constant-spec (K 'square-square)))
      (field-spec 'collide! (λ (_inh self other) (other 'accept! self)))))

  (let ()
    (def c (fix-record circle-vis))
    (def s (fix-record square-vis))
    (expect
     (c 'collide! c) => 'circle-circle
     (c 'collide! s) => 'circle-square
     (s 'collide! c) => 'square-circle
     (s 'collide! s) => 'square-square)))

;;;;; 9.3.4 Implementing Multiple Dispatch
;;
;; Automate the double dispatch / visitor pattern:
;; store partial method tables locally in each spec, backward-compatible with single dispatch.
;;
;; Table structure (parallel to sub-method-spec):
;;   sub-methods[gf][s2-tag] = method-fn
;;
;; Each spec exposes a 'spec-tag for second-dispatch identification.
;; method-fn calling convention: (pommette λ (self) → (other) → result)
;;   i.e. curried — same arity as constant-spec, so (constant-spec v) works for constants.
;;
;; Dispatch:   (obj1 'gf obj2)
;;   1. look up obj2's tag:  (obj2 'spec-tag)
;;   2. look up method:      (sub-methods[gf])[s2-tag]
;;   3. apply:               (method obj1 obj2)

(def (curry/list f l)
  (let loop ((f f) (l l))
    (if (pair? l)
        (loop (f (car l)) (cdr l))
        f)))

(expect
 (curry/list + '()) => +
 (curry/list + '(4)) => 4
 (curry/list (λ (x y z) (+ x y z)) '(5 6 7)) => 18)

(def (uncurry/list arity k)
  (let loop ((n arity) (r '()))
    (if (zero? n) (k (reverse r))
        (λ (x) (loop (- n 1) (cons x r))))))

(expect
 (uncurry/list 0 vector) => '#(())
 (uncurry/list 1 vector 'a) => '#((a))
 (uncurry/list 2 vector 'a 'b) => '#((a b))
 (uncurry/list 3 vector 'a 'b 'c) => '#((a b c)))

(define (register-multimethod multimethods method-tag specializers method-fn)
  (@ (apply field-update~* method-tag specializers) (K method-fn) multimethods))

(def (register-multimethods new-multimethods multimethods)
  (foldl (lambda (n m) (apply register-multimethod m n)) multimethods new-multimethods))

(def (multimethods-spec new-multimethods super _self)
  ((compose* (field-update~* #f 'multimethods)
             (register-multimethods new-multimethods))
   super))

(def (apply-generic-function arity compute-effective-method multimethods self)
  (uncurry/list arity
    (λ (args)
      (let* ((pls (map poi-precedence-list args))
             (sub-methods
              (λ (method-tag)
               (map (λ (m cnm) (curry/list (m cnm)))
                ((let loop ((mm (multimethods method-tag))
                            (pls pls)
                            (acc identity))
                   (cond
                    ((not mm) acc)
                    ((null? pls) (compose acc (λ (x) (cons mm x))))
                    (else
                     (foldl
                      (lambda (p acc) (loop (field-view~ p mm) (cdr pls) acc))
                      acc
                      (car pls)))))
                 '())))))
        ((compute-effective-method self sub-methods args))))))

(def (generic-function-spec arity compute-effective-method multimethods)
  (λ (super self x)
    (if (not x)
        (extend-record 'arity arity
          (extend-record 'compute-effective-method compute-effective-method
            (extend-record 'multimethods (register-multimethods multimethods empty-record)
              (super #f))))
        (let* ((spec (self #f))
               (arity (spec 'arity))
               (compute-effective-method (spec 'compute-effective-method))
               (multimethods (spec 'multimethods)))
          (apply-generic-function arity compute-effective-method multimethods self x)))))

(let ()
  (def shape (poi))
  (def lozenge (poi :e (constant-field-spec 'type 'lozenge)
                    :p shape))
  (def rectangle (poi :e (constant-field-spec 'type 'rectangle)
                      :p shape))
  (def square (poi :e (constant-field-spec 'type 'square)
                   :p rectangle lozenge))
  (def known-ancestor-pairs
       (poi :e (generic-function-spec
                2 (K compute-effective-method/list)
                `((list (,shape ,shape) ,(constant-spec (K '(shape shape))))
                  (list (,rectangle ,shape) ,(constant-spec (K '(rectangle shape))))
                  (list (,shape ,rectangle) ,(constant-spec (K '(shape rectangle))))
                  (list (,lozenge ,shape) ,(constant-spec (K '(lozenge shape))))
                  (list (,lozenge ,lozenge) ,(constant-spec (K '(lozenge lozenge))))
                  (list (,shape ,lozenge) ,(constant-spec (K '(shape lozenge))))
                  (list (,rectangle ,rectangle) ,(constant-spec (K '(rectangle rectangle))))
                  (list (,square ,square) ,(constant-spec (K '(square square))))))))
  (expect
    (shape 'type) => #f
    (rectangle 'type) => 'rectangle
    (lozenge 'type) => 'lozenge
    (square 'type) => 'square

    (known-ancestor-pairs shape shape)
    => '((shape shape))

    ;; rectangle x rectangle:
    (known-ancestor-pairs rectangle rectangle)
    => '((rectangle rectangle) (rectangle shape) (shape rectangle) (shape shape))

    ;; lozenge x lozenge: lozenge-lozenge + shape-shape
    (known-ancestor-pairs lozenge lozenge)
    => '((lozenge lozenge) (lozenge shape) (shape lozenge) (shape shape))

    ;; square x square: square inherits rectangle and lozenge
    (known-ancestor-pairs square square)
    => '((square square)
         (rectangle rectangle) (rectangle shape)
         (lozenge lozenge) (lozenge shape)
         (shape rectangle) (shape lozenge) (shape shape))

    ;; rectangle x shape
    (known-ancestor-pairs rectangle shape)
    => '((rectangle shape) (shape shape))

    ;; square x rectangle
    (known-ancestor-pairs square rectangle)
    => '((rectangle rectangle) (rectangle shape)
         (lozenge shape) (shape rectangle) (shape shape))

    ;; square x shape: all applicable pairs
    (known-ancestor-pairs square shape)
    => '((rectangle shape) (lozenge shape) (shape shape))))

#||#
#|
The End. (For Now)
|#

#|
p1 = { a: Int , ...}
p2 = { b: String, ... }
p1∩p2 = {a : Int, b : String , ... }
|#

;; TODO: add compute-once where appropriate in method-combinations?
;; TODO: add call-next-method support to binary-gf-init-spec for composable multimethods
