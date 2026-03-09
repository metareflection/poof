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
        (error "Expected " check-expr " to evaluate to " good-expr
               " but instead got " actual " instead of " expected))))
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
(def rproto-spec-setter rproto←spec)
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
;;   MethodFn   = ? → ? → ?  (see MethodFn below; TODO: refine types)
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

;; MethodFn = a function (call-next-method self arg ...) → result
;;   where call-next-method : (() | new-arg ...) → result
;;         self : the current object (the fixpoint record)
;;         arg ...: the method's own arguments
;;
;; TODO: refine to ModExt-like triple ? → ? → ?
;;   Inherited ≈ CallNextMethod (the value from the super chain)
;;   Required  ≈ Self (the whole record, for reading)
;;   Provided  ≈ result

;; make-call-next-method : Next → Args → CallNextMethod
;;   Next = ...Args → Result  (the remaining chain)
;; When called with no args, forwards the original args to next.
;; When called with new-args, forwards them instead.
(def (make-call-next-method next args)
  (case-lambda
    (()       (apply next args))
    (new-args (apply next new-args))))

;; call-chain : List(MethodFn) → OnExhausted → Self → EffectiveMethod
;;   EffectiveMethod = ...Args → Result   (self already captured via closure)
;;   Each MethodFn m is called as (m call-next-method self arg ...).
(def (call-chain methods on-exhausted self)
  (foldr
    (lambda (m next)
      (λ args
        (apply ((m (make-call-next-method next args)) self) args)))
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

;; standard-compute-effective-method : MethodId → PerMethodSubs → Self → EffectiveMethod
;;   PerMethodSubs = record {before: List(MethodFn), after: ..., around: ..., primary: ...}
(def (standard-compute-effective-method method-id per-method-subs self)
  (call-chain (per-method-subs 'around)
    (λ args
      (progn-methods-most-specific-first (per-method-subs 'before) self args)
      (let ((result
              (apply (call-chain (per-method-subs 'primary)
                       (λ args (apply no-applicable-method method-id args))
                       self)
                     args)))
        (progn-methods-most-specific-last (per-method-subs 'after) self args)
        result))
    self))

;; standard-method-init-spec : MethodId → ModExt
;; Initializes method-id to use the standard method combination.
;; The stored value is an EffectiveMethod; self is captured from the field-spec closure.
(def (standard-method-init-spec method-id)
  (mix
    (field-spec method-id
       (λ (_inherited self)
          (let ((subs (self 'sub-methods)))
            (standard-compute-effective-method method-id (subs method-id) self))))
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
;;   Name → Stop? → Op0 → Op1 → Op2 → Order → PerMethodSubs → Self → EffectiveMethod
;;   Name  = Symbol  (tag for the sub-method list)
;;   Stop? = Result → Bool    (short-circuit: stop folding when true)
;;   Op0   = #f → Result      (result when no methods; takes dummy arg)
;;   Op1   = Result → Acc     (transforms first method result into initial accumulator)
;;   Op2   = Result → Acc → Acc  (fold step: combine next result with accumulator)
;;   Order = 'most-specific-first | 'most-specific-last
(def (simple-compute-effective-method
       name stop? op0 op1 op2 order per-method-subs self)
  (let* ((arounds (per-method-subs 'around))
         (methods (per-method-subs name))
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

(def (compute-effective-method/progn per-method-subs self)
  (simple-compute-effective-method
    'progn (λ (_) #f) (λ (_) #f) (λ (x) x) (λ (r _) r)
    'most-specific-first per-method-subs self))

(def (compute-effective-method/and per-method-subs self)
  (simple-compute-effective-method
    'and not (λ (_) #t) (λ (x) x) (λ (r _) r)
    'most-specific-first per-method-subs self))

(def (compute-effective-method/+ per-method-subs self)
  (simple-compute-effective-method
    '+ (λ (_) #f) (λ (_) 0) (λ (x) x) (λ (x y) (+ x y))
    'most-specific-first per-method-subs self))

(def (compute-effective-method/* per-method-subs self)
  (simple-compute-effective-method
    '* (λ (_) #f) (λ (_) 1) (λ (x) x) (λ (x y) (* x y))
    'most-specific-first per-method-subs self))

(def (compute-effective-method/list per-method-subs self)
  (simple-compute-effective-method
    'list (λ (_) #f) (λ (_) '()) (λ (x) (list x)) (λ (x y) (cons x y))
    'most-specific-last per-method-subs self))

;; list-method-init-spec : MethodId → ModExt
;; Initializes method-id to collect contributions from all methods into a list.
;; Most-specific method's contribution appears first in the result list.
(def (list-method-init-spec method-id)
  (mix
    (field-spec method-id
       (λ (_inherited self)
          (let ((subs (self 'sub-methods)))
            (compute-effective-method/list (subs method-id) self))))
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
            (let ((subs (self 'sub-methods)))
              (compute-effective-method/+ (subs 'total) self))))
        (method-combination-init-spec 'total (simple-method-combination-init '+)))
      (standard-sub-method-spec '+ 'total (constant-spec 3))
      (standard-sub-method-spec '+ 'total (constant-spec 4)))))

(expect ((sum-obj 'total)) => 7)

#||#
#|
The End. (For Now)
|#

#|
p1 = { a: Int , ...}
p2 = { b: String, ... }
p1∩p2 = {a : Int, b : String , ... }
|#

;; TODO: AVL trees as example OO for 5.x ? Also versions without OO for it and other trees (plain binary, randomized binary, 2-3 tree, etc.)
;; TODO: double dispatch example for 9.3.2
;; TODO: visitor pattern example for 9.3.2
