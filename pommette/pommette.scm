;;;;;; pommette - a cheeky, barebones implementation of a meta-object protocol.
;;; Mini object systems written in Scheme as demonstration for LtUO.
;;;
;;; The entire purpose of pommette is CLARITY: to make it obviously clear what OO *means*.
;;; Educational value is what we seek, and simplicity is an important tool,
;;; with the caveat that it is a special kind of simplicity: not in terms of bytes, but of
;;; understandability to humans (and AIs, too, but second to humans, since
;;; AIs are already trained to understand what humans can, whereas not the other way around).
;;; Good naming matters, pleasant indentation, etc.
;;;
;;; Anything that gets in the way of clarity must be corrected or eliminated.
;;; If anything could be made clearer—please show me what and how.
#|
Working:
With Gerbil Scheme: gxi pommette.scm
With Chez Scheme: chezscheme pommette-chez.scm
With Racket: racket pommette.rkt

Not working:
With Gambit Scheme: gsi -:s pommette.scm
|#
#|
   Semicolons are comments to the end of line.
   Blocks between #| ... |# are multi-line comments.
|#

(cond-expand
 (gambit
  (load "~~/syntax-case")) ;; sadly, Gambit's syntax-case can't handle the stuff below :-(
 (gerbil
  (import :std/debug/DBG))
 (else
  #f))

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
       ((or chezscheme racket gambit)
        (define-syntax (name stx)
          (syntax-case stx ()
            ((_ . x) #'(list-expr . x))
            (_ #'symbol-expr))))
       ((or gerbil)
        (define-syntax name
          (syntax-rules ()
            ((_ . x) (list-expr . x))
            (_ symbol-expr))))))
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
    ((_ (pat) . body)
     (define pat (λ () . body)))
    ((_ (pat . vars) . body)
     (def pat (λ vars . body)))
    ((_ v . body)
     (begin
       ;; Allow autocurrying self-reference in the definition body
       ;; (name) with 0 args calls tmp; (name a ...) curries via @
       (define tmp (let () . body))
       (define-syntax @tmp (syntax-rules () ((_) (tmp)) ((_ . a) (@ tmp . a))))
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
           (chezscheme
            (guard (e (#t #t)) (thunk) #f))
           (gambit
            (with-exception-catcher (lambda (_) #t) (lambda () (thunk) #f)))
           (gerbil
            (with-catch true (lambda () (thunk) #f)))
           (guile
            (catch #t (lambda () (thunk) #f) (lambda args #t)))
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

;; curry/list : apply a curried function to a list of args, one at a time (companion to @).
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

;; Note that 'x is a constant expression returning the symbol x,
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

;; n-ary from a binary monoid op. NAME/list folds a list right-associatively (empty ⇒ id,
;; singleton ⇒ its element); NAME* is the varargs spelling — (define (NAME* . a) (NAME/list
;; a)). Same convention across the file's `*` operators (compose*, mix*, compose-lens*,
;; field-lens*, …). The `go` recursion lives here once (a letrec, not the auto-curry `def`).
(define (op/list←op2 op2 id)
  (letrec ((go (lambda (l)
                 (cond ((null? l) id)
                       ((null? (cdr l)) (car l))
                       (else (op2 (car l) (go (cdr l))))))))
    go))
(define (op*←op2 op2 id)
  (let ((go (op/list←op2 op2 id)))
    (lambda args (go args))))
;; …and for a curried operator that takes one argument then the next.
(define (op/list←op1.1 op1.1 id)
  (op/list←op2 (lambda (x y) (@ op1.1 x y)) id))
(define (op*←op1.1 op1.1 id)
  (op*←op2 (lambda (x y) (@ op1.1 x y)) id))

;; Generalizing compose to n-ary composition — an instance of the above.
(def compose/list (op/list←op1.1 compose identity))
(define (compose* . args) (compose/list args))

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

;; Memoizing variant of eta-conversion
(define-syntax η₁ (syntax-rules () ((_ f) (let ((df (delay f))) (λ (x) ((force df) x))))))

;; As a warm up, S K I combinators (that can also be useful later)
(def (S x y z) (x z (y z)))
(def (K x _y) x)
(def (I x) x)

;; B combinator, composition
;; a.k.a. Z for Schönfinkel (Zusammensetzungsfunktion) (compoZition function)
;; : (Y→X)→(Z→Y)→Z→X
(def (B x y z)
  (x (y z)))

;; Ue: U, eager -- self-application combinator, η-expanded U for use with eager Y
;; a.k.a. duplication combinator Δ \Delta, or ω, half of Ω = (ω ω)
;; "Half of Y"
;; https://en.wikipedia.org/wiki/SKI_combinator_calculus
;; https://www.tfeb.org/fragments/2020/03/09/the-u-combinator/
;; same as (def (Ue x) (η (x x)))
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
    (lambda _
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

(define (once! x) (x)) ;; force, even if from def or λ

;; Y, once; B, once; U, once; Y, once with combinators; Y, once, expanded
;; direct adaptations of Yl Bl Ul Ylc Ylx to once and once! instead of delay and force.
(def (Yo f) ;; : ^(^X→X)→X
  (letrec ((p (f (once p)))) p))
(def (Bo x y z) ;; : (^Y→X)→^(Z→Y)→Z→X
  (x (once ((once! y) z))))
(def (Uo x) ;; : µX.^(X→A)→A
  ((once! x) x))
(def (Yoc f) ;; : ^(^X→X)→X
  (Uo (once (Bo f (once Uo)))))
;; : ^(^X→X)→X
(def (Yox f) ((lambda (x) ((x) x)) (once (λ (x) (f (once ((once! x) x)))))))

(def once-pre-fact (λ (f n)
  (if (<= n 1) n (* n ((once! f) (- n 1))))))

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

(def mix/list (op/list←op1.1 mix idModExt))
(define (mix* . args) (mix/list args))

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
;;; (For a NESTED key path see field-spec~* / field-spec~/list in §9.1.2, built on the
;;; field-update~* lens family.)
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

(def my-binary-dict
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
  (def (left t)    (car t))
  (def (kv t)      (caadr t))
  (def (height t)  (if (null? t) 0 (cdadr t)))
  (def (right t)   (caddr t))
  (def (balance t) (if (null? t) 0 (- (height (right t)) (height (left t)))))
  (def (mk l ckv r)
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

(def my-avl-dict
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
(def my-hybrid-dict
  (foldl (lambda (kv t) (alist+avl-map 'acons (car kv) (cdr kv) t))
         (alist+avl-map 'empty)
         '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5)
           (f . 6) (g . 7) (h . 8) (i . 9) (j . 10))))

(expect
  (alist+avl-map 'ref my-hybrid-dict 'a (lambda () #f)) => 1   ;; in AVL part
  (alist+avl-map 'ref my-hybrid-dict 'j (lambda () #f)) => 10  ;; in alist
  (alist+avl-map 'ref my-hybrid-dict 'z (lambda () #f)) => #f) ;; absent

;; afoldr collects all 10 entries; alist values shadow any stale AVL entries.
(def my-hybrid-alist
  (alist+avl-map 'afoldr (λ (k v acc) (cons (cons k v) acc))
                 '() my-hybrid-dict))

(expect
  (length my-hybrid-alist)   => 10
  (assoc 'a my-hybrid-alist) => '(a . 1)
  (assoc 'j my-hybrid-alist) => '(j . 10))

;;;;; 6 Rebuilding OO from its Minimal Core

;;;; 6.1.2 Conflation: Crouching Typecast, Hidden Product

;; Same as (cons spec (fix-record spec)) but with a record shape
(def (pproto←spec spec)
  (cons spec (fix-record spec)))
(def spec←pproto car)
(def target←pproto cdr)
(def pproto-id (pproto←spec idModExt))
(def (pproto-mix parent child)
  (pproto←spec (mix (spec←pproto parent) (spec←pproto child))))
(def pproto-mix/list (op/list←op1.1 pproto-mix pproto-id))
(define (pproto-mix* . args) (pproto-mix/list args))

(def coord-pproto (pproto←spec coord-spec))
(def color-pproto (pproto←spec color-spec))
(def point-p-pproto (pproto-mix coord-pproto color-pproto))

(expect (map (target←pproto coord-pproto) '(x y z color)) => '(2 4 #f #f)
        (map (target←pproto color-pproto) '(x y z color)) => '(#f #f #f "blue")
        (map (target←pproto point-p-pproto) '(x y z color)) => '(2 4 #f "blue"))

(def (add-x-pproto dx) (pproto←spec (add-x-spec dx)))
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

;;; Trivial prototype conflation, as record of spec and target
(def (conflate spec target)
  (extend-record 'spec spec
    (extend-record 'target target empty-record)))
(def (get-spec tp) (tp 'spec))
(def (get-target tp) (tp 'target))

(def (qproto-wrapper spec super _self)
  (conflate spec super))
(def (qproto←spec spec)
  (fix-record (mix spec (qproto-wrapper spec))))
(def spec←qproto get-spec)
(def target←qproto get-target)
(def qproto-id (qproto←spec idModExt))
(def (qproto-mix parent child)
  (qproto←spec (mix (spec←qproto parent) (spec←qproto child))))
(def qproto-mix/list (op/list←op1.1 qproto-mix qproto-id))
(define qproto-mix* (op*←op1.1 qproto-mix qproto-id))

(def coord-qproto (qproto←spec coord-spec))
(def color-qproto (qproto←spec color-spec))
(def point-p-qproto (qproto-mix coord-qproto color-qproto))
(def (add-x-qproto dx) (qproto←spec (add-x-spec dx)))
(def area-qproto (qproto←spec (λ (super self) (area-spec super (η (get-target self))))))
(def rho-qproto (qproto←spec (λ (super self) (rho-spec super (η (get-target self))))))
(def point-q-qproto (qproto-mix area-qproto coord-qproto))
(def point-r-qproto (qproto-mix* rho-qproto coord-qproto (add-x-qproto 1)))
(def point-rc-qproto (qproto-mix* point-r-qproto color-qproto))

(expect (map (target←qproto coord-qproto) '(x y z color)) => '(2 4 #f #f)
        (map (target←qproto color-qproto) '(x y z color)) => '(#f #f #f "blue")
        (map (target←qproto point-p-qproto) '(x y z color)) => '(2 4 #f "blue")
        (map (target←qproto point-q-qproto) '(x y area color)) => '(2 4 8 #f)
        (map (target←qproto (qproto-mix area-qproto coord-qproto)) '(x y area rho color)) => '(2 4 8 #f #f)
        (map (target←qproto point-r-qproto) '(x y rho color)) => '(3 4 5 #f)
        (map (target←qproto point-rc-qproto) '(x y rho color)) => '(3 4 5 "blue"))

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
(def rproto-mix/list (op/list←op1.1 rproto-mix rproto-id))
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

;;;; 6.1.5 Conflation from U-encoding

;;; HPROTO encoding
;;; (pass half before method-id, not after as in YASOS
;;; also take a late-bound hyper/htop for mixin semantics)

;; Reversed (left-associative) fold — rop/list←op2 consumes a list, rop*←op2 is varargs.
(define (rop/list←op2 op2 id)
  (lambda (l)
    (if (null? l) id
        (let loop ((acc (car l)) (rest (cdr l)))
          (if (null? rest) acc (loop (op2 acc (car rest)) (cdr rest)))))))
(define (rop*←op2 op2 id)
  (let ((go (rop/list←op2 op2 id)))
    (lambda args (go args))))
;; Variants of rop/list←op2 / rop*←op2 for a curried operator that takes one arg then the next.
(define rop/list←op1.1 (lambda (op1.1 id)
  (rop/list←op2 (lambda (x y) (@ op1.1 x y)) id)))
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
(def hspec-rmix/list (rop/list←op1.1 hspec-rmix id-hspec))
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


;; Wrappers to a Y-style spec from a U-style hspec, and back.
;; hspec→spec threads the Y-style `super`/`self` straight into the hspec's
;; `hyper`/`half` slots (each ignores the late-bound half it is handed and yields
;; the Y-style value): the parent record for `hyper`, the whole object for `half`.
;; This is the mirror image of spec→hspec below. Correct at instantiation; a
;; converted hspec used as a non-most-specific mixin in a further chain may still
;; misbehave (its `hyper` fallthrough only reaches `super`, never a later spec).
(def (hspec→spec hspec super self)
  (hspec (λ (_half) super) (λ (_half) self)))
(expect (map (fix-record (hspec→spec (hspec-rmix* coord-hspec color-hspec (add-x-hspec 1) area-hspec)))
             '(x y z color area)) => '(3 4 #f "blue" 12))

(def (spec→hspec spec hyper half)
  ;; eta-conversions necessary in eager context
  (letrec ((self (η (half half))) ;; (λ (x) (half half x))
           (super (η (hyper half)))) ;; (λ (x) (hyper half x))
    (spec super self)))

(def u-comp (spec→hspec (mix* coord-spec area-spec (add-x-spec 1) color-spec)))

(expect (map (half-ref (hspec-half-record u-comp)) '(x y z color area)) => '(3 4 #f "blue" 12))

;;; More tests for the two converters under non-trivial inheritance.
;;; Naming: `super`/`self` are the Y-style channels (inherited value / whole object);
;;; `hyper`/`half` are their U-style counterparts. The point is to exercise both
;;; channels across a conversion boundary, and mixed-style chains where one mixin
;;; is a converted spec sitting among native ones (and vice versa).

;; hspec→spec at instantiation: an hspec chain that uses `hyper` (add-x-hspec chains
;; on the inherited x) AND `half` (area-hspec reads the fixed x,y), converted to a
;; Y-spec and closed with fix-record.
(expect (map (fix-record
              (hspec→spec (hspec-rmix* coord-hspec (add-x-hspec 1) area-hspec)))
             '(x y area color))
        => '(3 4 12 #f))

;; spec→hspec at instantiation: a Y-spec chain that uses `super` (add-x-spec) AND
;; `self` (area-spec, rho-spec), converted to a U-hspec and closed the U way.
(expect (map (half-ref (hspec-half-record
              (spec→hspec (mix* coord-spec (add-x-spec 1) area-spec rho-spec))))
             '(x y area rho color))
        => '(3 4 12 5 #f))

;; Round-trips are identity, with both inheritance channels in play.
(expect (map (fix-record
              (hspec→spec (spec→hspec
               (mix* coord-spec (add-x-spec 1) area-spec rho-spec))))
             '(x y area rho color))
        => '(3 4 12 5 #f))
(expect (map (half-ref (hspec-half-record
              (spec→hspec (hspec→spec
               (hspec-rmix* coord-hspec (add-x-hspec 1) area-hspec)))))
             '(x y area color))
        => '(3 4 12 #f))

;; Mixed chain, Y-style `mix*` with a converted hspec in the middle:
;; the hspec's `hyper` reads the Y-sibling to its left (x=10), and the Y-spec to
;; its right (add-x-spec 1) then chains on the hspec's result. 10 +5 +1 = 16.
(expect ((fix-record (mix* (constant-field-spec 'x 10)
                           (hspec→spec (add-x-hspec 5))
                           (add-x-spec 1)))
         'x)
        => 16)

;; Same, but the converted hspec uses `half`: area-hspec reads x,y contributed by
;; native Y-specs (no field conflict with the more-specific color-spec).
(expect (map (fix-record (mix* coord-spec (hspec→spec area-hspec) color-spec))
             '(x area color))
        => '(2 8 "blue"))

;; Mixed chain, U-style `hspec-rmix*` with a converted Y-spec:
;; area-spec's `self` becomes the full U-fixpoint, so it reads x,y from the
;; native hspecs regardless of whether it is the least- or most-specific mixin.
(expect ((half-ref (hspec-half-record
          (hspec-rmix* (spec→hspec area-spec)
                       (constant-field-hspec 'x 3)
                       (constant-field-hspec 'y 7))))
         'area)
        => 21)
(expect ((half-ref (hspec-half-record
          (hspec-rmix* (constant-field-hspec 'x 3)
                       (constant-field-hspec 'y 7)
                       (spec→hspec area-spec))))
         'area)
        => 21)

;; The converted spec's `self` sees a U-side override applied AFTER it:
;; (mix* coord-spec area-spec) is converted, then x is overridden to 5 by a
;; more-specific native hspec; area recomputes as 5*4 = 20.
(expect ((half-ref (hspec-half-record
          (hspec-rmix* (spec→hspec (mix* coord-spec area-spec))
                       (constant-field-hspec 'x 5))))
         'area)
        => 20)

;; A native hspec's `hyper` reading a converted-spec parent: 9 +4 = 13.
(expect ((half-ref (hspec-half-record
          (hspec-rmix* (spec→hspec (constant-field-spec 'x 9))
                       (add-x-hspec 4))))
         'x)
        => 13)

;; Deep: Y-style `mix*` whose middle mixin is a converted U-chain (native `hyper`
;; and `half`), flanked by Y-specs that use `super`/`self`. All four channels in
;; one pipeline: x = 2 +3 = 5, y = 4, area = half.x*half.y = 20, rho = |(5,4)|.
(expect (map (fix-record
              (mix* coord-spec
                    (hspec→spec (hspec-rmix* (add-x-hspec 3) area-hspec))
                    rho-spec))
             '(x y area))
        => '(5 4 20))
(expect ((fix-record
          (mix* coord-spec
                (hspec→spec (hspec-rmix* (add-x-hspec 3) area-hspec))
                rho-spec))
         'rho)
        => (sqrt 41))

;; A converted hspec that is NOT the most-specific mixin still resolves its
;; `half`/self-references against the final object: `x` is overridden to 6 by a
;; more-specific Y-spec to the right, and area-hspec (which reads x,y via `half`)
;; recomputes 6*4 = 24. Works because hspec→spec threads its `self` argument into
;; the `half` slot; it used to drop `self` and crash here on (* #f #f).
(expect ((fix-record (mix* (hspec→spec area-hspec)
                           coord-spec
                           (constant-field-spec 'x 6)))
         'area)
        => 24)


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

;;;; 7.4.4. The C4 Linearization Algorithm

;;;;; C4 Linearization: Multiple Inheritance with Suffix Support
;;;; Ported from gerbil/src/gerbil/runtime/c3.ss
;;;; See gerbil/doc/reference/gerbil/runtime/c3.md for the theory.
;;;; See gerbil/src/gerbil/test/c3-test.ss for the tests.

;;;; Portable hash tables (using eq? as the key equality predicate)

(cond-expand
  ((or gerbil gambit)
   (begin
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
;; NB: For ease of porting, C4 and the utilities it relies on are written in plain Scheme
;; rather than the autocurry dialect I've been using
;; for the conceptual exploration of OO in "functional programming" style.

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

;; filter-map: map f over lst, keeping only truthy results (not in R7RS-small).
(define (filter-map f lst)
  (let loop ((l lst) (acc '()))
    (if (null? l)
        (reverse acc)
        (let ((r (f (car l))))
          (loop (cdr l) (if r (cons r acc) acc))))))

;; c4-linearize head parents get-precedence-list suffix? [eq [get-name]]
;;   → (cons precedence-list super-suffix-or-#f)
;;
;; Compute the precedence list for a specification.
;;   head               - prefix list to prepend (typically (list x) or '())
;;   parents            - list of totally-ordered parent chains (each chain is a list);
;;                        supports an arbitrary DAG for the local precedence order,
;;                        e.g. '((A B C)) for a single chain, or '((A B) (C A)) for a DAG.
;;   get-precedence-list - procedure: x → its full precedence list (incl. x at front)
;;   suffix?            - predicate: is x a "suffix" (single-inh struct)?
;;   eq                 - equality on specs (optional, default: equal?)
;;   get-name           - name extractor for error messages (optional, default: identity)
;;
;; Returns a pair of:
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
       (cons head #f))

      ;; 1 parent-list with 1 parent: single inheritance
      ((and (null? (cdr parents)) (null? (cdar parents)))
       (let* ((parent (caar parents))
              (pl (get-precedence-list parent)))
         (cons (append head pl)
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

         ;; Build suffix-tail-index: element → position.
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

         (cons precedence-list ss))))))

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
        (let ((pl (car (c4-linearize (list x) (list (test-get-supers x))
                                      compute-pl test-struct? eq?))))
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
          (let ((pl (car (c4-linearize (list x) (list (cg-supers x)) cg-pl test-struct? eq?))))
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
          (let ((pl (car (c4-linearize (list x) (list (sbc-supers x)) sbc-pl test-struct? eq?))))
            (eqht-set! sbc-cache x pl) pl))))
  (expect (sbc-pl 'SBc) =>fail!))

;; Test c4-linearize* with DAG local precedence order (list-of-lists parents).
;; Each sub-list is a totally-ordered chain; together they express a partial order DAG.
(let ()
  (define (my-c4* local-order)
    (let ((pl (car (c4-linearize '() local-order compute-pl test-struct? eq?))))
      pl))
  (expect
   (my-c4* '((A) (B) (C)))    => '(A B C O)   ;; three unordered singletons
   (my-c4* '((A B) (C A)))    => '(C A B O)   ;; C before A, A before B
   (my-c4* '((C A) (C B)))    => '(C A B O)   ;; C before both A and B
   (my-c4* '((C B) (C A)))    => '(C B A O)   ;; C before both, B before A
   (my-c4* '((A B) (B C) (C A))) =>fail!))    ;; cycle: A<B<C<A

;;;; 7.4.6 Prototypes with Optimal Inheritance (POI)

;; POI is a prototype in the style of rproto, the spec accessible via #f
;;   'mod-ext         → ModExt             -- this spec's own modular extension
;;   'parents         → List(List(POI))    -- local precedence chains of direct parents
;;   'suffix?         → Bool               -- requires the suffix property (single-inh chain)
;;   'precedence-list → List(POI)          -- linearized ancestors, most-specific first (lazy)
;;
;; parents is a list of totally-ordered chains, the same format as c4-linearize's parents:
;;   e.g. (list (list A B C)) for a single chain, (list (list A B) (list C A)) for a DAG.

(def (poi-spec poi) (poi #f))
(def (poi-name poi) (poi-spec poi 'name))
(def (poi-precedence-list poi) (poi-spec poi 'precedence-list))
(def (poi-suffix poi) (poi-spec poi 'suffix))
(def (poi-mod-ext poi) (poi-spec poi 'mod-ext))
(def (poi-suffix? poi) (poi-spec poi 'suffix?))
(def (poi-parents poi) (poi-spec poi 'parents))

(def (make-poi name mod-ext suffix? parents)
  (letrec
      ((precedence-list-and-suffix*
        (delay (c4-linearize '() parents
                             poi-precedence-list
                             poi-suffix? eq? poi-name)))
       (pre-precedence-list* (delay (car (force precedence-list-and-suffix*))))
       (precedence-list* (delay (cons self (force pre-precedence-list*))))
       (suffix* (delay (cdr (force precedence-list-and-suffix*))))
       (effective-mod-ext* (delay (apply mix*
                                    (reverse
                                     (cons mod-ext
                                           (map poi-mod-ext (force pre-precedence-list*)))))))
       (spec
        (lambda (msg)
          (case msg
            ((name)            name)
            ((precedence-list) (force precedence-list*))
            ((suffix)          (force suffix*))
            ((mod-ext)         mod-ext)
            ((suffix?)         suffix?)
            ((parents)         parents)
            (else #f))))
       (self (η₁ (fix (record (#f spec)) (force effective-mod-ext*)))))
    self))

#;(begin (for-each (lambda (x y) (display x) (display ": ") (display y) (newline))
          '(poi-spec poi-precedence-list poi-suffix poi-mod-ext poi-suffix? poi-parents make-poi)
          (list poi-spec poi-precedence-list poi-suffix poi-mod-ext poi-suffix? poi-parents make-poi))
         (trace poi-spec poi-precedence-list poi-suffix poi-mod-ext poi-suffix? poi-parents make-poi))

(def (poi←record r)
  (make-poi #f (constant-spec r) #f '()))
(def (record←poi p)
  (extend-record #f #f p))

;; poi-mix/list : List(POI) → POI — a fresh anonymous POI inheriting from the given POIs
;;   as independent singleton parent chains; C4 merges them.
;;   Its own mod-ext is idModExt, so it only finalizes what the parents contribute.
;;   ARGUMENT ORDER: most-specific first, matching `:p` / defpoi / poi-precedence-list —
;;   this is the opposite of mix, whose arguments parent child are most-specific last.
;;   poi-mix* is the varargs spelling, poi-mix the binary one.
;;   poi-mix-maybe is the variant that considers #f as a neutral element.
(def (poi-mix/list pois) (make-poi #f idModExt #f (map list pois)))
(define (poi-mix* . pois) (poi-mix/list pois))
(def (poi-mix a b) (poi-mix* a b))
(def (poi-mix-maybe child parent)
  (cond
   ((not parent) child)
   ((not child) parent)
   (else (poi-mix child parent))))

;; poi-mix-spec : POI → (inherited-poi _self → poi) — given a poi, a ModExt
;;   that extends an inherited poi with the given poi:
;;   The C4-ordered mix* of ancestor mod-exts then chains these into one linearization, so a
;;   diamond's (RichGraph 'Node) resolves its methods exactly as the outer hierarchy does.
;;   Memoised by the inherited poi (the only thing the result depends on), so
;;   repeated field access — (ColorGraph 'Node) twice — yields the SAME object.
(def (poi-mix-spec contrib)
  (let ((combine (memo (lambda (inherited) (poi-mix-maybe contrib inherited)))))
    (λ (inherited _self) (combine inherited))))

;;; Prototype Target Update options (what to do when updating the target of a poi)
(def (poi-target-update/OutOfSync u poi) ;; just update fields, spec no longer matches
  (u poi))
(def (poi-target-update/OverwriteSpec u poi) ;; replace spec with constant-spec of current state
  (poi←record (u poi)))
(def (poi-target-update/NoMoreSpec u poi) ;; erase the magic spec field, no longer extensible
  (u (record←poi poi)))
(def (poi-target-update/Error u poi) ;; signal an error — safest default
  (abort "cannot update a poi target"))

(define-syntax poi
  (syntax-rules ()
    ((_ args ...) (poi-internal #f idModExt #f '() args ...))))
(define-syntax poi-internal
  (syntax-rules (:n :e :s :p :pp :p*)
    ((_ name mod-ext suffix? parents) (make-poi name mod-ext suffix? parents))
    ((_ _ e s p :n n args ...) (poi-internal n e s p args ...))
    ((_ n _ s p :e e args ...) (poi-internal n e s p args ...))
    ((_ n e _ p :s s args ...) (poi-internal n e s p args ...))
    ((_ n e s _ :p* p* args ...) (poi-internal n e s p* args ...))
    ;; NB: :p and :pp must be used as LAST keywords, because they eat the rest of the argument list
    ((_ n e s _ :p p ...) (poi-internal n e s (list (list p ...))))
    ((_ n e s _ :pp pp ...) (poi-internal n e s (list pp ...)))))
(def (struct-name? s)
  (>= (char->integer (string-ref (symbol->string s) 0)) 96))
(define-syntax defpoi
  (syntax-rules ()
    ((_ name args ...) (def name (poi :n 'name :s (struct-name? 'name) args ...)))))

;; memo-poi: poi that memoizes all method accesses
;; It is a suffix poi, because memoization must happen in the very beginning,
;; and thus the finalizer must be registered at the very end.
(defpoi memo-poi :e (register-finalizer-spec (λ (super _self) (memo super))))

;;;; Tests for POI

;; Simple diamond: O <- A, O <- B, {A,B} <- Z
;; Note: compute-value lambdas must use pommette's λ (auto-curried), not plain lambda,
;; because def-bound parameters become identifier macros that expand
;; (compute-value inherited self) to ((compute-value inherited) self).
(let ()
  (defpoi O)
  (defpoi A :e (constant-field-spec 'a 1) :p O)
  (defpoi B :e (constant-field-spec 'b 2) :p O)
  (defpoi Z :e (constant-field-spec 'z 3) :p A B)

  ;; Precedence lists
  (expect
   (poi-precedence-list Z) => (list Z A B O)
   ;; Instantiate: all fields accessible, each ancestor contributes once
   (map Z '(z b a o)) => '(3 2 1 #f)))

;; Suffix (single-inheritance) chain: s <- C  where s is a suffix spec
(let ()
  (defpoi s :e (constant-field-spec 's 0) :s #t)
  (defpoi C :e (constant-field-spec 'C 99) :p s)

  ;; s-oisp is the last (least-specific) in C's PL, as required by the suffix property
  (expect
   (poi-precedence-list C) => (list C s)
   (C 'C) => 99
   (C 's) => 0))

;; Overriding: child adds 10 to parent's field
(let ()
  (defpoi base :e (constant-field-spec 'val 5))
  (defpoi child :e (field-spec 'val (λ (inh _self) (+ inh 10))) :p base)
  (expect
   (base 'val) => 5
   (child 'val) => 15))   ;; child's +10 applied on top of base's 5

;;;; OISpec C4 hierarchy examples
;; The following tests replicate each major C4/C3 example hierarchy
;; but using OISpec instances instead of symbols.
;; We verify: (1) the precedence-list order matches the C4 expected result,
;;            (2) diamond ancestors appear exactly once,
;;            (3) for suffix hierarchies, the suffix property holds.

(define-syntax defhierarchy
  (syntax-rules ()
    ((_ (name . parents) ...)
     (begin (defpoi name :e (constant-field-spec 'name 'name) :p . parents) ...))))

(let ()
  (defhierarchy ;; same as expected-pls
    (O) (A O) (B O) (C O) (D O) (E O)
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
    (SBA) (SBB) (SBS SBA) (sBs SBA) (SBC SBS SBA SBB))

  ;; --- Wikipedia 2021: Z hierarchy ---
  ;; Classes: O, A B C D E O, K1=(A B C), K2=(D B E), K3=(D A), Z=(K1 K2 K3)
  ;; Expected PL: Z K1 K2 K3 D A B C E O
  (expect
   (poi-precedence-list Z)  => (list Z K1 K2 K3 D A B C E O)
   (poi-precedence-list K1) => (list K1 A B C O)
   (poi-precedence-list K2) => (list K2 D B E O)
   (poi-precedence-list K3) => (list K3 D A O))

  ;; --- Wikipedia 2023: Y hierarchy ---
  ;; J1=(C A B), J2=(B D E), J3=(A D), Y=(J1 J3 J2)
  ;; Expected PL: Y J1 C J3 A J2 B D E O
  (expect (poi-precedence-list Y) => (list Y J1 C J3 A J2 B D E O))

  ;; --- C3 paper: Boat hierarchy ---
  ;; boat(B), day-boat(DB=B), wheel-boat(WB=B), engine-less(EL=DB),
  ;; small-multihull(SM=DB), pedal-wheel-boat(PWB=EL WB),
  ;; small-catamaran(SC=SM), pedalo(P=PWB SC)
  ;; Expected PL: P PWB EL SC SM DB WB B O
  (expect (poi-precedence-list P) => (list P PWB EL SC SM DB WB B O))

  ;; --- C4 suffix hierarchy: lowercase = suffix (single-inheritance chain) ---
  ;; O, o=(O suffix), a=(o), b=(a), c=(b o), d=(D c) where D is a class
  ;; Expected PLs: o→(o O), a→(a o O), b→(b a o O), c→(c b a o O), d→(d D c b a o O)
  (expect
   (poi-precedence-list o) => (list o O)
   (poi-precedence-list a) => (list a o O)
   (poi-precedence-list b) => (list b a o O)
   (poi-precedence-list c) => (list c b a o O)
   (poi-precedence-list d) => (list d D c b a o O))

  ;; --- C4 regression: x5=(x4 x1) where x4=(x3), x3=(x2), x2=(x1), x1 base ---
  ;; Expected PL: x5 x4 x3 x2 x1
  (expect
   (poi-precedence-list x5) => (list x5 x4 x3 x2 x1))

  ;; --- Instantiation with C4 merged fields across the Z hierarchy ---
  ;; Each class contributes a unique field; Z's instance can access all of them.
  (expect
   (map Z '(Z K1 K2 K3 A B C D E O missing)) => '(Z K1 K2 K3 A B C D E O #f))

  ;; --- Full test-objects/test-supers/expected-pls coverage ---
  ;; Build POI instances for all test objects using the same test vectors
  ;; already validated for c4-linearize directly.  test-objects is in topological
  ;; order (each object's supers appear earlier in the list), so parents always
  ;; exist in the alist when we create a child.
  (let ()
    (define sym-poi-alist '())  ;; (sym . oisp) pairs, most-recently-added first
    (def (sym->poi sym)
      (let ((p (assq sym sym-poi-alist)))
        (if p (cdr p) (error "POI not found for symbol" sym))))

    ;; Reverse-lookup: OISpec -> symbol (for comparing PLs with expected-pls)
    (def (poi->sym poi)
      (let ((p (find (lambda (pair) (eq? (cdr pair) poi)) sym-poi-alist)))
        (if p (car p) (error "No symbol for POI" poi))))

    ;; Create one OISpec per test object
    (for-each
     (lambda (sym)
       (let* ((supers  (test-get-supers sym))
              ;; Single chain of direct parents (same as c4-linearize call-site above)
              (parents (if (null? supers) '() (list (map sym->poi supers))))
              (poi     (make-poi sym idModExt (test-struct? sym) parents)))
         (set! sym-poi-alist (cons (cons sym poi) sym-poi-alist))))
     test-objects)

    ;; Check every object's precedence-list matches the expected one
    (expect
     (map (lambda (sym) (map poi->sym (poi-precedence-list (sym->poi sym))))
          test-objects)
     => expected-pls)))

;;;;; 9 Extending the Scope of OO

;;;; 9.1.3 Short Recap on Lenses

;; type MonoLens s a       = { view : s → a       ; update : (a → a) → s → s }
;; type PolyLens s t a b    = { view : s → a       ; update : (a → b) → s → t }
;;                          = SkewLens a a b s s t   (MonoLens s a = PolyLens s s a a)
;; type View r s = s → r
;; type Update i p j q = (i → p) → j → q
;; type SkewLens i r p j s q = { view : View r s ; update : Update i p j q }
;;                          -- PolyLens s t a b = SkewLens a a b s s t

;;; Composing Lenses
;; compose-view : View s t → View r s → View r t
(def (compose-view v w)
  (compose w v))

;; compose-update : Update j q k r → Update i p j q → Update i p k r
(def (compose-update f g)
  (compose f g))

;; make-lens : View r s → Update i p j q → SkewLens i r p j s q
(def (make-lens v u)
  (extend-record 'view v
    (extend-record 'update u
      empty-record)))

;; compose-lens : SkewLens j s q jj ss qq → SkewLens i r p j s q →
;;                SkewLens i r p jj ss qq
(def (compose-lens l k)
  (make-lens
    (compose-view (l 'view) (k 'view))
    (compose-update (l 'update) (k 'update))))

;; id-lens : SkewLens i r p i r p
(def id-lens
  (make-lens identity identity))

(def compose-lens/list (op/list←op1.1 compose-lens id-lens))
(define compose-lens* (op*←op1.1 compose-lens id-lens))

;;; Getter and Setter (moved after make-lens)
;; type Setter s t b = b → s → t
;; lens←getter*setter : View a s → Setter s t b → PolyLens s t a b
;; setter←lens : PolyLens s t a b → Setter s t b
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

(def (field-lens/list keys) (compose-lens/list (map field-lens keys)))
(define (field-lens* . keys) (field-lens/list keys))

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

(def (field-view~/list keys) (compose/list (map field-view~ (reverse keys))))
(define (field-view~* . keys) (field-view~/list keys))

;; field-lens~/list / field-lens~* : Key... → Lens
;; Like field-lens* but each intermediate node is initialized to empty-record if #f.
(def (field-lens~/list keys) (compose-lens/list (map field-lens~ keys)))
(define (field-lens~* . keys) (field-lens~/list keys))

;; field-update~/list : List(Key) → (Value → Value) → Record → Record
;;   field-update~ nested over a key path; every missing record on the way (including the
;;   target) ⇒ empty-record. Empty list ⇒ identity, i.e. the record itself is the leaf and
;;   the call reads as (f rec).
(def (field-update~/list keys) (compose/list (map field-update~ keys)))
(define (field-update~* . keys) (field-update~/list keys))

;; field-spec~/list : List(Key) → compute → ModExt   (compute = (inherited-leaf self) → leaf)
;;   A field-spec whose target is the nested key path `keys`; missing records default to
;;   empty-record. inherited-leaf is #f when absent, so compute owns any leaf default
;;   (argument order as field-spec's (inherited self)). A single-key path ⇒ plain field-spec.
;;   The OUTER key stays lazy like field-spec; once it fires the whole sub-record is rebuilt
;;   eagerly — fine for descriptors (instance-methods, instance-fields), NOT for the
;;   per-field value initializers that can `abort`, which stay plain lazy field-spec.
(def (field-spec~/list keys compute)
  (field-spec (car keys)
    (λ (inherited-sub self)
      (field-update~/list (cdr keys) (λ (leaf) (compute leaf self)) inherited-sub))))
(define (field-spec~* . keys) (field-spec~/list keys))

(let ()
  (def s (fix-record (mix* ((field-spec~* 'm 'a) (λ (_i _s) 1))
                           ((field-spec~* 'm 'b) (λ (_i _s) 2))
                           ((field-spec~* 'm 'a) (λ (i _s) (+ i 10)))
                           ((field-spec~* 'p 'q 'r) (λ (i _s) (or i 'seed))))))
  (expect
   (s 'm 'a) => 11    ;; second 'a spec chains on the first (1 -> +10)
   (s 'm 'b) => 2
   (s 'm 'c) => #f
   (s 'p 'q 'r) => 'seed   ;; 3-key path: intermediate records auto-created
   (s 'other) => #f))


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

;;;; 9.1.4 Focusing a Modular Extension
;;; From Sick to Ripped
;; skew-ext : SkewLens i r p j s q → ModExt i r p → ModExt j s q
(def (skew-ext l m super self)
  (l 'update (λ (inner-super) (m inner-super (l 'view self))) super))

;;;; 9.1.5 Adjusting Context and Focus
;;; Adjusting the Extension Focus
;; update-only-lens : Update i p j q → SkewLens i r p j r q
(def (update-only-lens u)
  (make-lens identity u))

;; update-lens : SkewLens i r p j s q → Update ii pp i p → SkewLens ii r pp j s q
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
;; reverse-view : s → MonoLens s a → View s a
;; reverse-update : s → MonoLens s a → Update s s a a
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
;; view-only-lens : View r s → SkewLens i r p i s p
(def (view-only-lens v)
  (make-lens v identity))

;; view-lens : SkewLens i r p j s q → View rr r → SkewLens i rr p j s q
(def (view-lens l v)
  (make-lens (compose-view (l 'view) v) (l 'update)))

(expect
   ;; view-only-lens: view transforms, update is identity
  (view-only-lens mul10 'view 7) => 70
  (view-only-lens mul10 'update add1 7) => 8)

;;;; 9.1.6 Optics for Specifications, Prototypes and Classes
;; Hereafter use rproto everywhere instead of directly ModExt,
;; except for trivial rproto←ModExt ?
;; Or better, use the MI variant?

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

;;; List Position Lenses
;; car-lens : SkewLens a a a (Pair a d) (Pair a d) (Pair a d)
(def car-lens
  (lens←getter*setter car (λ (v p) (cons v (cdr p)))))
;; cdr-lens : SkewLens d d d (Pair a d) (Pair a d) (Pair a d)
(def cdr-lens
  (lens←getter*setter cdr (λ (v p) (cons (car p) v))))

(def list-first-lens  car-lens)
(def list-second-lens (compose-lens cdr-lens list-first-lens))
(def list-third-lens  (compose-lens cdr-lens list-second-lens))
(def list-fourth-lens (compose-lens cdr-lens list-third-lens))

(def test-quad (list 'a 'b 'c 'd))
(expect
  (car-lens 'view '(1 . 2)) => 1
  (car-lens 'update add1 '(1 . 2)) => '(2 . 2)
  (cdr-lens 'view '(1 . 2)) => 2
  (cdr-lens 'update add1 '(1 . 2)) => '(1 . 3)
  (list-first-lens  'view test-quad) => 'a
  (list-second-lens 'view test-quad) => 'b
  (list-third-lens  'view test-quad) => 'c
  (list-fourth-lens 'view test-quad) => 'd
  (list-first-lens  'update (K 'w) test-quad) => '(w b c d)
  (list-second-lens 'update (K 'x) test-quad) => '(a x c d)
  (list-third-lens  'update (K 'y) test-quad) => '(a b y d)
  (list-fourth-lens 'update (K 'z) test-quad) => '(a b c z))

;;; Prototype Specification
;; name is pommette's own debugging addition (absent from the book's rproto),
;; but make-poi takes it as its first argument, so it belongs in the spec-view too.
(def (poi-spec-view p) (list (poi-name p) (poi-mod-ext p) (poi-suffix? p) (poi-parents p)))
;; make-poi is curried; apply can't spread a runtime list onto it at once,
;; so destructure the (statically known 4-element) spec-view list instead.
(def (poi-spec-setter args _old-poi)
  (let ((name (car args)) (mod-ext (cadr args))
        (suffix? (caddr args)) (parents (cadddr args)))
    (make-poi name mod-ext suffix? parents)))
(def poi-spec-lens
  (lens←getter*setter poi-spec-view poi-spec-setter))
(def poi-name-lens
  (compose-lens poi-spec-lens list-first-lens))
(def poi-modext-lens
  (compose-lens poi-spec-lens list-second-lens))
(def poi-suffix?-lens
  (compose-lens poi-spec-lens list-third-lens))
(def poi-parents-lens
  (compose-lens poi-spec-lens list-fourth-lens))

;;; 9.1.6.5. Nested Specifications
(def (update-rproto/mix modext rp)
  (rproto-mix (rproto←spec modext) rp))
(def (update-poi-modext/mix modext poi)
  (poi-modext-lens 'update (mix modext) poi))


;; Optics: poi-spec-lens / poi-name-lens / poi-modext-lens /
;; poi-suffix?-lens / poi-parents-lens, and poi←record / record←poi.
;; (Exercised here, once poi and defpoi actually exist, rather than in 9.1.5
;; itself where those optics are only defined.)
(let ()
  (defpoi base :e (constant-field-spec 'val 5))
  (defpoi child :e (field-spec 'val (λ (inh _self) (+ inh 10))) :p base)
  (defpoi other-base :e (constant-field-spec 'val 100))

  ;; poi-parents-lens re-parents child onto other-base, keeping child's
  ;; own name and mod-ext (so its +10 field-spec still applies on top)
  (def reparented (poi-parents-lens 'update (K (list (list other-base))) child))

  ;; poi←record / record←poi round-trip a plain record
  (def wrapped (poi←record (record (val 42))))

  ;; the lenses agree with the plain getters
  (expect
   (poi-spec-lens    'view child) => (poi-spec-view child)
   (poi-name-lens    'view child) => (poi-name child)
   (poi-modext-lens  'view child) => (poi-mod-ext child)
   (poi-suffix?-lens 'view child) => (poi-suffix? child)
   (poi-parents-lens 'view child) => (poi-parents child)

   (poi-name reparented) => (poi-name child)
   (poi-mod-ext reparented) => (poi-mod-ext child)
   (poi-parents reparented) => (list (list other-base))
   (reparented 'val) => 110  ;; other-base's 100, plus child's own +10
   (poi-precedence-list reparented) => (list reparented other-base)

   (wrapped 'val) => 42
   (record←poi child 'val) => 15   ;; ordinary field access still works
   (record←poi child #f) => #f))   ;; but the magic spec key is erased

;;;; 9.1.7 Optics for Classes

(def (instance-method-lens method-id) (field-lens~* 'instance-methods method-id))
(def (instance-field-lens field-id) (field-lens~* 'instance-fields field-id))

;; instance-method-spec : MethodId → (call-next-method element → Result) → ModExt
;;   Installs instance-methods[method-id] = (λ (element) (method-body cnm element)) — auto-
;;   curried, so instance-call hands back that partial application and any remaining method
;;   args are supplied by the caller's auto-curry (a 0-arg method needs no extra ()).
;;   cnm IS the parent class's installed method (element → result): a body calls (cnm el) to
;;   re-run it on the same element, (cnm other-el) on a replacement.
(def (instance-method-spec method-id method-body)
  ((field-spec~* 'instance-methods method-id)
    (λ (next-method _self element)
      (method-body next-method element))))

;; base-instance-method-spec : leaf instance method, no next-method parameter.
(def (base-instance-method-spec method-id method-body)
  (instance-method-spec method-id (K method-body)))

;; field-name-insert : append x at the most-specific (tail) end if absent; if x is already
;;   present, keep the earlier (less-specific) mention and drop this one. Preserves the
;;   accumulation order least-specific → most-specific, which keeps the suffix-class
;;   optimization: a suffix class's own fields stay contiguous at the start of the list.
;; Plain `define` (not `def`): self-recursive, and every caller passes both args
;; directly, so the auto-curry identifier-macro would only get in the way.
(define (field-name-insert x lst)
  (cond ((null? lst) (list x))
        ((eq? x (car lst)) lst)
        (else (cons (car lst) (field-name-insert x (cdr lst))))))

;; A CHECK is either #f (designating the identity function, but optimizable away),
;; or a function that on valid inputs return the unmodified (or normalized) input
;; and on invalid inputs (abort …)s on invalid inputs.
;; @TODO: normalization is not actually wired up yet — check-instance calls the
;; check purely for its abort side-effect and discards the result.  Wiring it up
;; raises open questions: do later field checks see earlier fields' NORMALIZED
;; values? in what order are field checks applied? are cross-field checks
;; allowed?  Any of these needs check-instance to thread results into a fresh
;; instance POI rather than iterating for-effect.

;; apply-check : Check → Value →! Value
(def (apply-check check value)
  (if check (check value) value))

;; simple-check : String → (Any → Boolean) → Check
(def (simple-check name pred)
  (and pred (λ (v) (if (pred v) v (abort "check failed" name v)))))

;; mix-check : Check → Check → Check
(def (mix-check older newer)
  (cond ((not newer) older)
        ((not older) newer)
        (else (compose newer older))))

;; A CHECK-SPEC is a Modular Extension (inherited-check instance-self → check) or #f

(def (simple-check-spec name pred)
  (let ((check (simple-check name pred)))
    (λ (inherited-check _self) (mix-check inherited-check check))))
(def number-check-spec (simple-check-spec "number" number?))
(def string-check-spec (simple-check-spec "string" string?))
(def empty-check-spec (constant-spec #f))

;; mix-maybe : (OrFalse Spec) → (OrFalse Spec) → (OrFalse Spec)
(def (mix-maybe older newer)
  (cond ((not newer) older)
        ((not older) newer)
        (else (mix older newer))))

;; instance-field-spec : FieldId → InitSpec → CheckSpec → ModExt over the class descriptor.
;;   InitSpec  : inherited-value whole-object → value  -- CHAINS on the parent's init
;;   (call-next-method for initializers); a bare (λ (_i _o) v) still REPLACES, read `_i` to
;;   refine. CheckSpec : (inherited-check self) → check, or #f for none — see above;
;;   (simple-check-spec name pred) is the common case, empty-check-spec drops the inherited one.
;;   Two contributions to the class descriptor:
;;     instance-field-names    : field-name-insert into the inherited list (accumulation order)
;;     instance-fields[id] : REFINES the inherited (record (init …) (check …)) — both
;;                               init-spec and check-spec are stacked on the inherited one
;;                               with mix-maybe (the check-spec later chains its own checks
;;                               via mix-check when resolved).  The single home for
;;                               {init, check, doc?, mutable?}.
;;   base-class's `base-instance` field derives the per-instance initializer POI from
;;   this table (one field-spec per field that has an init).
(def (instance-field-spec field-id init-spec check-spec)
  (mix*
    (field-spec~ 'instance-field-names
      (λ (inh _self) (field-name-insert field-id (or inh '()))))
    ((field-spec~* 'instance-fields field-id 'init)
      (λ (inh _self) (mix-maybe inh init-spec)))
    ((field-spec~* 'instance-fields field-id 'check)
      (λ (inh _self) (mix-maybe inh check-spec)))))

;; simple-instance-field-spec : the book's name; no check.
(def (simple-instance-field-spec field-id init-spec) (instance-field-spec field-id init-spec #f))

;; base-class : the root class POI. Every class inherits it — directly via `defclass`, or
;;   transitively through a class parent. It defines one derived field, `base-instance` : a
;;   parentless POI for the class's default instance prototype — #t → the class POI (so
;;   type-of / instance-call resolve), plus, per field that has an init, a
;;   (field-spec id <chained init>) read straight from instance-fields. Mandatory fields
;;   (init #f) are skipped — the constructor supplies them.
;;   TODO: the checks belong in a finalizer / method combination / validation layer.
;; poi-add-parent : POI → POI → POI  -- append `parent` as a final (least-specific) chain.
(def (poi-add-parent parent)
  (poi-parents-lens 'update (λ (ps) (append (or ps '()) (list (list parent))))))

;; base-instance-builder : classPOI → the class's default-instance prototype POI.
;;   Memoised by `self` (the only thing it depends on) so (ColorGraph 'base-instance)
;;   is a stable object across accesses.  The memo table lives as long as base-class
;;   itself — fine, classes are few.
(def base-instance-builder
  (memo (λ (self)
          (make-poi 'base-instance
            (mix (constant-field-spec #t self)
              (mix/list (filter identity
                         (map (λ (id)
                                (let ((i (self 'instance-fields id 'init)))
                                  (and i (field-spec id i))))
                              (or (self 'instance-field-names) '())))))
            #f '()))))

(defpoi base-class :e
  (field-spec 'base-instance
    (λ (_inh self) (base-instance-builder self))))

;; class←poi : root a POI at base-class (least-specific parent).
(def class←poi (poi-add-parent base-class))

;; defclass : defpoi then class←poi — same :e / :p / :pp / :p* args as defpoi, just rooted
;;   at base-class. For a subclass of a class C, plain `defpoi … :p C` inherits base-class
;;   transitively. (No struct-name? suffix heuristic — that's an example convention only.)
(define-syntax defclass
  (syntax-rules ()
    ((_ name args ...)
     (def name (class←poi (poi :n 'name args ...))))))

(def Rectangle-class
  (make-poi
    'Rectangle-class
    (mix*
      (simple-instance-field-spec 'width #f)
      (simple-instance-field-spec 'height #f)
      (base-instance-method-spec 'area
        (λ (element) (* (element 'width) (element 'height)))))
    #f (list (list base-class))))

(def (make-rectangle width height)
  (make-poi
    'make-rectangle
    (mix (constant-field-spec 'width width)
         (constant-field-spec 'height height))
    #f (list (list (Rectangle-class 'base-instance)))))

(def my-rectangle (make-rectangle 10 20))

(expect
 (my-rectangle 'width) => 10
 (instance-call my-rectangle 'area) => 200)

;; Extending the class is just adding more fields or methods to the mix.
;; Here, a field `color` with default value "black"; `area` is still inherited.
(def ColoredRectangle-class
  (make-poi
    'ColoredRectangle-class
    (simple-instance-field-spec 'color (constant-spec "black"))
    #f (list (list Rectangle-class))))

(def my-colored-rectangle
  (make-poi
    'my-colored-rectangle
    ;; the book writes (mix/list (map constant-field-spec '(width height) '(3 5)))
    ;; but `map` calls the curried constant-field-spec uncurried; spell it out
    (mix (constant-field-spec 'width 3)
         (constant-field-spec 'height 5))
    #f (list (list (ColoredRectangle-class 'base-instance)))))

(expect
 (my-colored-rectangle 'color) => "black"
 (instance-call my-colored-rectangle 'area) => 15)



;;; Reflection + constructors -------------------------------------------------------------

;; mandatory-fields : classPOI → list of field ids whose meta init is #f (accumulation order).
(def (mandatory-fields cls)
  (filter (λ (id) (not (cls 'instance-fields id 'init)))
          (or (cls 'instance-field-names) '())))

;; check-instance : resolve each field's check-spec against the fixed instance (inherited-
;;   check #f, self = inst) and run the resulting check on the field value; each aborts if
;;   bad. A check-spec that yields #f means "no check" — the field is never forced.
(def (check-instance cls inst)
  (for-each (λ (id)
              (let ((cs ((field-view~* 'instance-fields id 'check) cls)))
                (when cs
                  (let ((c (@ cs #f inst)))
                    (when c
                      (c (inst id)))))))
            (or (cls 'instance-field-names) '()))
  inst)

;; instance←class : the main constructor — `ext-poi` is your extension POI (constant fields,
;;   method overrides, …); the instance is a live POI with `ext-poi` most-specific and
;;   (cls 'base-instance) least-specific (so ext's constants override the class's own field
;;   inits). check-instance then validates it. make-instance is a plist-taking convenience.
(def (instance←class cls ext-poi)
  (check-instance cls (poi-add-parent (cls 'base-instance) ext-poi)))

;; make-instance : the plain constructor — `cls` then a plist of field id/value pairs, each
;;   becoming a constant-field-spec in an ext POI; instance←class instantiates + validates.
;;   (make-instance cls) with no pairs ⇒ the class's default instance.
;;   Plain `define` (rest arg): NOT auto-curried, so read a field with explicit parens,
;;   e.g. ((make-instance cls 'x 1) 'x).
(define (make-instance cls . plist)
  (instance←class cls
    (make-poi 'make-instance
      (mix/list
        (let loop ((p plist))
          (if (null? p) '()
              (cons (constant-field-spec (car p) (cadr p)) (loop (cddr p))))))
      #f '())))

;;;; 9.1.7 Worked Example — Nested Classes and Family Polymorphism

;;; The plain class machinery (methods, fields, checks, constructors), exercised
;;; directly — no nesting / family machinery yet.  (Identifiers here are prefixed P0-.)
(defpoi P0-Widget :e
  (base-instance-method-spec 'render (λ (el) (string-append "<" (el 'tag) ">"))))
(defpoi P0-Boxed :e
  (instance-method-spec 'render (λ (cnm el) (string-append "[" (cnm el) "]")))
  :p P0-Widget)
(defpoi P0-Trace :e
  (instance-method-spec 'render
    (λ (cnm el) (cnm (extend-record 'tag (string-append "!" (el 'tag)) el))))
  :p P0-Widget)
;; a bare instance: #t → class POI, plus a constant 'tag field, fixed against empty-record
(def (bare cls tag)
  (fix-record (mix* (constant-field-spec #t cls) (constant-field-spec 'tag tag))))
(expect
 (instance-call (bare P0-Widget "b") 'render) => "<b>"
 (instance-call (bare P0-Boxed "b") 'render) => "[<b>]"     ;; call-next-method chains
 (instance-call (bare P0-Trace "b") 'render) => "<!b>")     ;; advanced CNM (updated element)

;; field init: whole instance = context, field value = focus; inits chain
(defclass P0-Base :e (instance-field-spec 'n (constant-spec 1) #f))
(defpoi   P0-Sub  :e (instance-field-spec 'n (λ (inh _obj) (* 10 inh)) #f) :p P0-Base)
(def (nfix cls) (cls 'base-instance))
(expect
 (nfix P0-Base 'n) => 1
 (nfix P0-Sub  'n) => 10)   ;; chained: 1 → *10

;; minimal reflection layer
(defclass P0-Rec :e (mix* (instance-field-spec 'tag #f string-check-spec)
                          (instance-field-spec 'n (constant-spec 1) #f)))
(expect
 (P0-Rec 'instance-field-names) => '(tag n)   ;; accumulation order: mandatory 'tag then 'n
 (@ P0-Rec 'instance-fields 'tag 'init) => #f
 (procedure? (@ P0-Rec 'instance-fields 'n 'init)) => #t
 (procedure? (@ P0-Rec 'instance-fields 'tag 'check)) => #t)

;; a whole plain class + subclass, built with the constructors
(defclass P0-Thing :e
  (mix* (instance-field-spec 'name #f string-check-spec)
        (instance-field-spec 'size (constant-spec 1) #f)
        (base-instance-method-spec 'show
          (λ (el) (string-append (el 'name) "×" (number->string (el 'size)))))))
(defpoi P0-Big :e (instance-field-spec 'size (λ (inh _o) (* 100 inh)) #f) :p P0-Thing)
(expect
 (mandatory-fields P0-Thing) => '(name)
 ((make-instance P0-Thing 'name "x") 'size) => 1         ;; `size` default init
 (instance-call (make-instance P0-Thing 'name "x") 'show) => "x×1"
 ((make-instance P0-Big 'name "x") 'size) => 100         ;; `size` init chains (1 → *100)
 (make-instance P0-Thing 'name 42) =>fail!               ;; `name` check aborts at construction
 (instance←class P0-Thing (poi :e (mix (constant-field-spec 'name "y")
                                          (constant-field-spec 'size 9))) 'size) => 9)

(def positive-check-spec (simple-check-spec "positive" positive?))

(defclass CheckedNumber
  :e (instance-field-spec 'n #f number-check-spec))

(defpoi PositiveNumber
  :e (instance-field-spec 'n #f positive-check-spec)
  :p CheckedNumber)

(expect
 ((make-instance PositiveNumber 'n 42) 'n) => 42
 (make-instance PositiveNumber 'n -1) =>fail!   ;; positive? fails
 (make-instance PositiveNumber 'n "oops") =>fail!) ;; inherited number? fails

;;; Nested classes as POI-valued fields --------------------------------------------------
;;
;; An inner class is just a POI held in a regular field of the family class.
;;   base declaration : (constant-field-spec 'Node graph-node)      -- graph-node a class POI
;;   covariant refine : (poi-mix-field-spec  'Node colorgraph-node) -- poi-mix onto inherited
;;   lateral replace  : (constant-field-spec 'NodeCodec json-codec) -- constant ignores inherited
;;   access           : (Graph 'Node), (ColorGraph 'Node)          -- plain field access
;; The family's own C4-ordered mix* of ancestor mod-exts drives the poi-mix chaining, so a
;; diamond's (RichGraph 'Node) linearizes exactly as the outer hierarchy does — no explicit
;; projection, and eq? identity is free (the base (Graph 'Node) is one constant object).

;; poi-mix-field-spec : Key → contributionPOI → ModExt over the class descriptor.
(def (poi-mix-field-spec key contrib) (field-spec key (poi-mix-spec contrib)))

;;; The example: Workspace (toplevel, namespace only) ⊃ Graph ⊃ Node / Edge / NodeCodec ---
;;; new-node / new-edge stamp `owner` (the family class) onto each instance they build;
;;; serialize and the Edge cross-family check read (node 'owner).

;; Node: base describe + a MANDATORY string-checked `label` + serialize (CONSUMES NodeCodec).
(defclass graph-node :e
  (mix* (base-instance-method-spec 'describe
          (λ (el) (string-append "N(" (el 'label) ")")))
        (instance-field-spec 'label #f string-check-spec)
        (base-instance-method-spec 'serialize
          (λ (node)
            (let ((codec (make-instance (node 'owner 'NodeCodec))))
              (instance-call codec 'write node))))))

;; Edge: describe errors on a cross-family edge, else brackets its endpoints' describe.
(defclass graph-edge :e
  (mix* (base-instance-method-spec 'describe
          (λ (el)
            (unless (eq? (el 'from 'owner) (el 'to 'owner))
              (error "cross-family edge"))
            (string-append "E[" (instance-call (el 'from) 'describe) "=>"
                               (instance-call (el 'to) 'describe) "]")))
        (instance-field-spec 'from #f #f)
        (instance-field-spec 'to #f #f)))

;; NodeCodec: inner class in CONTRAVARIANT position -- CONSUMED by Node.serialize.
;;   write : (codec) → (node)   → string        read : (codec) → (string) → field-record
(defclass line-codec :e
  (mix* (base-instance-method-spec 'write (λ (_c node) (string-append "label=" (node 'label))))
        (base-instance-method-spec 'read  (λ (_c s)    (record (label s))))))

;; The Graph family: the three inner classes as fields + family-instance factory methods.
;;   (type-of g) is the family class POI ⇒ new-node / new-edge build instances of THAT family.
(defclass graph :e
  (mix* (constant-field-spec 'Node     graph-node)
        (constant-field-spec 'Edge     graph-edge)
        (constant-field-spec 'NodeCodec line-codec)
        (base-instance-method-spec 'new-node
          (λ (g label)
            (instance←class (type-of g 'Node)
              (poi :e (mix (constant-field-spec 'owner (type-of g))
                           (constant-field-spec 'label label))))))
        (base-instance-method-spec 'new-edge
          (λ (g a b)
            (instance←class (type-of g 'Edge)
              (poi :e (mix* (constant-field-spec 'owner (type-of g))
                            (constant-field-spec 'from a)
                            (constant-field-spec 'to b))))))))

;; >=3 nesting levels: Workspace (namespace only) ⊃ Graph ⊃ Node / Edge / NodeCodec
(defpoi Workspace :e (constant-field-spec 'Graph graph))
(def Graph (Workspace 'Graph))

;; outer diamond over the Graph family -- covariant refinement of the Node inner class
(defpoi ColorGraph
  :e (poi-mix-field-spec 'Node
       (poi :e (mix* (instance-field-spec 'color (constant-spec "black") #f)
                     (instance-method-spec 'describe
                       (λ (cnm el) (string-append (cnm el) "@" (el 'color)))))))
  :p Graph)
(defpoi WeightedGraph
  :e (poi-mix-field-spec 'Node
       (poi :e (mix (instance-field-spec 'weight (constant-spec 1) #f)
                    (instance-method-spec 'describe (λ (cnm el) (string-append (cnm el) "#w"))))))
  :p Graph)
(defpoi RichGraph  :e idModExt
  :p* (list (list ColorGraph Graph) (list WeightedGraph Graph)))
(defpoi RichGraph2 :e idModExt
  :p* (list (list WeightedGraph Graph) (list ColorGraph Graph)))

;; field-init CHAINING (call-next-method for initializers): HeavyGraph multiplies the
;; inherited `weight` default.
(defpoi HeavyGraph
  :e (poi-mix-field-spec 'Node
       (poi :e (instance-field-spec 'weight (λ (inh _obj) (* 10 inh)) #f)))
  :p WeightedGraph)

;; advanced call-next-method: replacement element
(defpoi TracingGraph
  :e (poi-mix-field-spec 'Node
       (poi :e (instance-method-spec 'describe
                 (λ (cnm el) (cnm (extend-record 'label (string-append ">" (el 'label)) el))))))
  :p Graph)

;; NON-COVARIANT: the contravariant NodeCodec cannot be inherited-and-extended in lockstep
;; with Node; a family that changes Node's serialized form must RE-IMPLEMENT it.
;; (1) lateral swap: JsonGraph replaces NodeCodec wholesale (a constant field ignores inherited).
(defpoi JsonGraph
  :e (constant-field-spec 'NodeCodec
       (poi :e (mix* (base-instance-method-spec 'write
                       (λ (_c node) (string-append "{\"label\":\"" (node 'label) "\"}")))
                     (base-instance-method-spec 'read (λ (_c s) (record (label s)))))
            :p base-class))
  :p Graph)
;; (2) opposite-direction sibling: IdOnlyGraph makes Node SMALLER -- label init aborts, adds
;;     id -- so it must ALSO override new-node (the inherited factory passes a label). Its
;;     codec writes LESS, vs ColorGraph adding a field so the codec writes MORE.
(defpoi IdOnlyGraph
  :e (mix* (poi-mix-field-spec 'Node
             (poi :e (mix* (instance-field-spec 'id    (λ (_inh _obj) "0") #f)
                           ;; drop label: aborting init + empty-check-spec (so check-instance
                           ;; never forces it and the inherited string? is not enforced)
                           (instance-field-spec 'label (λ (_inh _obj) (abort "IdOnlyGraph: no label"))
                                                empty-check-spec))))
           (constant-field-spec 'NodeCodec
             (poi :e (mix* (base-instance-method-spec 'write (λ (_c node) (string-append "id=" (node 'id))))
                           (base-instance-method-spec 'read  (λ (_c s) (record (id s)))))
                  :p base-class))
           (base-instance-method-spec 'new-node
             (λ (g _label)
               (instance←class (type-of g 'Node)
                 (poi :e (constant-field-spec 'owner (type-of g)))))))
  :p Graph)

;;; The full worked example
(let ()
  (def GN (Graph 'Node))
  (def CN (ColorGraph 'Node))
  (def WN (WeightedGraph 'Node))
  (def RN (RichGraph 'Node))
  (def g   (make-instance Graph))
  (def cg  (make-instance ColorGraph))
  (def wg  (make-instance WeightedGraph))
  (def rg  (make-instance RichGraph))
  (def rg2 (make-instance RichGraph2))
  (def (dn n) (instance-call n 'describe))
  (def (nn graph label) (instance-call graph 'new-node label))

  ;; >=3-level nesting + outer diamond linearizes (base-class is the shared root)
  (expect
   (map poi-name (poi-precedence-list Graph)) => '(graph base-class)
   (map poi-name (poi-precedence-list RichGraph))
     => '(RichGraph ColorGraph WeightedGraph graph base-class)
   (map poi-name (poi-precedence-list RichGraph2))
     => '(RichGraph2 WeightedGraph ColorGraph graph base-class))

  ;; the inner Node class: covariant refinement, and the diamond converges on ONE graph-node
  (expect
   (memq graph-node (poi-precedence-list GN)) => (list graph-node base-class)
   (memq graph-node (poi-precedence-list CN)) => (list graph-node base-class)   ;; graph-node once
   (memq graph-node (poi-precedence-list RN)) => (list graph-node base-class)   ;; …in the diamond too
   (length (filter (λ (p) (eq? p graph-node)) (poi-precedence-list RN))) => 1)

  ;; nested-class identity is STABLE across accesses (poi-mix-spec / base-instance
  ;; are memoised), so a factory reading (type-of g 'Node) twice sees one object.
  (expect
   (eq? (Graph 'Node) (Graph 'Node)) => #t
   (eq? (ColorGraph 'Node) (ColorGraph 'Node)) => #t
   (eq? (RichGraph 'Node) (RichGraph 'Node)) => #t
   (eq? (ColorGraph 'base-instance) (ColorGraph 'base-instance)) => #t)

  ;; minimal reflection layer
  (expect
   (GN 'instance-field-names) => '(label)
   (CN 'instance-field-names) => '(label color)   ;; base 'label, then ColorGraph's own 'color
   (mandatory-fields GN) => '(label)
   (mandatory-fields CN) => '(label)
   (GN 'instance-fields 'label 'init) => #f
   (procedure? (CN 'instance-fields 'color 'init)) => #t
   ;; CN inherits GN's check-spec for 'label; resolving it yields a check enforcing string?.
   (procedure? (CN 'instance-fields 'label 'check)) => #t
   (CN 'instance-fields 'label 'check #f #f "s") => "s"
   (CN 'instance-fields 'label 'check #f #f 42) =>fail!)

  ;; plain plist constructor: (make-instance cls 'id val ...), each field checked
  (expect
   ((make-instance CN 'label "A") 'label) => "A"
   ((make-instance CN 'label "A") 'color) => "black"
   (make-instance CN 'label 42) =>fail!)

  ;; roll-your-own extension overrides a non-chaining class init
  (expect
   (instance←class CN (poi :e (mix (constant-field-spec 'label "A")
                                      (constant-field-spec 'color "red"))) 'color) => "red"
   (instance←class CN (poi :e (mix (constant-field-spec 'label "A")
                                      (constant-field-spec 'color "red"))) 'label) => "A")

  ;; field-init chaining (call-next-method for initializers)
  (expect
   (nn wg "n" 'weight) => 1
   (nn (make-instance HeavyGraph) "n" 'weight) => 10)

  ;; family polymorphism: the factory builds instances of ITS family's inner class
  (expect
   (nn g  "n" 'color) => #f
   (nn cg "n" 'color) => "black")

  ;; outer C4 order induces the inner describe method-resolution order
  (expect
   (dn (nn g   "A")) => "N(A)"
   (dn (nn cg  "A")) => "N(A)@black"
   (dn (nn rg  "A")) => "N(A)#w@black"
   (dn (nn rg2 "A")) => "N(A)@black#w")

  ;; advanced call-next-method (replacement element)
  (expect
   (dn (nn (make-instance TracingGraph) "A")) => "N(>A)")

  ;; edge factory: same family OK, cross-family errors
  (expect
   (let* ((a (nn cg "X")) (b (nn cg "Y")))
     (dn (instance-call cg 'new-edge a b))) => "E[N(X)@black=>N(Y)@black]"
   (let* ((a (nn cg "X")) (b (nn g "Y")))
     (dn (instance-call cg 'new-edge a b))) =>fail!)

  ;; contravariant inner class — lateral swap + opposite-direction sibling
  (expect
   (instance-call (nn (make-instance JsonGraph) "A") 'serialize) => "{\"label\":\"A\"}"
   (instance-call (nn g "A") 'serialize) => "label=A"
   (let ((ig (make-instance IdOnlyGraph)))
     (instance-call (nn ig "ignored") 'serialize)) => "id=0"
   (let ((ig (make-instance IdOnlyGraph)))
     ((nn ig "ignored") 'label)) =>fail!))

;;;;; 9.2 Method Combinations

;;;; 9.2.0 Calling Conventions
;; A generic function stores a CallingConvention: how to gather its call
;; arguments (the `accepter`) and how to apply them to a method (the
;; `method-invoker`), given an `arity` of leading (dispatch) arguments.  Method
;; combinations and multiple dispatch both use it.
;;
;; Both halves take a PARTIAL (initial) argument list — the args already consumed
;; by the surrounding representation (the receiver, a generic function's first
;; argument, a fresh call-next-method arglist).  They deal with whatever remains:
;;   accepter       : (FullArgList → R) → InitialArgList → SurfaceResidual
;;                    gathers the rest (curried one at a time, or bundled), then
;;                    calls (k <initial ++ gathered>).
;;   method-invoker : RawMethodFn → InitialArgList → (R | (Rest… → R))
;;                    processes the list it is given, then waits for the rest.
;;                    `method-invoker` (not `invoker`): it is built with an extra
;;                    mandatory slot for the `cnm` a caller prepends, so it is NOT
;;                    the inverse of `accepter`.
;; A short prefix ⇒ a residual awaiting more; an over-long prefix ⇒ an error.
;; (There is deliberately no "too few arguments" check any more — underflow just
;; yields the residual.)

;; over-arity? : mandatory optionals n → Bool  (#t when n exceeds the max arity)
(def (over-arity? mandatory optionals n)
  (and (not (eq? optionals #t)) (> n (+ mandatory optionals))))

;; uncurried-accepter : mandatory optionals → accepter
;; mandatory: number of required args ; optionals: 0 | positive int (max extra) | #t (unlimited)
;; The remainder past `initial-args` is supplied in ONE further application.
(def (uncurried-accepter mandatory optionals)
  (λ (k initial-args)
    (let ((n (length initial-args)))
      (when (over-arity? mandatory optionals n)
        (error "uncurried-accepter: too many arguments" (+ mandatory optionals) initial-args))
      (if (>= n mandatory)
        (k initial-args)
        (λ rest
          (let ((all (append initial-args rest)))
            (when (over-arity? mandatory optionals (length all))
              (error "uncurried-accepter: too many arguments" (+ mandatory optionals) all))
            (k all)))))))

;; opposite of uncurried-accepter: a method-invoker that applies a list of
;; arguments to a function, then waits (in one application) for any remainder.
(def (uncurried-method-invoker mandatory optionals)
  (λ (f initial-args)
    (let ((n (length initial-args)))
      (when (over-arity? mandatory optionals n)
        (error "uncurried-method-invoker: too many arguments" (+ mandatory optionals) initial-args))
      (if (>= n mandatory)
        (apply f initial-args)
        (λ rest (apply f (append initial-args rest)))))))

;; curried-accepter : mandatory optionals optionals-with-last? → accepter
;; mandatory: number of curried args collected one at a time
;; optionals: 0 (none), positive integer (max extra), or #t (unlimited rest)
;; optionals-with-last?: if #t, optionals bundled with last mandatory arg, otherwise, as extra call.
(def (curried-accepter mandatory optionals optionals-with-last?)
  (λ (k initial-args)
    (let ((n (length initial-args)))
      (when (over-arity? mandatory optionals n)
        (error "curried-accepter: too many arguments" (+ mandatory optionals) initial-args))
      (let loop ((remaining (max 0 (- mandatory n))) (acc (reverse initial-args)))
        (cond
          ((and (zero? remaining) (equal? optionals 0))
           (k (reverse acc)))
          ((or (zero? remaining)
               (and optionals-with-last? (= remaining 1)))
           (λ rest
             (let ((all (append (reverse acc) rest)))
               (when (over-arity? mandatory optionals (length all))
                 (error "curried-accepter: too many arguments" (+ mandatory optionals) all))
               (k all))))
          (else
           (λ (x) (loop (- remaining 1) (cons x acc)))))))))

;; opposite of curried-accepter: a method-invoker that applies a list of
;; arguments to a curried function.  With no optionals tail it is plain
;; currying — a short list yields a partial application, a complete one the
;; result.  With an optionals tail, the curried prefix is applied then the
;; bundled remainder is fed through uncurried-method-invoker.
(def (curried-method-invoker mandatory optionals optionals-with-last?)
  (let* ((last? (and optionals-with-last? (> mandatory 0)))
         (stop  (if last? (- mandatory 1) mandatory)))
    (λ (f initial-args)
      (let ((n (length initial-args)))
        (when (over-arity? mandatory optionals n)
          (error "curried-method-invoker: too many arguments" (+ mandatory optionals) initial-args))
        (cond
          ((and (equal? optionals 0) (not last?))
           (curry/list f initial-args))
          ((< n stop)
           (curry/list f initial-args))
          (else
           (@ (uncurried-method-invoker (if last? 1 0) optionals)
              (curry/list f (take initial-args stop))
              (list-tail initial-args stop))))))))

(def (make-calling-convention arity accepter method-invoker)
  (record (arity arity)
          (accepter accepter)
          (method-invoker method-invoker)))

(def (uncurried-convention arity extra-mandatory optionals)
  (let ((mandatory (+ arity extra-mandatory)))
    (make-calling-convention
      arity
      (uncurried-accepter mandatory optionals)
      (uncurried-method-invoker (+ mandatory 1) optionals))))  ;; +1 for cnm

(def (curried-convention arity extra-mandatory optionals optionals-with-last?)
  (let ((mandatory (+ arity extra-mandatory)))
    (make-calling-convention
      arity
      (curried-accepter mandatory optionals optionals-with-last?)
      (curried-method-invoker (+ mandatory 1) optionals optionals-with-last?))))  ;; +1 for cnm

(def (default-convention arity)
  (curried-convention arity 0 0 #f))

;; as-calling-convention : Designator → CallingConvention
;;   A bare number n designates (default-convention n); anything else is already
;;   a convention record.
(def (as-calling-convention designator)
  (if (number? designator) (default-convention designator) designator))

;; --- accepter / method-invoker on partial (initial) argument lists ---

;; (accepter and method-invoker are curried — hence the @ / nesting below.)

;; curried-method-invoker corner case (was a bug): optionals-with-last? = #t with
;; optionals = 0 must still apply the bundled last argument, not return a
;; partially-applied procedure.
(expect
  (@ (curried-method-invoker 2 0 #t) (λ (x y) (list x y)) '(a b)) => '(a b))

;; accepter: a short initial list yields a residual that gathers the rest;
;; a complete one calls k straight away; an over-long one errors.
(expect
  (((@ (curried-accepter 3 0 #f) identity '(a)) 'b) 'c) => '(a b c)     ;; curried remainder
  (@ (curried-accepter 3 0 #f) identity '(a b c)) => '(a b c)           ;; already saturated
  (@ (curried-accepter 2 0 #f) identity '(a b c)) =>fail!              ;; over-long
  (@ (uncurried-accepter 3 0) identity '(a b c)) => '(a b c)           ;; bundled, saturated
  ((@ (uncurried-accepter 3 0) identity '(a)) 'b 'c) => '(a b c)       ;; bundled remainder
  (@ (uncurried-accepter 2 0) identity '(a b c)) =>fail!)              ;; over-long

;; method-invoker: complete list ⇒ result, short list ⇒ a procedure awaiting the
;; rest, over-long ⇒ error.
(expect
  (@ (curried-method-invoker 3 0 #f) (λ (x y z) (list x y z)) '(a b c)) => '(a b c)
  (procedure? (@ (curried-method-invoker 3 0 #f) (λ (x y z) (list x y z)) '(a))) => #t
  (@ (curried-method-invoker 3 0 #f) (λ (x y z) (list x y z)) '(a) 'b 'c) => '(a b c)
  (@ (uncurried-method-invoker 2 0) (λ (x y) (list x y)) '(a b c)) =>fail!)


;;;; 9.2.1 Representing Sub-Methods

;; sub-method-spec : MethodCons → Qualifier → MethodId → MethodFn → ModExt
;;   MethodCons = MethodFn → MethodFns → MethodFns
;;   MethodNil  = MethodFns
;;   Qualifier  = Symbol  ('primary 'before 'after 'around — as in CLOS — or the
;;                         operator name for a simple combination)
;;   MethodId   = Symbol  (method name in the record, e.g. 'compute 'greet)
;;   MethodFn   = CallNextMethod → Arg... → Result  (see 9.2.2; Self, under single
;;                dispatch, is merely the first argument)
;;   ModExt     = ? → ? → ?  (modular extension; see field-spec)
;;
;; Creates a ModExt that prepends method-fn to sub-methods[method-id][qualifier].
;; The 3-deep nesting (sub-methods → method-id → qualifier, leaf = a list) is
;; field-spec~* (§5.3.5); the leaf defaults #f, so `compute` supplies the empty list.
(def (sub-method-spec method-cons methods-nil qualifier method-id method-fn)
  ((field-spec~* 'sub-methods method-id qualifier)
    (λ (qualified-methods _self) (method-cons method-fn (or qualified-methods methods-nil)))))

;; standard-sub-method-spec : Qualifier → MethodId → MethodFn → ModExt
;;   (sub-method-spec with the plain cons; 1st arg is Qualifier, 2nd is MethodId)
(def standard-sub-method-spec (sub-method-spec (λ (x y) (cons x y)) '()))

;; sub-method-lens : MethodId → Qualifier → SkewLens into the sub-methods record
;; (Useful for rproto encoding; in Y encoding prefer sub-method-spec directly.)
(def (sub-method-lens method-id qualifier)
  (field-lens~* 'sub-methods method-id qualifier))

;; self-sub-methods : Self → MethodId → (OrFalse SubMethods)
;; The per-method-id sub-method record, or #f if none was ever contributed (no
;; method-combination-init-spec, no sub-method-spec).  The compute-effective-method
;; functions treat #f — whether the whole record or an individual qualifier — as
;; "empty", so method-combination-init-spec is a convenience, not a requirement.
(def (self-sub-methods self method-id)
  (let ((sm (self 'sub-methods)))
    (and sm (sm method-id))))

;; method-combination-init-spec : MethodId → InitRecord → ModExt
;;   InitRecord = record {qualifier: List(MethodFn), ...}
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

;; MethodFn = CallNextMethod → Arg... → Result   (can have its own lambda list)
;;   A method declares whatever parameters it wants after CallNextMethod; the
;;   generic function's `method-invoker` (from its calling convention, §9.2.0)
;;   applies the gathered argument list to it.  For a single-dispatch method
;;   combination the first argument is the receiver, so a method that needs it
;;   writes (λ (call-next-method self x ...) ...); one that doesn't just omits it.
;;   CallNextMethod = () → Result   re-run the rest of the chain on the same args,
;;                  | Arg... → Result   re-run it on a fresh argument list, gathered
;;                                      through the convention's accepter.

;; make-call-next-method : Next → Accepter → ArgList → CallNextMethod
;;   Next     = ArgList → Result   (the remaining chain)
;;   Accepter = the convention's accepter ((k initial-args) → residual)
(def (make-call-next-method next accepter args)
  (λ new
    (if (null? new)
      (next args)                    ;; (cnm) — replay the same arguments
      (accepter next new))))         ;; (cnm x ...) — gather new args past `new`, then next

;; call-chain : List(RawMethodFn) → OnExhausted → Accepter → MethodInvoker → EffectiveMethod
;;   EffectiveMethod = ArgList → Result ;  OnExhausted has the same shape.
;;   Each raw method m is applied via (method-invoker m (cons cnm args)).
;;   A missing sub-method list is #f — treat it as empty.
(def (call-chain methods on-exhausted accepter method-invoker)
  (foldr
    (lambda (m next)
      (λ (args)
        (method-invoker m (cons (make-call-next-method next accepter args) args))))
    on-exhausted
    (or methods '())))

;; progn-methods-most-specific-first : List(RawMethodFn) → ArgList → MethodInvoker → #f
;; Runs each method in order for side-effects; call-next-method = abort.
(def (progn-methods-most-specific-first methods args method-invoker)
  (foldl (lambda (m _) (method-invoker m (cons abort args))) #f (or methods '())))

;; progn-methods-most-specific-last : List(RawMethodFn) → ArgList → MethodInvoker → #f
(def (progn-methods-most-specific-last methods args method-invoker)
  (foldr (lambda (m _) (method-invoker m (cons abort args))) #f (or methods '())))

;; standard-no-applicable-method : MethodId → ...Args → Error
(define (standard-no-applicable-method method-id . args)
  (error "no applicable method" method-id args))

(define no-applicable-method standard-no-applicable-method)

;; standard-compute-effective-method : MethodId → SubMethods → Accepter → MethodInvoker → EffectiveMethod
;;   SubMethods = record {before/after/around/primary: List(RawMethodFn)} or #f when
;;   the method-id has no sub-methods record at all.  `sub` yields #f (⇒ empty) for
;;   a missing record or a missing qualifier; with no primary method, the primary
;;   chain is just `no-applicable-method`.
(def (standard-compute-effective-method method-id sub-methods accepter method-invoker)
  (let ((sub (lambda (qualifier) (and sub-methods (sub-methods qualifier)))))
    (call-chain (sub 'around)
      (λ (args)
        (progn-methods-most-specific-first (sub 'before) args method-invoker)
        (let ((result
                ((call-chain (sub 'primary)
                   (λ (args) (apply no-applicable-method method-id args))
                   accepter method-invoker)
                 args)))
          (progn-methods-most-specific-last (sub 'after) args method-invoker)
          result))
      accepter method-invoker)))

;; standard-method-init-spec : ConventionDesignator → MethodId → ModExt
;;   The stored value is the convention's accepter primed with the receiver
;;   (dispatch argument 0) as its initial argument list; it gathers the rest of
;;   the call's arguments, then runs the standard method combination, invoking
;;   each sub-method via the method-invoker.
(def (standard-method-init-spec convention method-id)
  (let* ((cc             (as-calling-convention convention))
         (accepter       (cc 'accepter))
         (method-invoker (cc 'method-invoker)))
    (mix
      (field-spec method-id
        (λ (_inherited self)
          (@ accepter
             (standard-compute-effective-method
              method-id (self-sub-methods self method-id) accepter method-invoker)
             (list self))))
      (method-combination-init-spec method-id standard-method-combination-init))))

;; Convenience specs for each standard qualifier (Qualifier → MethodId → MethodFn → ModExt)
(def primary-method-spec (standard-sub-method-spec 'primary))
(def before-method-spec  (standard-sub-method-spec 'before))
(def after-method-spec   (standard-sub-method-spec 'after))
(def around-method-spec  (standard-sub-method-spec 'around))

;; Tests for standard method combination.
;; The init-spec's first argument is a calling-convention designator: a number n
;; = (default-convention n), n counting the receiver as dispatch argument 0.  A
;; method is a plain curried lambda over what it needs: (λ (cnm self x ...) ...).

;; Single primary method, convention 2 (receiver + one argument): (obj 'compute x)
(def smc-obj-mul10
  (fix-record
    (mix*
      (standard-method-init-spec 2 'compute)
      (primary-method-spec 'compute (λ (_cnm _self x) (* x 10))))))

(expect
  (smc-obj-mul10 'compute 3) => 30
  (smc-obj-mul10 'compute 5) => 50)

;; Around method wraps primary; (call-next-method) replays the same arguments,
;; (call-next-method self v) re-runs the chain with a fresh argument list.
(def smc-obj-around
  (fix-record
    (mix*
      (standard-method-init-spec 2 'compute)
      (primary-method-spec 'compute (λ (_cnm _self x) (* x 10)))
      (around-method-spec 'compute (λ (cnm _self _x) (+ (cnm) 1))))))

(expect (smc-obj-around 'compute 3) => 31    ;; (* 3 10) = 30; around adds 1
        (((fix-record
            (mix* (standard-method-init-spec 2 'compute)
                  (primary-method-spec 'compute (λ (_c _self x) (* x 10)))
                  (around-method-spec 'compute
                    (λ (cnm self _x) (cnm self 9)))))
          'compute)
         3) => 90)  ;; around ignores 3, re-runs the chain with 9

;; A primary method taking two arguments, convention 3 (receiver + two args).
(def smc-obj-sum
  (fix-record
    (mix*
      (standard-method-init-spec 3 'total)
      (primary-method-spec 'total (λ (_cnm _self a b) (+ a b))))))

(expect (smc-obj-sum 'total 3 4) => 7
        (smc-obj-sum 'total 0 0) => 0)

;; Before/after run for side-effects; call-next-method = abort (must not be called)
(define smc-log '())
(def smc-obj-logged
  (fix-record
    (mix*
      (standard-method-init-spec 2 'op)
      (primary-method-spec 'op (λ (_c _s x) (* x x)))
      (before-method-spec  'op (λ (_c _s x)
                                 (set! smc-log (cons (list 'before x) smc-log))))
      (after-method-spec   'op (λ (_c _s x)
                                 (set! smc-log (cons (list 'after x) smc-log)))))))

(expect (smc-obj-logged 'op 4) => 16)
(expect smc-log => '((after 4) (before 4)))

;; Empty sub-methods (#f record): standard-compute-effective-method doesn't crash
;; on the missing qualifiers — every qualifier reads as empty and it falls through to
;; no-applicable-method rather than erroring on (#f 'around).
(expect ((let ((cc (default-convention 2)))
           (standard-compute-effective-method 'op #f (cc 'accepter) (cc 'method-invoker)))
         '(recv 4)) =>fail!)

;;;; 9.2.3 Simple Method Combination

;; simple-compute-effective-method :
;;   Name → Stop? → Op0 → Op1 → Op2 → Order → Finish → SubMethods → Accepter → MethodInvoker → EffectiveMethod
;;   Name   = Symbol  (the qualifier for the sub-method list)
;;   Stop?  = Result → Bool        (short-circuit: stop folding when true)
;;   Op0    = #f → Result          (result when no methods; takes dummy arg)
;;   Op1    = Result → Acc         (transforms first method result into initial accumulator)
;;   Op2    = Result → Acc → Acc   (fold step; curried, result-first to match the
;;                                  Scheme foldl at hand)
;;   Order  = 'most-specific-first | 'most-specific-last  (CLOS's :most-specific-…
;;                                  option; the order methods are evaluated in)
;;   Finish = Acc → Result         (post-process the accumulator, e.g. reverse for
;;                                  `list`, which builds its accumulator by cons)
;;
;; Common Lisp's built-in short-form combinations are all :most-specific-first,
;; and its such is also the default for user-defined short-form combinations.
;; The effective method is (operator (most-specific …) … (least-specific …)) and
;; `operator` evaluates its arguments left to right — but a user-defined one may
;; ask for :most-specific-last, which flips the evaluation (and, via the same
;; Finish, the result) to least-specific-first.
;;
;; Simple MethodFn = same contract as a standard method (see 9.2.2): its own
;;   curried lambda list, applied by `method-invoker`.  `run` invokes each with
;;   `abort` as cnm (a simple primary method must not call-next-method; an
;;   `around` one calls it to reach the folded inner value).  `around` too.
(def (simple-compute-effective-method
       name stop? op0 op1 op2 order finish sub-methods accepter method-invoker)
  (let* ((sub     (lambda (qualifier) (and sub-methods (sub-methods qualifier))))
         (arounds (sub 'around))          ;; #f (⇒ empty) if never initialized
         (methods (or (sub name) '()))    ;; most-specific-first, as stored
         (ordered (case order
                    ((most-specific-first) methods)
                    ((most-specific-last)  (reverse methods)))))
   (call-chain arounds
    (λ (args)
      (letrec ((run (λ (m) (method-invoker m (cons abort args))))
               (f   (lambda (acc lst)
                      (if (and (not (stop? acc)) (pair? lst))
                        (let ((v (op2 (run (car lst)) acc)))
                          (if (stop? v) v (f v (cdr lst))))
                        acc))))
        (finish
          (if (pair? ordered)
            (f (op1 (run (car ordered))) (cdr ordered))
            (op0 #f)))))
    accepter method-invoker)))

(def compute-effective-method/progn
  (simple-compute-effective-method
    'progn (λ (_) #f) (λ (_) #f) (λ (x) x) (λ (r _) r) 'most-specific-first identity))

(def compute-effective-method/and
  (simple-compute-effective-method
    'and not (λ (_) #t) (λ (x) x) (λ (r _) r) 'most-specific-first identity))

(def compute-effective-method/+
  (simple-compute-effective-method
    '+ (λ (_) #f) (λ (_) 0) (λ (x) x) (λ (x y) (+ x y)) 'most-specific-first identity))

(def compute-effective-method/*
  (simple-compute-effective-method
    '* (λ (_) #f) (λ (_) 1) (λ (x) x) (λ (x y) (* x y)) 'most-specific-first identity))

(def compute-effective-method/list
  (simple-compute-effective-method
    'list (λ (_) #f) (λ (_) '()) (λ (x) (list x)) (λ (x y) (cons x y))
    'most-specific-first reverse))

;; simple-method-init-spec : ConventionDesignator → ComputeEffectiveMethod → Name → MethodId → ModExt
;;   Like standard-method-init-spec but for a simple (operator) combination `cem`
;;   named `cname`: accepter primed with the receiver, method-invoker threaded through.
(def (simple-method-init-spec convention cem cname method-id)
  (let* ((cc             (as-calling-convention convention))
         (accepter       (cc 'accepter))
         (method-invoker (cc 'method-invoker)))
    (mix
      (field-spec method-id
        (λ (_inherited self)
          (@ accepter
             (λ (args)
               ((cem (self-sub-methods self method-id) accepter method-invoker) args))
             (list self))))
      (method-combination-init-spec method-id (simple-method-combination-init cname)))))

;; list-method-init-spec : ConventionDesignator → MethodId → ModExt
;; Initializes method-id to collect contributions from all methods into a list,
;; most-specific first.
(def (list-method-init-spec convention method-id)
  (simple-method-init-spec convention compute-effective-method/list 'list method-id))

;; list-method-spec : MethodId → MethodFn → ModExt  (qualifier = 'list)
(def list-method-spec (standard-sub-method-spec 'list))

;; Tests for simple method combination (list).  Convention 2 = receiver + one
;; argument; a method is (λ (cnm self x) ...) — here it labels its part with x.
(def list-parts-obj
  (fix-record
    (mix*
      (list-method-init-spec 2 'parts)
      (list-method-spec 'parts (λ (_cnm _self size) (list 'wheel size)))
      (list-method-spec 'parts (λ (_cnm _self size) (list 'engine size))))))

;; most-specific-first evaluation (as in CL), most-specific-first result list
;; engine was added last (most specific) → runs first, appears first in result
(expect ((list-parts-obj 'parts) 'big) => '((engine big) (wheel big)))

;; + combination via simple-method-init-spec — each method scales its argument.
(def sum-obj
  (fix-record
    (mix*
      (simple-method-init-spec 2 compute-effective-method/+ '+ 'total)
      (standard-sub-method-spec '+ 'total (λ (_cnm _self n) (* n 3)))
      (standard-sub-method-spec '+ 'total (λ (_cnm _self n) (* n 4))))))

(expect ((sum-obj 'total) 2) => 14)

;;; More tests for simple-compute-effective-method: the base case (no methods →
;;; op0), the single-method seed, the fold (op2), receiver access, short-circuit
;;; (stop?), repeatability, that the argument reaches every sub-method, and around
;;; sub-methods.  `simple-cem-obj` takes a convention designator, a combination,
;;; its name, then sub-method-specs, and returns the effective method (for
;;; convention 1 that is already the folded value; for 2, apply it to one arg).
(define (simple-cem-obj convention cem cname . sub-method-specs)
  ((fix-record
     (mix* (simple-method-init-spec convention cem cname 'm)
           (mix/list sub-method-specs)))
   'm))
(def (simple-sub cname fn) (standard-sub-method-spec cname 'm fn))

;; Base case: no methods at all ⇒ op0's neutral element, for every combinator.
;; Convention 1 (receiver only) ⇒ (obj 'm) is already the value.
(expect
  (simple-cem-obj 1 compute-effective-method/list  'list)  => '()
  (simple-cem-obj 1 compute-effective-method/+     '+)     => 0
  (simple-cem-obj 1 compute-effective-method/*     '*)     => 1
  (simple-cem-obj 1 compute-effective-method/progn 'progn) => #f
  (simple-cem-obj 1 compute-effective-method/and   'and)   => #t)

;; #f sub-methods (no init-spec / no sub-method): the compute functions treat a
;; missing record — and any missing qualifier — as empty; no crash.
(expect (let ((cc (default-convention 1)))
          ((compute-effective-method/list #f (cc 'accepter) (cc 'method-invoker)) '(recv)))
        => '()
        (let ((cc (default-convention 1)))
          ((compute-effective-method/+ #f (cc 'accepter) (cc 'method-invoker)) '(recv)))
        => 0)

;; Single method, convention 2: the call argument reaches it.
(expect ((simple-cem-obj 2 compute-effective-method/list 'list
           (simple-sub 'list (λ (_c _s x) (* x 2)))) 5) => '(10)
        ((simple-cem-obj 2 compute-effective-method/+ '+
           (simple-sub '+ (λ (_c _s x) (+ x 1)))) 4) => 5)

;; Fold across several methods, evaluated most-specific-first; op2 combines, and
;; for /list the reversed accumulator is put back in order by Finish = reverse.
(expect ((simple-cem-obj 2 compute-effective-method/+ '+
           (simple-sub '+ (λ (_c _s x) (* x 2)))
           (simple-sub '+ (λ (_c _s x) (* x 3)))
           (simple-sub '+ (λ (_c _s x) (* x 4)))) 1) => 9
        ((simple-cem-obj 2 compute-effective-method/list 'list
           (simple-sub 'list (λ (_c _s k) (list 'a k)))
           (simple-sub 'list (λ (_c _s k) (list 'b k)))
           (simple-sub 'list (λ (_c _s k) (list 'c k)))) 0)
        => '((c 0) (b 0) (a 0)))

;; Evaluation order is most-specific-first for every simple combination, as in
;; CL's short-form combinations.  Three `list` methods (convention 1) log as they
;; run: `most` (last mix* arg ⇒ most specific) runs first and lands first.
(define lmc-log '())
(expect (simple-cem-obj 1 compute-effective-method/list 'list
          (simple-sub 'list (λ (_c _s) (set! lmc-log (cons 'least lmc-log)) 'least))
          (simple-sub 'list (λ (_c _s) (set! lmc-log (cons 'mid   lmc-log)) 'mid))
          (simple-sub 'list (λ (_c _s) (set! lmc-log (cons 'most  lmc-log)) 'most)))
        => '(most mid least))
(expect (reverse lmc-log) => '(most mid least))  ;; ran most-specific-first

;; The `order` argument (CLOS's :most-specific-last): a `list` combination that
;; runs least-specific-first both evaluates and lands its results that way.
(def compute-effective-method/list-msl
  (simple-compute-effective-method
    'list (λ (_) #f) (λ (_) '()) (λ (x) (list x)) (λ (x y) (cons x y))
    'most-specific-last reverse))
(define lmc-log2 '())
(expect (simple-cem-obj 1 compute-effective-method/list-msl 'list
          (simple-sub 'list (λ (_c _s) (set! lmc-log2 (cons 'least lmc-log2)) 'least))
          (simple-sub 'list (λ (_c _s) (set! lmc-log2 (cons 'mid   lmc-log2)) 'mid))
          (simple-sub 'list (λ (_c _s) (set! lmc-log2 (cons 'most  lmc-log2)) 'most)))
        => '(least mid most))
(expect (reverse lmc-log2) => '(least mid most))  ;; ran least-specific-first

;; A method that reads the receiver: `self` is the fixpoint object, so it can look
;; up its own fields.  21 * 2 + 21 * 2 = 84.
(expect (((fix-record
            (mix* (constant-field-spec 'x 21)
                  (simple-method-init-spec 2 compute-effective-method/+ '+ 'm)
                  (simple-sub '+ (λ (_cnm self k) (* k (self 'x))))
                  (simple-sub '+ (λ (_cnm self k) (* k (self 'x))))))
          'm)
         2)
        => 84)

;; short-circuit: /and stops at the first falsy result; later methods don't run.
(define scem-log '())
(expect
  (simple-cem-obj 1 compute-effective-method/and 'and
    (simple-sub 'and (λ (_c _s) (set! scem-log (cons 1 scem-log)) 'ok))
    (simple-sub 'and (λ (_c _s) (set! scem-log (cons 2 scem-log)) #f))
    (simple-sub 'and (λ (_c _s) (set! scem-log (cons 3 scem-log)) 'never)))
  => #f)
;; most-specific-first: 3 (last mix* arg) ran, then 2 returned #f and folding
;; stopped before 1.
(expect scem-log => '(2 3))

;; call-next-method inside a simple (non-around) method is abort ⇒ it errors.
(expect (simple-cem-obj 1 compute-effective-method/+ '+
          (simple-sub '+ (λ (cnm _s) (cnm)))) =>fail!)

;; around sub-methods use the same contract; (call-next-method) yields the folded
;; inner result, or (call-next-method self v …) re-runs the chain with a fresh
;; argument list.  Outermost (most-specific) around is applied first.
(expect
  ((simple-cem-obj 2 compute-effective-method/list 'list
     (simple-sub 'list   (λ (_c _s x) (list 'a x)))
     (simple-sub 'around (λ (cnm _s _x) (cons 'x (cnm))))) 7) => '(x (a 7))
  ;; around wrapping the empty base case (convention 1).  DELIBERATELY unlike
  ;; CLOS, which errors on an around with no primary method: here op0 supplies
  ;; the neutral element, so (cnm) into an empty primary chain is well-defined.
  (simple-cem-obj 1 compute-effective-method/list 'list
    (simple-sub 'around (λ (cnm _s) (cons 'x (cnm))))) => '(x)
  ;; stacked arounds: x (more specific) outside y
  ((simple-cem-obj 2 compute-effective-method/list 'list
     (simple-sub 'list   (λ (_c _s x) (list 'a x)))
     (simple-sub 'around (λ (cnm _s _x) (cons 'y (cnm))))
     (simple-sub 'around (λ (cnm _s _x) (cons 'x (cnm))))) 1) => '(x y (a 1))
  ;; an around may replace the result outright without calling cnm
  ((simple-cem-obj 2 compute-effective-method/+ '+
     (simple-sub '+      (λ (_c _s x) (* x 100)))
     (simple-sub 'around (λ (_c _s _x) 7))) 3) => 7
  ;; an around may re-run the chain with a fresh argument list (receiver included)
  ((simple-cem-obj 2 compute-effective-method/+ '+
     (simple-sub '+      (λ (_c _s x) (* x 10)))
     (simple-sub 'around (λ (cnm self _x) (cnm self 5)))) 1) => 50)

;;;;; 9.3 Multiple Dispatch

;;;; 9.3.2 Double Dispatch and Visitor Pattern

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

  (def c (fix-record circle-dd))
  (def s (fix-record square-dd))
  (expect
   (c 'collide! c) => 'circle-circle
   (c 'collide! s) => 'circle-square
   (s 'collide! c) => 'square-circle
   (s 'collide! s) => 'square-square))

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

;;;; 9.3.4 Implementing Multiple Dispatch
;;
;; Automate the double dispatch / visitor pattern:
;; store partial method tables locally in each spec, backward-compatible with single dispatch.
;;
;; Table structure (parallel to sub-method-spec — a `multimethods` record nested
;; first by qualifier, then by one dispatch-class per generic-function argument):
;;   multimethods[qualifier][class-of-arg-1]...[class-of-arg-N] = method-fn
;;
;; apply-generic-function walks that nesting along each argument's C4 precedence
;; list (poi-precedence-list-with-top), collecting every applicable method-fn.
;; method-fn calling convention: whatever the generic function's convention says —
;; with the DEFAULT (curried) convention it is CallNextMethod → arg-1 → ... →
;; arg-N → result, the same shape as constant-spec, so (constant-spec v) works
;; for constants; another convention bundles the arguments differently.
;;
;; SPECIALIZER PROJECTION: which POI a dispatch argument is linearized on.  The
;; default `identity` matches prototype-style specializers (dispatch on the
;; argument POI itself); `type-of` matches class-style specializers (dispatch on
;; the argument's class).  A per-position list of projections is also accepted.
;; @TODO: eql / predicate specializers layer on this same hook (see book §MD).


(define (register-multimethod multimethods qualifier specializers method-fn)
  (@ (apply field-update~* qualifier specializers) (K method-fn) multimethods))

(def (register-multimethods new-multimethods multimethods)
  (foldl (lambda (n m) (apply register-multimethod m n)) multimethods new-multimethods))

(def (multimethods-spec new-multimethods super _self)
  (@ (field-update~* #f 'multimethods)
     (register-multimethods new-multimethods)
     super))


(def (poi-precedence-list-with-top poi)
  (append (poi-precedence-list poi) (list #t)))

;; dispatch-precedence-lists : Specializer → List(Arg) → List(PrecedenceList)
;;   Specializer is one projection (applied to every position) or a per-position list.
(define (dispatch-precedence-lists specializer dispatch-args)
  (if (procedure? specializer)
    (map (lambda (a) (poi-precedence-list-with-top (specializer a))) dispatch-args)
    (map (lambda (proj a) (poi-precedence-list-with-top (proj a))) specializer dispatch-args)))

(def (apply-generic-function
       arity accepter method-invoker compute-effective-method multimethods specializer initial-args)
  (accepter
    (λ (args)
      (let* ((pls (dispatch-precedence-lists specializer (take args arity)))
             (sub-methods
              ;; the raw multimethod fns applicable at this argument tuple, for a
              ;; given qualifier; compute-effective-method invokes them via the
              ;; method-invoker.  No receiver — a multiple-dispatch gf has none.
              (λ (qualifier)
               ((let loop ((mm (multimethods qualifier))
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
                '()))))
        (compute-effective-method sub-methods accepter method-invoker args)))
    initial-args))

;; Note that for a generic function to be both a function yet extensible with new multimethods,
;; it has to be both funcallable and modifiable.
;; The trick here is that the value #f is magically recognized as first argument;
;; a different value more “hidden” could be used; or the implementation could provide
;; “funcallable instances” that allow calling of an object without sacrificing one input value.
;; Or you could make generic functions not extensible, or hardwire a reference
;; to a separate object that you modify (in a pure context, those two references would mean
;; separate lenses to access each of the latest function and its the backing object).
;; Our arity management also means generic functions need at least one argument.
;;   specializer : one projection (default identity ⇒ prototype-style dispatch) or
;;   a per-dispatch-position list of projections (e.g. type-of ⇒ class-style).
(def (generic-function-spec calling-convention compute-effective-method multimethods specializer)
  (let ((calling-convention (as-calling-convention calling-convention)))
    (λ (super self x)
      (if (not x)
        (extend-record 'arity (calling-convention 'arity)
         (extend-record 'accepter (calling-convention 'accepter)
          (extend-record 'method-invoker (calling-convention 'method-invoker)
           (extend-record 'specializer specializer
            (extend-record 'compute-effective-method compute-effective-method
             (extend-record 'multimethods (register-multimethods multimethods empty-record)
              (super #f)))))))
        (let* ((spec (self #f))
               (arity (spec 'arity))
               (accepter (spec 'accepter))
               (method-invoker (spec 'method-invoker))
               (specializer (spec 'specializer))
               (compute-effective-method (spec 'compute-effective-method))
               (multimethods (spec 'multimethods)))
          (apply-generic-function
            arity accepter method-invoker compute-effective-method multimethods
            specializer (list x)))))))

(let ()
  (defpoi Shape)
  (defpoi Lozenge :e (constant-field-spec 'type 'lozenge) :p Shape)
  (defpoi Rectangle :e (constant-field-spec 'type 'rectangle) :p Shape)
  (defpoi Square :e (constant-field-spec 'type 'square) :p Rectangle Lozenge)
  (defpoi known-ancestor-pairs
    :e (generic-function-spec
        2 compute-effective-method/list
        `((list (,Shape ,Shape) ,(constant-spec (K '(shape shape))))
          (list (,Rectangle ,Shape) ,(constant-spec (K '(rectangle shape))))
          (list (,Shape ,Rectangle) ,(constant-spec (K '(shape rectangle))))
          (list (,Lozenge ,Shape) ,(constant-spec (K '(lozenge shape))))
          (list (,Lozenge ,Lozenge) ,(constant-spec (K '(lozenge lozenge))))
          (list (,Shape ,Lozenge) ,(constant-spec (K '(shape lozenge))))
          (list (,Rectangle ,Rectangle) ,(constant-spec (K '(rectangle rectangle))))
          (list (,Square ,Square) ,(constant-spec (K '(square square)))))
        identity))  ;; prototype-style: dispatch on the argument POI itself
  (expect
    (Shape 'type) => #f
    (Rectangle 'type) => 'rectangle
    (Lozenge 'type) => 'lozenge
    (Square 'type) => 'square

    (known-ancestor-pairs Shape Shape)
    => '((shape shape))

    ;; rectangle x rectangle:
    (known-ancestor-pairs Rectangle Rectangle)
    => '((rectangle rectangle) (rectangle shape) (shape rectangle) (shape shape))

    ;; lozenge x lozenge: lozenge-lozenge + shape-shape
    (known-ancestor-pairs Lozenge Lozenge)
    => '((lozenge lozenge) (lozenge shape) (shape lozenge) (shape shape))

    ;; square x square: square inherits rectangle and lozenge
    (known-ancestor-pairs Square Square)
    => '((square square)
         (rectangle rectangle) (rectangle shape)
         (lozenge lozenge) (lozenge shape)
         (shape rectangle) (shape lozenge) (shape shape))

    ;; rectangle x shape
    (known-ancestor-pairs Rectangle Shape)
    => '((rectangle shape) (shape shape))

    ;; square x rectangle
    (known-ancestor-pairs Square Rectangle)
    => '((rectangle rectangle) (rectangle shape)
         (lozenge shape) (shape rectangle) (shape shape))

    ;; square x shape: all applicable pairs
    (known-ancestor-pairs Square Shape)
    => '((rectangle shape) (lozenge shape) (shape shape))))

;; multimethods-spec + the `self #f` trick: a descendant generic function whose
;; :e adds a multimethod, running through the generic-function-spec code it
;; INHERITS, still resolves against its OWN (augmented) table — because
;; apply-generic-function reads (self #f), not (super #f).
(let ()
  (defpoi mmA)
  (defpoi mmB :e idModExt :p mmA)
  (defpoi gf0
    :e (generic-function-spec 1 compute-effective-method/list
         `((list (,mmA) ,(constant-spec 'a))) identity))   ;; arity 1 ⇒ no K needed
  (defpoi gf1
    :e (multimethods-spec `((list (,mmB) ,(constant-spec 'b))))
    :p gf0)
  (expect
    (gf0 mmB) => '(a)          ;; only the mmA method is applicable
    (gf1 mmB) => '(b a)))      ;; gf1's own mmB method, then the inherited mmA one

;; Specializer projection: prototype-style (identity) dispatches on the argument
;; POI itself; class-style (type-of) dispatches on the argument's class, so a
;; multimethod keyed on a class POI matches that class's instances.
(let ()
  (defclass Pt :e (mix* (simple-instance-field-spec 'x (constant-spec 0))
                        (simple-instance-field-spec 'y (constant-spec 0))))
  (defpoi where-class
    :e (generic-function-spec 1 compute-effective-method/list
         `((list (,Pt) ,(constant-spec 'point))) type-of))
  (defpoi where-proto
    :e (generic-function-spec 1 compute-effective-method/list
         `((list (,Pt) ,(constant-spec 'point))) identity))
  (def p (make-instance Pt))
  (expect
    ;; an instance's own precedence list does NOT contain its class POI …
    (memq Pt (poi-precedence-list p)) => #f
    ;; … so prototype-style dispatch on the instance finds nothing:
    (where-proto p) => '()
    ;; class-style projects the instance to its class first, and matches:
    (where-class p) => '(point)
    (where-class (make-instance Pt 'x 3)) => '(point)))


;;;;; 10 Efficient Object Implementation

;;;; 10.1.2 Records as Finite Maps

;; TODO: records as alists

;; TODO: records as weight-balanced trees ? trie-based hashmap ?

;;;; 10.1.3 Records as Records

;; TODO: mapping from name to offset; global cache; per-class/per-site inline caches; etc.

;; TODO: perfect hashing from name to offset


;;;; 10.2.1 Where did the Fixpoint Go?

;; Creating a suspension, extracting its outcome (implemented with Scheme's delay/force)
;; suspend : (→ V) → (Suspension V)
;; outcome : (Suspension V) → V
(define-syntax suspend (syntax-rules () ((_ expr) (delay expr))))
(def outcome force)

;; Optional short syntax:
(define-syntax ^ (syntax-rules ()
                   ((_ expr) (suspend expr))
                   ((_ e1 e2 e3 ...) (suspend (e1 e2 e3 ...)))))
(def ! outcome)
(define-syntax ^! (syntax-rules () ((_ expr ...) (^ (! expr ...)))))

;;;; 10.2.2 Suspended Records or Records of Suspensions

;; Arguments and results suspended representation, matches lazy languages.
;; Simple naming convention inspired by "hungarian notation":
;; first symbol after variable name: v^ means it contains a suspension, v! means a value
;; second symbols after variable name: v!^ v^^ mean it takes a suspension as argument, v!! v^! a value
;; further symbols after variable name: suspensions or values for more arguments.
;; first symbol before variable name: ^v means it returns a suspension, !v means it returns a value
;; Thus, v! is a simple value, v^ a simple suspension,
;; ^v!^^ is a value for function that takes two suspensions and returns one suspension.

;; Y combinator for a "return suspensions by default" representation
(def (^Y!! ^f!^) (letrec ((p^ (^f!^ (^! p^)))) p^))
(def (^Y2!! ^f!^) ((λ (^h!^) (^h!^ (^ ^h!^))) (λ (^g^^) (^f!^ (^! (! ^g^^ ^g^^))))))

;; Y combinator for a representation where arguments are suspended but results are values.
(def (!Y!! !f!^) (letrec ((p^ (^ !f!^ p^))) (! p^)))
(def (!Y2!! !f!^) ((λ (!h!^) (!h!^ (^ !h!^))) (λ (^g^^) (!f!^ (^! ^g^^ ^g^^)))))

(def (^fix!^! base^ ^spec!^^) (^Y!! (^spec!^^ base^)))

;; Suspended mirror of (mix p c) = (λ (t s) (c (p t s) s)). The modexts already return a
;; suspended record, so call them directly — an earlier `(^! …)` here re-suspended the
;; parent's result, handing the child a promise where it expected a record (Gambit's `force`
;; chains through and hides it; Racket's does not).
(def (^mix!!! ^parent!^^ ^child!^^ super^ self^)
  (^child!^^ (^parent!^^ super^ self^) self^))

;; !extend-record!!!! : value-returning, value-arg form of extend-record (which already
;; returns a plain record-function and takes plain values) — just the naming-convention alias.
(def !extend-record!!!! extend-record)

(def (^field-spec!! field-id! !fun!^^ super^ self^)
  (^ !extend-record!!!! field-id! (!fun!^^ super^ self^) (! super^)))

;;; Tests for §10.2.1 (suspensions) and §10.2.2 (suspended combinators + records).
;; All `def`s first, then one `expect` (Racket bodies want definitions before expressions).
(let ()
  ;; fact-gen returns a VALUE — a generator for the !-combinators (!Y!! / !Y2!!).
  (def fact-gen (λ (self^ n) (if (= n 0) 1 (* n (! self^ (- n 1))))))
  ;; ^fact-gen returns a SUSPENSION — a generator for the ^-combinators (^Y!! / ^Y2!!),
  ;; which return suspensions iff their generator does.
  (def ^fact-gen (λ (^self) (^ (λ (n) (if (= n 0) 1 (* n (! ^self (- n 1))))))))
  (def base^ (^ empty-record))
  (def px^ (^fix!^! base^ (^field-spec!! 'x (λ (_s^ _f^) 2))))
  (def pxy^ (^fix!^! base^ (^mix!!! (^field-spec!! 'x (λ (_s^ _f^) 2))
                                    (^field-spec!! 'y (λ (_s^ _f^) 4)))))
  (def chained^ (^fix!^! base^ (^mix!!! (^field-spec!! 'x (λ (_s^ _f^) 10))
                                        (^field-spec!! 'x (λ (super^ _f^) (+ 1 (! super^ 'x)))))))
  ;; TODO: a self-referential field — one whose compute-value reads (! self^ 'other) — cannot
  ;;   be tested here yet: it forces p^ while p^ is still being forced (Gambit loops, Racket
  ;;   raises "reentrant promise"). The non-suspended `Yes` avoids this because `(η p)` is a
  ;;   function and a finished record is a value; a suspended-record fixpoint needs an
  ;;   η-style self guard or the "records of suspensions" representation (key → Suspension).
  (expect
   ;; §10.2.1 — ^ suspends, ! forces; ^ is lazy; ^! forces its operand then re-suspends the
   ;; call. `^!`'s operand must itself be a suspension (its whole point) — passing a bare
   ;; procedure means `!`/force sees a non-promise, which Gambit passes through but Chez
   ;; forces as a 0-arg thunk.
   (! (^ 42)) => 42
   (! (^ (+ 1 2))) => 3
   (begin (^ (error "must not run")) 'not-forced) => 'not-forced
   (! (^! (^ (λ (a b) (+ a b))) 2 3)) => 5
   ;; §10.2.2 — suspended fixpoint combinators (factorial). ^-combinators return a
   ;; suspension, so force it; !-combinators return the value directly.
   (! (^Y!!  ^fact-gen) 5) => 120
   (! (^Y2!! ^fact-gen) 5) => 120
   (!Y!!  fact-gen 5) => 120
   (!Y2!! fact-gen 5) => 120
   ;; §10.2.2 — a single suspended field-spec, fixed
   (! px^ 'x) => 2
   (! px^ 'y) => #f
   ;; §10.2.2 — ^mix!!! of two independent specs
   (! pxy^ 'x) => 2
   (! pxy^ 'y) => 4
   (! pxy^ 'z) => #f
   ;; §10.2.2 — ^mix!!! chaining: child spec reads super
   (! chained^ 'x) => 11))

#|
The End. (For Now)
|#

#|
p1 = { a: Int , ...}
p2 = { b: String, ... }
p1∩p2 = {a : Int, b : String , ... }
|#
