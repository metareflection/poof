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
       (define-syntax @tmp (syntax-rules () ((_ . a) (@ tmp . a))))
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

(def (B x y z)
  (x (y z)))
(def (applicative-D x y)
  ((x x) y)) ;; same as (x x y)
(def (applicative-Y f)
  (applicative-D (B f applicative-D)))
(def (applicative-Y-expanded f)
  ((λ (x y) (x x y))
   (λ (x) (f (λ (y) (x x y))))))

(def (stateful-Y f)
  (letrec ((p (f (λ (y) (p y))))) p))

(def Y stateful-Y)

;; lazy convention: arguments are delayed, results are forced
;; TODO: add types for these and for other variants. ^ X = delayed X
(def (lazy-Y f) ;; : (^X→X)→X
  (letrec ((p (f (delay p)))) p))
(def (lazy-B f g x) ;; : (^Y→X)→^(Z→Y)→Z→X
  (f (delay ((force g) x))))
(def (lazy-D x) ;; : µX.^(X→A)→A
  ((force x) x))
(def (lazy-Y-with-combinators f) ;; : ^(^X→X)→X
  (lazy-D (delay (lazy-B f (delay lazy-D)))))
(def (lazy-Y-expanded f) ;; : ^(^X→X)→X
  ((λ (x) ((force x) x))
   (delay (λ (x) (f (delay ((force x) x)))))))

;; TODO: would this work? with what type?
;; (def (Y^ f) (letrec ((x (delay (f x)))) x))

;; Compute Factorial 6 with Y
(def (eager-pre-fact f n)
  (if (<= n 1) n (* n (f (- n 1)))))
(def lazy-pre-fact (λ (f n)
  (if (<= n 1) n (* n ((force f) (- n 1))))))

(expect ((applicative-Y eager-pre-fact) 6) => 720
        ((applicative-Y-expanded eager-pre-fact) 6) => 720
        ((stateful-Y eager-pre-fact) 6) => 720
        ((lazy-Y lazy-pre-fact) 6) => 720
        ((lazy-Y-with-combinators lazy-pre-fact) 6) => 720
        ((lazy-Y-expanded lazy-pre-fact) 6) => 720)

;;; Poor man's implementation of lazy as a function that always returns the results
;;; of the first successful evaluation.
;;; Tries to survive escaping continuations by consistently returning the first successful result.
;;; Not remotely thread safe though.
(define (make-once thunk)
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
    ((_ body ...) (make-once (lambda () body ...)))))

(define (! x) (x)) ;; force, even if from def or λ
(def (once-Y f) ;; : ^(^X→X)→X
  (letrec ((p (f (once p)))) p))
(def (once-B x y z) ;; : (^Y→X)→^(Z→Y)→Z→X
  (x (once ((! y) z))))
(def (once-D x) ;; : µX.^(X→A)→A
  ((! x) x))
(def (once-Y-with-combinators f) ;; : ^(^X→X)→X
  (once-D (once (once-B f (once once-D)))))
(def (once-Y-expanded f) ;; : ^(^X→X)→X
  ((lambda (x) ((x) x))
   (once (λ (x) (f (once ((! x) x)))))))

(def once-pre-fact (λ (f n)
  (if (<= n 1) n (* n ((! f) (- n 1))))))

(expect
 ((once-Y once-pre-fact) 6) => 720
 ((once-Y-with-combinators once-pre-fact) 6) => 720
 ((once-Y-expanded once-pre-fact) 6) => 720)

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
(def (mix c p s t)
  (c s (p s t)))

(def (idModExt _s)
  identity) ;; neutral element for mix

;;; 5.3.3 Closing Modular Extensions
(def (fix t m)
  (Y (λ (s) (m s t))))

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
(def (op-super-spec op self super)
  (op super))

(expect
  (fix 4 (mix*)) => 4
  (fix 4 (mix* (op-super-spec add1) (op-super-spec add1))) => 6
  (fix 4 (apply mix* (map op-super-spec (list mul10 add1 add1)))) => 60
  (fix 4 (apply mix* (map op-super-spec (list add1 mul10 add1 add1)))) => 61)

;;; 5.3.4 Default and non-default Top Type
(def fixt (fix top))

(def (fixt/inlined m)
  (Y (λ (s) (m s top))))

(def (record-spec _self _super)
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
(def (record!-spec _self super)
  (or super empty-record))

(def fix-record (fix empty-record))
(def (fix-record/inlined m)
  (Y (λ (s) (m s empty-record))))
(def (fix-record/fixt m)
  (fixt (mix m record-spec)))

;;; 5.3.5 Minimal OO Indeed
(def (field-spec key compute-value self super method-id)
  (let ((inherited (super method-id)))
    (if (equal? key method-id)
        (compute-value self inherited)
        inherited)))

;;; 5.3.6 Minimal Colored Point
(def coord-spec
  (mix* (field-spec 'x (λ (_self _inherited) 2))
        (field-spec 'y (λ (_self _inherited) 4))))

(def color-spec
  (field-spec 'color (λ (_self _inherited) "blue")))

(def point-p (fix-record (mix* color-spec coord-spec)))

(expect (point-p 'x) => 2
        (point-p 'color) => "blue"
        (map point-p '(x y z color)) => '(2 4 #f "blue")
        (map (fix-record/inlined ((mix color-spec) coord-spec)) '(x y z color)) => '(2 4 #f "blue")
        (map (fix-record/fixt ((mix color-spec) coord-spec)) '(x y z color)) => '(2 4 #f "blue"))

(def (constant-spec value _self _super)
  value)
(def (constant-field-spec key value)
  (field-spec key (constant-spec value)))

;;; 5.3.7 Minimal Extensibility and Modularity Examples
(def (add-x-spec dx)
  (field-spec 'x (λ (_self inherited) (+ dx inherited))))

(def (sqr x)
  (* x x))

(def area-spec
  (field-spec 'area (λ (self _inherited)
    (* (self 'x)) (self 'y))))

(def rho-spec
  (field-spec 'rho (λ (self _inherited)
    (sqrt (+ (sqr (self 'x)) (sqr (self 'y)))))))

(def point-r
  (fix-record (mix* (add-x-spec 1) coord-spec rho-spec)))

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
  (stateful-Y (λ (self) (my-contents-spec self (my-modular-def self)))))

(expect (my-contents 'contents) => my-saying
        (my-contents 'length my-saying) => 20
        (my-contents 'size) => 15)

;; TODO: don't use _ here
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
  (stateful-Y (λ (self) (my-contents-spec self (my-modular-def-without-global-recursion self)))))

(expect (my-contents-2 'contents) => my-saying
        (my-contents-2 'length my-saying) => 20
        (my-contents-2 'size) => 15)

(def (base-bill-of-parts self super method-id)
  (case method-id
    ((parts) '())
    ((part-count) (length (self 'parts)))
    (else (super method-id))))

(def (part-spec part)
  (field-spec 'parts (λ (_self inherited) (cons part inherited))))

(def torso-spec (part-spec 'torso))
(def head-spec (part-spec 'head))
(def arms-spec (part-spec 'arms))
(def legs-spec (part-spec 'legs))

(def body-rec (fix-record (mix* head-spec arms-spec legs-spec torso-spec base-bill-of-parts)))

(expect (map body-rec '(parts part-count)) => '((head arms legs torso) 4))

;;;;; 6 Rebuilding OO from its Minimal Core

;;;; 6.1.2 Conflation: Crouching Typecast, Hidden Product

(def (pproto←spec spec)
  (cons spec (delay (fix-record spec))))
(def spec←pproto car)
(def (target←pproto pproto)
  (force (cdr pproto)))
(def pproto-id (pproto←spec idModExt))
(def (pproto-mix child parent)
  (pproto←spec (mix (spec←pproto child) (spec←pproto parent))))
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

(def point-r-pproto (pproto-mix* (add-x-pproto 1) coord-pproto rho-pproto))
(def point-rc-pproto (pproto-mix* color-pproto point-r-pproto))

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
(def (qproto-wrapper spec _self super)
  (cons spec super))
(def (qproto←spec spec)
  (delay (fix-record (mix (qproto-wrapper spec) spec))))
(def spec←qproto car)
(def (target←qproto qproto)
  (force (cdr qproto)))
(def (qproto-mix child parent)
  (qproto←spec (mix (spec←qproto child) (spec←qproto parent))))
(define (qproto-mix* . l) (foldl (uncurry2 qproto-mix) pproto-id l))


;;;; 6.1.4 Conflation for Records

(def (rproto-wrapper spec self super method-id)
  (if method-id (super method-id) spec))
(def (rproto←spec spec)
  (fix-record (mix (rproto-wrapper spec) spec)))
(def rproto-id (rproto←spec idModExt))
(def (spec←rproto rproto)
  (rproto #f))
(def target←rproto identity)
(def (rproto-mix child parent)
  (rproto←spec (mix (spec←rproto child) (spec←rproto parent))))
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

(def point-r-rproto (rproto-mix* (add-x-rproto 1) coord-rproto rho-rproto))
(def point-rc-rproto (rproto-mix* color-rproto point-r-rproto))

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
  (mext self (parent self)))
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

;;;;; 8 Extending the Scope of OO

;;;; 8.1.2 Short Recap on Lenses

;; type View r s = s → r
;; type Update i p j q = (i → p) → j → q
;; type SkewLens r i p s j q = { view : View r s ; update : Update i p j q }

;;; Composing Lenses
;; composeView : View s t → View r s → View r t
(def (composeView v w)
  (compose w v))

;; composeUpdate : Update i p j q → Update j q k r → Update i p k r
(def (composeUpdate f g)
  (compose f g))

;; makeLens : View r s → Update i p j q → SkewLens r i p s j q
(def (makeLens v u)
  (extend-record 'view v
    (extend-record 'update u
      empty-record)))

;; composeLens : SkewLens s j q ss jj qq → SkewLens r i p s j q →
;;                SkewLens r i p ss jj qq
(def (composeLens l k)
  (makeLens
    (composeView (l 'view) (k 'view))
    (composeUpdate (l 'update) (k 'update))))

;; idLens : SkewLens r i p r i p
(def idLens
  (makeLens identity identity))

(define composeLens* (op*←op1.1 composeLens idLens))

;;; Getter and Setter (moved after makeLens)
(def (lensOfGetterSetter get set)
  (makeLens get (λ (f s) (set (f (get s)) s))))
(def (setterOfLens l)
  (λ (b) (@ l 'update (λ (_a) b))))

;;; Field Lens
(def (fieldView key r)
  (r key))
(def (fieldUpdate key f r)
  (extend-record key (f (r key)) r))
(def (fieldLens key)
  (makeLens (fieldView key) (fieldUpdate key)))

(define (fieldLens* . keys)
  (apply composeLens* (map fieldLens keys)))

(def test-rec (record (a (record (b (record (c 42)))))))
(def test-point (record (x 10) (y 20)))
(def x-lens (fieldLens 'x))
(def set-x (setterOfLens x-lens))
(def x-lens-2 (lensOfGetterSetter (fieldView 'x) (extend-record 'x)))
(expect
  (@ (composeLens*) 'view test-rec) => test-rec
  (@ (fieldLens* 'a 'b 'c) 'view test-rec) => 42
  (@ (fieldLens*) 'view test-rec) => test-rec
  (fieldView 'x test-point) => 10
  (fieldView 'y test-point) => 20
  (fieldLens 'x 'view test-point) => 10
  (fieldLens 'y 'view test-point) => 20
  (fieldLens 'x 'update add1 test-point 'x) => 11
  (fieldLens 'x 'update mul10 test-point 'x) => 100
  (fieldLens 'x 'update mul10 test-point 'y) => 20  ;; y unchanged
  (idLens 'view test-point) => test-point
  (idLens 'update add1 5) => 6
  (composeLens (fieldLens 'a) (fieldLens 'b) 'view
    (record (a (record (b 99))))) => 99
  (set-x 999 test-point 'x) => 999
  (set-x 999 test-point 'y) => 20
  (x-lens-2 'view test-point) => 10
  (x-lens-2 'update add1 test-point 'x) => 11)

;;;; 8.1.3 Focusing a Modular Extension
;;; From Sick to Ripped
;; skewExt : SkewLens r i p s j q → ModExt r i p → ModExt s j q
(def (skewExt l m)
  (compose* (l 'update) m (l 'view)))

;;;; 8.1.4 Adjusting Context and Focus
;;; Adjusting the Extension Focus
;; updateOnlyLens : Update i p j q → SkewLens r i p r j q
(def (updateOnlyLens u)
  (makeLens identity u))

;; updateLens : SkewLens r i p s j q → Update j q jj qq → SkewLens r i p r jj qq
(def (updateLens l u)
  (makeLens (l 'view) (compose (l 'update) u)))

(def outer-rec (record (inner (record (val 5)))))
(def inner-val-lens (fieldLens* 'inner 'val))
(def (double-ext _self super) (* 2 super))
(def focused-ext (skewExt (updateOnlyLens (inner-val-lens 'update)) double-ext))
(expect
  (fix outer-rec focused-ext 'inner 'val) => 10)
(expect
  ;; updateOnlyLens: view is identity, update applies transformation
  (updateOnlyLens (compose mul10) 'view 7) => 7
  (updateOnlyLens (compose mul10) 'update add1 7) => 80)  ;; mul10 (add1 7)

;;; Broadening the Focus
;; reverseView : s → MonoLens s a → View a s
;; reverseUpdate : s → MonoLens s a → Update a s a s
;; reverseLens : s → MonoLens s a → MonoLens a s
(def (reverseView s l a)
  (setterOfLens l a s))
(def (reverseUpdate s l f a)
  (l 'view (f (reverseView s l a))))
(def (reverseLens s l)
  (makeLens (reverseView s l) (reverseUpdate s l)))

(def rev-x (reverseLens test-point x-lens))

(expect
  (x-lens 'view test-point) => 10
  (rev-x 'view 42 'x) => 42
  (rev-x 'view 42 'y) => 20 ;; y unchanged from test-point

  ;; update transforms the record, then extracts the value
  (rev-x 'update (fieldLens 'x 'update mul10) 10) => 100
  (rev-x 'update (fieldLens 'y 'update mul10) 10) => 10) ;; y unchanged from test-point

;;; Adjusting the Context
;; viewOnlyLens : View r s → SkewLens r i p s i p
(def (viewOnlyLens v)
  (makeLens v identity))

;; viewLens : SkewLens r i p s j q → View rr r → SkewLens rr i p r j q
(def (viewLens l v)
  (makeLens (composeView (l 'view) v) (l 'update)))

(expect
   ;; viewOnlyLens: view transforms, update is identity
  (viewOnlyLens mul10 'view 7) => 70
  (viewOnlyLens mul10 'update add1 7) => 8)

;;;; 8.1.5 Optics for Specifications, Prototypes and Classes

;;; Specification Methods
(def widget-shop
  (record (widgets (record (foo (record (x-pos 100) (y-pos 500)))))))
(expect
 (skewExt
  (updateLens (fieldLens* 'widgets 'foo) (fieldUpdate 'x-pos))
  (λ (_self super) (+ super 50))
  widget-shop
  widget-shop
  'widgets 'foo 'x-pos) => 150)

;;; Prototype Specification
(def rprotoSpecView spec←rproto)
(def rprotoSpecSetter rproto←spec)
(def rprotoSpecLens (lensOfGetterSetter rprotoSpecView rprotoSpecSetter))

#||#
#|
The End. (For Now)
|#

#|
p1 = { a: Int , ...}
p2 = { b: String, ... }
p1∩p2 = {a : Int, b : String , ... }
|#
