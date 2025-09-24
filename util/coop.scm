;;;; Categorical Object Oriented Prototypes -*- Gerbil -*-
;; Here "Categorical" means that we use unary functions everywhere,
;; with lots of Haskell-like combinators.

;; NB: For an example of the Prototypes at work, see protodoc.rkt,
;; itself used by ../slides-2023-njpls.rkt or ../slides-2024-lambdaconf.rkt

;;; 1. Categorical Functional Programming in Scheme

;;; 1.1. Macros for curried functions

;;; 1.1.1. The identity function
;; : x -> x
(define (identity x) x)

;; 1.1.2. Apply a curried function to a list of argument
(define (%app . a)
  (cond ((null? a) identity)
        ((null? (cdr a)) (car a))
        ((null? (cddr a)) ((car a) (cadr a)))
        (else (apply %app ((car a) (cadr a)) (cddr a)))))

(define-syntax %app/list
  (syntax-rules ()
    ((_) identity)
    ((_ fun) fun)
    ((_ fun arg) (fun arg))
    ((_ fun arg . more) (%app/list (fun arg) . more))))

;; This indirection into three definitions is necessary because
;; Gerbil Scheme and Racket (and presumably others) disagree on how to define an identifier macro
(define-identifier-macro @ %app %app/list)

;; 1.1.3. Mutually recursive fun and defn

;; Unicode lambda to define curried function

(define-syntax fn
  (syntax-rules ()
    ((_ () . body) (begin . body))
    ((_ (x) . body) (lambda (v) (defn x v) . body))
    ((_ (x . y) . body) (fn (x) (fn y . body)))
    ((_ v . body) (fn (v) . body)))) ;; also accept a single var without paren, same as with paren

;; defn
(define-syntax defn
  (syntax-rules ()
    ((_ (pat . vars) . body)
     (defn pat (fn vars . body)))
    ((_ v . body)
     (begin
       ;; Allow autocurrying self-reference in the definition body
       (define-syntax tmp/app (syntax-rules () ((_ . a) (%app/list tmp . a))))
       (define-identifier-macro v tmp tmp/app)
       (define tmp (let () . body))))))

(define-identifier-macro λ fn)
(define-identifier-macro def defn)

;;; 1.2. Basic combinators

;; S K I combinators https://www.johndcook.com/blog/2014/02/06/schonfinkel-combinators/
;; See also https://en.wikipedia.org/wiki/Combinatory_logic
;; https://writings.stephenwolfram.com/2020/12/where-did-combinators-come-from-hunting-the-story-of-moses-schonfinkel/

;; identity - Identitätsfunktion - Schönfinkel's I combinator
;; : x -> x
;(def (id x x))
(def id identity) ;; already defined above, but this time it's autocurrying!

;; constant - Konstanzfunktion - Schönfinkel's K combinator
;; : x -> y -> x
(def (ko x y) x)

;; verSchmelzungsfunktion: amalgamation function; Smelting - Schönfinkel's S combinator
;; : (z -> yz -> r) -> (z -> yz) -> z -> r
(def (sc x y z) (x z (y z)))

;; verTauschungsfunktion (exchange funcTion); flip in Haskell - Schönfinkel's T combinator
;; : (z -> y -> r) -> y -> z -> r
(def (ta x y z) (x z y))

;; Zusammensetzungsfunktion (compoZition function); (.) in Haskell; also B combinator
;; : (yz -> r) -> (z -> yz) -> z -> r
(def (zu x y z) (x (y z)))

;; Composition; (.) in Haskell
;; : (b -> c) -> (a -> b) -> a -> c
(def comp zu)

;; Composition in reverse order
;; : (z -> xz) -> (xz -> r) -> z -> r
(def (pmoc x y z) (y (x z)))

;; Applicative D combinator: protect the (x x) from over-eager evaluation with
;; eta-conversion with a layer of (λ (y) (... y)). Unnecessary for lazy D.
;; : (μ x . x -> y -> r) -> y -> r
(def (D x y) (x x y))

;; Purely functional definition of the Y combinator
;; Y combinator: return fixed-point x such that x = (f x).
;; Each recursive use of f will involve duplication of the term.
;; : (s -> x -> r) -> (μ s . s -> x -> r) ???
(def (pure-Y f) (D (comp f D)))

;; More efficient implementation of the same,
;; with *more* sharing and therefore fewer recomputations
;; by using the builtin self-reference in define (which is equivalent to a letrec)
;; such that the fixed-point itself is not duplicated.
(def (Y f)
  (define (fixed-point x) (f fixed-point x))
  fixed-point)

;;; 1.3. Macros and functions to call curried functions with n-ary Lisp syntax
;; TODO: the macro optimization is left as an exercise to the reader

(define (compose . l) ;; n-ary composition
  (cond
   ((null? l) id)
   ((null? (cdr l)) (car l))
   (else (comp (car l) (apply compose (cdr l))))))

(define (rcompose . l) ;; n-ary composition left-to-right flow
  (cond
   ((null? l) id)
   ((null? (cdr l)) (car l))
   (else (pmoc (car l) (apply rcompose (cdr l))))))

;;; 2. Functional Records
;; Let's define "records" as functions from message to value

;;; 2.1. Basic Record combinators

;; The top value, one from which you can extract no information
;; When called with a message, it bottoms with an error -- ⊤ = (λ (_) ⊥) !!!
;; Bottom is a computation that never returns a value.
;; : _ -> ⊥
(def (top msg) (error "method not found" msg))

;; The record constructor. A bit like acons, but for records.
;; : k -> v -> (k -> v) -> k -> v
(def (rcons key val record msg)
  (if (eq? msg key) val (record msg)))

;; Applicative variant of rcons that thunks the value to protect evaluation
;; : k -> (_ -> v) -> (k -> v) -> k -> v
#;
(def (rcons* key thunk record msg)
  (if (equal? key msg) (thunk '_) (record msg)))
;; Same but with promise for sharing vs recomputation
;; -- useful for performance, necessary to converge in cases of circularity
(def (rcons* key thunk)
  (def promise (delay (thunk '_)))
  (λ (record msg)
    (if (equal? key msg) (force promise) (record msg))))

;;; 2.2. n-ary functions and syntax for records

(define (make-record . l) ;; plist syntax for record
  (cond ((null? l) top)
        ((null? (cdr l)) (car l))
        (else (rcons (car l) (cadr l) (apply make-record (cddr l))))))

(define-syntax record
  (syntax-rules () ((_ . a) (record1 () . a))))
(define-syntax record1
  (syntax-rules ()
    ((_ ke) (record0 top . ke ))
    ((_ ke top) (record0 top . ke ))
    ((_ ke k e . m) (record1 ((k e) . ke) . m))))
(define-syntax record0
  (syntax-rules ()
    ((_ top (key expr ...) ...)
     (let ((key (delay (begin expr ...))) ...)
       (lambda (msg) (case msg ((key) (force key)) ... (else (top msg))))))))

;;; 3. Prototypes as Mixin Functions

;; type Proto self super = self <: super => self -> super -> self
;; type $Proto self = Proto self self

;;; 3.1. We can define our two magic functions this way:
;; : Proto self top -> top -> self
(def (fix p t) (Y (λ (s) (p s t)))) ;; instantiate a prototype with top value

;; : Proto self super -> Proto super s2 -> Proto self s2
(def (mix p q s t) (p s (q s t))) ;; compose prototypes p and q

;; : Proto self self
(def ($ix s t) t) ;; neutral prototype -- $ prefix for prototypes

;; With longer names, in Lispier style with implicit recursion rather than using Y.
;; : Proto self top -> top -> self
(def (instantiate proto top)
  (Y (lambda (self) (proto (λ (msg) (self msg)) top))))

;; : Proto self super -> Proto super s2 -> Proto self s2
(def (inherit child parent)
  (λ (self super)
    (child self (parent self super))))

;;; 3.2. Directly composable representation for prototype functions
;; type CProto self super = Proto super s2 -> Proto self s2
;; : Proto self super -> CProto self super
(def p->cp mix)

;; : CProto self super -> Proto self super
(def (cp->p cp) (cp $ix))

;; : CProto self top -> top -> self
(def (cfix cp t) (Y (λ (s) (cp $ix s t))))

;; : CProto self super -> CProto super s2 -> CProto self s2
(def cmix comp)

;; : CProto self self
(def cid id)

;;; 3.3. n-ary function for mixing prototypes
(define (mix* . ps)
  (cond ((null? ps) $ix)
        ((null? (cdr ps)) (car ps))
        (else (mix (car ps) (apply mix* (cdr ps))))))
(define (rmix* . ps) (apply mix* (reverse ps)))

;;; 4. Prototypes for Records
;; We adopt the convention of a $ prefix for prototype functions

;;; 4.1. Basic function to create a prototype for a record

;; self <: k -> v => k -> (self -> (_ -> v) -> v) -> Proto self super
(def ($method name body self super)
  (def (next-method _) (super name))
  (rcons* name (λ (_) (body self next-method)) super))

;;; Trivial variants thereof

;; self <: k -> v => k -> v -> Proto self super
(def ($method/const name value)
  ($method name (λ (_self _next-method) value)))

;; self <: k -> v => k -> ((_ -> v) -> v) -> Proto self super
(def ($method/next name f)
  ($method name (λ (_self next-method) (f next-method))))

;; self <: k -> v => k -> (self -> v) -> Proto self super
(def ($method/self name f)
  ($method name (λ (self _next-method) (f self))))

;; self <: k -> v => k -> v -> Proto self super
(def ($kv k v s t) (rcons k v t)) ;; indeed the same as $method/const

;; self <: k -> v => k -> (self -> (_ -> v) -> v) -> Proto self super
(def ($kb k b s t) (rcons* k (λ (_) (b s (λ (_) (t k)))) t)) ;; indeed the same as $method

;;; 4.2. n-ary functions and syntax to deal with record prototypes

;; Quickly instantiate a record from prototypes
(define (rfix . ps) (fix (apply mix* ps) top))

;; Multiple methods in one call
(define ($methods . l)
  (unless (even? (length l)) (error "odd length of $rec arguments" l))
  (λ (s t) (apply make-record (append l (list t)))))

;; Syntax for somewhat more efficient record prototypes
(define-syntax $record
  (syntax-rules () ((_ . a) ($record1 () . a))))
(define-syntax $record1
  (syntax-rules ()
    ((_ ke) ($record0 . ke ))
    ((_ ke k e . m) ($record1 ((k e) . ke) . m))))
(define-syntax $record0
  (syntax-rules ()
    ((_ (key expr ...) ...)
     (λ (self super) ;; TODO: a variant that exposes self, super
       (let ((key (delay (begin expr ...))) ...)
         (lambda (msg) (case msg ((key) (force key)) ... (else (super msg)))))))))

;;; 5. Lenses

;;; 5.1. Van Laarhoven composable lenses, with extra evaluation protection
;; type FMap s t a b = (a -> b) -> (f a -> f b)

;; Haskell: type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)
;; type Lens s t a b = forall f . ((a -> b) -> (f a -> f b)) -> (a -> f b) -> (s -> f t)
;; type SimpleLens s a = Lens s s a a

;; : (s -> a) -> (s -> b -> t) -> Lens s t a b
(def (lens getter setter fmap k x) (fmap (setter x) (k (getter x))))

;; : SimpleLens s a -> s -> a -> s
(def (set lens s a) (lens id (ko a) s))

;; : SimpleLens s a -> s -> a
(def (get lens s) (lens (ko id) id s))

;;; 5.2. Fields for records

;; Fields for eager records
;; : k -> (k -> v) -> v
(def (getField name record) (record name))

;; : k -> (k -> v) -> v -> k -> v
(def (setField name record value) (rcons name value record))

;; : k -> SimpleLens (k -> v) v
(def (field name) (lens getField setField))

;; Fields for records, with thunks rather than values

;; : k -> (k -> v) -> (_ -> v)
(def (getField* name record) (λ (_) (record name)))

;; : k -> (k -> v) -> (_ -> v) -> k -> v
(def (setField* name record thunk) (rcons* name thunk record))

;; : k -> SimpleLens (k -> v) (_ -> v)
(def (field* name) (lens getField* setField*))

;;; 5.3. Generalizing method definition using lenses

;; Generalizing $method.
;; The lens specifies which aspect of the self will be overridden
;; The body takes the global self and the inherited value at lens, and computes the new value at lens
;; self and super are the global self and super
;;
;; The specialized variant method definition for a record field is then just
;; plugging the field lens into the more general lens variant.
;; ($method name) == ($Method (field* name))
;; type Method self super mself msuper = self <: super => mself <: msuper => self -> msuper -> mself
;; : Lens self vself /\ Lens super vsuper -> Method self super mself msuper -> Proto self super
(def ($Method lens body self super)
  (set lens super (body self (get lens super))))

;; Mixing in a proto for a subvalue at lens
;; The lens specifies which aspect of the self will be overridden
;; The body takes the self aspect at lens and the inherited value at lens,
;; and computes the new value at lens
;; self and super are the global self and super, but will be all lensed in.
;; ($Mix lens subproto) == ($Method lens (λ (self) (subproto (get lens self))))
;; : Lens Global Local
;; : Lens self vself /\ Lens super vsuper -> Proto mself msuper -> Proto self super
(def ($Mix lens subproto self super)
  (set lens super (subproto (get lens self) (get lens super))))

;;; 6. Algebraic data structures

;; class Functor f where
;;    fmap :: (a -> b) -> f a -> f b
;;    (<$) :: a -> f b -> f a ;; (<$) = fmap . k

;; The Identity functor
(define Identity
  (record fmap id))

;; The Constant functor
(define Const
  (record fmap (ko id)))
