;;;;;; pommette - a cheeky, barebones implementation of a meta-object protocol.
;;;;;; LtUO demonstration mini object systems written in R7RS Scheme
;;#lang lang/scheme/r7rs
#|
With Gerbil Scheme: gxi pommette.scm
With Racket (fails so far): raco pkg install r7rs ; racket -I r7rs --script pommette.scm
|#

;;; Flag for verbose execution:
(define verbose #t)

;;;;; Chapter 5: Minimal Object System
;;;; Prelude: general purpose utilities

;; Abbreviation for the whole book: Unicode λ instead of ASCII lambda.
#| This abbreviation also illustrates use of simple Scheme hygienic macros
   To learn more about Scheme syntax and semantics in general,
   please consult the R7RS-small, some Scheme tutorial,
   your implementation’s reference manual,
   or the docs.racket-lang.org website.
   Beware that various implementations have their own extensions and limitations.

   Semicolons are comments to the end of line.
   Blocks between #| ... |# are multi-line comments.
|#
(define-syntax λ
  (syntax-rules ()
    ((_ args . body)
     (lambda args . body))))

;; Expectations -- trivial test suite
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
(define-syntax expect
  (syntax-rules (=>)
    ((expect) #t)
    ((expect expr => result . r)
     (begin
       (check-expectation 'expr 'result (lambda () expr) (lambda () result))
       (expect . r)))))

(expect (+ 2 3) => 5
        (+ 20 3) => (+ 3 20)
        (* 6 7) => 42)

;; TODO: use cond-expand to define expect-failure


;; Let's not forget to minimally check the previously defined λ
(expect ((λ (x) (+ x 3)) 2) => 5)

;; Aborting
#| Note the use of variable-length arguments:
   The unparenthesized args variable catches all arguments;
   apply passes them at the end of the arguments to the function error. |#
(define abort (λ args (apply error "Aborting" args)))

#;(abort "foo") ;<== should fail the evaluation if uncommented


;;; 5.2.2 Records (moved ahead, because we use it in 5.1.2 already)
(define (empty-record _)
  #f)
(define extend-record (λ (key) (λ (value) (λ (rec) (λ (i)
  (if (equal? i key) value (rec i)))))))
(define record-ref (λ (key) (λ (rec)
  (rec key))))
(define-syntax record
  (syntax-rules ()
    ((record) empty-record)
    ((record (k v) . r) (((extend-record 'k) v) (record . r)))))

(expect (empty-record 'foo) => #f
        ((((extend-record 'foo) 1) empty-record) 'foo) => 1
        ((((extend-record 'foo) 1) empty-record) 'bar) => #f)

;;; 5.1.2 Coloring a point
(define point-a (record (x 2) (y 4))) ;; Using the syntax above
(define (paint-blue p) (((extend-record 'color) "blue") p)) ;; Using regular functions
(define p1 (paint-blue point-a))
(define p2 (record (x 2) (y 4) (color "blue")))

;; Not that 'x is a constant expression returning the symbol x,
;; as opposed to plain x which is an expression that dereferences variable x.
(expect (point-a 'x) => 2
        (point-a 'y) => 4
        (point-a 'z) => #f
        (point-a 'color) => #f
        ((record-ref 'x) point-a) => 2
        ((record-ref 'y) point-a) => 4)

;; To speed up those tests, we use map function point-p over various values
(expect (map point-a '(x y z color)) => '(2 4 #f #f)
        (map p1 '(x y z color)) => '(2 4 #f "blue")
        (map p2 '(x y z color)) => '(2 4 #f "blue"))

;;; 5.1.4
;; Simple function composition
(define compose (λ (ext1) (λ (ext2) (λ (val)
  (ext1 (ext2 val))))))
(define identity (λ (val) val))

;; Example of the short form syntax for defining functions without writing a lambda
(define (10* x) (* x 10))
(define (1+ x) (+ x 1))
(define (2- x) (- x 2))

;; Check that our composition works as expected:
(expect (((compose 10*) 1+) 4) => 50
        (((compose 1+) 10*) 4) => 41)

;; Generalizing compose to n-ary composition.
(define compose*
  (case-lambda
    (() identity)
    ((x) x)
    ((x y) ((compose x) y))
    ((x . r) ((compose x) (apply compose* r))))) ;; or (foldl compose* x r)

(expect
  ((compose*) 5) => 5
  ((compose* 1+) 99) => 100
  ((compose* 1+ 1+) 67) => 69
  ((compose* 1+ 1+ 1+) 20) => 23
  ((compose* 1+ 1+ 10*) 4) => 42
  ((compose* 1+ 10* 2-) 0) => -19)

;;; 5.1.5
(define top #f)

(define point-c
  (record (x 3) (y 4) (color "blue")))

(expect (map point-c '(x y z color)) => '(3 4 #f "blue"))

#;(define ls-sorted (λ (ctx) (compose* (ctx 'sort) (ctx 'ls))))

;;; Y combinator

(define B (λ (x) (λ (y) (λ (z)
  (x (y z))))))
(define applicative-D (λ (x) (λ (y)
  ((x x) y))))
(define applicative-Y (λ (f)
  (applicative-D ((B f) applicative-D))))
(define applicative-Y-expanded (λ (f)
  ((λ (x) (λ (y) ((x x) y)))
   (λ (x) (f (λ (y) ((x x) y)))))))

(define stateful-Y (λ (f)
  (letrec ((p (f (λ (y) (p y))))) p)))

(define Y stateful-Y)

(define lazy-Y (λ (f)
  (letrec ((p ((force f) (delay p)))) p)))
(define lazy-B (λ (x) (λ (y) (λ (z)
  ((force x) (delay ((force y) z)))))))
(define lazy-D (λ (x)
  ((force x) x)))
(define lazy-Y-with-combinators (λ (f)
  (lazy-D (delay ((lazy-B f) (delay lazy-D))))))
(define lazy-Y-expanded (λ (f)
  ((λ (x) ((force x) x))
   (delay (λ (x) ((force f) (delay ((force x) x))))))))

;; Compute Factorial 6 with Y
(define eager-pre-fact (λ (f) (λ (n)
  (if (<= n 1) n (* n (f (1- n)))))))
(define lazy-pre-fact (delay (λ (f) (λ (n)
  (if (<= n 1) n (* n ((force f) (1- n))))))))

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
(define once (λ (fun)
  (let ((computed? #f)
        (value #f))
    (λ args
      (or computed?
          (let ((result (apply fun args)))
            (or computed?
                (begin
                  (set! computed? #t)
                  (set! value result)))))
      value))))
;; Trivial implementation of lazy from once
;;(define-syntax delay (syntax-rules () ((_ . body) (once (λ () . body)))))
;;(define force (λ (p) (p)))
;;(define once-thunk (λ (thunk) (lazy (thunk)))) ;; only for nullary thunks
(define foo (once 1+))
(expect (foo 41) => 42
        (foo 4) => 42
        (foo) => 42
        (foo 1 2 3) => 42)

;;; 5.3.2 Composing Modular Extensions
(define mix (λ (c) (λ (p) (λ (s) (λ (t)
  ((c s) ((p s) t)))))))

(define idModExt (λ (s) (λ (t)
  t)))

;;; 5.3.3 Closing Modular Extensions
(define fix (λ (t) (λ (m)
  (Y (λ (s) ((m s) t))))))

(define mix*
  (case-lambda
    (() idModExt)
    ((x) x)
    ((x y) ((mix x) y))
    ((x . r) ((mix x) (apply mix* r))))) ;; or (foldl mix* x r)

(expect
;;  ((fix 4) (mix*)) => 4
;;  ((fix 4) (mix* 1+ 1+)) => 6
;;  ((fix 4) (mix* 10* 1+ 1+)) => 60
;;  ((fix 4) (mix* 1+ 10* 1+ 1+)) => 61
  )

;;; 5.3.4 Default and non-default Top Type
(define fixt (fix top))

(define fixt/inlined (λ (m)
  (Y (λ (s) ((m s) top)))))

(define record-spec (λ (self) (λ (super)
  empty-record)))

(define fix-record (fix empty-record))
(define fix-record/inlined (λ (m)
  (Y (λ (s) ((m s) empty-record)))))
(define fix-record/fixt (λ (m)
  (fixt ((mix m) record-spec))))

;;; 5.3.5 Minimal OO Indeed
(define method-spec (λ (key) (λ (compute-value) (λ (self) (λ (super) (λ (method-id)
  (let ((inherited (super method-id)))
    (if (equal? key method-id)
        ((compute-value self) inherited)
        inherited))))))))

;;; 5.3.6 Minimal Colored Point
(define coord-spec
  ((mix ((method-spec 'x) (λ (self) (λ (inherited) 2))))
        ((method-spec 'y) (λ (self) (λ (inherited) 4)))))

(define color-spec
  ((method-spec 'color) (λ (self) (λ (inherited) "blue"))))

(define point-p (fix-record ((mix color-spec) coord-spec)))

(expect (point-p 'x) => 2
        (point-p 'color) => "blue"
        (map point-p '(x y z color)) => '(2 4 #f "blue")
        (map (fix-record/inlined ((mix color-spec) coord-spec)) '(x y z color)) => '(2 4 #f "blue")
        (map (fix-record/fixt ((mix color-spec) coord-spec)) '(x y z color)) => '(2 4 #f "blue"))

;;; 5.3.7 Minimal Extensibility and Modularity Examples
(define add-x-spec (λ (dx)
  ((method-spec 'x) (λ (self) (λ (inherited) (+ dx inherited))))))

(define sqr (λ (x)
  (* x x)))

(define rho-spec
  ((method-spec 'rho) (λ (self) (λ (inherited)
    (sqrt (+ (sqr (self 'x)) (sqr (self 'y))))))))

(define point-r (fix-record
  (mix* (add-x-spec 1) coord-spec rho-spec)))

(expect (point-r 'x) => 3
        (point-r 'rho) => 5
        (map point-r '(x y rho color)) => '(3 4 5 #f))

;;; 5.3.8 Interaction of Modularity and Extensibility

(define my-modular-def (λ (self) (λ (method-id)
  (case method-id
    ((start) 5)
    ((length) (λ (l) (if (null? l) 0 (+ 1 ((self 'length) (cdr l))))))
    ((size) (- ((self 'length) (self 'contents)) (self 'start)))
    (else #f)))))

(define my-saying
  '(Designing a computer programming system that doesn’t address transactional
    persistence means that you’re proud of having no data worth keeping.))

(define my-contents-spec
  ((method-spec 'contents) (λ (self) (λ (inherited) my-saying))))

(define my-contents
  (stateful-Y (λ (self) ((my-contents-spec self) (my-modular-def self)))))

(expect (my-contents 'contents) => my-saying
        ((my-contents 'length) my-saying) => 20
        (my-contents 'size) => 15)

(define my-modular-def-without-global-recursion
  (let ((_start 42))
    (letrec ((_length (λ (l) (if (null? l) 0 (+ 1 (_length (cdr l)))))))
      (λ (self) (λ (method-id)
        (case method-id
          ((start) _start)
          ((length) _length)
          ((size) (- (_length (self 'contents)) _start))
          (else #f)))))))

(define my-contents-2
  (stateful-Y (λ (self) ((my-contents-spec self) (my-modular-def self)))))

(expect (my-contents-2 'contents) => my-saying
        ((my-contents-2 'length) my-saying) => 20
        (my-contents-2 'size) => 15)

(define base-bill-of-parts
  (λ (self) (λ (super) (λ (method-id)
    (case method-id
      ((parts) '())
      ((part-count) (length (self 'parts)))
      (else (super method-id)))))))

(define part-spec (λ (part)
  ((method-spec 'parts) (λ (self) (λ (inherited) (cons part inherited))))))

(define torso-spec (part-spec 'torso))
(define head-spec (part-spec 'head))
(define arms-spec (part-spec 'arms))
(define legs-spec (part-spec 'legs))

(define body-rec (fix-record (mix* head-spec arms-spec legs-spec torso-spec base-bill-of-parts)))

(expect (map body-rec '(parts part-count)) => '((head arms legs torso) 4))

;;;;; 6 Rebuilding OO from its Minimal Core
;;;; 6.1.2 Conflation: Crouching Typecast, Hidden Product

(define pproto←spec (λ (spec)
  (cons spec (fix-record spec))))
(define spec←pproto car)
(define target←pproto cdr)
(define pproto-mix (λ (child) (λ (parent)
  (pproto←spec (mix* (spec←pproto child) (spec←pproto parent))))))

(define coord-pproto (pproto←spec coord-spec))
(define color-pproto (pproto←spec color-spec))
(define point-p-pproto ((pproto-mix coord-pproto) color-pproto))

(expect (map (target←pproto coord-pproto) '(x y z color)) => '(2 4 #f #f)
        (map (target←pproto color-pproto) '(x y z color)) => '(#f #f #f "blue")
        (map (target←pproto point-p-pproto) '(x y z color)) => '(2 4 #f "blue"))

(define add-x-pproto (λ (dx)
  (pproto←spec (add-x-spec dx))))

(define rho-pproto (pproto←spec rho-spec))

(define point-r-pproto ((pproto-mix (add-x-pproto 1))
                        ((pproto-mix coord-pproto)
                         rho-pproto)))

#||#
#|
The End. (For Now)
|#
