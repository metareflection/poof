#lang racket
;; Examples and checks from poof.scrbl, manually extracted

(require "poof-code-only.rkt"
         rackunit)

(printf "Running tests...\n")

;; Change the expected result to 5 to see how that affects the output
(check-equal? (+ 2 2) 4)

;; x1-y2 : (Fun 'x -> Nat | 'y -> Nat)
(define (x1-y2 msg) (case msg ((x) 1)
                              ((y) 2)
                              (else (error "unbound slot" msg))))

(check-equal? (list (x1-y2 'x) (x1-y2 'y)) '(1 2))

;; $x3 : (Proto (Fun 'x -> Nat | A) A)
(define ($x3 self super) (λ (msg) (if (eq? msg 'x) 3 (super msg))))

;; $z<-xy : (Proto (Fun (Or 'x 'y) -> Real | 'z -> Complex | A)
;;                 (Fun (Or 'x 'y) -> Real | A))
(define ($z<-xy self super)
  (λ (msg) (case msg
             ((z) (+ (self 'x) (* 0+1i (self 'y))))
             (else (super msg)))))

;; $double-x : (Proto (Fun 'x -> Number | A) (Fun 'x -> Number | A))
(define ($double-x self super)
  (λ (msg) (if (eq? msg 'x) (* 2 (super 'x)) (super msg))))

;; x3-y2 : (Fun 'x -> Nat | 'y -> Nat)
(define x3-y2 (fix $x3 x1-y2))
(check-equal? (list (x3-y2 'x) (x3-y2 'y)) '(3 2))

;; z1+2i : (Fun 'x -> Nat | 'y -> Nat | 'z -> Complex)
(define z1+2i (fix $z<-xy x1-y2))
(check-equal? (list (z1+2i 'x) (z1+2i 'y) (z1+2i 'z)) '(1 2 1+2i))

;; x2-y2 : (Fun 'x -> Nat | 'y -> Nat)
(define x2-y2 (fix $double-x x1-y2))
(check-equal? (list (x2-y2 'x) (x2-y2 'y)) '(2 2))

;; z6+2i : (Fun 'x -> Nat | 'y -> Nat | 'z -> Complex)
(define z6+2i (fix (mix $z<-xy (mix $double-x $x3)) x1-y2))
(check-equal? (map z6+2i '(x y z)) '(6 2 6+2i))

(check-equal? (list ((fix (mix $z<-xy (mix $double-x $x3)) x1-y2) 'z)
                  ((fix (mix $double-x (mix $z<-xy $x3)) x1-y2) 'z)
                  ((fix (mix $double-x (mix $x3 $z<-xy)) x1-y2) 'z))
            '(6+2i 6+2i 6+2i))

(check-equal? (list ((fix (mix (mix $z<-xy $double-x) $x3) x1-y2) 'z)
                  ((fix (mix (mix $double-x $z<-xy) $x3) x1-y2) 'z)
                  ((fix (mix (mix $double-x $x3) $z<-xy) x1-y2) 'z))
            '(6+2i 6+2i 6+2i))

;; x6-y2 : (Fun 'x -> Nat | 'y -> Nat)
(define x6-y2 (fix (mix $double-x $x3) x1-y2))
;; x3-y2b : (Fun 'x -> Nat | 'y -> Nat)
(define x3-y2b (fix (mix $x3 $double-x) x1-y2))
(check-equal? (list (x6-y2 'x) (x3-y2b 'x)) '(6 3))

(define ($slot k v) ($slot-gen k (λ (__self __inherit) v)))
(define ($slot-modify k modify) ($slot-gen k (λ (__ inherit) (modify (inherit)))))
(define ($slot-compute k fun) ($slot-gen k (λ (self __) (fun self))))

;; Alternate more abstract redefinitions of some things defined above
(let ()
(define $x3 ($slot 'x 3))
(define $double-x ($slot-modify 'x (λ (x) (* 2 x))))
(define $z<-xy ($slot-compute 'z (λ (self) (+ (self 'x) (* 0+1i (self 'y))))))

;; x3 : (Fun 'x -> Nat)
(define x3 (fix $x3 bottom-record))
(check-equal? (x3 'x) 3)

;; x1-y2 : (Fun 'x -> Nat | 'y -> Nat)
(define x1-y2 (fix (mix ($slot 'x 1) ($slot 'y 2)) bottom-record))
(check-equal? (map x1-y2 '(x y)) '(1 2)))

(check-equal? (list ((number-order '<) 23 42) ((number-order 'compare) 8 4)
  ((string-order '<) "Hello" "World") ((string-order 'compare) "Foo" "FOO")
  ((string-order 'compare) "42" "42"))
  '(#t > #t > =))

(check-equal? (list ((symbol-order '<) 'aardvark 'aaron) ((symbol-order '=) 'zzz 'zzz)
  ((symbol-order '>) 'aa 'a) ((symbol-order 'compare) 'alice 'bob)
  ((symbol-order 'compare) 'b 'c) ((symbol-order 'compare) 'c 'a))
  '(#t #t #t < < >))

(define my-binary-dict ;; heavily skewed right, height 5
  (foldl (λ (kv t) ((symbol-tree-map 'acons) (car kv) (cdr kv) t))
         (symbol-tree-map 'empty)
         '((a . "I") (b . "II") (c . "III") (d . "IV") (e . "V"))))
(check-equal? my-binary-dict '(() ((a . "I")) (() ((b . "II")) (() ((c . "III")) (() ((d . "IV")) (() ((e . "V")) ()))))))

(check-equal? (map (λ (k) ((symbol-tree-map 'ref) my-binary-dict k (λ () #f))) '(a b c d e z))
            '("I" "II" "III" "IV" "V" #f))

(define my-avl-dict
  (foldl (λ (kv t) ((Dict 'acons) (car kv) (cdr kv) t))
         (Dict 'empty)
         '((a . "I") (b . "II") (c . "III") (d . "IV") (e . "V"))))
(check-equal? my-avl-dict
                '((() ((a . "I") . 1) ()) ((b . "II") . 3)
                  ((() ((c . "III") . 1) ()) ((d . "IV") . 2) (() ((e . "V") . 1) ()))))
(check-equal? (map (λ (k) ((Dict 'ref) my-avl-dict k (λ () #f))) '(a b c d e z))
            '("I" "II" "III" "IV" "V" #f))

(define ($even self super) (λ (x) (if (< x 0) (self (- x)) (super x))))

(define ($cube self super) (λ (x) (let ((y (super x))) (* y y y))))

(define absx3 (instance $even $cube ($constant-prototype (λ (x) x))))
(check-equal? (map absx3 '(3 -2 0 -1)) '(27 8 0 1))

(define (p1+ __ b) (λ () (+ 1 (b))))
(define (p2* __ b) (λ () (* 2 (b))))
(check-equal? (list ((fix (mix p1+ p2*) (λ () 30)))
                  ((fix (mix p2* p1+) (λ () 30))))
            '(61 62))

(define test-inheritance-dag-alist
  '((O) (A O) (B O) (C O) (D O) (E O)
    (K1 A B C) (K2 D B E) (K3 D A) (Z K1 K2 K3)))
(define (test-get-supers x) (cdr (assoc x test-inheritance-dag-alist)))
(define (test-compute-precedence-list x)
  (c3-compute-precedence-list x test-get-supers test-compute-precedence-list))

(check-equal? (map not-null? '(() (1) (a b c) nil)) '(#f #t #t #t))
(check-equal? (remove-nulls '((a b c) () (d e) () (f) () ())) '((a b c) (d e) (f)))
(check-equal? (map test-compute-precedence-list '(O A K1 Z))
  '((O) (A O) (K1 A B C O) (Z K1 K2 K3 D A B C E O)))

(define test-instance
  (alist->Dict `((a . ,(delay 1)) (b . ,(delay 2)) (c . ,(delay 3)))))
(define test-prototype
  (alist->Dict `((function . ,(delay (λ (self super)
                                (Dict-merge (force test-instance) (force super)))))
                 (supers . ,(delay '()))
                 (precedence-list . ,(delay '())))))
(define test-object (make-object test-instance test-prototype))
(define test-p1 ($slot/value 'foo 1))

(check-equal? (map (λ (x) (slot-ref test-object x)) '(a b c)) '(1 2 3))
(check-equal? (slot-ref (instantiate '() test-p1) 'foo) 1)
(check-equal? (map (slot-ref (ListOf Number) 'is?) '(() (1 2 3) (1 a 2) (1 . 2)))
  '(#t #t #f #f))

(define (overly-simple-fix p b)
  (define f (p f b))
  f)

(define (delayed-fix p b)
  (define f (delay (p f b)))
  f)

(define (fix--0 p b)
  (define (f . i) (apply (p f b) i))
  f)

(define (fix--1 p b)
  (define f (p (λ i (apply f i)) b))
  f)

(define (fix--2 p b)
  (letrec ((f (p (λ i (apply f i)) b)))
    f))

(define (fix--3 p b)
  ((λ (yf) (yf yf)) (λ (yf) (p (λ i (apply (yf yf) i)) b))))

(define ((mix--1 p q) f b) (p f (q f b)))

(define my-counter (make-counter))
(define my-memo-counter (memoize my-counter))

(check-equal? (my-counter) 0)
(check-equal? (my-memo-counter) 1)
(check-equal? (my-counter) 2)
(check-equal? (my-memo-counter) 1)

(define count-fun (instance count-proto))
(check-equal? (count-fun) 0)
(check-equal? (count-fun) 1)
(check-equal? (count-fun) 2)

(define zero-fun (instance memoize-proto count-proto))
(check-equal? (zero-fun) 0)
(check-equal? (zero-fun) 0)

(printf "Done\n")
