;;;; Chapter II. Trivial objects, the naive OO on top of poof

(displayln "II. Defining trivial object system")

;; II.1. Trivial "objects" as unary functions from keys to values
;;
;; Let's define our "objects" as functions that receive a "message",
;; being a symbol typically bound to variable msg,
;; and depending on which key k the message matches,
;; returns a field value of type (A_ k).
;;
;; Assuming some suitable dependent type notation, we could define
#;(deftype (Object A_) (Fun (A_ k) <- (: k Any)))

(displayln "II.1.1. Simple objects with constant field-value mapping")
;; To define an object with field k mapped to constant value v,
;; we could thus use this function:
(define (field k v) ;; k v: constant key and value for this defined field
  (lambda (self super) ;; self super: usual variables for prototype fixed-point
    (lambda (msg) ;; msg: message received by the object
      (if (equal? msg k) v ;; if the message matches the key, return the value
        (super msg))))) ;; otherwise, recurse to the object's super object

;; Let's test simple objects.
(define foo-0 (instance (field 'foo 0)))
(check! (= (foo-0 'foo) 0))

(define x1-y2 (instance (field 'x 1) (field 'y 2)))
(check! (= (x1-y2 'x) 1))
(check! (= (x1-y2 'y) 2))

(displayln "II.1.2. methods that refer to other fields")
;; An object prototype may define some fields (in this case, z, rho and theta)
;; by referring to other fields of the object (in this case, x and y):
(define (polar-from-rect self super)
  (lambda (msg)
    (case msg
      ((z) (+ (self 'x) (* 0+i (self 'y))))
      ((rho) (magnitude (self 'z)))
      ((theta) (angle (self 'z)))
      (else (super msg)))))

;; NB: not that these prototypes commute, since there is no override:
(define my-object2
  (instance (field 'x 1) polar-from-rect (field 'y 2)))
(check! (= (my-object2 'rho) (sqrt 5)))

;; Note how all the functions defined above are pure functional!
;; No side-effect was used whatsoever. No set! no tricky use of call/cc.
