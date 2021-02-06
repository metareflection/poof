;;; These two functions are the essence of our OO system.

;;; The function `fix` instantiates a prototype `p` into a default object `b`.
(define (fix p d) (define f (p (lambda i (apply f i)) d)) f)

;;; The function `mix` takes two arbitrary prototypes `p` and `q` and returns a prototype that inherits first from p and then from q.
(define (mix p q) (lambda (f b) (p f (q f b))))

;;; An example of a prototype is `(field 'foo 0)` where `field`
;; To define an object with field k mapped to constant value v,
;; we could thus use this function:
(define (field k v) ;; k v: constant key and value for this defined field
  (lambda (self super) ;; self super: usual variables for prototype fixed-point
    (lambda (msg) ;; msg: message received by the object
      (if (equal? msg k) v ;; if the message matches the key, return the value
        (super msg))))) ;; otherwise, recurse to the object's super object

;;; To turn our prototype into an instance, we `fix` it.
;;; We can use `bottom` as the default object of the `fix`.
(define (bottom . x) (apply error "bottom" x))
(define foo-0 (fix (field 'foo 0) bottom))
(check! (= (foo-0 'foo) 0))


