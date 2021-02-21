;; II- Building usable prototype OO

;; II.1- Basic utility functions

;; II.1.1- Keeping nesting and indentation under control

(define-syntax nest
  (syntax-rules ()
    ((nest (x ...) y z ...) (x ... (nest y z ...)))
    ((nest x) x)))

(check! (= (nest (+ 1 2) (* 3 4) (- 5 6)) -9)

;; II.1.2- Memoizing values, so field access isn't O(n) every time.
;; Usage: Put memoize-proto as first prototype.
;; NB1: Memoization uses side-effects internally, but does not expose them.
;; NB2: It's still O(n^2) overall rather than O(n); we can do better, later.
(define (memoize f)
  (nest (let ((cache (make-hash)))) (λ x) (apply values) (hash-ref! cache x)
        (λ ()) (call-with-values (λ () (apply f x))) list))

(define (make-counter)
  (nest (let ((count 0))) (λ ())
        (let ((result count)) (set! count (+ count 1)) result)))

(define my-counter (make-counter))
(define my-memo-counter (memoize my-counter))

(check! (= (my-counter) 0)
(check! (= (my-memo-counter) 1)
(check! (= (my-counter) 2)
(check! (= (my-memo-counter) 1)

(define (memoize-proto self super) (memoize super))

(define (count-proto self super)
  (make-counter))

(define count-fun (instance count-proto))
(check! (= (count-fun) 0)
(check! (= (count-fun) 1)
(check! (= (count-fun) 2)

(define zero-fun (instance memoize-proto count-proto))
(check! (= (zero-fun) 0))
(check! (= (zero-fun) 0))

;; II.1.3- Trivial records as functions from keys to values and set of keys
;;    We need them as functions, so that we can compute them directly
;;    as fixed-points. Also because that's a trivial OOP technique in Scheme.
;; II.1.3.1- Interface: a record has two methods get and keys
;; Basic interface

;; Consults a record to see if a key is in it.
;; If yes, call the yes continuation with a thunk that computes the value.
;; If no, call the no continuation with no argument.
(define (record-get record key yes no)
  (record 'get key yes no))

;; Returns a list of all keys in the record.
;; Keys may or may not appear several times, depending on the record.
(define (record-keys record)
  (record 'keys))

;; II.1.3.2- Using that trivial record interface...

;; Trivial constructors, work in O(n).
(define (trivial-singleton-record k v)
  (nest (λ (m))
    (if (eq? m 'get) (λ (x) (if (equal? x k) (yes (λ () v)) (no))))
    (if (eq? m 'keys) (list k))
    (bottom)))
(define (trivial-record-union r1 r2)
  (nest (let ((keys (append (record-keys r1) (record-keys r2)))))
        (λ (m))
        (if (eq? m 'get)
            (nest (λ (x)) (record-get r1 key yes)
                  (λ ()) (record-get r2 key yes no)))
        (if (eq? m 'keys) keys)
        (bottom)))

;; Some convenience functions
(define (trivial-record-empty? record)
  (null? (record-keys record)))
(define (trivial-record-ref record key (default #f))
  (record-get record key (λ (x) x)
              (if (procedure? default) default (λ () default))))

;; BUT, that will be very inefficient, not extensible, etc.
;; Can we do better?

;; II.1.3.2- Trivial adapter to use Racket's pure hash-tables as records.
;; They could easily though painfully be implemented in the pure
;; lambda-calculus, but that's not the focus of this exercise.
(define (hash-keys hash) (map car (hash->list hash)))
(define ((hash-get hash) key yes no)
  (if (hash-has-key? record key) (yes (λ () (hash-ref record key))) (no)))
(define (record-from-hash hash (super bottom))
  (nest (λ (m))
        (if (eq? m 'get) (hash-get hash))
        (if (eq? m 'hash) hash)
        (if (eq? m 'keys) (hash-keys hash))
        (super m)))
(define (hash-from-record record)
  (record 'hash))
(define (empty-hash-record) (record-from-hash (hash)))
(define (singleton-hash-record k v)
  (record-from-hash (hash k v)))
(define (hash-record-union r1 r2) ;; consult r1 before r2
  (record-from-hash (hash-union (r1 'hash) (r2 'hash) #:combine (λ (v1 v2) v1))))

(define (record-from-alist alist)
  (record-from-hash (make-immutable-hash alist)))
(define (alist-from-record record)
  (hash->list (hash-from-record record)))

#|
;; A record maps keys to values of appropriate for the key.
;; In Racket, we could represent them as immutable hash tables.
(define (alist-from-record record) (hash->list record))
(define (record-get record key yes no)
(define (record . args) (apply hash args))
(define empty-record (record))
|#



;; II- Classes as higher-order prototypes.

#|
Bibliography: TODO: read the original SELF papers.
Also, I'm pretty sure some experimental "object" systems in Scheme
may have provided prototype-based OO long before Self, but may just never
have seriously peddled the result as a new paradigm.
TODO: track literature about Objects in Scheme.

NB: I tried to use delay/force to create fixed-points for values other
than functions, but Racket wouldn't allow reentrant promises :-(
Looks like I would have to convert all functions and their bodies
to use delay and force systematically (and/or SRFI-45 lazy) so that
the entire program be effectively written in a lazy language.

ftp://www.cs.indiana.edu/pub/techreports/TR276.pdf
|#
|#
