;;; Scheme implementation of the C4 algorithm
;; This code is portable, and should work on any Scheme implementation.
;; It uses side-effects for a few optimizations, though pure variants are suggested.
;; You may add various implementation-specific static type declarations for further speed.

;; Utilities

;; Append the elements of the list in the first argument to the front of the list
;; in second argument until an element satisfies a predicate.
;; Return two values, the rest of the first list including the element
;; satisfying the predicate if any (or the empty list if none exists),
;; and the tail with the reverse of the rhead up till then appended in front.
;; : (X -> Bool) (List X) (List X) -> (List X) (List X)
(define (append-reverse-until pred rhead tail)
  (let loop ((rhead rhead) (tail tail))
    (if (null? rhead)
      (values '() tail)
      (let ((a (car rhead))
            (r (cdr rhead)))
       (if (pred a)
         (values rhead tail)
         (loop r (cons a tail)))))))

;; Destructive variant of (define (append1 l x) (append l (list x)))
;; : (List X) X -> (NonEmptyList X)
(define (append1! l x)
  (let ((l2 (list x)))
    (if (pair? l)
        (begin (set-cdr! (last-pair l) l2) l)
        l2)))

;; From SRFI-1
;; : (List X) -> (List X)
(define (last-pair l)
  (let loop ((p #f) (r l))
    (if (pair? r)
      (loop r (cdr r))
      p)))

;; Destructively remove the empty lists from a list of lists, returns the list.
;; NB: we use a destructive variant for efficiency,
;; but a pure variant would work, too:
;;    (define (remove-nulls l) (filter (lambda (x) (not (null? x))) l))
;; : (List (List X)) -> (List (NonEmptyList X))
(define (remove-nulls! l)
  (if (pair? l)
    (let ((r (cdr l)))
      (if (null? (car l))
        (remove-nulls! r)
        (begin
          (let loop ((l l) (r r))
            (if (pair? r)
              (let ((rr (cdr r)))
                (if (null? (car r))
                  (set-cdr! l (remove-nulls! rr))
                  (loop r rr)))))
          l)))
    l))


;; C4 linearization algorithm: given a top object x from which to compute the precedence list,
;; - head is a prefix for the precedence list, typically (list x) or '()
;;   depending on whether to include x as head of the result.
;; - supers the list of direct supers of x, typically (get-supers x)
;; - get-precedence-list gets the precedence list for a super s, including s itself in front.
;;   For testing purposes, it could be defined as:
;;   (define (get-precedence-list x) (c4-linearize (list x)
;; - struct is a predicate that tells if a class follows single inheritance, and
;;   must have its precedence list be a suffix of any subclass' precedence list.
;; - eq is an equality predicate between list elements
;; - get-name gets the name of a object/class, for debugging only.
;; Returns the linearized precedence list, and the most specific struct superclass if any
;; (Fun #|head|# (List X) \
;;  #|supers|# (List X) \
;;  #|get-precedence-list|# (Fun X -> (NonEmptyList X)) \
;;  #|struct?|# (Fun X -> Bool) \
;;  #|eq?#| (Fun X X -> Bool) \
;;  #|get-name|# (Fun X -> Y) \
;; -> (List X) (OrFalse X))
(define (c4-linearization head supers get-precedence-list struct? eq get-name)
  (cond
   ((null? supers) ;; 0 direct superclass: base class
    (values head #f))
   ((null? (cdr supers)) ;; 1 direct superclass: effective single inheritance
    (let ((pl (get-precedence-list (car supers))))
      (values (append head pl)
              (find struct? pl))))
   (else ;; 2 direct superclasses or more: effective multiple inheritance
    (let ((pls (map get-precedence-list supers)) ;; (List (List X)) ;; precedence lists to merge
          (sis '())) ;; (List X) ;; single-inheritance suffix

      ;; Split every precedence list at the first struct, consider whatever
      ;; follows as a suffix of the precedence-list. Merge all the suffixes,
      ;; where two suffixes are compatible if one is a suffix of the other.
      ;; Then in each remaining precedence list, (a) remove from the end the
      ;; classes that are in the correct order in the suffix, until you reach one
      ;; that isn't in the suffix, then check that no more class there is in the
      ;; suffix (or else there's an incompatibility).
      ;; Use that as suffix of the precedence list,
      ;; and for the (reverse) head, proceed as usual with C3.

      (define (get-names lst)
        (map get-name lst))

      (define (err . a)
        (apply error "Inconsistent precedence graph"
               'head (get-names head)
               'precedence-lists (map get-names pls)
               'single-inheritance-suffix (get-names sis) a))

      (define (eqlist? l1 l2)
        (or (eq? l1 l2)
            (and (null? l1) (null? l2))
            (and (not (null? l1))
                 (not (null? l2))
                 (eq (car l1) (car l2)) ; NB: eq is an argument to c4-linearization
                 (eqlist? (cdr l1) (cdr l2)))))

      (define (every pred l) ;; as simplified from SRFI-1
        (or (null? l)
            (and (pred (car l)) (every pred (cdr l)))))

      ;;; Deal with the struct suffix
      (def (merge-sis! sis2)
        (cond
         ((null? sis2) (void)) ;; no new struct suffix
         ((null? sis) (set! sis sis2)) ;; yes new struct suffix
         (else
          (let loop ((t1 sis) (t2 sis2))
            (cond
             ((eqlist? t1 sis2) (void)) ;; sis is a prefix of sis2
             ((eqlist? t2 sis) (set! sis sis2)) ;; sis2 is a prefix of sis
             ((null? t1) (if (member (car sis) t2 eq) (set! sis sis2)
                             (err struct-incompatibility: (list (get-names sis) (get-names sis2)))))
             ((null? t2) (if (member (car sis2) t1 eq) (void)
                             (err struct-incompatibility: (list (get-names sis) (get-names sis2)))))
             (else (loop (cdr t1) (cdr t2))))))))
      ;;; For each superclass's precedence list, handle its struct suffix,
      ;;; and collect the reverse of its structless prefix.
      (def rpls
        (map (lambda (pl)
               (let-values (((tl rh) (append-reverse-until struct? pl '())))
                 (merge-sis! tl)
                 rh))
             pls))
      ;; Now that structs can inherit from classes, the superclasses from another
      ;; direct superclass could have inherited from one of those classes, so
      ;; we must remove those classes from the current precedence list structless prefix,
      ;; after checking that each was included in the right order in the overall suffix.
      ;; We can safely assume that there was no inconsistency
      ;; within the direct superclass's precedence list, only between precedence list
      ;; (or else, trying to compute the inconsistent precedence list would already have errored out).
      ;; This function processes the structless prefix of one direct superclass's precedence list,
      ;; given in "reverse" least-specific to most-specific order, and returns those classes
      ;; not already included in the struct prefix, in regular most-specific to least-specific order,
      ;; erroring out if an inconsistency is found between the order of those classes
      ;; and that of the single-inheritance suffix.
      (define (unsisr-rpl rpl)
        (let u ((pl-rhead rpl) ;; structless prefix to process, least- to most- specific
                (pl-tail '()) ;; structless prefix processed, most- to least- specific
                (sis-rhead (reverse sis)) ;; single-inheritance suffix to process, least- to most- specific
                (sis-tail '())) ;; single-inheritance suffix processed, most- to least- specific
          (if (null? pl-rhead)
            pl-tail ;; done processing -- superclasses not in the sis, most- to least- specific
            (let ((c (car pl-rhead))
                  (plrh (cdr pl-rhead)))
              (if (member c sis-tail eq) ;; caught a superclass out of order with the sis
                (err 'precedence-list-head (get-names (reverse pl-rhead))
                     'precedence-list-tail (get-names pl-tail)
                     'single-inheritance-head (get-names (reverse sis-rhead))
                     'single-inheritance-tail (get-names sis-tail)
                     'super-out-of-order-vs-single-inheritance-tail (get-name c))
                (let-values (((sis-rh2 sis-tl2)
                              (append-reverse-until
                              (lambda (x) (eq c x)) sis-rhead sis-tail)))
                 (if (null? sis-rh2)
                   (u plrh (cons c pl-tail) '() sis-tl2)
                   (u plrh pl-tail (cdr sis-rh2) sis-tl2))))))))

      ;; Add the list of direct-supers to the set of precedence-lists to be
      ;; compatible with. Reset the precedence-lists to be in the C3 most-specific to
      ;; least-specific order excluding any class in the single-inheritance suffix.
      (append1! rpls (reverse supers)) ;; TODO: use cons after proving it is equivalent in this case.
      (def hpls (map unsisr-rpl rpls))

      ;; Now for the C3 algorithm proper (that technically includes the append1! above):
      ;; Extract classes in the precedence list one by one, applying a simple heuristic
      ;; that prioritizes classes based on a depth-first traversal.

      ;; Next super selection loop, enforcing the ordering constraint and
      ;; otherwise implementing the earlier-in-list-first search heuristic.
      ;; : (NonEmptyList (NonEmptyList X)) -> X
      (define (c3-select-next tails)
        (let (candidate? ;; : X -> Bool
              (lambda (c) (every (lambda (tail) (not (member c (cdr tail) eq))) tails)))
          (let loop ((ts tails))
            (if (and (pair? ts) (pair? (car ts)))
              (let ((c (caar ts))
                    (rts (cdr ts)))
                (if (candidate? c)
                  c
                  (loop rts)))
              (err)))))

      ;; Cleanup after lists after next element in the precedence list was chosen
      ;; : X (List (NonEmptyList X)) -> (List (NonEmptyList X))
      (define (remove-next! next tails)
        (let loop ((t tails))
          (if (null? t)
            tails
            (let ((head (caar t))
                  (tail (cdar t))
                  (more (cdr t)))
              (if (eq head next)
                (set-car! t tail))
              (loop more)))))

      ;; Now for the regular C3 loop
      ;; NB: if we cached the lengths of the precedence lists,
      ;; we could walk the precedence list to check which longest tail has the same length
      ;; as that of the precedence list of its top element, thereby being that very same list,
      ;; and then share the tail. But we don't, so we eschew that sharing optimization.
      (define precedence-list
        (let c3loop ((rhead (reverse head)) (tails hpls))
          (let ((tails (remove-nulls! tails)))
            (cond
             ((null? tails) (append-reverse rhead sis))
             ((null? (cdr tails)) (append-reverse rhead (append (car tails) sis)))
             (else
              (let* ((next (c3-select-next tails)))
                (c3loop (cons next rhead)
                        (remove-next! next tails))))))))
      (define super-struct (and (pair? sis) (car sis)))
      (values precedence-list super-struct)))))
