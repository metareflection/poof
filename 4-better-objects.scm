;;;; Chapter IV. Better objects, still pure

(displayln "IV. Better objects, still pure")

;; In chapter II, we explained how to implement a trivial object system
;; Now, let's try to build a more featureful one.

;; First, one downside of the previous object system is that
;; there was no introspection as to what keys were provided by a prototype.

;; A few possible solutions:
;; - Have prototype functions respond to a special magic message 'keys.
;;   It is the responsibility of the programmer to make sure this message
;;   is and remains in synch --- or we could provide some macros for it.
;; - Instead of passing around prototype functions alone, we could pass around
;;   a product of a function and a list of keys, as a list or other structure.
;;   There again, macros can do that for us, and even abstract over which
;;   encoding is used, whether the above or any.
;; - Directly pass around a structure that embodies a dictionary mapping
;;   symbols to functions, e.g. a hash-table from SRFI-125, or even just
;;   the pure symbol-avl-map we just implemented in chapter II.
;;
