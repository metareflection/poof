;;;; Chapter IV. Better objects, still pure

(displayln "IV. Better objects, still pure")

;; In chapter II, we explained how to implement a trivial object system
;; Now, let's try to build a more featureful one.

;; 1. One downside of the previous object system is that
;;    there was no introspection as to what keys were provided by a prototype.
;; 2. Another downside was that we had to separately manipulate two different
;;    kinds of entities, prototypes and instances, even though, often,
;;    we want to deal with only one entity that can play either/both roles.
;; 3. Finally, the object system was barebones and doesn't support additional
;;    features like multiple inheritance, type annotations, method combinations,
;;    multiple-dispatch, ...

;; A few possible solutions to the first issue:
;; - Have prototype functions respond to a special magic message 'keys.
;;   It is the responsibility of the programmer to make sure this message
;;   is and remains in synch with the actual list of messages supported
;;   --- or we could provide some macros for it.
;; - Instead of passing around prototype functions alone, we could pass around
;;   a product of a function and a list of keys, as a list or other structure.
;;   There again, macros can do that for us, and even abstract over which
;;   encoding is used, whether the above or any.
;; - Directly pass around a structure that embodies a dictionary mapping
;;   symbols to functions, e.g. a hash-table from SRFI-125, or even just
;;   the pure symbol-avl-map we just implemented in chapter II.

;; Possible solutions to the second issue:
;; - Use the prototypes as a way to compute the instance through fixed-point,
;;   but use a wrapper to present a better interface to the result, that
;;   among other things will keep the prototype information available together
;;   with the instance information, in a single entity.
;;   Separating instance computation and instance usage also allows us to
;;   provide two distinct representations for the same information, one
;;   optimized for construction, the other for consumption.

;; Possible solutions to the third issue:
;; - Consider that the composable "prototype information" isn't just
;;   a function prototype, but also other meta-information like
;;   a list of direct super prototypes (for multiple inheritance),
;;   a separate prototype for type declarations, (or a merged prototype
;;   with separate calling conventions for type information), yet another
;;   prototype for method combinations, etc.
;; - Instead of a long litany of features hardwired in an ad hoc way in
;;   a giant object system, a composable Meta-Object Protocol that enables
;;   all the features in a modular fashion.
