;;;; Chapter V. Stateful Objects.
(displayln "V. Stateful Objects.")

;; Note how all the functions defined in the previous chapter were pure:
;; They didn't use any side-effect whatsoever.
;; No set! no tricky use of call/cc.

;; But what if we are OK with using side-effects? How much does that simplify things?

;; We can do away with one of the two self super arguments, and instead pass a single mutable value
;; as argument, where the identity provides the self, and the current state of the value provides the super.
;;
;; Thus, we could for instance use (deftype Proto (Fun MutableHashTable ->)),
;; where the hash-table contains the effective methods to compute each slot as thunks
;; or lazy computations already closed over the hash-table.
;; Overriding a slot would replace the effective method based on the new method, the self and super method.
;; Thus, individual methods would still be parameterized over self and super,
;; even though the object prototype only has the self as parameter.
;;
;; Alternatively, each individual slot contains an object, and the methods are prototypes
;; for these "objects", with the same mutable signature. But that means that only mutable objects
;; are allowed as values in the language. Yikes.
