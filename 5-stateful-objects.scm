;;;; Chapter V. Stateful Objects.
(displayln "V. Stateful Objects.")

;; Note how all the functions defined in the previous chapter were pure:
;; They didn't use any side-effect whatsoever.
;; No set! no tricky use of call/cc.

;; But what if we are OK with using side-effects? How much does that simplify things?

;; We can do away with one of the two self super arguments, and instead pass a single mutable value
;; as argument, where the identity provides the self, and the current state of the value provides the super.
