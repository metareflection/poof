#lang slideshow

 (require slideshow/code
          slideshow/text
          (only-in pict/color white))

(slide
 #:title "Prototypes: Object-Orientation, Functionally"
 (t "by François-René Rideau, Alex Knauth, and Nada Amin")
 (blank-line)
 (small
  (code (define (fix p b) (define f (p (lambda i (apply f i)) b)) f)
        (define (mix c p) (lambda (f b) (c f (p f b)))))))

(slide
 #:title "Pillars of Object-Oriented Programming"
 (item "Abstraction and Interfaces" (white (t "← Functions")))
 (item "Encapsulation" (white (t "← Closures")))
 (item "Inheritance" (white (t "← Prototype Composition")))
 (item "Ad-hoc Polymorphism and Dispatch" (white (t "← Prototype")))
 (comment "Since there are multiple kinds of Polymorphism,"
          "and parametric polymorphism is already common in functional programming,"
          "calling it ad-hoc polymorphism is more specific."))

(slide
 #:title "Pillars of Object-Oriented Programming"
 (item "Abstraction" "← Functions and Interfaces")
 (item "Encapsulation" "← Closures")
 (item "Inheritance" "← Prototype Composition")
 (item "Ad-hoc Polymorphism" "← Prototype Dispatch"))

(slide
 #:title "Prototypes: Object-Orientation, Functionally"
 (item "Functions")
 (item "Prototype Composition")
 (item "Prototype Instantiation")
 (comment "Functions and Prototypes combine to make a simpler set of concepts,"
          "simpler yet more expressive."))

(slide
 #:title "Function Value Representation"
 (item "Records as Functions")
 (item "Prototypes for Functions")
 (small (para "Function Eta-Expansion:" (code (lambda i (apply f i)))
              "                              "
              "Used in fix:"
              (code (define (fix p b) (define f (p (lambda i (apply f i)) b)) f))))
 (comment "Function Eta-Expansion allows computations to be delayed"
          "inside the fixpoint instantiation."))

(slide
 #:title "Lazy Record Representation"
 (item "Records with Lazy Fields")
 (item "Prototypes for Lazy Computations")
 (small (para (code (delay f)) "instead of" (code (lambda i (apply f i)))
              "                              "
              "Used in δfix:"
              (code (define (δfix p b) (define f (delay (p f b))) f))))
 (comment "Prototypes are for Computations not Values."
          "Using laziness directly inside the fixpoint instantiation."))

(slide
 (item "Distictions")
 (item "Composition")
 (item "Multiple inheritance")
 (item "Classes")
 (item "Pure")
 (item "Mutation")
 (comment "Distictions: The conceptual distinction between"
          "instances, prototypes, wrappers and generators,"
          "objects, classes and class instances."
          "Composition: Composition of wrappers rather than their application to a generator"
          "as the algebraic structure of interest."
          "Multiple inheritance: Explanations of both why multiple inheritance is useful"
          "and how to formalize it."
          "Classes: How to derive class OOP from the more primitive prototype OOP."
          "Pure: A pure functional approach that provides not only denotational semantics"
          "atop the pure untyped lambda-calculus, but also a practical constructive implementation."
          "Mutation: A constructive model that does not rely on mutation,"
          "yet that can be extended to play well with it."))

(slide
 #:title "Distinctions"
 (item "Instance")
 (item "Wrapper")
 (item "Generator")
 (item "Prototype")
 (item "Object")
 (item "Class")
 (item "Class Instance")
 (comment "Instance is a record."
          "Wrapper is (Self Super -> Self) where (Self <: Super),"
          "Wrapper is one way of representing a prototype."
          "Generator of A is (A -> A),"
          "Single Inheritance is a much less expressive special case."
          "Prototype is a modular increment of computation"
          "Object is often a bundle/conflation of an instance and a prototype."
          "Class is a prototype for type descriptors."
          "Class Instance is vtable / method-dictionary"))

(slide
 #:title "Composition"
 (item "Composition of wrappers rather than their application to a generator"
       "as the algebraic structure of interest."))

(slide
 #:title "Multiple Inheritance"
 (item "Explanations of both why multiple inheritance is useful"
       "and how to formalize it."))

(slide
 #:title "Classes"
 (item "How to derive class OOP from the more primitive prototype OOP."))

(slide 
 #:title "Pure"
 (item "A pure functional approach that provides not only denotational semantics"
       "atop the pure untyped lambda-calculus, but also a practical constructive implementation."))

(slide
 #:title "Mutation"
 (item "A constructive model that does not rely on mutation,"
       "yet that can be extended to play well with it."))
        
