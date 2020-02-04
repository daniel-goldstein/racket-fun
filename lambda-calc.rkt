#lang lazy
(require rackunit)

; This is so we can actually write tests that we can see right now
(define (check-expect actual expected)
  (check-equal? (! actual) (! expected)))

; factorial : Nat -> Nat
(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (sub1 n)))))
(check-expect (factorial 0) 1)
(check-expect (factorial 5) 120)

; This is fine and all, but this uses a lot of things that are not lambda!
; How can we limit our toolbelt to see lambda's power?

; Well, what things are not lambda?
; - define function
; - if
; - zero? (predicates/booleans)
; - 1 (Natural numbers)
; - *, sub1 (arithmetic)

; So define should be easy, right?

#;(define factorial
    (λ (n)
      (if (zero? n)
          1
          (* n (factorial (sub1 n))))))

; oh no! what does factorial mean now in the recursive call? It's not defined.
; so define is ok as long as we don't have recursion.
; This means we need to do recursion with lambda.

; UPDATED:
; Well, what things are not lambda?
; - define function
; - if
; - zero? (predicates/booleans)
; - 1 (natural numbers)
; - *, sub1 (arithmetic)
; - recursion


; λNat -------------------------------------------

; What do natural numbers represent? One interpretation could
; be "the amount of a thing", or "the times a certain
; action occurs". We're going to represent numbers kind
; of like the latter.

; A λNat is a [[X -> X] -> [X -> X]]
; and represents the times a function is applied onto an input
; the "template" for using an λnat is like
#;(define (λnat-temp λnat)
    ((λnat ...) ...))

; a sort of "template" for creating a λnat would be like
#;(define λnat
    (λ (f)
      (λ (x) ...)))

; λone : λNat
; applies a function to an input once
(define λone
  (λ (f)
    (λ (x) (f x))))

; λtwo : λNat
; applies a function twice to an input
(define λtwo
  (λ (f)
    (λ (x) (f (f x)))))

; what would zero be?

; λzero : λNat
; applies a function no times at all to an input
(define λzero
  (λ (f)
    (λ (x) x)))

; These functions encapsulate the ~essence~ of natural numbers!!
; But we don't just trust ourselves in computer science, we write checks!

; Let's write some translation functions back into the world of numerals
; so we can interpret the lambda expressions that we are writing

; λnat->nat : λNat -> Nat
(define (λnat->nat λnum)
  ((λnum add1) 0))
(check-expect (λnat->nat λzero) 0)
(check-expect (λnat->nat λone)  1)
(check-expect (λnat->nat λtwo)  2)

; It's getting kind of tedious to write out all these numbers

; λadd1 : λNat -> λNat
; produces an λNat that applies a function one more time than the supplied λnat
(define λadd1
  (λ (n)
    (λ (f)
      (λ (x) (f ((n f) x))))))
(check-expect (λnat->nat (λadd1 λtwo)) 3)
(check-expect (λnat->nat (λadd1 λzero)) 1)

; Now we can define
(define λthree (λadd1 λtwo))

; λadd : λNat λNat -> λNat
; adds the two λnats together
(define λ+
  (λ (n1 n2)
    (λ (f)
      (λ (x) ((n2 f) ((n1 f) x))))))
(check-expect (λnat->nat (λ+ λzero λzero)) 0)
(check-expect (λnat->nat (λ+ λthree λtwo)) 5)

; Now we're cooking. We really want to get * and sub1 though

; λ* : λNat λNat -> λNat
; multiplies the two λnats together
(define λ*
  (λ (n1 n2)
    (λ (f)
      (λ (x) ((n1 (n2 f)) x)))))
(check-expect (λnat->nat (λ* λzero λthree)) 0)
(check-expect (λnat->nat (λ* λthree λtwo))  6)

; OK but how in the world do we do sub1?
; Well... this is a little harder, since we don't really have a concept of negation.
; We'll come back to this once we've built up a little more tooling in our language

; Let's do booleans! Remember, our goal is to be able to ask the question zero?

; What are booleans? Like numbers, we'll ask how they're used. With either true
; or false, we use booleans to make decisions! The basic part of a language that
; deals with booleans are `if` expressions. Let's look at how an if works

(check-expect (if true 1 2) 1) ; true here is associated with "choose the first option"
(check-expect (if false 1 2) 2) ; false here is associated with "choose the second option"

; so let's define booleans as "choosers".

; A λBoolean is a (A) [A A -> A]
; where it picks one of the options

; a way to sanity check ourselves would be
(define (λboolean->boolean b) (b #true #false))

; λtrue : λBoolean
; pick the first option
(define λtrue (λ (x y) x))
(check-expect (λtrue 1 2) 1)

; λfalse : λBoolean
; pick the second option
(define λfalse (λ (x y) y))
(check-expect (λfalse 1 2) 2)

; λif : (X) λBoolean X X -> X
(define λif
  (λ (bool true-case false-case)
    (bool true-case false-case)))
(check-expect (λif λtrue 1 2) 1)
(check-expect (λif λfalse 1 2) 2)

; λzero? : λNat -> λBoolean
; is the given λNat zero?
(define λzero?
  (λ (n) ((n (λ (_) λfalse)) λtrue)))
(check-expect (λboolean->boolean (λzero? λzero)) #true)
(check-expect (λboolean->boolean (λzero? λone)) #false)
(check-expect (λboolean->boolean (λzero? λtwo)) #false)

; we're almost there to implementing sub1!
; one last thing we need is pairs (and as a result we get lists!!)

; what is a pair? What do you do with it... call first or second on it.
; When given some selector, give the first or second thing

; A λPair is a [X X -> [[X X -> X] -> X]]
(define λmake-pair
  (λ (x y) (λ (chooser) (chooser x y))))

(define λfirst (λ (pair) (pair λtrue)))
(define λsecond (λ (pair) (pair λfalse)))

(check-expect (λfirst (λmake-pair 1 2)) 1)
(check-expect (λsecond (λmake-pair 1 2)) 2)
(check-expect (λfirst (λsecond (λmake-pair 1 (λmake-pair 2 3)))) 2)

; λinc-cons : λPair -> λPair
; copy the first into the second and increment the first
(define λinc-cons
  (λ (pair)
    (λmake-pair (λadd1 (λfirst pair)) (λfirst pair))))

; sub1 : λnat -> λnat
; produce an λnat that performs given function one less time than the original
(define λsub1
  (λ (n)
    (λsecond ((n λinc-cons) (λmake-pair λzero λzero)))))
(check-expect (λnat->nat (λsub1 λzero)) 0)
(check-expect (λnat->nat (λsub1 λone)) 0)
(check-expect (λnat->nat (λsub1 λthree)) 2)

; We've nearly got it!!

; Let's check in on how we could rewrite factorial

; λfactorial : λNat -> λNat
(define (λfactorial-with-named-recursion n)
  (λif (λzero? n)
       λone
       (λ* n (λfactorial-with-named-recursion (λsub1 n)))))
(check-expect (λnat->nat (λfactorial-with-named-recursion λzero)) 1)
(check-expect (λnat->nat (λfactorial-with-named-recursion λthree)) 6)

; We were able to replace everything except
; for the function define construct that allow

; But earlier we figured out that we can't just do

#;(define λfactorial
    (λ (n)
      (λif (λzero? n)
           λone
           (λ* n (λfactorial (λsub1 n))))))

; because we're trying to use λfactorial inside
; its own definition and we can't do that with normal
; defines!!

; So do we give up? Never! Let's figure out how to implement
; recursion -- without names.

; Oof ok so it's about to get weird.

; What happens when we do recursion? You can imagine copying and pasting
; the function body of the recursive call into our original function.
; And we keep copying and pasting the recursive calls until we get to our base case.
; So we're kind of passing on a blob of code that is the function body.
; We just can't refer to the same name.

; So what if we called a different function instead? In lambdas, we access to variables
; as inputs to the lambda. What if we just took the function we call to do recursion
; as an input?

#;(define λfactorial
    (... (λ (recur)
           (λ (n)
             (λif (λzero? n)
                  λone
                  (λ* n (recur (λsub1 n))))))))

; So this is technically allowed, but how the hell does this work? What needs to
; happen? Well, recur should figure out somehow to take the n-1 and pass it back
; into the λ (n), and we need to make sure that λfactorial still matches its
; original signature.

#;((λ (f) (f f)) (λ (f) (f f)))

; We just created an infinite loop! Without "recursion"!
; This was a big step, but we don't want infinite recursion, we want
; to only recur a certain number of times.

(define Y
  (λ (g)
    ((λ (f) (f f))
     (λ (f) (g (f f))))))

#;(define λfactorial
    (Y
     (λ (recur)
       (λ (n)
         (λif (λzero? n)
              λone
              (λ* n (recur (λsub1 n))))))))

(define λfactorial
  ((λ (g)
    ((λ (f) (f f))
     (λ (f) (g (f f)))))
   (λ (fct)
     (λ (n)
       ((λ (bool t-case f-case)
          (bool t-case f-case))
        ((λ (rep) ((rep (λ (x) (λ (x y) y))) (λ (x y) x))) n)
        (λ (f) (λ (x) (f x)))
        ((λ (rep1 rep2)
           (λ (f)
             (λ (x) ((rep1 (rep2 f)) x))))
         n
         (fct ((λ (rep)
                 (λ (f)
                   (λ (x)
                     ((((λ (pair) (pair (λ (x y) y)))
                        ((rep (λ (p)
                                ((λ (a b)
                                   (λ (selector) (selector a b)))
                                 ((λ (rep)
                                    (λ (f)
                                      (λ (x) (f ((rep f) x)))))
                                  ((λ (pair) (pair (λ (x y) x))) p))
                                 ((λ (pair) (pair (λ (x y) x))) p))))
                         ((λ (a b)
                            (λ (selector) (selector a b)))
                          (λ (f) (λ (x) x))
                          (λ (f) (λ (x) x)))))
                       f)
                      x))))
               n))))))))

(λnat->nat
 (((λ (g)
     ((λ (f) (f f))
      (λ (f) (g (f f)))))
   (λ (fct)
     (λ (n)
       ((λ (bool t-case f-case)
          (bool t-case f-case))
        ((λ (rep) ((rep (λ (x) (λ (x y) y))) (λ (x y) x))) n)
        (λ (f) (λ (x) (f x)))
        ((λ (rep1 rep2)
           (λ (f)
             (λ (x) ((rep1 (rep2 f)) x))))
         n
         (fct ((λ (rep)
                 (λ (f)
                   (λ (x)
                     ((((λ (pair) (pair (λ (x y) y)))
                        ((rep (λ (p)
                                ((λ (a b)
                                   (λ (selector) (selector a b)))
                                 ((λ (rep)
                                    (λ (f)
                                      (λ (x) (f ((rep f) x)))))
                                  ((λ (pair) (pair (λ (x y) x))) p))
                                 ((λ (pair) (pair (λ (x y) x))) p))))
                         ((λ (a b)
                            (λ (selector) (selector a b)))
                          (λ (f) (λ (x) x))
                          (λ (f) (λ (x) x)))))
                       f)
                      x))))
               n)))))))
  (λ (f) (λ (x) (f (f (f (f (f x)))))))))
