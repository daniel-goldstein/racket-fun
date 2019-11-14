;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lambda-calc) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; factorial : Nat -> Nat
(check-expect (factorial 0) 1)
(check-expect (factorial 5) 120)
(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (sub1 n)))))

; This is fine and all, but this uses a lot of things that are not lambda!
; How can we limit our toolbelt to see lambda's power?

; Well, what things are not lambda?
; - define
; - if
; - zero? (predicates/booleans)
; - 1 (Natural numbers)
; - * (arithmetic)

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
; - define
; - if
; - zero? (predicates/booleans)
; - 1 (natural numbers)
; - * (arithmetic)
; - recursion


; LNat -------------------------------------------

; What do natural numbers represent? One interpretation could
; be "the amount of a thing", or "the times a certain
; action occurs". We're going to represent numbers kind
; of like the latter.

; A LNat is a [[X -> X] -> [X -> X]]
; and represents the times a function is applied onto an input
; the "template" for using an LNat is like
#;(define (lnat-temp lnat)
    ((lnat ...) ...))

; a sort of "template" for creating a LNat would be like
#;(define lnat
    (λ (f)
      (λ (x) ...)))

; lone : LNat
; applies a function to an input once
(define lone
  (λ (f)
    (λ (x) (f x))))

; ltwo : LNat
; applies a function twice to an input
(define ltwo
  (λ (f)
    (λ (x) (f (f x)))))

; what would zero be?

; lzero : LNat
; applies a function no times at all to an input
(define lzero
  (λ (f)
    (λ (x) x)))

; These functions encapsulate the ~essence~ of natural numbers!!
; But we don't just trust ourselves in computer science, we write tests!

; Let's write some translation functions back into the world of numerals
; so we can interpret the lambda expressions that we are writing

; lnat->nat : LNat -> Nat
(check-expect (lnat->nat lzero) 0)
(check-expect (lnat->nat lone)  1)
(check-expect (lnat->nat ltwo)  2)
(define (lnat->nat lnum)
  ((lnum add1) 0))

; It's getting kind of tedious to write out all these numbers

; ladd1 : LNat -> LNat
; produces an LNat that applies a function one more time than the supplied LNat
(check-expect (lnat->nat (ladd1 ltwo)) 3)
(check-expect (lnat->nat (ladd1 lzero)) 1)
(define ladd1
  (λ (n)
    (λ (f)
      (λ (x) (f ((n f) x))))))

; Now we can define
(define lthree (ladd1 ltwo))

; ladd : LNat LNat -> LNat
; adds the two LNats together
(check-expect (lnat->nat (l+ lzero lzero)) 0)
(check-expect (lnat->nat (l+ lthree ltwo)) 5)
(define l+
  (λ (n1 n2)
    (λ (f)
      (λ (x) ((n2 f) ((n1 f) x))))))

; Now we're cooking. We really want to get * and sub1 though

; l* : LNumber LNumber -> LNumber
; multiplies the two LNats together
(check-expect (lnat->nat (l* lzero lthree)) 0)
(check-expect (lnat->nat (l* lthree ltwo))  6)
(define l*
  (λ (n1 n2)
    (λ (f)
      (λ (x) ((n1 (n2 f)) x)))))

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

; A LBoolean is a (A) [A A -> A]
; where it picks one of the options

; a way to sanity check ourselves would be
(define (lboolean->boolean b) (b #true #false))

; ltrue : LBoolean
; pick the first option
(check-expect (ltrue 1 2) 1)
(define ltrue (λ (x y) x))

; lfalse : LBoolean
; pick the second option
(check-expect (lfalse 1 2) 2)
(define lfalse (λ (x y) y))

; lif : (X) LBoolean X X -> X
(check-expect (lif ltrue 1 2) 1)
(check-expect (lif lfalse 1 2) 2)
(define lif
  (λ (bool true-case false-case)
    (bool true-case false-case)))

; lzero? : LNat -> LBoolean
; is the given lnat zero?
(check-expect (lboolean->boolean (lzero? lzero)) #true)
(check-expect (lboolean->boolean (lzero? lone)) #false)
(check-expect (lboolean->boolean (lzero? ltwo)) #false)
(define lzero?
  (λ (lnat) ((lnat (λ (_) lfalse)) ltrue)))

; we're almost there to implementing sub1!
; one last thing we need is pairs (and as a result we get lists!!)

; what is a pair? What do you do with it... call first or second on it.
; When given some selector, give the first or second thing

; A LPair is a [X X -> [[X X -> X] -> X]]
(check-expect (lfirst (lmake-pair 1 2)) 1)
(check-expect (lsecond (lmake-pair 1 2)) 2)
(check-expect (lfirst (lsecond (lmake-pair 1 (lmake-pair 2 3)))) 2)

(define lmake-pair
  (λ (x y) (λ (chooser) (chooser x y))))

(define lfirst (λ (pair) (pair ltrue)))
(define lsecond (λ (pair) (pair lfalse)))

; linc-cons : LPair -> LPair
; copy the first into the second and increment the first
(define linc-cons
  (λ (pair)
    (lmake-pair (ladd1 (lfirst pair)) (lfirst pair))))

; sub1 : LNat -> LNat
; produce an LNat that performs given function one less time than the original
(check-expect (lnat->nat (lsub1 lzero)) 0)
(check-expect (lnat->nat (lsub1 lone)) 0)
(check-expect (lnat->nat (lsub1 lthree)) 2)
(define lsub1
  (λ (lnat)
    (λ (f)
      (λ (x)
        (((lsecond ((lnat linc-cons) (lmake-pair lzero lzero))) f) x)))))

; We've nearly got it!!

; Let's check in on how we could rewrite factorial

; lfactorial : LNat -> LNat
(check-expect (lnat->nat (lfactorial lzero)) 1)
(define (lfactorial lnat)
  (lif (lzero? lnat) lone lone))

; PROBLEM: I need lazy ifs and I can't do that with ISL function calls!!!
; Going to have to switch over to #lang lazy




