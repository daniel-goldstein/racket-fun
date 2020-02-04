#lang racket

(module+ main
  (collect + '(lambda (i) (* i i)) 10))


#; { (X Y) [X ... -> Y] SExpr Nat -> Y }
(define (collect combine f n-ways)
  (define results (distribute f n-ways))
  (for-each (lambda (x) (printf "~a\t" x)) results)
  (printf "\n")

  (define res (apply combine results))
  (printf "The final result is: ~a\n" res)
  res)


#; { (X Y) [Nat -> X] Nat [X ... -> Y] -> Y }
(define (distribute f n-ways)
  (for/list ([worker-ch (spawn-workers n-ways)])
    (place-channel-put worker-ch f)
    (place-channel-get worker-ch)))


#; { Nat -> Place }
(define (spawn-workers n)
  (for/list ([index (in-range n)])
    (define worker (dynamic-place "place-worker.rkt" 'place-main))
    (place-channel-put worker index)
    worker))
