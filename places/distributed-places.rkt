#lang racket

(module+ main
  (start-master '(lambda (i) (+ i 2))))

; { Sexpr -> Void }
(define (start-master expr)
  (let ([pls (for/list ([i (in-range 2)])
                (dynamic-place "place-worker.rkt" 'place-main))])
     (for ([i (in-range 2)]
           [p pls])
        (place-channel-put p i)
        (place-channel-put p expr)
        (printf "~a\n" (place-channel-get p)))
     (map place-wait pls)))
