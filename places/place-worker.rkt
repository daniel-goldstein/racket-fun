#lang racket

(provide place-main)

(define (place-main pch)
  (define worker-index (place-channel-get pch))
  (define expr (place-channel-get pch))
  (define answer (format "The result from worker ~a is: ~a"
                         worker-index
                         (eval-load expr worker-index)))
  (place-channel-put pch answer))

(define (eval-load expr worker-index)
  (define ns (make-base-namespace))
  (eval `(,expr ,worker-index) ns))
