#lang racket

(provide place-main)

(define (place-main pch)
  (define worker-index (place-channel-get pch))
  (define f (place-channel-get pch))
  (place-channel-put pch (execute f worker-index)))

(define (execute f worker-index)
  (define ns (make-base-namespace))
  (eval `(,f ,worker-index) ns))
