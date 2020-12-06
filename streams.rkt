;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname streams) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Stream is something that can be folded
; So...

; A {T} [Stream T] = {U} [[T U -> U] U -> U] 
; and represents some stream of elements of
; type T. It is a function that given some
; folding operation and base case can be
; consumed to produce a single U

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Some ways to fold a stream
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; stream-sum : [Stream Number] -> Number
; sums the elements of the given stream
(define (stream-sum s) (s + 0))

; stream->list : {T} [Stream T] -> [List-of T]
; collects the elements of the stream into a list
(define (stream->list s) (reverse (s cons '())))

; stream-take : {T} [Stream T] Number -> [List-of T]
; takes n elements from the front of s
(define (stream-take s n)
  (reverse (s (lambda (x y) (if (= (length y) n)
                                y
                                (cons x y)))
              '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Some Stream constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; list->stream : {T} [List T] -> [Stream T]
; produces an iterable stream from the list
(check-expect (stream->list (list->stream '())) '())
(check-expect (stream->list (list->stream '(1 2 3 4 5 6))) '(1 2 3 4 5 6))
(check-expect (stream-take (list->stream '(1 2 3 4 5 6)) 3) '(1 2 3))
(define (list->stream l)
  (λ (f base) (foldl f base l)))

; range->stream : Number Number -> [Stream Number]
; turns a range of numbers [start, end) into a stream
(check-expect (stream->list (range->stream 0 0)) '())
(check-expect (stream->list (range->stream 0 10)) (build-list 10 identity))
(check-expect (stream-take (range->stream 0 10) 5) (build-list 5 identity))
(define (range->stream start end) ; start < end
  (local [(define (loop n f acc)
            (if (= n end) acc (loop (add1 n) f (f n acc))))]
    (λ (f base) (loop start f base))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Stream processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; stream-fold : {T U} [T U -> U] U [Stream T] -> U
; folds the elements of the stream according to the given
; function and base case
(check-expect (stream-fold + 0 (range->stream 0 10)) 45)
(define (stream-fold f base s) (s f base))

; stream-map : {T U} [T -> U] [Stream T] -> [Stream U]
; applies f to all elements of the stream as they are produced
(check-expect (stream-sum (stream-map sqr (range->stream 0 10))) 285)
(check-expect (stream-sum (stream-map sqr (list->stream (list 1 2 3)))) 14)
(define (stream-map f s)
  (λ (fold-f base)
    (stream-fold (λ (t u) (fold-f (f t) u)) base s)))

; stream-filter : {T} [T -> Boolean] [Stream T] -> [Stream T]
; filters out any elements of the stream that do not pass pred
(check-expect (stream->list (stream-filter even? (stream-map sqr (list->stream (list 1 2 3 4 5)))))
              (list 4 16))
(define (stream-filter pred s)
  (λ (f base)
    (stream-fold (λ (t u) (if (pred t) (f t u) u)) base s)))

; stream-ormap : {T} [T -> Boolean] [Stream T] -> Boolean
; do any elements of the stream pass pred?
(check-expect (stream-ormap positive? (range->stream -10 10)) true)
(check-expect (stream-ormap positive? (range->stream -10 -5)) false)
(define (stream-ormap pred s)
  (stream-fold (λ (t result) (or (pred t) result)) false s))

; stream-andmap : {T} [T -> Boolean] [Stream T] -> Boolean
; do all elements of the stream pass pred?
(check-expect (stream-andmap positive? (stream-map sqr (range->stream -10 -5))) true)
(check-expect (stream-andmap positive? (range->stream -10 10)) false)
(define (stream-andmap pred s)
  (stream-fold (λ (t result) (and (pred t) result)) true s))
