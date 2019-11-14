;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname streams) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Stream is something that can be Folded
; So...

; (T) [Stream T] = (U) [[Folder T U] -> U]

; (T U) [Folder T U]
; f : T U -> U
; base : U
(define-struct folder (f base))

; [Folder Number Number]
(define sum (make-folder + 0))

; (X) [Folder X [List-of X]]
(define collect (make-folder cons '()))

; stream-sum : [Stream Number] -> Number
(define (stream-sum s) (s sum))

; to-list : (T) [Stream T] -> [List-of T]
(define (to-list s) (s collect))

; list->stream : (T) [List T] -> [Stream T]
(define (list->stream l)
  (λ (a-folder)
    (foldr (folder-f a-folder) (folder-base a-folder) l)))

; range->stream : Number Number -> [Stream Number]
(define (range->stream start end) ; start <= end
  (local [(define (loop n f acc)
            (if (= n end) acc (loop (add1 n) f (f n acc))))]
    (λ (a-folder)
      (loop start (folder-f a-folder) (folder-base a-folder)))))

; stream-foldr : (T U) [T U -> U] U [Stream T] -> U
(define (stream-foldr f base s)
  (s (make-folder f base)))

; stream-map : (T U) [T -> U] [Stream T] -> [Stream U]
(check-expect (stream-sum (stream-map sqr (range->stream 0 10))) 285)
(check-expect (stream-sum (stream-map sqr (list->stream (list 1 2 3)))) 14)
(define (stream-map f s)
  (λ (a-folder)
    (s (make-folder (λ (t u) ((folder-f a-folder) (f t) u))
                    (folder-base a-folder)))))

; stream-filter : (T) [T -> Boolean] [Stream T] -> [Stream T]
(check-expect (to-list (stream-filter even?
                                      (stream-map sqr (list->stream (list 1 2 3 4 5)))))
              (list 4 16))
(define (stream-filter pred s)
  (λ (a-folder)
    (s (make-folder (λ (t u) (if (pred t) ((folder-f a-folder) t u) u))
                    (folder-base a-folder)))))

; stream-ormap : (T) [T -> Boolean] [Stream T] -> Boolean
(check-expect (stream-ormap positive? (range->stream -10 10)) true)
(define (stream-ormap pred s)
  (s (make-folder (λ (t result) (or (pred t) result)) false)))

; stream-andmap : (T) [T -> Boolean] [Stream T] -> Boolean
(check-expect (stream-andmap positive? (stream-map sqr (range->stream -10 -5))) true)
(define (stream-andmap pred s)
  (stream-foldr (λ (t res) (and (pred t) res)) true s))


