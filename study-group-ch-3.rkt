#lang racket

"Streams"

#;(define (display-stream s)
  (if (null? s)
      'ok
      (begin
        (displayln (car s))
        (display-stream (force (cdr s))))))

(define (enumerate-interval start stop)
  (if (> start stop)
      empty-stream
      (stream-cons start (enumerate-interval (+ start 1) stop))))

(define a (enumerate-interval 1 10))
(stream-for-each displayln a)

(define (stream-map proc s)
  (if (stream-empty? s)
      empty-stream
      (stream-cons (proc (stream-first s))
                   (stream-map proc (stream-rest s)))))

; Example

(define nums (enumerate-interval 1 10)) ;; (stream-for-each displayln nums)
(define squares (stream-map (lambda (x) (* x x)) nums))
(define invsquares (stream-map (lambda (x) (/ 1.0 x)) squares))

; Implement stream-sum

(define (stream-sum s)
  (if (stream-empty? s)
      0
      (+ (stream-first s) (stream-sum (stream-rest s)))))

(stream-sum invsquares)

; (stream-for-each displayln invsquares) ; this is a racket function

"Exercise 3.51"

(define (show x)
  (displayln "****")
  (displayln x)
  x)

(define x (stream-map show (enumerate-interval 0 10)))

(stream-ref x 7)
; ****
; 7
; 7
(stream-ref x 7)
; 7

"Bottom of page 328 through 329"

(define (add-streams s1 s2)
  (if (or (stream-empty? s1) (stream-empty? s2))
      empty-stream
      (stream-cons (+ (stream-first s1) (stream-first s2))
                   (add-streams (stream-rest s1) (stream-rest s2)))))

(define ones (stream-cons 1 ones))
(stream-ref ones 23434) ; 1
(define integers (stream-cons 1 (add-streams ones integers)))

(stream-ref integers 10) ; 11
(stream-ref integers 0) ; 1

; define `fibs` in a function that only references `fibs`.
; `stream-rest` gets the nth until
(define fibs
  (stream-cons 0
               (stream-cons 1
                            (add-streams (stream-rest fibs) fibs))))

(stream-ref fibs 0) ; 0
(stream-ref fibs 5) ; 5
(stream-ref fibs 10) ; 55