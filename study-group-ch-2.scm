#lang racket

(define (inc x) (+ x 1))

"Exercise 2.1"

(define (make-rat n d)
  (cond ((and (< n 0) (< d 0)) (cons n (* -1 d)))
        ((< d 0) (cons (* -1 n) (* -1 d)))
        (else (cons n d))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat 2 -3)) ; -2/3

(newline)
"TODO: Exercise 2.2"

(define (average x y)
  (/ (+ x y) 2))

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))
(x-point (make-point 2 3))  ; 2

(define (make-segment start-seg end-seg)
  (cons start-seg end-seg))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define segment (make-segment (make-point 2 3) (make-point 4 5)))
segment ;;'((2 . 3) 4 . 5)
(start-segment segment) ;; '(2 . 3)
(end-segment segment) ;; '(4 . 5)

(define (midpoint-segment segment)
  (let ((a (start-segment segment))
        (b (end-segment segment)))
    (cons 
     (average (x-point a) (x-point b))
     (average (y-point a) (y-point b)))))

(midpoint-segment segment) ;; '(3 . 4)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point (make-point 3 4)) ;; (3,4)

"Exercise 2.4"

(define (cons* x y) (lambda (m) (m x y)))

((cons* 2 3) +) ;; 5

(define (car* z) (z (lambda (p q) p)))
(define (cdr* z) (z (lambda (p q) q))) 

;; (car* (cons* x y))
;;         ((lambda (m) (m x y)) (lambda (p q) p))
;;   z: (lambda (m) (m x y))
;;   m: (lambda (p q) p)

(car* (cons* 1 2)) ; 1
(cdr* (cons* 1 2)) ; 2

"Exercise 2.5"

;; This works because the numbers 2 and 3 are prime and close to one another

(define (cons-i a b)
  (* (expt 2 a) (expt 3 b)))

(define (car-i id)
  (if (= (remainder id 2) 0)
      (+ 1 (car-i (/ id 2)))
      0))

(define (cdr-i id)
  (if (= (remainder id 3) 0)
      (+ 1 (cdr-i (/ id 3)))
      0))

(car-i (cons-i 4 5)) ;; 4
(cdr-i (cons-i 4 5)) ;; 5

"TODO: Exercise 2.6"

(define zero (lambda (f) (lambda (x) x))) ; means don't do the function at all
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

((three inc) 0) ; 3

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(((add-1 two) inc) 0) ;; 3

;; (define (plus a b) (lambda (f) (lambda (x) ... )))
;; How do you implement plus (+)

(define (plus a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;; 1. `((b f) x)`: means do `f` b-times
;; 2. `(a f)`: do `f` a-times.
;; 3. Apply the result of `((b f) x)` to the result of `(a f)`
;; = a[f f f] b[f f f f]

(define (zero? n) ((n (lambda (x) #f)) #t))

(zero? three) ;; #f
(zero? zero) ;; #t

;; -------------

(define (TRUE x y) x)
(define (FALSE x y) y)

(TRUE 0 1) ; 0
(FALSE 0 1) ; 1

(define (NOT x) (x FALSE TRUE))
(NOT TRUE) ; #<procedure:FALSE>

(define (NOT* x) (lambda (b a) (x a b)))
((NOT* TRUE) TRUE FALSE)

(define (AND x y) (x y x))

(AND FALSE TRUE) ; #<procedure:FALSE>

;; T/T = T, F/T = F, T/F = F, F/F = F

(define (OR x y) (x x y))

"Exercise 2.17"

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

(last-pair '(1 2 3 4 5)) ;; '(5)

"Exercise 2.18"

(define (reverse items)
  (define (iter items reversed-items)
    (if (null? (cdr items))
        (cons (car items) reversed-items)
        (iter (cdr items) (cons (car items) reversed-items))))
  (iter items '()))

(reverse '(1 2 3)) ; '(3 2 1)

"Exercise 2.20"

(define (f** x y . z) z)
(f** 1 2 3 4 5 6) ; '(3 4 5 6)

(define (same-parity x . y)
  (define (even-odd f x) (if (f x) x null))  
  (define (iter f items parity-items)
    (cond ((null? items) parity-items)
          ((null? (even-odd f (car items))) (iter f (cdr items) parity-items))
          (else (iter f (cdr items) (cons (even-odd f (car items)) parity-items)))))
  (if (even? x)
      (iter even? y '())
      (iter odd? y '())))

(same-parity 1 2 4 5 6) ; '(5)
(same-parity 2 2 4 5 6) ; '(6 4 2)

"Exercise 2.21"

(define (square x)
  (* x x))

(define (square-list items)
  (if (null? items)
      null
      (cons (square (car items)) (square-list (cdr items)))))

(square-list '(1 2 3)) ; '(1 4 9)

(define (square-list* items)
  (map square items))

(square-list* '(1 2 3)) ; '(1 4 9)

"Exercise 2.22"

(define (square-list** items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
 (iter items null))

(square-list** '(1 2 3)) ; '(9 4 1)

;; The answer is in the reverse order because it recurs all the way down to the end of the list before `cons`ing.
;; It is implemented using iterative linear recursion where it holds the state in memory at every step. It `cons`es the state together from back-to-front.

(define (square-list*** items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items null))

(square-list*** '(1 2 3)) ; '(((() . 1) . 4) . 9)

;; Expansion:
;; (iter '(2 3) (cons null (square 1)))
;; (iter '(3) (cons (cons null (square 2))))
;; (iter '() (cons (cons (cons null (square 2))) (square 3)))
;; (cons (cons (cons null (square 2))) (square 3))
;; In the simplest case, `(cons null 2)` -> `'(() . 2)`, meaning the data is in the content decrement register. See:
(list? '(1 2)) ; #t
(list? '(1 . 2)) ; #f
;; Thus in the linked list, `((() . 1) . 4)` `(() . 1)` is the car and the `4` is the cdr.
;; `((() . 1) . 4)` is the car for `(((() . 1) . 4) . 9)` For more information on the difference, see:
(list '() 1) ; '(() 1)
(cons '() 1) ; '(() . 1)

"Exercise 2.23"

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
;
; 57
; 321
; 88

(define (for-each* f items)
  (if (null? items)
      null
      ((lambda (item rest-items)
         (f item)
         (for-each f rest-items)) (car items) (cdr items))))

(for-each* (lambda (x) (newline) (display x))
          (list 57 321 88))
;
; 57
; 321
; 88
  