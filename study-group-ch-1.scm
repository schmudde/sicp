#lang sicp

; Exercise 1.2

(/ (+ 5 (+ 4 (- 2 (- 3 (+ 6 (/ 4 5)))))) (* 3 (* (- 6 2) (- 2 7))))

; Exercise 1.3

(define (square x)
  (* x x))

(define (f x y z)
  (if (>= x y z)
      (if (> y z) (+ (square x)(square y))
          (+ (square x)(square z)))
      (+ (square y)(square z))))

(f 1 2 3)

; Exercise 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; (test 0 (p))

;; Applicitave order - is
;; Normal order 

; Exercise 1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5)

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(good-enough? 1.414213562 2)

(define (improve guess x)
  (average guess (/ x guess)))

(improve 1.414 2.0)

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

;; Why doesn't `new-if` work?
;; new-if is normal
;; cond is applicitave.

;; Exercise 1.7

(define (sqrt-iter* guess last-guess x)
  (if (< (/ (abs (- guess last-guess)) guess) 0.00001)
      guess
      (sqrt-iter* (improve guess x)
                  guess
                  x)))
                  
(define (sqrt x)
  (sqrt-iter* 1.4 0 x))

(sqrt 4)

;; Exercise 1.9

;; Example 1: recursive - keep putting stuff on the stack

;;(define (+ a b)
;;  (if (= a 0)
;;      b
;;      (inc (+ (dec a) b))))

;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6))))
;; (inc (inc 7))
;; (inc 8)
;; 9

;; Example 2: iterative/recursive TCO

;;(define (+ a b)
;;  (if (= a 0)
;;      b
;;      (+ (dec a) (inc b))))

;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9

;; Exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10) ;; 1024

(A 2 4) ;; 65536

(A 3 3) ;; 65536

(define (f2 n) (A 0 n)) ;; 2y
(define (g2 n) (A 1 n)) ;; 2^n where y = 4: (A (A (- 1 1) (A x (- 4 1)))) > 0 (* 2 (A x (- 3 1))) > ... (g2 4) = 16
(define (h2 n) (A 2 n)) ;; (h2 4) = 16*256
(define (k2 n) (* 5 n n)) ;; 5n^2

(g2 4)

(h2 4)

;; Exercise 1.11

;; recursive

(define (f3 n)
  (if (< n 3)
      n
      (+ (f3 (- n 1))
         (* 2 (f3 (- n 2)))
         (* 3 (f3 (- n 3))))))

(f3 2) ;; 2
(f3 3) ;; 4
(f3 4) ;; 11
(f3 5) ;; 25
;; iterative
;; seed the function with three values and a counter
;; n-1, n-2, n-3


(f3 4)
(* 2 2)
(* 3 (* 3 1))

(define (f4 n)
  (f4-iter n 1 2 4))
  
(define (f4-iter i 3ago 2ago last)
  (if (= i 0)
      3ago
      (f4-iter (- i 1)
               last
               2ago
               (+ (* 3 3ago) (* 2 2ago)))))

(f4 3)
               
               
