#lang sicp

;; Exercise 1.1

10 ;; 10
(+ 5 3 4) ;; 12
(- 9 1) ;; 8
(/ 6 2) ;; 3
(+ (* 2 3) (- 4 6)) ;; 4
(define a 3) ;; 3
(define b (+ a 1)) ;;4
(+ a b (* a b)) ;; 19
(= a b) ;; #f
(if (and (> b a) (< b (* a b))) b a) ;; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ;; 16
(+ 2 (if (> b a) b a)) ;; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ;; 16

; Exercise 1.2

(/ (+ 5 (+ 4 (- 2 (- 3 (+ 6 (/ 4 5)))))) (* 3 (* (- 6 2) (- 2 7)))) ;; -37/150

; Exercise 1.3

(define (square x)
  (* x x))

(define (f x y z)
  (if (>= x y z)
      (if (> y z) (+ (square x)(square y))
          (+ (square x)(square z)))
      (+ (square y)(square z))))

(f 1 2 3)

; Exercise 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; Evaluating `+` -> `#<procedure:+>`
; If returns either `#<procedure:+>` or `#<procedure:->` not the SYMBOL `+` or `-` because the if evaluates and then returns
; The order of operation allows for operators (objects in the first position of the list) that are compound expressions (i.e. `(+ (* 2 2) (* 2 1))`)   

; Exercise 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; evaluates it in the procedure call!! not in the `if`

;; Applicitive order `(test 0 (p))` would never evaluate `y` but it blows up because it evaluates both possible returns
;; Normal order `(test 0 (p))` returns `0`, `(test 0 (p))` blows up (try it with `#lang lazy` in Racket)

; Exercise 1.6

(define (new-if predicate consequent alternative)
  (cond (predicate consequent)
        (else alternative)))

(new-if (= 2 3) 0 5)

(define (average x y)
  (/ (+ x y) 2))

;; guess = 1
;; x = the number we are search for the square root of

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(good-enough? 1.414213562 2) ;; #t

(define (improve guess x)
  (average guess (/ x guess)))

(improve 1.414 2.0) ;; 1.414213578500707

;; guess = 1
;; x = the number we are search for the square root of

(define (sqrt-iter-alt guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-alt (improve guess x)
                     x)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                  x)))

;; Why doesn't `new-if` work?
;; In this specific case, `if` is a **special form**. The recursive statement isn't evaluated unless the `<alternative>` is called. `new-if` is a normal procedure. In Scheme (and many other languages), arguments are *fully evaluated* before the procedure is called. This is known as **applicative order**. &there4; `sqrt-iter` is called every time `new-if` is called, resulting in an infinite loop.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using nested functions with lexical scope ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; guess = 1
;; x = the number we are search for the square root of

(define (sqrt* x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
  
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

;; Exercise 1.8

(define (cube x) (* x x x))

(define (good-enough-cubrt? guess x)
  (< (abs (- (cube guess) x)) 0.0001))

(define (improve-cubrt guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess))
     3))

(define (cubert-iter guess x)
  (if (good-enough-cubrt? guess x)
      guess
      (cubert-iter (improve-cubrt guess x) x)))

(define (cubert x)
  (cubert-iter 1.0 x))

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

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (plus1 a b) ;; recursive
  (if (= a 0)
      b
      (inc (plus1 (dec a) b))))

;; (plus1 2 2)
;; (inc (plus1 1 2))
;;      (inc (plus1 0 2))
;;           2

(define (plus2 a b) ;; iterative
  (if (= a 0)
      b
      (plus2 (dec a) (inc b))))

;; (plus2 2 2)
;; (plus2 1 3)
;; (plus2 0 4)
;; 4

"Exercise 1.10"

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

;; TODO: Exercise 1.11

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


;; far left [2, 0] [y x] = (+ [1 -1] [1 0]): row 2 indexed by 0
;; far right 

;; [1 2] = (+ [0 1] [1 1])

"Exercise 1.12"
(define (pascals-triangle x y)
  (cond ((= x 0) 1)
        ((< x 0) 0)
        ((= x y) 1)
        ((> x y) 0)
        (pascals-triangle (+ (pascals-triangle (- x 1) (- y 1))
                             (pascals-triangle x (- y 1))))))

(pascals-triangle 0 4)
(pascals-triangle 1 4)
(pascals-triangle 2 4)
(pascals-triangle 3 4)
(pascals-triangle 4 4)

"Exercise 1.13"

(define (psi) (/ (- 1 (sqrt 5)) 2))
(define (phi) (/ (+ 1 (sqrt 5)) 2))

(define (fib n) (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (fibber n) (/ (- (expt (phi) n) (expt (psi) n)) (sqrt 5)))

(fib 7)
(fibber 7)

"Exercise 1.15"
(define (proc x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle i)
  (display i)
  (if (not (> (abs angle) 0.1))
      i 
      (proc (sine (/ angle 3.0) (+ i 1) ))))

(sine 12.15 0)

;; space requirement is logrithmic
;; number of steps is logrithmic

"Exercise 1.16"

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (faster-expt b n)
  (define (faster-expt-iter b n result)
    (cond ((= n 0) result)
          ((even? n) (faster-expt-iter (square b) (/ n 2) result)) ;; doesn't really move the calculation forward, it just changes the nature of the problem (result doesn't update)
          (else (faster-expt-iter b (- n 1) (* b result)))))
  (faster-expt-iter b n 1))

(faster-expt 2 5)

; (faster-expt 2 5 1)
; (faster-expt 2 4 2)
; (faster-expt 4 2 2)
; (faster-expt 16 1 2)
; (faster-expt 16 0 32)
; 32

"Exercise 1.17"

;; in which it is assumed that our language can only add, not multiply)

(define (dumb-* a b)
  (if (= b 0)
      0
      (+ a (dumb-* a (dec b)))))

(dumb-* 4 5) ;; 20

"Exercise 1.18"

(define (dumb-*-i a b)
  (define (iter a b result)
    (if (= b 0)
        result
        (iter a (dec b) (+ a result))))
  (iter a b 0))

(dumb-*-i 5 4) ;; 20

"Exercise 1.30"

;; calculate cubes of integers from 1 to 10: `(sum-cubes 1 10)`: `3025`

(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(sum square 1 inc 5) ;; 55

;; iterative sum
;; the term is the `<procedure>` that you want to apply. Since we want to sum, we use `+`

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (+ result (iter (next a) (term a)))))
  (iter a 0))

(sum-iter square 1 inc 3) ;; 55

"Exercise 1.31"

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(product square 1 inc 5) ;; 14400

(define (identity x) x)
(define (factorial-using-product n) (product identity 1 inc n))

"Exercise 1.32"

;; implement sum/product as a more general procedure called `accumulate`

(define (accumulate combiner null-value a b)
  (define next inc)
  (define term square)
  (define (ranger a b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (ranger (next a) b))))
  (ranger a b))

(accumulate + 0 1 3) ;; 14

(define (accumulate-i combiner null-value a b)
  (define next inc)
  (define term square)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(accumulate-i + 0 1 5) ;; 55