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

"Exercise 2.25"

(cadar (cddr '(1 3 (5 7) 9))) ; 7
(caar '((7))) ; 7
(cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7))))))))) ; 7

;; ((six cadr) '(1 (2 (3 (4 (5 (6 7))))))) could do 6 `cadr`'s on the list.

"Exercise 2.26"

(define x '(1 2 3))
(define y '(4 5 6))

(append x y) ; '(1 2 3 4 5 6)
(cons x y) ; '((1 2 3) 4 5 6)
(cons '(1 2 3) 4) ; '((1 2 3) . 4)
(list x y) ; '((1 2 3) (4 5 6)) 

"TODO: Exercise 2.27"

"TODO: Exercise 2.28"

"TODO: Exercise 2.33"

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 2 '(1 2 3)) ; 8

(define (map* p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(map (lambda (x) (+ 2 x)) '(1 2 3)) ; '(3 4 5)
(map* (lambda (x) (+ 2 x)) '(1 2 3)) ; '(3 4 5)

(define (append* seq1 seq2)
  (accumulate cons seq2 seq1))

;; initial: '(4 5 6) sequence: '(1 2 3)
;; (cons '(4 5 6) (accum cons '(4 5 6) '(1 2 3)))
;;                (cons 1 (accum cons '(4 5 6) '(2 3)))
;;                         (cons 2 (accum cons '(4 5 6) '(2 3)))
;;                                  (cons 3 (accum cons '(4 5 6) '()))
;;                                           return cons '(4 5 6) 

(append* '(1 2 3) '(4 5 6)) ; '(1 2 3 4 5 6)

(define fold-right accumulate)
(fold-right / 1 (list 1 2 3))
; (fold-left / 1 (list 1 2 3))
(fold-right list null (list 1 2 3))
; (fold-left list null (list 1 2 3))

; (1 2 3 4)

(append* '(1 2 3) '(4 5 6)) ; '(1 2 3 4 5 6)

#;(define (length* sequence)
  (accumulate -- 0 sequence))

"TODO: Exercise 2.38"

"TODO: Exercise 2.39"

"Exercise 2.53"

(list 'a 'b 'c) ; '(a b c)
(list (list 'george)) ; '((george))
(cdr '((x1 x2) (y1 y2))) ; '((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; '(y1 y2)
(pair? (car '(a short list))) ; #f
(memq 'red '((red shoes (blue socks)))) ; #f
(memq 'red '(red shoes blue socks)) ; '(red shoes blue socks)

"TODO: Exercise 2.54"



"Exercise 2.55"

(car (quote (quote abracadabra)))
;;           ^^^^^ returns this quote

"Bonus Exercise: Key/Value Pairs"

(define record '((foo 2) (bar 3) (spam 4)))

(define (assoc* key items)
  (if (null? items)
      false
      (if (eq? (caar items) key)
          (car items)
          (assoc key (cdr items)))))

;; add/replace a record
(define (add-entry key value record)
  (define (iter record front-of-list)
    (if (null? record)
        (append front-of-list (list (list key value)))
        (if (eq? (caar record) key)
            (append front-of-list (list (list key value)) (cdr record))
            (iter (cdr record) (cons (car record) front-of-list)))))
  (iter record '()))

(add-entry 'bar 23 record) ;; '((foo 2) (bar 23) (spam 4))
(add-entry 'foo 23 record) ;; '((foo 23) (bar 3) (spam 4))
(add-entry 'spam 23 record) ;; '((bar 3) (foo 2) (spam 23))
(add-entry 'spams 23 record) ;; '((spam 4) (bar 3) (foo 2) (spams 23))

"Bonus Exercise: Patterns"

(define record-2 '(job (Hacker Alyssa P) (computer programmer)))

(define (match job record)
  (cond ((and (null? job) (null? record)) #t)
        ((or (null? job) (null? record)) #f)
        ((eq? job '?) #t)
        ((and (list? job) (list? record) (match (car job) (car record)))
         (match (cdr job) (cdr record)))
        (else (equal? job record))))

(match '(job ? ?) record-2) ; #t

;; ((and (list? job) (list? record) (match (car job) (car record))) (match (cdr job) (cdr record)))
;;                                          'job       'job
;;                                          (equal? 'job 'job) #t
;;                                                                          '(? ?)   '((Hacker Alyssa P) (computer programmer))
;;                                                                          '(? ?)   '((Hacker Alyssa P) (computer programmer))
;;                                                                           ((and (list? job) (list? record) (match (car job) (car record))) (match (cdr job) (cdr record)))
;;                                                                                                                      '?        '(Hacker Alyssa P)        |     |
;;                                                                                                                    ((eq? job '?) #t)                     |     |
;;                                                                                                                                                         '()   '()
;;                                                                                                                                             ((and (null? job) (null? record)) #t)

(match '(job ? (? coder)) record) ; #f
(match '(? ? (computer ?)) record-2) ; #t

"Exercise 2.56"

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
;; (define (make-sum a1 a2) (list '+ a1 a2))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
;; (define (make-product m1 m2) (list '* m1 m2))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))


(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base x) (cadr x))
(define (exponent x) (caddr x))
(define (make-exponent e1 e2)
  (cond ((=number? e2 0) 1)
        ((=number? e2 1) e1)
        (else (list '** e1 e2))))

(make-exponent 2 2) ; '(** 2 2)
(exponentiation? (make-exponent 2 2)) ; #t
(base (make-exponent 2 3)) ; 2
(exponent (make-exponent 2 3)) ; 3

(define (deriv exp var)
  (cond ((number? exp) 0) ;; 0 **for c a constant** or a variable different than x. A number is a constant.
        ((variable? exp)  ;; 1 for a variable that is the same as x. 0 for c a constant or **a variable different than x**.
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-exponent (base exp) (exponent exp)))
        (else
         (error "unknown expression type -- DERIV" exp))))


(deriv '(* (* x y) (+ x 3)) 'x) ; '(+ (* x y) (* y (+ x 3)))

;; Implement the differentiation rule `d(u^n)/dr=nu^(n-1)(du/dr)
"Exercise 2.57"

"Objects: Exercise 1"

;; Type Tagging

(define (attach-tag tag contents) (cons tag contents))
(define (type-tag datum) (car datum))
(define (contents datum) (cdr datum))


(define (make-bob-box x y w h)
  (attach-tag 'bob-box
              (cons (cons x y) (cons w h))))
; type-check function
(define (bob-box? box) (eq? (type-tag box) 'bob-box))
(define (bob-width box)
  (car (cdr (contents box))))
(define (bob-height box)
  (cdr (cdr (contents box))))
(define (bob-area box)
  (* (bob-width box)
     (bob-height box)))


(define (make-alice-box x1 y1 x2 y2)
  (attach-tag 'alice-box
              (cons (cons x1 y1) (cons x2 y2))))
; type-check function
(define (alice-box? box) (eq? (type-tag box) 'alice-box))
(define (alice-width box)
  (abs (- (car (cdr (contents box)))
          (car (car (contents box))))))
(define (alice-height box)
  (abs (- (cdr (cdr (contents box)))
          (cdr (car (contents box))))))
(define (alice-area box)
  (* (alice-width box)
     (alice-height box)))

(define a (make-alice-box 1 2 3 4))
(alice-area a)
(define b (make-bob-box 1 2 3 4))
(bob-area b)

(define (width* b)
  (cond ((bob-box? b) (bob-width b))
        ((alice-box? b) (alice-width b))))

(define (height* b)
  (cond ((bob-box? b) (bob-height b))
        ((alice-box? b) (alice-height b))))

(define (area* b)
  (cond ((bob-box? b) (bob-area b))
        ((alice-box? b) (alice-area b))))

(width* a) ; 2

"Racket Hash Table: Dispatch Approach"

; (make-hash) ; Creates a mutable hash table
; (hash-set! h key val) ; Add an entry
; (hash-ref h key) ; Look up a value

(define registry (make-hash))
(define (register name tag func)
  (hash-set! registry (list name tag) func))
(define (lookup name tag)
  (hash-ref registry (list name tag)))

; Registration of functions
(register 'width 'bob-box bob-width)
(register 'width 'alice-box alice-width)
(register 'height 'bob-box bob-height)
(register 'height 'alice-box alice-height)
(register 'area 'bob-box bob-area)
(register 'area 'alice-box alice-area)

; Generic procedure
(define (width box)
  ((lookup 'width (type-tag box)) box))

(define (height box)
  ((lookup 'height (type-tag box)) box))

(define (area box)
  ((lookup 'area (type-tag box)) box))

(width a) ; 2
(height a) ; 2

; Complaint: The code is a mess of names: width, height, bob-width, bob-height, alice-width, alice-height

"Message Passing Approach"

(define (make-bob-box** x y width height)
  (define (dispatch message)
    (cond ((equal? message 'width) width)
          ((equal? message 'height) height)
          ((equal? message 'type) 'bob-box)))
  dispatch)

(define (make-alice-box** x1 y1 x2 y2)
  (define (width)
    (abs (- x1 x2)))
  (define (height)
    (abs (- y1 y2)))
  (define (dispatch message)
    (cond ((equal? message 'width) (width))
          ((equal? message 'height) (height))
          ((equal? message 'type) 'alice-box)))
  dispatch)

(define a** (make-alice-box** 1 2 3 4))
(define b** (make-bob-box** 1 2 3 4))

(define (width** box)
  (box 'width))
(define (height** box)
  (box 'height))

(width** a**) ; 2
(height** a**) ; 2