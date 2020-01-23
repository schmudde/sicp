#lang sicp

(define (counter n)
  (define (incr)
    (set! n (+ n 1))
    n)
  incr)

((counter 5)) ; 6

(define c (counter 0))
(define d (counter 10))

(c) ; 1
(c) ; 2
(d) ; 11
(d) ; 12

"Exercise 3.1"

(define (make-accumulator value)
  (define (update delta)
    (set! value (+ value delta))
    value)
  update)
  
(define A (make-accumulator 5))
(A 10) ; 15
(A 10) ; 25

"Exercise 3.2"

(define (make-monitored f)
  (define incr 0)
  (define (mf args)
    (cond ((eq? args 'how-many-calls?) incr)
          ((eq? args 'reset-count) (set! incr 0))
          (else
           (set! incr (+ incr 1))
           (f args))))
  mf)

(define s (make-monitored sqrt))
(s 100) ; 10
(s 100) ; 10
(s 'how-many-calls?) ; 2
(s 'reset-count)
(s 'how-many-calls?) ; 0
(s 100) ; 10
(s 100) ; 10
(s 100) ; 10
(s 'how-many-calls?) ; 3

"TODO: Exercise 3.8"

(define (minuser)
  (define local-state 1)
  (define (f n)
    (set! local-state (* local-state -1))
    (display (+ local-state n))
    (+ local-state n))
  f)

; (+ (f 0) (f 1)) ; 0
; (+ (f 1) (f 0)) ; 1

"Exercise 3.9"

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 6) ; 720

; GLOBALS: 1. `(factorial n)` is a procedure > a procedure is an object with 2. a reference back to the environment where they were defined.
; LOCAL: `(factorial 6)` links back to `(factorial n)`   
;                          recurse                       |
;        `(factorial 5)` links back to `(factorial n)`   8
;                          recurse                       |  chain of environments is a "call stack"
;        `(factorial 4)` links back to `(factorial n)`   8
;                           ....                         |

(define (g x)
  (+ x y))
(define y 10)
;; Global environment is `g(x)` and `y`: 10.
(define (h y)
  (g 2))
;; Local stack `h(y)` y: 1000
;; Calls `g(x)`. `g(x)` only sees the Global `y`.
(h 1000) ; therefore: 12, *local scope*.
;; if the solution as 1012, it would be *dynamic scope*

(define (factorial-i n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

; GLOBAL: `(factorial-i n)` and `(fact-iter p c m)`
; LOCAL: `(factorial-i 6)`
;        `(factorial-iter 1 1 6)`
;                 call
;        `(factorial-iter 6 2 6)` without TCO, this is new environment. With TCO it updates in place.
;                  ...

"Exercise 3.11"

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE ACCOUNT"
                       m))))
  dispatch)

; Where is the local state for `acc` kept?
; How is `acc` kept distinct from `(define acc2 (make-account 100))`

(define acc (make-account 50))

; GLOBAL: (ENVIRONMENT 0) `(make-account balance)` -> procedure object
;                         `(define acc (make-account 50))` -> procedure object
; LOCAL: `(make-account 50)` E1: balance: 50, withdraw: proc, deposit: proc, dispatch: proc
;                                                                            displatch: ENVIRONMENT: `(make-account 50)` LOCAL env
;                                                                                                    `(make-account balance)` GLOBAL env THROUGH the LOCAL
;                                                                                                                   (because `acc` was declared in GLOBAL)
; LOCAL: `((acc 'deposit) 40)` E2: `dispatch`, linked to E1, via E1 access to E0
;                           invokes
; LOCAL: `(define (deposit amount)` linked to E1, via E1 access to E0.
;                                   DOES NOT SEE E2 because it's linked to the environment where it is defined (E1) not where it is called (E2).

"Exercise 3.12"

; In Racket, must use mcons, mcar, mcdr

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define x* (list 'a 'b))
(define y* (list 'c 'd))
(define z* (append x* y*))
z* ; (a b c d)
(cdr (list 'a 'b)) ; (b)
(define w* (append! x* y*))
w* ; (a b c d)
(cdr x*) ; (b c d)

"Exercise 3.16"

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; (define c3 (set-car! 1 (set-cdr! 2 (set-cdr! 3 nil))))
(define c3 (cons 1 (cons 2 (cons 3 nil))))


;; 3: [x|→]→[x|→]→[x|.]
;; 4: [x|→]→[↓|→]→[x|.]
;;           →→→→→→↑
;;
;;     ↑→→→→↓ ↑→→→→↓
;; 7: [↑|↓] [↓|→]→[x|.]
;;       ↓→→→→↑↓→→→↑

"TODO: Exercise 3.22"


(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))
(define (set-front-ptr! q item) (set-car! q item))
(define (set-rear-ptr! q item) (set-cdr! q item))

(define (empty-queue?  q) (null? (front-ptr q)))
;; (define (make-queue) (cons '() '()))
(define (front-queue q)
  (if (empty-queue? q)
      (error "FRONT called with an empty queue" q)
      (car (front-ptr q))))
(define (insert-queue! q item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? q)
           (set-front-ptr! q new-pair)
           (set-rear-ptr! q new-pair)
           q)
          (else
           (set-cdr! (rear-ptr q) new-pair)
           (set-rear-ptr! q new-pair)
           q))))
(define (delete-queue! q)
  (cond ((empty-queue? q)
         (error "DELETE! called with an empty queue" q))
        (else
         (set-front-ptr! q (cdr (front-ptr q)))
         q)))

;;;;;;;;;;;;;

(define (make-queue)
  (let ((front-ptr nil)
        (rear-ptr nil))

    (define (empty-queue?) (null? front-ptr))
    (define (front-queue q)
      (if (empty-queue? q)
          (error "FRONT called with an empty queue" q)
          (car (front-ptr q))))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)))))
    (define (delete-queue! q)
      (cond ((empty-queue? q)
             (error "DELETE! called with an empty queue" q))
            (else
             (set-front-ptr! q (cdr (front-ptr q)))
             q)))

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr!)
            (else "Bad call, dude")))
    dispatch))