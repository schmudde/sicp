#lang racket

; Metacircular evaluator (Chapter 4)
; Three thigns:
; 1. Primitives
; 2. Symbols
; 3. Combinations (tuples)

(define (seval sexp environment)
  (cond ((primitive? sexp) (seval-primitive sexp environment))
        ((symbol? sexp) (seval-symbol sexp environment))
        ((combination? sexp) (seval-combination sexp environment))
        (else (error "Bad experssion")))
  )

(define (primitive? sexp)
  (or (number? sexp)
      (boolean? sexp)
      
      ))

(define (combination? sexp)
  (list? sexp))

(define (seval-primitive sexp environment) sexp)
(define (seval-symbol sexp environment)
  (lookup-environment environment sexp))

; Special forms/procedures
(define (seval-combination sexp environment)
  (cond ((define? sexp) (seval-define sexp environment))
        ((procedure? sexp) (seval-procedure sexp environment))
        (else "Unknown combination --- seval-combination")))

(define (define? sexp)
  (eq? (car sexp) 'define))

(define (define-name sexp) (cadr sexp))
(define (define-value sexp) (caddr sexp))
(define (seval-define sexp environment)
  (define-in-environment! environment
    (define-name sexp)
    (seval (define-value sexp) environment)))

; Evaluate a procedure
(define (procedure? sexp) (pair? sexp))
(define (procedure-proc sexp) (car sexp))
(define (procedure-args sexp) (cdr sexp))
(define (seval-procedure sexp environment)
  (let ((proc (seval (procedure-proc sexp) environment))
        ; Must evaluate each argument in the environment before calling the proc
        ; `args` may be a list of any valid primitives, symbols, or combinations (i.e. any valid s-expression)
        (args (map (lambda (arg) (seval arg environment)) (procedure-args sexp))))
    ;; Call the procedure
    (apply proc args) ;; executing the proc on the args
    ))

;;;;;;;;;;;;;;;;;
;; Environment ;;
;;;;;;;;;;;;;;;;;

(define (make-environment)
  (make-hash)) ; Racket hash table

(define (lookup-environment environment symbol)
  (hash-ref environment symbol))

(define (define-in-environment! environment symbol value)
  (hash-set! environment symbol value))

; Create the global environment
(define env (make-environment))
; Define "built-in" procedures
(define-in-environment! env '+ +)
(define-in-environment! env '* *)
(define-in-environment! env '- -)
(define-in-environment! env '/ /)

;;;;;;;;;;;
;; Tests ;;
;;;;;;;;;;;

(define-in-environment! env 'foo "string")
(lookup-environment env 'foo) ; "string"
(seval '(define foo 42) env)
(seval 'foo env) ; 42
(seval '(+ (* 2 3) (* 4 5)) env) ; 26