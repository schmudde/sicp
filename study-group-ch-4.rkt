#lang racket

; Metacircular evaluator (Chapter 4)
; Three thigns:
; 1. Primitives
; 2. Symbols
; 3. Combinations (tuples)

(define (seval sexp environment)
  ;(displayln sexp) ; good for testing
  (cond ((primitive? sexp) (seval-primitive sexp environment))
        ((symbol? sexp) (seval-symbol sexp environment))
        ((combination? sexp) (seval-combination sexp environment))
        (else (error "Bad expression"))))

(define (primitive? sexp)
  (or (number? sexp)
      (boolean? sexp)))

(define (combination? sexp) (list? sexp))

(define (seval-primitive sexp environment) sexp)
(define (seval-symbol sexp environment)
  (lookup-environment environment sexp))

; Special forms/procedures
(define (seval-combination sexp environment)
  (cond ((define? sexp) (seval-define sexp environment))
        ((if? sexp) (seval-if sexp environment))
        ((begin? sexp) (begin-expression-list sexp environment))
        ((lambda? sexp) (seval-lambda sexp environment))
        ((and? sexp) (seval-and sexp environment))
        ((or? sexp) (seval-or sexp environment))
        ((procedure? sexp) (seval-procedure sexp environment))
        (else "Unknown combination --- seval-combination")))

; define
(define (define? sexp) (eq? (car sexp) 'define))
(define (define-name sexp) (cadr sexp))
(define (define-value sexp) (caddr sexp))
(define (seval-define sexp environment)
  (define-in-environment! environment
    (define-name sexp)
    (seval (define-value sexp) environment)))

; (if test consequence alternative)
(define (if? sexp) (eq? (car sexp) 'if))
(define (if-test sexp) (cadr sexp))
(define (if-consequence sexp) (caddr sexp))
(define (if-alternative sexp) (cadddr sexp))
(define (seval-if sexp environment)
  (if (seval (if-test sexp) environment)
      (seval (if-consequence sexp) environment)
      (seval (if-alternative sexp) environment)))

; (begin expr1 expr2 ... exprn) `begin` returns `exprn`
; `(begin (displayln "hello") 2)` returns `2`.
(define (begin? sexp) (eq? (car sexp) 'begin))
(define (begin-expression-list sexp environment) (seval-expression-list (cdr sexp) environment))
(define (seval-expression-list sexp environment)
  (cond ((null? (cdr sexp)) (seval (car sexp) environment))
        (else (seval (car sexp) environment)
              (seval-expression-list (cdr sexp) environment))))

; `(and? exp1 exp2 ... expn)` also `(and)`
(define (and? sexp) (eq? (car sexp) 'and))
(define (seval-and sexp environment)
  (if (null? (cdr sexp))
      #t
      (seval-expression-list (cdr sexp) environment)))

; `(or? exp1 exp2 ... expn)` also `(or)`
(define (or? sexp) (eq? (car sexp) 'or))
(define (seval-or sexp environment)
  (if (null? (cdr sexp))
      #f
      (seval-expression-list (cdr sexp) environment)))

; (lambda (args) expr1 expr2 ... exprn)
; lambda takes a sequence of expressions (like begin). Returns value of evaluating the last one. 
(define (lambda? sexp) (eq? (car sexp) 'lambda))
(define (lambda-args sexp) (cadr sexp))
(define (lambda-exprs sexp) (cddr sexp))
(define (seval-lambda sexp environ)
  (make-procedure (lambda-args sexp) (lambda-exprs sexp) environ))


(define (make-procedure args exprs environ)
  (list 'user-procedure args exprs environ)) ; A "lambda" object

(define (procedure-environment proc) (cadddr proc))
(define (procedure-exprs proc) (caddr proc))
(define (procedure-argnames proc) (cadr proc))
(define (user-procedure? proc)
  (and (list? proc) (eq? (car proc) 'user-procedure)))

(define (apply-user-procedure proc argvalues)
  ; Must make some data structure representing the elements of a procedure (the arguments, expression, and environnment in which it was defined)  
  ;; proc: (lambda (x y) (+ x y))
  ;; argvalues: 2 3
  ; 1. Create a new environment
  (let ((newenv (make-child-environment (procedure-environment proc))))
  ; 2. Bind the argument names in the new environment
    (bind-names newenv (procedure-argnames proc) argvalues) ; x y: put it in the environment
  ; 3. Evaluate the procedure expressions in the new environment
    (seval-expression-list (procedure-exprs proc) newenv) ; (+ x y)
    ))

(define (bind-names env argnames argvalues)
  (cond ((and (null? argnames) (null? argvalues)) 'ok)
        ((or (null? argnames) (null? argvalues)) (error "Argument mismatch"))
        (else (define-in-environment! env (car argnames) (car argvalues))
              (bind-names env (cdr argnames) (cdr argvalues)))))

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
    (if (user-procedure? proc)
        (apply-user-procedure proc args) ;user-define procedure (lambda)
        (apply proc args))               ; Built-in procedure (racket): executing the proc on the args
    ))


;;;;;;;;;;;;;;;;;
;; Environment ;;
;;;;;;;;;;;;;;;;;

(define (make-environment)
  (cons (make-hash) null)) ; Racket hash table in a list

(define (make-child-environment env)
  (cons (make-hash) env))

(define (lookup-environment environment symbol)
  ;; walk through the environments and see if the symbol exists in any parent, grandparent, ..., global 
  (if (null? environment)
      (error "undefined name")
      (if (hash-has-key? (car environment) symbol)
          (hash-ref (car environment) symbol)
          (lookup-environment (cdr environment) symbol))))

(define (define-in-environment! environment symbol value)
  (hash-set! (car environment) symbol value)) ;; FIFO: the latest environment off the stack

; Create the global environment
(define env (make-environment))
; Define "built-in" procedures
(define-in-environment! env '+ +)
(define-in-environment! env '* *)
(define-in-environment! env '- -)
(define-in-environment! env '/ /)
(define-in-environment! env '< <)
(define-in-environment! env '> >)
(define-in-environment! env '<= <=)
(define-in-environment! env '>= >=)
(define-in-environment! env '= =)
(define-in-environment! env 'true true)
(define-in-environment! env 'false false)

;;;;;;;;;;;
;; Tests ;;
;;;;;;;;;;;

(define-in-environment! env 'foo "string")
(lookup-environment env 'foo) ; "string"
(seval '(define foo 42) env)
(seval 'foo env) ; 42
(seval '(+ (* 2 3) (* 4 5)) env) ; 26
(seval '(if (> 1 0) true false) env) ; #t
(seval '(begin 4 3) env) ; 3
(seval '(lambda (x) (* x x)) env) ; returns user-procedure with (x) ((* x x)) and the env
(seval '(define square (lambda (x) (* x x))) env)
(seval '(square 4) env) ; 16
(seval '(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) env)
(seval '(fact 6) env) ; 720
(seval '(and) env) ; #t
(seval '(and 1 2) env) ;2
(seval '(or 1) env) ; 1
(seval '(or) env) ; #f