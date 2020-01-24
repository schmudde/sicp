#lang racket

; AMB Evaluator
; API Breakage...
; All (seval-* procedures now take success and fail arguments.
; success is a procedure taht is called to make forward progress.
; fail is a procedure that is called to backtrack
;
; `success` is called with (success result fail). (2 arguments)
; `fail` is called with no arguments.
;
; You can never return any result. ALL RESULTS are communicated through the `success` procedure

(define (seval sexp success fail environment) ;; add `success`/`fail` to the `seval`
  ;(displayln sexp) ; good for testing
  (cond ((primitive? sexp) (seval-primitive sexp success fail environment))
        ((symbol? sexp) (seval-symbol sexp success fail environment))
        ((combination? sexp) (seval-combination sexp success fail environment))
        (else (error "Bad expression"))))

(define (primitive? sexp)
  (or (number? sexp)
      (boolean? sexp)))

(define (combination? sexp) (list? sexp))

(define (seval-primitive sexp success fail environment) (success sexp fail))

(define (seval-symbol sexp success fail environment)
  (success (lookup-environment environment sexp) fail))

; Special forms/procedures
(define (seval-combination sexp success fail environment)
  (cond ((define? sexp) (seval-define sexp success fail environment))
        ((if? sexp) (seval-if sexp success fail environment))
        ((begin? sexp) (begin-expression-list sexp success fail environment))
        ((amb? sexp) (seval-amb sexp success fail environment))        
        ((lambda? sexp) (seval-lambda sexp success fail environment))
        ((procedure? sexp) (seval-procedure sexp success fail environment))        
        (else "Unknown combination --- seval-combination")))

; NEW FEATURE: AMB
(define (amb? sexp) (eq? (car sexp) 'amb))

(define (amb-exprs sexp) (cdr sexp))

(define (seval-amb sexp success fail environment)
  (define (try-next remaining)
    (if (null? remaining)
        (fail) ; NO CHOICES. Call fail.
        (seval (car remaining) success
               (lambda () (try-next (cdr remaining))) ; On failure. Try next value.
               environment)))
  (try-next (amb-exprs sexp)))
                 
  

; define
(define (define? sexp) (eq? (car sexp) 'define))
(define (define-name sexp) (cadr sexp))
(define (define-value sexp) (caddr sexp))

;; define doesn't return anything, so it's different
(define (seval-define sexp success fail environment)
  (seval (define-value sexp)
         (lambda (result fail) ;; callback function madness
           (define-in-environment! environment (define-name sexp) result)
           (success 'ok fail))
        fail
        environment))
         

; (if test consequence alternative)
(define (if? sexp) (eq? (car sexp) 'if))
(define (if-test sexp) (cadr sexp))
(define (if-consequence sexp) (caddr sexp))
(define (if-alternative sexp) (cadddr sexp))
(define (seval-if sexp success fail environment)
  (seval (if-test sexp) ; there is no return value
         (lambda (result fail2) 
           (if result
               (seval (if-consequence sexp) success fail2 environment) ; this is the return
               (seval (if-alternative sexp) success fail2 environment) ; or this is the return
               )
           ) fail environment))

; (begin expr1 expr2 ... exprn) `begin` returns `exprn`
; `(begin (displayln "hello") 2)` returns `2`.
(define (begin? sexp) (eq? (car sexp) 'begin))
(define (begin-expression-list sexp success fail environment) (seval-expression-list (cdr sexp) success fail environment))
(define (seval-expression-list sexp success fail environment)
  (cond ((null? (cdr sexp)) (seval (car sexp) success fail environment))
        (else (seval (car sexp) success fail environment)
              (seval-expression-list (cdr sexp) success fail environment))))

; (lambda (args) expr1 expr2 ... exprn)
; lambda takes a sequence of expressions (like begin). Returns value of evaluating the last one. 
(define (lambda? sexp) (eq? (car sexp) 'lambda))
(define (lambda-args sexp) (cadr sexp))
(define (lambda-exprs sexp) (cddr sexp))
(define (seval-lambda sexp success fail environ)
  (success (make-procedure (lambda-args sexp) (lambda-exprs sexp) environ) fail))


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

(define (seval-procedure sexp success fail environment)
  (seval (procedure-proc sexp)
         (lambda (proc fail2)
           ;proc is the procedure
           (seval-all-arguments (procedure-args sexp)
                                (lambda (args fail3)
                                  (success (if (user-procedure? proc)
                                               (apply-user-procedure proc args)
                                               (apply proc args)) fail3)
                                  ) fail2 environment)
           ) fail environment))

(define (seval-all-arguments argexprs success fail environment)
  (define (iter remaining argvalues fail2)
    ; remaining is arguments not yet evaluated
    ; argvalues is a list of arguments evaluated so far
    (if (null? remaining)
        (success argvalues fail2)
        (seval (car remaining)
               (lambda (value fail3)
                 (iter (cdr remaining) (append argvalues (list value)) fail3)
                 ) fail2 environment))
        )
    (iter argexprs '() fail)
    )


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

(seval 23 (lambda (result fail) (display result)) (lambda () (displayln "Fail")) env) ; 23
(seval '+ (lambda (result fail) (display result)) (lambda () (displayln "Fail")) env) ; #<procedure:+>
(seval '(define x 42) (lambda (result fail) (display result)) (lambda () (displayln "Fail")) env) ; ok
(seval 'x (lambda (result fail) (display result)) (lambda () (displayln "Fail")) env) ; 42
(seval '(if false 2 3) (lambda (result fail) (display result)) (lambda () (displayln "Fail")) env) ; 3
(seval '(if true 2 3) (lambda (result fail) (display result)) (lambda () (displayln "Fail")) env) ; 2
(seval '(+ 1 2) (lambda (result fail) (display result)) (lambda () (displayln "Fail")) env) ; 3
(seval '(amb 1 2 3) (lambda (result fail) (display result)) (lambda () (displayln "Fail")) env) ; 1
(seval '(amb 1 (amb 2 3 4) 5) (lambda (result fail) (display result) (fail)) (lambda () (displayln "Fail")) env) ; 12345Fail
