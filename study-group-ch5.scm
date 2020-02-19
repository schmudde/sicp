#lang sicp


(define (make-register)
  (let ((value 'unassigned))
    (define (dispatch m . args)
      (cond ((eq? m 'get) value)
            ((eq? m 'set!) (set! value (car args)))
            (else (error "bad operator"))))
    dispatch))

(define (make-stack)
  (let ((items '()))
    (define (dispatch m . args)
      (cond ((eq? m 'push) (set! items (cons (car args) items)))
            ((eq? m 'pop) (let ((item (car items))) nil)) ;; TODO: replace `nil`
            (else (error "smooth operator"))))
    dispatch))

; create some registers
(define n (make-register))
(define result (make-register))

; "machine instructions". Hardwired. There are no inputs.
(define (dec->n) (n 'set! (- (n 'get) 1)))

; Compute product of n and result. Put value back in result.
(define (p->result) (result 'set! (* (n 'get) (result 'get))))
(define (1->result) (result 'set! 1))
(define (n-is-one?) (= (n 'get) 1))

; program counter

(define pc (make-register))

(define (goto-fact-test)
  (pc 'set! fact-test))
(define (goto-fact-update)
  (pc 'set! fact-update))
(define (branch-fact-done-if-n-is-1)
  (if (= (n 'get) 1)
      (pc 'set! fact-done)
      '()))

(define fact-entry (list 1->result goto-fact-test))
(define fact-test (list branch-fact-done-if-n-is-1 goto-fact-update))
(define fact-update (list p->result dec->n goto-fact-test))
(define fact-done (list))

(define (execute)
  (let ((instructions (pc 'get)))
    (cond ((null? instructions) fact-done)
          (else
           (pc 'set! (cdr instructions))
           ((car instructions))
           (execute)))))

;; tests
(n 'set! 4)
(1->result)
(n-is-one?) ; #f
(p->result)
(dec->n)
(n-is-one?) ; #f
(p->result)
(dec->n)
(n-is-one?) ; #f
(p->result)
(dec->n)
(n-is-one?) ; #t
(result 'get) ;24
"---------------"
(n 'set! 4)
(pc 'set! fact-entry)
(execute)
(result 'get)