#lang sicp

;; Exercise 1

 ;; >>> count_multiples(2, 6)     # 2 * 3 = 6
 ;; 1
 ;; >>> count_multiples(2, 12)    # 2 * 2 * 3 = 12
 ;; 2
 ;; >>> count_multiples(3, 11664): 6

(define (count-multiples x y) x)

;;  Exercise 2: Write a recursive function that finds the maximum value in a list
;; (maxval '(1 9 -3 7 13 2 3)) -> 13

(define (find-largest x largest)
  (if (eq? '() x)
      largest
      (if (> (car x) largest)
          (find-largest (cdr x) (car x))
          (find-largest (cdr x) largest))))

(define (maxval x) (find-largest x (car x)))
     
;;  Exercise 3: Write a function that flattens nested Python lists. For example:
;; flatten( '(1 '(2 '(3 4) 5) 6)) -> (1 '(2 '(3 4) 5) 6)



