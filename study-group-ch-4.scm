#lang sicp

"Exercise 4.25"

;;  In applicative-order Scheme, when call (factorial 5), the call will not end. because, when call unless, even if (= n 1) is true, (factorial (- n 1)) will be called. so n will be 5, 4, 3, 2, 1, 0, -1 .... . In normal-order Scheme, this will work, Because normal-order Scheme uses lazy evaluation, when (= n 1) is true, (factorial n) will not be called. (via http://community.schemewiki.org/?sicp-ex-4.25)


#; (define-macro (unless-lazy predicate action alternative)
  `(if (not ,predicate) ,action ,alternative))

#; (define (unless-applicative predicate action alternative)
  (if (not predicate) action alternative))

#; (define (factorial n)
  (unless-applicative (= n 1)
                      (* n (factorial (- n 1)))
                      1))

#; (factorial 5)

; Section 4.3

#; (define (f x y)
  (+ x y))

#; (define (f x y success fail)
  (success (+ x y) fail))


(f 2 3 (lambda (result fail) (displayln "good") (displayln result )) (lambda () (displayln "fail")))