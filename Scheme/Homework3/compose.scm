(define (id x) x)
(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define (square x) (* x x))
(define (cube x) (* x x x))

;(define (compose f g x)
;  (f (g x)))
 
(define (compose f g) (lambda (x) (f (g x))))