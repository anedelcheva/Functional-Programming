; a function which puts the digits of a number into a list
(define (digits-to-list n)
  (define (iter n result)
    (cond
      ((= n 0) result)
      (else (iter (quotient n 10) (cons (remainder n 10) result)))
    )
  )
  (iter n '())
)

; reduce
(define (reduce oper start-value l)
  (if (null? l)
      start-value
      (oper (car l) (reduce oper start-value (cdr l)))))

; function range puts the numbers from the interval [a, b] into a list
(define (range a b)
  (define (iter a b result)
     (if (> a b)
         result
         (iter (+ a 1) b (cons a result))
     )
  )
  (iter a b '())
)

; factorial with range and reduce
(define (fact n)
  (reduce * 1 (range 1 n)))

; definition of sum using reduce
(define (sum l)
  (reduce + 0 l))

; sum of the factorials of the digits of a number
(define (sum-factorial-digits n)
  (reduce + 0 (map fact (digits-to-list n))))

(define (list n)
  (reverse (range 1 n)))

; digit-factorials n
(define (digit-factorials n)
  (define (iter l result)
    (cond
      ((null? l) result)
      ((= (sum-factorial-digits (car l)) (car l)) (iter (cdr l) (cons (car l) result)))
      (else (iter (cdr l) result))
    )
  )
  (reverse (iter (list n) '()))
)