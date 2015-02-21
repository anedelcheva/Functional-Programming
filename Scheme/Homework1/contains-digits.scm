(define (contains-digit? n x)
    (cond
        ((= n x) #t)
        ((zero? n) #f)
        (else (or (= x (remainder n 10)) (contains-digit? (quotient n 10) x)))))

(define (contains-digits? x y)
  (cond
    ((= x y) #t)
    ((< y 10) (contains-digit? x y))
    ((contains-digit? x (remainder y 10)) (contains-digits? x (quotient y 10)))
    (else #f)
  )
)