; sum of divisors
(define (sum-of-divisors n)
  (define (iter n i result)
    (cond
      ((> i n) result)
      ((= (remainder n i) 0) (iter n (+ i 1) (+ i result)))
      (else (iter n (+ i 1) result))
    )
  )
 (iter n 1 0))

; checks whether a number is prime
(define (is-prime? n)
  (=(sum-of-divisors n) (+ 1 n))
)

; sum of prime divisors
(define (sum-of-prime-divisors n)
  (define (iter n i result)
    (cond
      ((> i n) result)
      ((and (is-prime? i) (= (remainder n i) 0)) (iter n (+ i 1) (+ i result)))
      (else (iter n (+ i 1) result))
    )
  )
 (iter n 1 0)
)