;1. The function "reverse-int" returns the reverse number of n
(define (reverse-int n)
  (define (iter n result)
    (cond
      ((= n 0) result)
      (else (iter (quotient n 10) (+ (remainder n 10) (* result 10))))
    )
  )
  (iter n 0)
)

;2. The function "palindrome" checks whether the number n is a palindrome 
(define (palindrome n)
  (= n (reverse-int n))
)

;3. The function "decimal-to-binary" converts the number n from decimal to binary
(define (decimal-to-binary n)
  (string->number (number->string n 2))
)

;4. The function "count-the-ones" counts the number of ones in a decimal number n 
(define (count-the-ones n)
  (define (iter n counter)
    (cond
      ((= n 0) counter)
      ((= (remainder n 10) 1) (iter (quotient n 10) (+ counter 1)))
      (else (iter (quotient n 10) counter))
    )
  )
 (iter n 0)
)

;5. The function "is-it-odd?" checks whether the number of ones in the binary converted number n is an odd number
(define (is-it-odd? n)
  (odd? (count-the-ones (decimal-to-binary n)))
)

;6. The function "is-hack-number" returns true if the number n in binary is a palindrome and the number of ones in its binary representation is an odd number 
(define (is-hack-number? n)
  (and (palindrome(decimal-to-binary n)) (is-it-odd? n))
)
;===============================================================================================================================================================
; identity
(define (id x) x)

; increment
(define (inc x) (+ x 1))

; the abstract function of higher order
(define (aggregate oper term pred? a b next initial-value)
  (define (get-result a prev-result)
  (if (pred? a) 
      (oper (term a) prev-result)
      prev-result
  )
)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (get-result a result))
    )
  )
  (iter a initial-value)
)

; the sum of the hack-numbers in the interval [a, b]
(define (sum-of-hack a b)
  (aggregate + id is-hack-number? a b inc 0))