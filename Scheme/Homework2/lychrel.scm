; 1. помощна функция за обръщане на цифрите на числото n в обратен ред
(define (reverse-int n)
  (define (iter n result)
    (cond
      ((= n 0) result)
      (else (iter (quotient n 10) (+ (remainder n 10) (* 10 result))))
    )
  )
(iter n 0)
)

; 2. помощна функция, проверяваща дали числото n е палиндром
(define (palindrome n)
  (= n (reverse-int n))
)
; 3. помощна функция, която връща истина, ако числото n не е число на lychrel
(define (not-lychrel n)
  (palindrome (+ n (reverse-int n)))
)

; 4. иии супер функцията, която сумира числата, които не са числа на lychrel, в затворения интервал [a, b]
(define (non-lychrel-sum a b)
  (define (iter a b result)
    (cond
      ((> a b) result)
      ((not-lychrel a) (iter (+ a 1) b (+ a result)))
      (else (iter (+ a 1) b result))
    )
  )
 (iter a b 0)
)