; дава сумата от делителите на числото n
(define (sum-of-divisors n)
  (define (iter n i result)
    (cond
      ((> i n) result)
      ((= (remainder n i) 0) (iter n (+ i 1) (+ i result)))
      (else (iter n (+ i 1) result))
    )
  )
 (iter n 1 0)
)

;проверява дали числото n е просто
(define (is-prime n)
  (= (sum-of-divisors n) (+ n 1))
)

;Според хипотезата на Голдбах всяко четно число по-голямо от 2 може да се представи като сума на 2 прости числа.
;Да се напише функция goldbach, която приема един аргумент х и връща по-малкото от двете прости числа, сумата на които дава х.
;Hint: тук трябва да обходим числата от 2 до х - 1 и на всяка стъпка да проверяваме дали текущото число и (х - текущото число) са прости. 
;Проверката за просто число може да правим с помощта на предиката prime? от упражнението.

(define (goldbach x)
  (define (iter x counter)
    (cond
      ((odd? x) "The input is invalid!")
      ((= x 2) "2 is a prime number.")
      ((and (is-prime counter) (is-prime(- x counter)) (> (- x 1) counter)) counter)
      (else (iter x (+ counter 1)))
    )
  )
 (iter x 2)
)