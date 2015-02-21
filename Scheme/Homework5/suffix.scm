(define (suffix? l1 l2)
  (cond
    ((null? l1) #t)
    ((null? l2) #f)
    ((= (car (reverse l1)) (car (reverse l2))) (suffix? (cdr l1) (cdr l2)))
    (else #f)
  )
)