(define (take-while pred? l)
  (define (iter result list)
    (cond 
      ((null? list) result)
      ((pred? (car list)) (iter (cons (car list) result) (cdr list)))
      ((not (pred? (car list))) result)
      ;(else (iter result (cdr list)))
    )
  )
  (reverse(iter '() l))
)