(define (drop-while pred? l)
  (define (iter result list)
    (cond
      ((null? list) result)
      ((pred? (car list)) (iter result (cdr list)))
      (else list)
    )
  )
  (iter '() l))