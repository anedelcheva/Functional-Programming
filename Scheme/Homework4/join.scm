(define (join glue parts)
  (cond 
    ((null? parts) "")
    ((= (length parts) 1) (string-append (car parts) (join glue (cdr parts))))
    (else (string-append (car parts) glue (join glue (cdr parts))))
  )
)