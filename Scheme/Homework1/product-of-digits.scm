(define (product-of-digits n)
  (if (< n 10)
      n
  (* (remainder n 10) (product-of-digits (quotient n 10)))
  )
)