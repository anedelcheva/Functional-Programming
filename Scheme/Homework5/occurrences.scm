(define (reduce oper start-value l)
  (if (null? l)
      start-value
      (oper (car l) (reduce oper start-value (cdr l)))))

(define (count x l)
  (reduce + 0 (map (lambda (y) (if (= x y) 1 0)) l))) 

(define (occurrences list1 list2)
  (define (iter l1 l2 result)
    (cond
      ((null? l1) result)
      (else (iter (cdr l1) l2  (cons (count (car l1) l2) result)))
    )
  )
  (reverse (iter list1 list2 '())))