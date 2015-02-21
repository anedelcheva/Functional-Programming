(define (nth index list)
  (cond
    ((= index 0) (car list))
    (else (nth (- index 1) (cdr list)))))

(define (column index M)
  (map (lambda (row)
         (nth index row))
       M))

(define (range a b)
  (if (> a b)
      '()
      (cons a (range (+ a 1) b))))

(define (transpose M)
  (map (lambda (index)
         (column index M))
       (range 0 (- (length (car M)) 1))))

(define (dot-product v1 v2)
(sum (map * v1 v2)))

(define (reduce oper start-value list)
  (define (iter l result)
    (cond
      ((null? l) result)
      (else (iter (cdr l) (oper (car l) result)))))
  (iter list start-value))

(define (sum l)
(reduce + 0 l))

(define (mult-matrices M1 M2)
  (map (lambda (row)
         (map (lambda (column) (dot-product row column))
              (transpose M2)))
          M1))

(define (matrix-power M n)
  (if (= n 1)
      M
      (mult-matrices (matrix-power M (- n 1)) M)))

(define M '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)))