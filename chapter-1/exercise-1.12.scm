(define (pascal n m)
  (if (or (= n m) (= m 1))
  1
  (+ (pascal (- n 1) (- m 1)) (pascal (- n 1) m))))
