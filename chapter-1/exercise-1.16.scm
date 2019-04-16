(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))
(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expt b n)
  (define (expt-succ-sq a b n)
  (cond ((= n 0) a)
        ((even? n) (expt-succ-sq a (square b) (/ n 2)))
        (else (expt-succ-sq (* a b) b (- n 1)))))
  (expt-succ-sq 1 b n))
