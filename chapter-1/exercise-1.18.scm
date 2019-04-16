(define (even? n)
  (= (remainder n 2) 0))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (*-iter a b)
  (define (iter counter a b)
    (cond ((= a 0) counter)
          ((even? a) (iter counter (halve a) (double b)))
          (else (iter (+ counter b) (- a 1) b))))
  (iter 0 a b))
