(define (even? n)
  (= (remainder n 2) 0))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (*-my a b)
  (cond ((= a 0) 0)
        ((even? a) (double (*-my (halve a) b))
         (else (+ a (*-my (- a 1) b)))))
