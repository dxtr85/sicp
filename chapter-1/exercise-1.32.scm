(define (accumulate combiner null-value term a next b)
(if (> a b)
    null-value
(combiner (term a)
(accumulate combiner null-value term (next a) next b))))

(define (inc n) (+ n 1))
(define (identity x) x)
(define (square x) (* x x))

(define (iteraccumulate combiner null-value term a next b)
(define (iter a result)
  (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
(iter a null-value))

(define (factorial n)
  (iteraccumulate * 1 identity 1 inc n))

(define (pi n)
  (define (next number)
    (+ number 2))
  (define (term number)
    (/ (* (+ number 1) (+ number 3)) (square (+ 2 number))))
  (* 4. (iteraccumulate * 1 term 1 next n)))
