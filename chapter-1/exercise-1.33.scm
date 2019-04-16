(define (filtered-accumulate filter combiner null-value term a next b)
(if (> a b)
    null-value
    (let ((next-value (next a)))
      (define value-to-add (if (filter a)
			       (term a)
			       null-value))
      (combiner value-to-add (filtered-accumulate filter combiner null-value term next-value next b)))))

(define (inc n) (+ n 1))
(define (identity x) x)
(define (square x) (* x x))

(define (iter-filtered-accumulate filter combiner null-value term a next b)
(define (iter a result)
  (if (> a b)
      result
      (let ((next-value (next a)))
      (define value-to-add (if (filter a)
			       (term a)
			       null-value))
        (iter next-value (combiner result value-to-add)))))
(iter a null-value))

(define (smallest-divisor n)
(find-divisor n 2))

(define (gcd a b)
(if (= b 0)
a
(gcd b (remainder a b))))

(define (find-divisor n test-divisor)
(cond ((> (square test-divisor) n) n)
((divides? test-divisor n) test-divisor)
(else (find-divisor n (next test-divisor)))))

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (divides? a b)
(= (remainder b a) 0))

(define (prime? n)
(= n (smallest-divisor n)))

(define (primes-square-sum from to)
  (iter-filtered-accumulate prime? + 0 square from inc to))

(define (relative-primes-product number)
  (define (relative-prime? n)
    (= 1 (gcd n number)))
  (iter-filtered-accumulate relative-prime? * 1 identity 1 inc (- number 1)))
