(define (product term a next b)
(if (> a b)
1
(* (term a)
(product term (next a) next b))))

(define (inc n) (+ n 1))
(define (identity x) x)
(define (square x) (* x x))
(define (iterproduct term a next b)
(define (iter a result)
  (if (> a b)
      result
      (iter (next a) (* result (term a)))))
(iter a 1))

(define (factorial n)
  (iterproduct identity 1 inc n))

(define (pi n)
  (define (next number)
    (+ number 2))
  (define (term number)
    (/ (* (+ number 1) (+ number 3)) (square (+ 2 number))))
  (* 4 (iterproduct term 1 next n)))
