(define (qrt-iter guess previous-guess x)
  (if (good-enough? guess previous-guess)
      guess
      (qrt-iter (improve guess x)
                 guess
                 x)))
(define (improve guess x)
(/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough? guess previous-guess)
  (< (abs (- guess previous-guess)) 0.000001))
(define (qrtb x)
  (qrt-iter 1.0 0.9 x))
(define (square x)
  (* x x))
