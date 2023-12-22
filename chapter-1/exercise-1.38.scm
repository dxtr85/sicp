;Exercise 1.38. In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus
;Continuis, which included a continued fraction expansion for e - 2, where e is the base of the natural
;logarithms. In this fraction, the Ni are all 1, and the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... Write a
;program that uses your cont-frac procedure from exercise 1.37 to approximate e, based on Euler's expansion.

(define (dec n)
  (- n 1))
(define (cont-frac-iter n d k)
  (define (cont-frac-int sum kk)
    (cond ((= kk 0)
           sum)
          (else
           (cont-frac-int (/ (n kk) (+ (d kk) sum)) (dec kk)))))
  (cont-frac-int 0 k))

(define (d n)
  (cond ((= (modulo n 3) 2)
         (* 2 (+ 1 (floor (/ n 3)))))
        (else
         1)))
(define e
(+ 2 (cont-frac-iter (lambda (i) 1.0) d 10)))
