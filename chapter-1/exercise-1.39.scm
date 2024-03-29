;Exercise 1.39. A continued fraction representation of the tangent function was published in 1770 by the
;German mathematician J.H. Lambert:
;
;*
;
;where x is in radians. Define a procedure (tan-cf x k) that computes an approximation to the tangent
;function based on Lambert's formula. K specifies the number of terms to compute, as in exercise 1.37.

(define (dec n)
  (- n 1))

(define (cont-frac-iter n d k)
  (define (cont-frac-int sum kk)
    (cond ((= kk 0)
           sum)
          (else
           (cont-frac-int (/ (n kk) (+ (d kk) sum)) (dec kk)))))
  (cont-frac-int 0 k))

(define (tan-cf x k)
  (define (n k)
    "Internal function for tangent computation."
    (cond ((= k 1)
           x)
          (else (* -1.0 x x))))
  (define (d k)
    (- (* 2 k) 1))
  (cont-frac-iter n d k))
