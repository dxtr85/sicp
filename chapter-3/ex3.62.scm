; Exercise 3.62: Use the results of Exercise 3.60 and Exer-
; cise 3.61 to deﬁne a procedure div-series that divides two
; power series. div-series should work for any two series,
; provided that the denominator series begins with a nonzero
; constant term. (If the denominator has a zero constant term,
; then div-series should signal an error.) Show how to use
; div-series together with the result of Exercise 3.59 to gen-
; erate the power series for tangent.

(define (div-series nom-series den-series)
  (if (= 0 (stream-car den-series))
      (display "ERROR: Denominator series' constant term can not be equal to zero.")
        (mul-series nom-series 
          (scale-stream
            (invert-unit-series (scale-stream
                                  den-series 
                                  (/ 1 (stream-car den-series))))
            (stream-car den-series)))))

(define cosine-series 
  (cons-stream 1 
               (integrate-series (scale-stream sine-series -1))))

(define sine-series 
  (cons-stream 0 
               (integrate-series cosine-series)))

(define tangent-series
  (div-series sine-series
              cosine-series))
