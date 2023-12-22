; Exercise 3.60: With power series represented as streams
; of coefficients as in Exercise 3.59, adding series is imple-
; mented by add-streams. Complete the deﬁnition of the fol-
; lowing procedure for multiplying series:
; (define (mul-series s1 s2)
;   (cons-stream
;     ⟨ ?? ⟩ (add-streams ⟨ ?? ⟩ ⟨ ?? ⟩ )))
; You can test your procedure by verifying that sin^2(x) + cos^2(x) = 1,
; using the series from Exercise 3.59.

; A: a0      a1              a2                      a3
; B: b0      b1              b2                      b3
; C: a0*b0   a0*b1 + a1*b0   a0*b2 + a1*b1 + a2*b0   a0*b3 + a1*b2 + a2*b1 + a3*b0
;
; C: a0*b0   a0*b1 +        a0*b2 +                  a0*b3 + 
;            a1*b0          a1*b1 +                  a1*b2 +
;                           a2*b0                    a2*b1 +
;                                                    a3*b0
(define (add-streams s1 s2) (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
    stream))

(define (mul-series s1 s2)
  (cons-stream
    (* (stream-car s1)
       (stream-car s2))
    (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                 (mul-series (stream-cdr s1) s2))))

