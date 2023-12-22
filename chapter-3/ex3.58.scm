; Exercise 3.58: Give an interpretation of the stream com-
; puted by the following procedure:
(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))
; ( quotient is a primitive that returns the integer quotient of
; two integers.) What are the successive elements produced
; by (expand 1 7 10) ? What is produced by (expand 3 8
; 10) ?

; This stream allows us to get a precise value of both rational 
; and irrational numbers given their numerator and denominator
; in a numeral system containing radix amount of unique digits.

