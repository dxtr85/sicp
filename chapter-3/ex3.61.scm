; Exercise 3.61: Let S be a power series (Exercise 3.59) whose
; constant term is 1. Suppose we want to ﬁnd the power se-
; ries 1/S, that is, the series X such that S X = 1. Write
; S = 1 + S R where S R is the part of S aer the constant
; term. en we can solve for X as follows:

;          S · X = 1,
; (1 + S R ) · X = 1,
;    X + S R · X = 1,
;              X = 1 − S R · X .

; In other words, X is the power series whose constant term
; is 1 and whose higher-order terms are given by the negative
; of S R times X . Use this idea to write a procedure invert-
; unit-series that computes 1/S for a power series S with
; constant term 1. You will need to use mul-series from Ex-
; ercise 3.60.

(define (invert-unit-series S)
  (cons-stream 1
               (scale-stream (mul-series (stream-cdr S) 
                                         (invert-unit-series S)) 
                             -1)))

