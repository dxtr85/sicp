; Exercise 3.59: In Section 2.5.3 we saw how to implement
; a polynomial arithmetic system representing polynomials
; as lists of terms. In a similar way, we can work with power
; series , such as

; e^x = 1 + x + (x^2/2) + (x^3/(3*2)) + (x^4/(4*3*2)) + ...
; cosx = 1 - (x^2/2) + (x^4/(4*3*2)) - ...
; sinx = x - (x^3/(3*2)) + (x^5/(5*4*3*2)) - ...

; represented as inﬁnite streams. We will represent the series
; a0 + a1*x + a2*x^2 + a3*x^3 + . . . as the stream whose elements
; are the coeﬃcients a0 , a1 , a2 , a3 , . . ..

; a. The integral of the series a0 + a1*x + a2*x^2 + a3*x^3 + . . .
; is the series

; c + a0*x + (1/2)*a1*x^2 + (1/3)*a2*x^3 + (1/4)*a3*x^4 + ...,

; where c is any constant. Deﬁne a procedure integrate-
; series that takes as input a stream a0 , a1 , a2 , ... rep-
; resenting a power series and returns the stream a0 ,
; (1/2)a1, (1/3)a2... of coeffcients of the non-constant terms
; of the integral of the series. (Since the result has no
; constant term, it doesn’t represent a power series; when
; we use integrate-series , we will cons on the ap-
; propriate constant.)

; b. The function x → e^x is its own derivative. This im-
; plies that e^x and the integral of e^x are the same se-
; ries, except for the constant term, which is e^0 = 1.
; Accordingly, we can generate the series for e^x as
; (define exp-series
;   (cons-stream 1 (integrate-series exp-series)))
; Show how to generate the series for sine and cosine,
; starting from the facts that the derivative of sine is
; cosine and the derivative of cosine is the negative of
; sine:

; (define cosine-series (cons-stream 1 <??>))
; (define sine-series (cons-stream 0 <??>))

; a)
(define (div-streams s1 s2) (stream-map / s1 s2))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1.0))

(define (integrate-series series)
  (div-streams series integers))

; b)
; cosx = 1 - (x^2/2) + (x^4/(4*3*2)) - ...
; sinx = x - (x^3/(3*2)) + (x^5/(5*4*3*2)) - ...
(define cosine-series 
  (cons-stream 1 
               (integrate-series (scale-stream sine-series -1))))

(define sine-series 
  (cons-stream 0 
               (integrate-series cosine-series)))
; or:
(define (mul-streams s1 s2) (stream-map * s1 s2))

(define plus-zero-minus-zero (cons-stream 1.0
                                (cons-stream 0.0 (cons-stream -1.0 (cons-stream 0.0 plus-zero-minus-zero)))))
(define zero-minus-zero-plus 
  (cons-stream 0.0 
               (cons-stream -1.0 
                            (cons-stream 0.0 
                                         plus-zero-minus-zero))))

(define factorials
  (cons-stream 1.0 (mul-streams
    factorials
    integers)))

(define cosine-series 
  (cons-stream 1 
               (div-streams zero-minus-zero-plus (stream-cdr factorials))))

(define sine-series 
  (cons-stream 0 
               (div-streams plus-zero-minus-zero (stream-cdr factorials))))

