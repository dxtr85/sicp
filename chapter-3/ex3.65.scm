; Exercise 3.65: Use the series
;            1    1   1
; ln 2 = 1 - −  + − - − + . . .
;            2    3   4
; to compute three sequences of approximations to the nat-
; ural logarithm of 2, in the same way we did above for π .
; How rapidly do these sequences converge?
(load "ex3.63.scm")

(define (log2-summands n)
  (cons-stream (/ 1.0 n)
    (stream-map - (log2-summands (+ n 1)))))

(define log2-stream
  (partial-sums (log2-summands 1)))

; (display-line "Basic log2 stream")
; (display-line (stream-ref log2-stream 0))
; (display-line (stream-ref log2-stream 1))
; (display-line (stream-ref log2-stream 2))
; (display-line (stream-ref log2-stream 3))
; (display-line (stream-ref log2-stream 4))
; (display-line (stream-ref log2-stream 5))
; (display-line (stream-ref log2-stream 6))
; (display-line (stream-ref log2-stream 7))

; (display-line "Euler transfom")
; (display-line (stream-ref (euler-transform log2-stream) 0))
; (display-line (stream-ref (euler-transform log2-stream) 1))
; (display-line (stream-ref (euler-transform log2-stream) 2))
; (display-line (stream-ref (euler-transform log2-stream) 3))
; (display-line (stream-ref (euler-transform log2-stream) 4))
; (display-line (stream-ref (euler-transform log2-stream) 5))
; (display-line (stream-ref (euler-transform log2-stream) 6))
; (display-line (stream-ref (euler-transform log2-stream) 7))

; (display-line "Accelerated sequence")
; (display-line (stream-ref (accelerated-sequence euler-transform log2-stream) 0))
; (display-line (stream-ref (accelerated-sequence euler-transform log2-stream) 1))
; (display-line (stream-ref (accelerated-sequence euler-transform log2-stream) 2))
; (display-line (stream-ref (accelerated-sequence euler-transform log2-stream) 3))
; (display-line (stream-ref (accelerated-sequence euler-transform log2-stream) 4))
; (display-line (stream-ref (accelerated-sequence euler-transform log2-stream) 5))
; (display-line (stream-ref (accelerated-sequence euler-transform log2-stream) 6))
; (display-line (stream-ref (accelerated-sequence euler-transform log2-stream) 7))

; Basic log2 stream
; 1.
; .5
; .8333333333333333
; .5833333333333333
; .7833333333333332
; .6166666666666666
; .7595238095238095
; .6345238095238095
; Euler transfom
; .7
; .6904761904761905
; .6944444444444444
; .6924242424242424
; .6935897435897436
; .6928571428571428
; .6933473389355742
; .6930033416875522
; Accelerated sequence
; 1.
; .7
; .6932773109243697
; .6931488693329254
; .6931471960735491
; .6931471806635636
; .6931471805604039
; .6931471805599445
; ;... done

