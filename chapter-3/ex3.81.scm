; Exercise 3.81: Exercise 3.6 discussed generalizing the random-
; number generator to allow one to reset the random-number
; sequence so as to produce repeatable sequences of “ran-
; dom” numbers. Produce a stream formulation of this same
; generator that operates on an input stream of requests to
; generate a new random number or to reset the sequence
; to a speciﬁed value and that produces the desired stream of
; random numbers. Don’t use assignment in your solution.

(load "ex3.53.scm")

(define (build-rng a c m rand-init)
  (lambda (input-stream)
    (define mapping-function
      (lambda (what-to-do current-random-value)
            ; (display-line what-to-do)
            ; (display-line current-random-value)
            (cond
              ((eq? 'generate what-to-do)
                  (/ (modulo (+ (* (numerator current-random-value )a) c) m) m))
              (else 
                  (/ (modulo (+ (* what-to-do a) c) m) m))
              )))

    (define random-stream 
      (cons-stream 
        (/ (modulo (+ (* rand-init a) c) m) m)
        (stream-map mapping-function input-stream random-stream)))
    random-stream))

(define rng (build-rng 69069 1 (expt 2 32) 19390901))
(define g (cons-stream 'generate g))
(define r (cons-stream 19850607 g))
(define random-stream (rng r))
(define (sr x)
  (display (stream-ref random-stream x))
  'done)
(sr 0)

; Streaming idea works generational idea is tweaked because modulo wants integers as inputs
; and algorithm generates fractional numbers
