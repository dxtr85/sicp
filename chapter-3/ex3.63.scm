(load "ex3.50.scm")
(load "ex3.55.scm")
(load "ex3.60.scm")

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
      1.0
      (stream-map (lambda (guess) (sqrt-improve guess x))
        guesses)))
  guesses)
; (display-stream (sqrt-stream 2))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
    (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

; (display-stream pi-stream)

; Using Euler's transfor for accelerated sequence with terms
;
;  S2 - (S2 - S1)^2/(S0 -2S1 +S2)
;
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
    (s1 (stream-ref s 1))
    (s2 (stream-ref s 2)))
      (cons-stream (- s2 (/ (square (- s2 s1))
          (+ s0 (* -2 s1) s2)))
        (euler-transform (stream-cdr s)))))

; (display-stream (euler-transform pi-stream))

; Even better, we can accelerate the accelerated sequence, and recursively
; accelerate that, and so on. Namely, we create a stream of streams (a
; structure we’ll call a tableau ) in which each stream is the transform of
; the preceding one:

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

; (display-stream
;   (accelerated-sequence euler-transform pi-stream))

; Exercise 3.63: Louis Reasoner asks why the sqrt-stream
; procedure was not written in the following more straight-
; forward way, without the local variable guesses:

(define (sqrt-stream x)
  (cons-stream 1.0 (stream-map
    (lambda (guess)
      (sqrt-improve guess x))
        (sqrt-stream x))))

; Alyssa P. Hacker replies that this version of the procedure
; is considerably less efficient because it performs redundant
; computation. Explain Alyssa’s answer. Would the two ver-
; sions still differ in efficiency if our implementation of delay
; used only (lambda () ⟨ exp ⟩ ) without using the optimiza-
; tion provided by memo-proc (Section 3.5.1)?

; Proposed solution is worse than original, because in it we
; create a stream that has, as it's cdr a stream based on a
; lambda function that creates a new stream context each and 
; every time it get's called. Original solution uses local
; variable definition inside of lambda's body so no new context
; is being created when this lambda function is called.
; If we used non-optimized delay for proposed solution it would
; get exponentially worse, since each call to sqrt-stream would
; need to perform all the computations from ground up.
