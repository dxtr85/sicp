(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
      (add-streams (scale-stream integrand dt)
        int)))
  int)

; Exercise 3.73: We can model electrical circuits using streams
; to represent the values of currents or voltages at a sequence
; of times. For instance, suppose we have an RC circuit con-
; sisting of a resistor of resistance R and a capacitor of capac-
; itance C in series. The voltage response v of the circuit to
; an injected current i is determined by the formula in Fig-
; ure 3.33, whose structure is shown by the accompanying
; signal-ﬂow diagram.
; Write a procedure RC that models this circuit. RC should
; take as inputs the values of R, C, and dt and should return
; a procedure that takes as inputs a stream representing the
; current i and an initial value for the capacitor voltage v0
; and produces as output the stream of voltages v. For ex-
; ample, you should be able to use RC to model an RC circuit
; with R = 5 ohms, C = 1 farad, and a 0.5-second time step by
; evaluating (define RC1 (RC 5 1 0.5)) . This defines RC1
; as a procedure that takes a stream representing the time
; sequence of currents and an initial capacitor voltage and
; produces the output stream of voltages.

(load "ex3.53.scm")

(define (RC R C dt)
  (lambda (current-stream v0)
    (add-streams 
      (scale-stream current-stream R)
      (integral 
        (scale-stream current-stream (/ 1 C))
        v0
        dt))))

(define circuit (RC 5 1 0.5))
(define current (cons-stream 1 current))
(define voltage (circuit current 0))

(display-line (stream-ref voltage 0))
(display-line (stream-ref voltage 10))
(display-line (stream-ref voltage 20))
(display-line (stream-ref voltage 30))
(display-line (stream-ref voltage 40))
(display-line (stream-ref voltage 50))
(display-line (stream-ref voltage 60))
(display-line (stream-ref voltage 70))
(display-line (stream-ref voltage 80))
(display-line (stream-ref voltage 90))
(display-line (stream-ref voltage 100))
(display-line (stream-ref voltage 110))
(display-line (stream-ref voltage 120))
(display-line (stream-ref voltage 130))
(display-line (stream-ref voltage 140))
(display-line (stream-ref voltage 150))
(display-line (stream-ref voltage 160))
(display-line (stream-ref voltage 170))
(display-line (stream-ref voltage 180))
(display-line (stream-ref voltage 190))
(display-line (stream-ref voltage 200))
(display-line (stream-ref voltage 210))
(display-line (stream-ref voltage 220))
(display-line (stream-ref voltage 230))
(display-line (stream-ref voltage 240))
(display-line (stream-ref voltage 250))
(display-line (stream-ref voltage 260))
(display-line (stream-ref voltage 270))
(display-line (stream-ref voltage 280))
(display-line (stream-ref voltage 999999))
