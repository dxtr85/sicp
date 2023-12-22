; Exercise 3.80: A series RLC circuit consists of a resistor, a
; capacitor, and an inductor connected in series, as shown
; in Figure 3.36. If R, L, and C are the resistance, inductance,
; and capacitance, then the relations between voltage (v) and
; current (i) for the three components are described by the
; equations
;                              diL               dvC
;    vR = iR * R,     vL = L * −−−,     iC = C * −−−,
;                              dt                dt

; and the circuit connections dictate the relations

;    iR = iL = -iC,          vC = vR + vL.

; Combining these equations shows that the state of the cir-
; cuit (summarized by vC, the voltage across the capacitor,
; and iL, the current in the inductor) is described by the pair
; of differential equations

;    dvC     iL      diL   1        R
;    −−− = - −−,     −−− = − * vC - − * iL.
;    dt      C       dt    L        L

; The signal-ﬂow diagram representing this system of differ-
; ential equations is shown in Figure 3.37.
; Write a procedure RLC that takes as arguments the param-
; eters R, L, and C of the circuit and the time increment dt.
; In a manner similar to that of the RC procedure of Exercise
; 3.73, RLC should produce a procedure that takes the initial
; values of the state variables, vC0 and iL0, and produces a
; pair (using cons ) of the streams of states vC and iL. Using
; RLC , generate the pair of streams that models the behavior
; of a series RLC circuit with R = 1 ohm, C = 0.2 farad, L = 1
; henry, dt = 0.1 second, and initial values iL0 = 0 amps and
; vC0 = 10 volts.

(load "ex3.53.scm")
(load "ex3.77.scm")

(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC (integral 
                 (delay (add-streams
                   (scale-stream iL (- (/ R L)))
                   (scale-stream vC (/ 1 L)))) vC0 dt))
    (define iL (integral 
                (delay (add-streams
                  (scale-stream iL (- (/ R L)))
                  (scale-stream vC (/ 1 L)))) iL0 dt))
    ; (define diL (add-streams
    ;         (scale-stream iL (- (/ R L)))
    ;         (scale-stream vC (/ 1 L))))
    ; (define dvC (scale-stream iL (- (/ 1 C))))
    (cons vC iL)))

(define vC-iL-pair ((RLC 1 1 0.2 0.1) 10 0))
(define vC (car vC-iL-pair))
(define iL (cdr vC-iL-pair))

