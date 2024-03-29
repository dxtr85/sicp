; Exercise 3.78: Consider the problem of designing a signal-
; processing system to study the homogeneous second-order
; linear diﬀerential equation

;      d^2y     dy
;      −−−− − a*−− − by = 0.
;      dt^2     dt

; e output stream, modeling y, is generated by a network
; that contains a loop. is is because the value of d 2 y/dt 2 de-
; pends upon the values of y and dy/dt and both of these are
; determined by integrating d 2 y/dt 2 . e diagram we would
; like to encode is shown in Figure 3.35. Write a procedure
; solve-2nd that takes as arguments the constants a, b, and
; dt and the initial values y 0 and dy 0 for y and dy/dt and gen-
; erates the stream of successive values of y.

(load "ex3.53.scm")

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy 
    (add-streams 
      (scale-stream dy a)
      (scale-stream y b)))
  y)
