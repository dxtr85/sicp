; Exercise 3.79: Generalize the solve-2nd procedure of Ex-
; ercise 3.78 so that it can be used to solve general second-
; order differential equations d^2y/dt^2 = f(dy/dt, y).

(load "ex3.53.scm")

(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
    y)
