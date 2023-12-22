; Exercise 3.77: e integral procedure used above was
; analogous to the “implicit” deﬁnition of the inﬁnite stream
; of integers in Section 3.5.2. Alternatively, we can give a def-
; inition of integral that is more like integers-starting-
; from (also in Section 3.5.2):
; (define (integral integrand initial-value dt)
;   (cons-stream
;     initial-value
;     (if (stream-null? integrand)
;       the-empty-stream
;       (integral (stream-cdr integrand)
;         (+ (* dt (stream-car integrand))
;         initial-value)
;         dt))))
; When used in systems with loops, this procedure has the
; same problem as does our original version of integral .
; Modify the procedure so that it expects the integrand as
; a delayed argument and hence can be used in the solve
; procedure shown above.
(load "ex3.53.scm")

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (integral delayed-integrand initial-value dt)
  (cons-stream
    initial-value
    (let ((integrand (force delayed-integrand)))
      (if (stream-null? integrand)
        the-empty-stream
        (integral (delay (stream-cdr integrand))
          (+ (* dt (stream-car integrand))
            initial-value)
          dt)))))

(display (stream-ref (solve (lambda (y) y) 1 0.00001) 100000))
