;Exercise 1.45. We saw in section 1.3.3 that attempting to compute square roots by naively finding a fixed
;point of y
;*
;x/y does not converge, and that this can be fixed by average damping. The same method works for finding cube
;roots as fixed points of the average-damped y
;*
;x/y2. Unfortunately, the process does not work for fourth roots -- a single average damp is not enough to make
;a fixed-point search for y
;*
;x/y3 converge. On the other hand, if we average damp twice (i.e., use the average damp of the average damp
;of y
;*
;x/y3) the fixed-point search does converge. Do some experiments to determine how many average damps are
;required to compute nth roots as a fixed-point search based upon repeated average damping of y
;*
;x/yn-1. Use this to implement a simple procedure for computing nth roots using fixed-point, average-damp,
;and the repeated procedure of exercise 1.43. Assume that any arithmetic operations you need are available as
;primitives.

(define tolerance 0.0000001)
(define (average a b)
  (/ (+ a b) 2))
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 0)
      (lambda (x)
        (identity x))
      (compose f (repeated f (- n 1)))))

(define (^ x n)
  (if (= n 0)
         1
     (* x (^ x (- n 1)))))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (n-th-root x n)
    (fixed-point-of-transform (lambda (y) (/ x (^ y (- n 1))))
                              (repeated average-damp (floor (/ n 2)))
                              1.0))
