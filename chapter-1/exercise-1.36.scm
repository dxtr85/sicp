;Exercise 1.36. Modify fixed-point so that it prints the sequence of approximations it generates, using the newline and display
;primitives shown in exercise 1.22. Then find a solution to xx = 1000 by finding a fixed point of x 
* 
;log(1000)/log(x). (Use Scheme's primitive log procedure, which computes natural logarithms.) Compare the number of steps this takes
;with and without average damping. (Note that you cannot start fixed-point with a guess of 1, as this would cause division by log(1) = 0.) 

(define tolerance 0.00001)
(define (average a b)
  (/ (+ a b) 2))
(define (fixed-point f first-guess)
(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))
(define (try guess)
  (let ((next (f guess)))
    (newline)
    (display "Trying: ")
    (display next)
(if (close-enough? guess next)
    next
    (try next))))
(try first-guess))

(define (x^x y)
  (fixed-point (lambda (x) (/ (log y) (log x)))
	       10.0))

(define (x^xb y)
  (fixed-point (lambda (x) (average x (/ (log y) (log x))))
	       10.0))
