;Exercise 2.3. Implement a representation for rectangles in a plane. (Hint: You may want to make use of
;exercise 2.2.) In terms of your constructors and selectors, create procedures that compute the perimeter and
;the area of a given rectangle. Now implement a different representation for rectangles. Can you design your
;system with suitable abstraction barriers, so that the same perimeter and area procedures will work using
;either representation?

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (let ((start-p-x (x-point (start-segment segment)))
        (start-p-y (y-point (start-segment segment)))
        (end-p-x (x-point (end-segment segment)))
        (end-p-y (y-point (end-segment segment))))
    (make-point (/ (+ start-p-x end-p-x) 2) (/ (+ start-p-y end-p-y) 2))))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point  p)
  (cdr p))

(define (make-rectangle p1 p2)
  (cons  p1 p2))

(define (area-rectangle r)
  (let ((p1-x (x-point (car r)))
        (p1-y (y-point (car r)))
        (p2-x (x-point (cdr r)))
        (p2-y (y-point (cdr r))))
    (abs (* (- p2-x p1-x) (- p2-y p1-y)))))

(define (perimeter-rectangle r)
  (let ((p1-x (x-point (car r)))
        (p1-y (y-point (car r)))
        (p2-x (x-point (cdr r)))
        (p2-y (y-point (cdr r))))
    (abs (+ (* (- p2-x p1-x) 2 ) (* (- p2-y p1-y) 2)))))
