;Exercise 2.9.  The width of an interval is half of the difference between its upper and lower bounds. The width is
;a measure of the uncertainty of the number specified by the interval. For some arithmetic operations the width of
;the result of combining two intervals is a function only of the widths of the argument intervals, whereas for others
;the width of the combination is not a function of the widths of the argument intervals. Show that the width of the
;sum (or difference) of two intervals is a function only of the widths of the intervals being added (or subtracted).
;Give examples to show that this is not true for multiplication or division. 

(define (make-interval x y)
  (cons x y))

  (define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (upper-bound interval) (max (car interval) (cdr interval)))

(define (lower-bound interval) (min (car interval) (cdr interval)))

(define (width interval) (/ (- (upper-bound interval) (lower-bound interval)) 2))

(width (make-interval 10 14))
(width (make-interval 16 18))
(width (add-interval (make-interval 10 14) (make-interval 16 18)))
(width (sub-interval (make-interval 10 14) (make-interval 16 18)))
(width (mul-interval (make-interval 10 14) (make-interval 16 18)))
(width (mul-interval (make-interval 0 4) (make-interval 18 20)))
(width (div-interval (make-interval 10 14) (make-interval 16 18)))
(width (div-interval (make-interval 0 4) (make-interval 18 20)))
