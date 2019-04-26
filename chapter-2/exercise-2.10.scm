;Exercise 2.10.  Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and comments that
;it is not clear what it means to divide by an interval that spans zero. Modify Alyssa's code to check for this
;condition and to signal an error if it occurs. 

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
  (if (>= 0 (* (lower-bound y) (upper-bound y)))
      (error "Dividing by an Interval that spans zero!")
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

(define (upper-bound interval) (max (car interval) (cdr interval)))

(define (lower-bound interval) (min (car interval) (cdr interval)))

(define (width interval) (/ (- (upper-bound interval) (lower-bound interval)) 2))
