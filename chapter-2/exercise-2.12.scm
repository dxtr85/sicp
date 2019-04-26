;Exercise 2.12.  Define a constructor make-center-percent that takes a center and a percentage tolerance
;and produces the desired interval. You must also define a selector percent that produces the percentage
;tolerance for a given interval. The center selector is the same as the one shown above. 

(define (make-interval x y)
  (cons x y))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent c p)
  (make-interval (* (/ (- 100 p) 100) c) (* (/ (+ 100 p) 100) c)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (let ((ci (center i))
	(wd (width i)))
    (if (= ci 0)
	0
	(* (/ wd ci) 100))))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

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
