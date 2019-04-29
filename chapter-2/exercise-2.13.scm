;Exercise 2.13.  Show that under the assumption of small percentage tolerances there is a simple formula for the
;approximate percentage tolerance of the product of two intervals in terms of the tolerances of the factors. You
;may simplify the problem by assuming that all numbers are positive. 

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
  (let ((cx (center x))
	(px (percent x))
	(cy (center y))
	(py (percent y)))
    (if (and (<= 0 cx) (<= 0 cy) (>= 10 px) (>= 10 py))
	(make-center-percent (* cx cy) (+ px py))
	(let ((lbx (lower-bound x))
	(ubx (upper-bound x))
	(lby (lower-bound y))
	(uby (upper-bound y)))
    (if (and (<= 0 lbx) (<= 0 ubx) (<= 0 lby) (<= 0 uby))
	(make-interval (* lbx lby) (* ubx uby))
    (if (and (<= 0 lbx) (<= 0 ubx) (> 0 lby) (<= 0 uby))
	(make-interval (* ubx lby) (* ubx uby))
    (if (and (<= 0 lbx) (<= 0 ubx) (> 0 lby) (> 0 uby))
	(make-interval (* ubx uby) (* lbx lby))
    (if (and (> 0 lbx) (<= 0 ubx) (<= 0 lby) (<= 0 uby))
	(make-interval (* lbx uby) (* ubx uby))
    (if (and (> 0 lbx) (<= 0 ubx) (> 0 lby) (<= 0 uby))
	(make-interval (min (* ubx lby) (* lbx uby)) (* ubx uby))
    (if (and (> 0 lbx) (<= 0 ubx) (> 0 lby) (> 0 uby))
	(make-interval (* ubx lby) (* lbx lby))
    (if (and (> 0 lbx) (> 0 ubx) (<= 0 lby) (<= 0 uby))
	(make-interval (* lbx uby) (* ubx lby))
    (if (and (> 0 lbx) (> 0 ubx) (> 0 lby) (<= 0 uby))
	(make-interval (* lbx uby) (* lbx lby))
    (if (and (> 0 lbx) (> 0 ubx) (> 0 lby) (> 0 uby))
	(make-interval (* ubx uby) (* lbx lby)))))))))))))))

(define (div-interval x y)
  (if (>= 0 (* (lower-bound y) (upper-bound y)))
      (error "Dividing by an Interval that spans zero!")
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

(define (upper-bound interval) (max (car interval) (cdr interval)))

(define (lower-bound interval) (min (car interval) (cdr interval)))

(define (width interval) (/ (- (upper-bound interval) (lower-bound interval)) 2))

; Exercises 14, 15, 16 are more theoretical and expand some of properties of interval arithmetic.
