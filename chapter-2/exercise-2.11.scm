;Exercise 2.11.  In passing, Ben also cryptically comments: ``By testing the signs of the endpoints of the
;intervals, it is possible to break mul-interval into nine cases, only one of which requires more than two
;multiplications.'' Rewrite this procedure using Ben's suggestion. 

(define (make-interval x y)
  (cons x y))

  (define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
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
	(make-interval (* ubx uby) (* lbx lby)))))))))))))

(define (div-interval x y)
  (if (>= 0 (* (lower-bound y) (upper-bound y)))
      (error "Dividing by an Interval that spans zero!")
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

(define (upper-bound interval) (max (car interval) (cdr interval)))

(define (lower-bound interval) (min (car interval) (cdr interval)))

(define (width interval) (/ (- (upper-bound interval) (lower-bound interval)) 2))
