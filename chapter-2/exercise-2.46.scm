;We will use coordinates in the unit square (0< x,y< 1) to specify images. With each frame, we associate a frame coordinate map, which will be used to
;shift and scale images to fit the frame. The map transforms the unit square into the frame by mapping the vector v = (x,y) to the vector sum 

;* 

;For example, (0,0) is mapped to the origin of the frame, (1,1) to the vertex diagonally opposite the origin, and (0.5,0.5) to the center of the frame. We can
;create a frame's coordinate map with the following procedure:

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;Exercise 2.46.  A two-dimensional vector v running from the origin to a point can be represented as a pair consisting of an x-coordinate and a
;y-coordinate. Implement a data abstraction for vectors by giving a constructor make-vect and corresponding selectors xcor-vect and ycor-vect. In terms of
;your selectors and constructor, implement procedures add-vect, sub-vect, and scale-vect that perform the operations vector addition, vector subtraction,
;and multiplying a vector by a scalar: 

;* 

(define (make-vect xcor ycor)
  (cons xcor ycor))

(define (xcor-vect vector)
  (car vector))
(define (ycor-vect vector)
  (cdr vector))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect vector scalar)
  (make-vect (* (xcor-vect vector) scalar)
             (* (ycor-vect vector) scalar)))
