(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;The segments are given using coordinates with respect to the unit square. For each segment in the list, the painter transforms the segment endpoints with
;the frame coordinate map and draws a line between the transformed points.

;Representing painters as procedures erects a powerful abstraction barrier in the picture language. We can create and intermix all sorts of primitive
;painters, based on a variety of graphics capabilities. The details of their implementation do not matter. Any procedure can serve as a painter, provided
;that it takes a frame as argument and draws something scaled to fit the frame.

;Exercise 2.48.  A directed line segment in the plane can be represented as a pair of vectors -- the vector running from the origin to the start-point of the
;segment, and the vector running from the origin to the end-point of the segment. Use your vector representation from exercise 2.46 to define a
;representation for segments with a constructor make-segment and selectors start-segment and end-segment. 

(define (make-segment vstart vend)
  (cons vstart vend))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))
