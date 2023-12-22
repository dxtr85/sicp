;Exercise 2.52.  Make changes to the square limit of wave shown in figure 2.9 by working at each of the levels described above. In particular:
;a.  Add some segments to the primitive wave painter of exercise  2.49 (to add a smile, for example).

(define wave
  (segments->painter (list (make-segment (make-vect .25 0) (make-vect .35 .5)) 
                           (make-segment (make-vect .35 .5) (make-vect .3 .6)) 
                           (make-segment (make-vect .3 .6) (make-vect .15 .4)) 
                           (make-segment (make-vect .15 .4) (make-vect 0 .65)) 
                           (make-segment (make-vect 0 .65) (make-vect 0 .85)) 
                           (make-segment (make-vect 0 .85) (make-vect .15 .6)) 
                           (make-segment (make-vect .15 .6) (make-vect .3 .65)) 
                           (make-segment (make-vect .3 .65) (make-vect .4 .65)) 
                           (make-segment (make-vect .4 .65) (make-vect .35 .85)) 
                           (make-segment (make-vect .35 .85) (make-vect .4 1)) 
                           (make-segment (make-vect .4 1) (make-vect .6 1)) 
                           (make-segment (make-vect .6 1) (make-vect .65 .85)) 
                           (make-segment (make-vect .65 .85) (make-vect .6 .65)) 
                           (make-segment (make-vect .6 .65) (make-vect .75 .65)) 
                           (make-segment (make-vect .75 .65) (make-vect 1 .35)) 
                           (make-segment (make-vect 1 .35) (make-vect 1 .15)) 
                           (make-segment (make-vect 1 .15) (make-vect .6 .45)) 
                           (make-segment (make-vect .6 .45) (make-vect .75 0)) 
                           (make-segment (make-vect .75 0) (make-vect .6 0)) 
                           (make-segment (make-vect .6 0) (make-vect .5 .3)) 
                           (make-segment (make-vect .5 .3) (make-vect .4 0)) 
                           (make-segment (make-vect .4 0) (make-vect .25 0))

                           (make-segment (make-vect .42 .9) (make-vect .45 0.9)) 
                           (make-segment (make-vect .55 .9) (make-vect .58 .9)) 
                           (make-segment (make-vect .5 .8) (make-vect .5 .73))
                           (make-segment (make-vect .44 .76) (make-vect .46 .73)) 
                           (make-segment (make-vect .46 .73) (make-vect .54 .73)) 
                           (make-segment (make-vect .54 .73) (make-vect .56 .76)))))

;b.  Change the pattern constructed by corner-split (for example, by using only one copy of the up-split and right-split images instead of two).
;orig:
;(define (corner-split painter n)
;  (if (= n 0)
;      painter
;      (let ((up (up-split painter (- n 1)))
;            (right (right-split painter (- n 1))))
;        (let ((top-left (beside up up))
;              (bottom-right (below right right))
;              (corner (corner-split painter (- n 1))))
;          (beside (below painter top-left)
;                  (below bottom-right corner))))))

(define (corner-split painter n) 
   (if (= n 0) 
       painter 
       (beside (below painter (up-split painter (- n 1))) 
               (below (right-split painter (- n 1)) (corner-split painter (- n 1))))))


;c.  Modify the version of square-limit that uses square-of-four so as to assemble the corners in a different pattern. (For example, you might make the big
;Mr. Rogers look outward from each corner of the square.) 

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((combine4 (square-of-four rotate270 rotate180 rotate90 quarter)))
      (combine4 quarter))))
