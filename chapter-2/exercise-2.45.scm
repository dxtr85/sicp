;For example, flipped-pairs and square-limit each arrange four copies of a painter's image in a square pattern; they differ only in how they orient the
;copies. One way to abstract this pattern of painter combination is with the following procedure, which takes four one-argument painter operations and
;produces a painter operation that transforms a given painter with those four operations and arranges the results in a square. Tl, tr, bl, and br are the
;transformations to apply to the top left copy, the top right copy, the bottom left copy, and the bottom right copy, respectively.

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;Then flipped-pairs can be defined in terms of square-of-four as follows:24

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

;and square-limit can be expressed as25

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;Exercise 2.45.  Right-split and up-split can be expressed as instances of a general splitting operation. Define a procedure split with the property that
;evaluating 

(define right-split (split beside below))
(define up-split (split below beside))

;produces procedures right-split and up-split with the same behaviors as the ones already defined. 

(define (split first second)
  (let ((generic-split (lambda (painter iter)
    (if (= iter 0)
      painter
      (let ((smaller (generic-split painter (- iter 1))))
        (first painter (second smaller smaller)))))))
    generic-split))
