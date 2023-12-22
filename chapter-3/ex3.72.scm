; Exercise 3.72: In a similar way to Exercise 3.71 generate a
; stream of all numbers that can be written as the sum of two
; squares in three different ways (showing how they can be
; so written).

(load "ex3.71.scm")

(define ramanujan-numbers-3 
  ((lambda()
    (define (get-next stream prev-weight prev-pair pprev-weight pprev-pair)
           (let ((cur-weight (sum-cube-pair-elements (stream-car stream)))
                 (cur-pair (stream-car stream)))
             (if (= cur-weight prev-weight pprev-weight)
               (cons-stream (list cur-weight pprev-pair prev-pair cur-pair)
                            (get-next (stream-cdr stream) 
                                      cur-weight cur-pair 
                                      prev-weight prev-pair))
               (get-next (stream-cdr stream) 
                         cur-weight cur-pair
                         prev-weight prev-pair))))
         (get-next int-pairs 0 (list 0 0) 0 (list 0 0)))))

(display-line (stream-ref ramanujan-numbers-3 0))
(display-line (stream-ref ramanujan-numbers-3 1))
(display-line (stream-ref ramanujan-numbers-3 2))
