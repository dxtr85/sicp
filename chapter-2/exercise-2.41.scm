;Exercise 2.41.  Write a procedure to find all ordered triples of distinct positive integers i, j,
;and k less than or equal to a given integer n that sum to a given integer s. 

(define (unique-pairs n)
  (flatmap
   (lambda (i)
          (map (lambda (j) (list i j))
               (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n)))

(define (triple-sum-eq limit sum)
  (define (tri-sum-eq? triple)
    (= sum (+ (car triple) (cadr triple) (caddr triple))))

  (filter tri-sum-eq?
          (flatmap (lambda (pair) (map (lambda (k) (list (car pair) (cadr pair) k))
                                               (enumerate-interval 1 (- (cadr pair) 1))))
                   (unique-pairs limit))))
