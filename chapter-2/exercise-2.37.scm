;Exercise 2.37.  Suppose we represent vectors v = (vi) as sequences of
;numbers, and matrices m = (mij) as sequences of vectors (the rows of the
;matrix). For example, the matrix
;| 1 2 3 4 |
;| 4 5 6 6 |
;| 6 7 8 9 |
;is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)).
;With this representation, we can use sequence operations to concisely
;express the basic matrix and vector operations. These operations (which
;are described in any book on matrix algebra) are the following:
;
;We can define the dot product as
;
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;Fill in the missing expressions in the following procedures for computing
;the other matrix operations. (The procedure accumulate-n is defined in
;exercise 2.36.)

(define (matrix-*-vector m v)
  (map (lambda (x) (accumulate cons '() (map * x v))) m))

(define (vector-*-vector w v)
  (map * w v))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x y) (vector-*-vector x y)) m cols)))

