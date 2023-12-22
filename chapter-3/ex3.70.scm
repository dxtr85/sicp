; Exercise 3.70: It would be nice to be able to generate streams
; in which the pairs appear in some useful order, rather than
; in the order that results from an ad hoc interleaving pro-
; cess. We can use a technique similar to the merge procedure
; of Exercise 3.56, if we define a way to say that one pair of
; integers is “less than” another. One way to do this is to de-
; fine a “weighting function” W (i, j) and stipulate that (i1, j1)
; is less than (i2, j2) if W(i1, j1) < W(i2, j2). Write a proce-
; dure merge-weighted that is like merge , except that merge-
; weighted takes an additional argument weight , which is a
; procedure that computes the weight of a pair, and is used
; to determine the order in which elements should appear in
; the resulting merged stream. Using this, generalize pairs
; to a procedure weighted-pairs that takes two streams, to-
; gether with a procedure that computes a weighting func-
; tion, and generates the stream of pairs, ordered according
; to weight. Use your procedure to generate
;   a. the stream of all pairs of positive integers (i, j) with
;      i ≤ j ordered according to the sum i + j,
;   b. the stream of all pairs of positive integers (i, j) with
;      i ≤ j, where neither i nor j is divisible by 2, 3, or 5, and
;      the pairs are ordered according to the sum 2i + 3j + 5ij.

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    (else
      (let ((s1carw (weight (stream-car s1)))
            (s2carw (weight (stream-car s2)))
            (s1car (stream-car s1))
            (s2car (stream-car s2)))
        (cond ((< s1carw s2carw)
            (cons-stream
              s1car
              (merge-weighted (stream-cdr s1) s2 weight)))
          ((> s1carw s2carw)
            (cons-stream
              s2car
              (merge-weighted s1 (stream-cdr s2) weight)))
          (else
            (cons-stream
              s1car
              (cons-stream
                s2car
                (merge-weighted (stream-cdr s1)
                  (stream-cdr s2) weight)))))))))

(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
        (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))

; a)
(define (sum-pair-elements a-pair)
  (+ (car a-pair) (cadr a-pair)))

(define int-pairs 
  (weighted-pairs integers 
                  integers 
                  sum-pair-elements))

; (display "\nint-pairs: ")
; (display (stream-ref int-pairs 0))
; (display (stream-ref int-pairs 1))
; (display (stream-ref int-pairs 2))
; (display (stream-ref int-pairs 3))
; (display (stream-ref int-pairs 4))
; (display (stream-ref int-pairs 5))
; (display (stream-ref int-pairs 6))
; (display (stream-ref int-pairs 7))
; (display (stream-ref int-pairs 8))
; (display (stream-ref int-pairs 9))
; (display (stream-ref int-pairs 10))
; (display (stream-ref int-pairs 11))
; (display (stream-ref int-pairs 12))
; (display (stream-ref int-pairs 13))
; (display (stream-ref int-pairs 14))
; (display (stream-ref int-pairs 15))
; (display (stream-ref int-pairs 16))
; (display (stream-ref int-pairs 17))
; (display (stream-ref int-pairs 18))
; (display (stream-ref int-pairs 19))

(define (divisible? x y) (= (remainder x y) 0))

(define no-235-ints
  (stream-filter (lambda (x) (and (not (divisible? x 2))
                                  (not (divisible? x 3))
                                  (not (divisible? x 5))))
    integers))

; (display "\nno-235-ints: ")
; (display (stream-ref no-235-ints 0))
; (display " ")
; (display (stream-ref no-235-ints 1))
; (display " ")
; (display (stream-ref no-235-ints 2))
; (display " ")
; (display (stream-ref no-235-ints 3))
; (display " ")
; (display (stream-ref no-235-ints 4))
; (display " ")
; (display (stream-ref no-235-ints 5))
; (display " ")
; (display (stream-ref no-235-ints 6))
; (display " ")
; (display (stream-ref no-235-ints 7))
; (display " ")
; (display (stream-ref no-235-ints 8))
; (display " ")
; (display (stream-ref no-235-ints 9))

; b)
(define (weight-235 a-pair)
  (let ((i (car a-pair))
        (j (cadr a-pair)))
        
    (+ (* 2 i)
       (* 3 j)
       (* 5 i j))))

(define no-235-pairs 
  (weighted-pairs no-235-ints
                  no-235-ints
                  weight-235))

; (display "\nweighted no-235-pairs: ")
; (display (stream-ref no-235-pairs 0))
; (display (stream-ref no-235-pairs 1))
; (display (stream-ref no-235-pairs 2))
; (display (stream-ref no-235-pairs 3))
; (display (stream-ref no-235-pairs 4))
; (display (stream-ref no-235-pairs 5))
; (display (stream-ref no-235-pairs 6))
; (display (stream-ref no-235-pairs 7))
; (display (stream-ref no-235-pairs 8))
; (display (stream-ref no-235-pairs 9))
; (display (stream-ref no-235-pairs 10))
