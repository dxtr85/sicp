; Exercise 3.71: Numbers that can be expressed as the sum of
; two cubes in more than one way are sometimes called Ra-
; manujan numbers, in honor of the mathematician Srinivasa
; Ramanujan. Ordered streams of pairs provide an elegant
; solution to the problem of computing these numbers. To
; ﬁnd a number that can be written as the sum of two cubes
; in two different ways, we need only generate the stream of
; pairs of integers (i, j) weighted according to the sum i^3 + j^3
; (see Exercise 3.70), then search the stream for two consecu-
; tive pairs with the same weight. Write a procedure to gener-
; ate the Ramanujan numbers. The ﬁrst such number is 1,729.
; What are the next ﬁve?
(load "ex3.50.scm")
(load "ex3.70.scm")

(define (sum-cube-pair-elements a-pair)
  (let ((i (car a-pair))
        (j (cadr a-pair)))
    (+ (* i i i) (* j j j))))

(define int-pairs 
  (weighted-pairs integers 
                  integers 
                  sum-cube-pair-elements))

(define ramanujan-numbers 
  ((lambda()
    (define (get-next stream prev-weight prev-pair)
           (let ((cur-weight (sum-cube-pair-elements (stream-car stream)))
                 (cur-pair (stream-car stream)))
             (if (= cur-weight prev-weight)
               (cons-stream (list cur-weight prev-pair cur-pair)
                            (get-next (stream-cdr stream) cur-weight cur-pair))
               (get-next (stream-cdr stream) cur-weight cur-pair))))
         (get-next int-pairs 0 0))))

(display-line (stream-ref ramanujan-numbers 0))
(display-line (stream-ref ramanujan-numbers 1))
(display-line (stream-ref ramanujan-numbers 2))
(display-line (stream-ref ramanujan-numbers 3))
(display-line (stream-ref ramanujan-numbers 4))
(display-line (stream-ref ramanujan-numbers 5))
(display-line (stream-ref ramanujan-numbers 6))
(display-line (stream-ref ramanujan-numbers 7))
(display-line (stream-ref ramanujan-numbers 8))
(display-line (stream-ref ramanujan-numbers 9))
(display-line (stream-ref ramanujan-numbers 10))
(display-line (stream-ref ramanujan-numbers 11))
