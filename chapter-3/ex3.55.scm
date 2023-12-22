; Exercise 3.55: Deﬁne a procedure partial-sums that takes
; as argument a stream S and returns the stream whose ele-
; ments are S 0 , S 0 +S 1 , S 0 +S 1 +S 2 , . . .. For example, (partial-
; sums integers) should be the stream 1, 3, 6, 10, 15, . . ..

(define (partial-sums stream)
  (cons-stream (stream-car stream) 
    (add-streams (stream-cdr stream) (partial-sums stream))))
