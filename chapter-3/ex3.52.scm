; Exercise 3.52: Consider the sequence of expressions
(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum
  (stream-enumerate-interval 1 20)))
; sum: 1
(define y (stream-filter even? seq))
; sum: 6
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
  seq))
; sum: 10
(stream-ref y 7)
; 1
; 3
; y ref 0: 6 *
; y ref 1: 10 *
; 15
; 21
; y ref 2: 28 *
; y ref 3: 36 *
; 45
; 55
; y ref 4: 66 *
; y ref 5: 78 *
; 91
; 105
; y ref 6: 120 *
; y ref 7: 136 *
; sum: 136
(display-stream z)
; 10
; 15
; 45
; 55
; 105
; 120
; 190
; 210
; sum: 210

; What is the value of sum aer each of the above expressions
; is evaluated? What is the printed response to evaluating
; the stream-ref and display-stream expressions? Would
; these responses diﬀer if we had implemented (delay ⟨ exp ⟩ )
; simply as (lambda () ⟨ exp ⟩ ) without using the optimiza-
; tion provided by memo-proc ? Explain.

; It would differ because for every stream-cdr call we would
; evaluate the accum procedure which would count given sequence
; element multiple times. With memo-fun implemented we only call
; accum once, and every subsequent call to stream-cdr results in
; retrieval of already calculated value from memory instead of
; calling accum once again.
