; Our problem, then, is to produce the stream int-pairs . More generally,
; suppose we have two streams S = (Si) and T = (Tj), and imagine the
; inﬁnite rectangular array
; (S 0 , T 0 ) (S 0 , T 1 ) (S 0 , T 2 ) . . .
; (S 1 , T 0 ) (S 1 , T 1 ) (S 1 , T 2 ) . . .
; (S 2 , T 0 ) (S 2 , T 1 ) (S 2 , T 2 ) . . .
; ...

; (S 0 , T 0 ) (S 0 , T 1 ) (S 0 , T 2 ) . . .
;              (S 1 , T 1 ) (S 1 , T 2 ) . . .
;                           (S 2 , T 2 ) . . .
;                                        . . .

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
      (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
        (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

; Exercise 3.66: Examine the stream (pairs integers integers) .
; Can you make any general comments about the order in
; which the pairs are placed into the stream? For example,
; approximately how many pairs precede the pair (1, 100)?
; the pair (99, 100)? the pair (100, 100)? (If you can make pre-
; cise mathematical statements here, all the beer. But feel
; free to give more qualitative answers if you ﬁnd yourself
; geing bogged down.)

; For pair (1, 100) which has it's indices equal to i=2 and j=101
; all the pairs with indices that are lower or equal to those values
; are already included in the stream, so in above case pairs from 
; first row ranging (1, 1) to (1, 100) - 100,
; For indices (i, j) we can calculate it with formula:

(define (how-many-precede i j)
  (cond ((< j i)
         (begin (display "ERROR: First integer can not be greater than secord.")
                0))
    ((= j 1) 0)
    ((= i 1) (- j 1))
    (else (+ j
     (how-many-precede (- i 1) (- j 1))))))
