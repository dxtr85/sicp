; Exercise 3.67: Modify the pairs procedure so that (pairs
; integers integers) will produce the stream of all pairs of
; integers (i, j) (without the condition i ≤ j). Hint: You will
; need to mix in an additional stream.

; 1,1 | 1,2 1,3 1,4
; ----------------
; 2,1 | 2,2 2,3
(load "ex3.53.scm")

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
      (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
        (interleave 
          (stream-map (lambda (x) (list (stream-car s) x))
            (stream-cdr t))
          (stream-map (lambda (x) (list x (stream-car t)))
            (stream-cdr s)))
        (pairs (stream-cdr s) (stream-cdr t)))))

; (define pii (pairs integers integers))
; (display (stream-ref pii 0))
; (display (stream-ref pii 1))
; (display (stream-ref pii 2))
; (display (stream-ref pii 3))
; (display (stream-ref pii 4))
; (display (stream-ref pii 5))
; (display (stream-ref pii 6))
; (display (stream-ref pii 7))
; (display (stream-ref pii 8))
