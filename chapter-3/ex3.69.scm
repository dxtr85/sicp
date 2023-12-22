; Exercise 3.69: Write a procedure triples that takes three
; infinite streams, S, T , and U , and produces the stream of
; triples (S i , T j , U k ) such that i ≤ j ≤ k. Use triples to gen-
; erate the stream of all Pythagorean triples of positive inte-
; gers, i.e., the triples (i, j, k) such that i ≤ j and i 2 + j 2 = k 2 .

(load "ex3.53.scm")
(load "ex3.66.scm")

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
      (interleave s2 (stream-cdr s1)))))

(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (x) (append (list (stream-car s)) x))
                              (stream-cdr (pairs t u)))
        (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define tiii (triples integers integers integers))
; (display (stream-ref tiii 0))
; (display (stream-ref tiii 1))
; (display (stream-ref tiii 2))
; (display (stream-ref tiii 3))
; (display (stream-ref tiii 4))
; (display (stream-ref tiii 5))
; (display (stream-ref tiii 6))
; (display (stream-ref tiii 7))
; (display (stream-ref tiii 8))
; (display (stream-ref tiii 9))
; (display (stream-ref tiii 10))
; (display (stream-ref tiii 11))
; (display (stream-ref tiii 12))
; (display (stream-ref tiii 13))
; (display (stream-ref tiii 14))
; (display (stream-ref tiii 15))

(define pythagorean-triples
  (stream-filter (lambda (x) (let ((i (car x))
                                   (j (cadr x))
                                   (k (caddr x)))
                         (= (+ (* i i) (* j j))
                            (* k k)))) tiii))
; (display-line (stream-ref pythagorean-triples 0))
; (display-line (stream-ref pythagorean-triples 1))
; (display-line (stream-ref pythagorean-triples 2))
; (display-line (stream-ref pythagorean-triples 3))
; (display-line (stream-ref pythagorean-triples 4))
; (display-line (stream-ref pythagorean-triples 5))
