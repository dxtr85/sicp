(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n) (= n (smallest-divisor n)))

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
  (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
      (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x) (newline) (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))

; (stream-car
;   (stream-cdr
;     (stream-filter prime?
;       (stream-enumerate-interval
;       10000 1000000))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
    ((pred (stream-car stream))
      (cons-stream (stream-car stream)
        (stream-filter
          pred
          (stream-cdr stream))))
    (else (stream-filter pred (stream-cdr stream)))))

(define (force delayed-object) (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
        (begin (set! result (proc))
          (set! already-run? true)
          result)
        result))))

; Exercise 3.50: Complete the following deﬁnition, which
; generalizes stream-map to allow procedures that take mul-
; tiple arguments, analogous to map in Section 2.2.1, Footnote
; 12.
; (define (stream-map proc . argstreams)
;   (if ( ⟨ ?? ⟩ (car argstreams))
;     the-empty-stream
;     ( ⟨ ?? ⟩
;       (apply proc (map
;         ⟨ ?? ⟩ argstreams))
;       (apply stream-map
;         (cons proc (map
;           ⟨ ?? ⟩ argstreams))))))

(define (stream-map proc . argstreams)
  (if (empty-stream? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map
        stream-car argstreams))
      (apply stream-map
        (cons proc (map
          stream-cdr argstreams))))))
