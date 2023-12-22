; Exercise 3.64: Write a procedure stream-limit that takes
; as arguments a stream and a number (the tolerance). It should
; examine the stream until it ﬁnds two successive elements
; that differ in absolute value by less than the tolerance, and
; return the second of the two elements. Using this, we could
; compute square roots up to a given tolerance by
; (define (sqrt x tolerance)
; (stream-limit (sqrt-stream x) tolerance))

; (define (stream-limit stream tolerance)
;   (if (< (- (stream-car stream) (stream-cdr stream)) tolerance)
;       (stream-cdr stream)
;       (stream-limit (stream-cdr stream) tolerance)))

(define (stream-limit stream tolerance)
  (cond ((stream-null? stream) the-empty-stream)
        ((stream-null? (stream-cdr stream)) (stream-car stream))
        ((< (abs (stream-car stream) (stream-cdr stream)) tolerance) (stream-cdr stream))
        (else (stream-limit (stream-cdr stream) tolerance))))
