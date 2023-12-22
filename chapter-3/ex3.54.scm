; Exercise 3.54: Deﬁne a procedure mul-streams , analogous
; to add-streams , that produces the elementwise product of
; its two input streams. Use this together with the stream of
; integers to complete the following deﬁnition of the stream
; whose n th element (counting from 0) is n + 1 factorial:
; (define factorials
; (cons-stream 1 (mul-streams
; ⟨ ?? ⟩ ⟨ ?? ⟩ )))
(define (mul-streams s1 s2) (stream-map * s1 s2))

; Commented out is as required by exercise
; (define factorials
; (cons-stream 1 (mul-streams
;   factorials
;   (integers-starting-from 2))))

; Following solutions is redefined to
; match arguments from stream-ref
; e.g (stream-ref factorials 5) is 120 (5!) not 720 (5 + 1)!
(define factorials
(cons-stream 1 (mul-streams
  factorials
  integers)))

