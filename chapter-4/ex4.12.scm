; Exercise 4.12: The procedures set-variable-value! , define-
; variable! and lookup-variable-value can be expressed
; in terms of more abstract procedures for traversing the en-
; vironment structure. Deﬁne abstractions that capture the
; common patterns and redefine the three procedures in terms
; of these abstractions.

; (define (lookup-variable-value var env)
;   (display "\nuser defined lookup\n")
;   (loop-over-env-and 
;     (lambda (vars vals)
;       (display "lookup-variable-value returning\n")
;       (display (car vals))
;       (car vals)) '() var '() env))

; (define (set-variable-value! var val env)
;   (loop-over-env-and 
;     (lambda (vars vals)
;       (set-car! vals val)) '() var val env))

; (define (define-variable! var val env)
;   (display "in defajn\n")
;   (loop-over-env-and 
;   (lambda (vars vals)
;     (set-car! vals val)) 'true var val env))


(define (loop-over-env-and fun no-loop-into-subframes? var val env)
  (display "inside user defined loop")
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
          (if no-loop-into-subframes?
              (add-binding-to-frame! var val (first-frame env))
              (env-loop (enclosing-environment env))))
        ((eq? var (car vars)) (fun vars vals))
        (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
          (display "\nframe:\n")
          (display frame)
        (scan (frame-variables frame)
          (frame-values frame)))))
  (env-loop env))
