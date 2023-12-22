; Exercise 4.5: Scheme allows an additional syntax for cond
; clauses, (⟨test⟩ => ⟨recipient⟩). If ⟨test⟩ evaluates to a
; true value, then ⟨recipient⟩ is evaluated. Its value must be a
; procedure of one argument; this procedure is then invoked
; on the value of the ⟨test⟩, and the result is returned as the
; value of the cond expression. For example
; (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;   (else false))
; returns 2. Modify the handling of cond so that it supports
; this extended syntax.

; Below allows for usage of hybrid cond expressions
; where clauses with => can interleave with those
; old ones that do not contain =>
(define (expand-clauses-extended clauses)
  (if (null? clauses)
    'false             ; no else clause
    (let ((first (car clauses))
        (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last: COND->IF"
            clauses))
        (if (eq? '=> (car (cond-actions first)))
          (let ((predicate-evaluated (eval (cond-predicate first) env)))
              (if (true? predicate-evaluated)
                  ((cdr (cond-actions first)) predicate-evaluated)
                  (expand-clauses-extended rest)))
          (make-if (cond-predicate first)
            (sequence->exp (cond-actions first))
          (expand-clauses-extended rest)))))))

(define (apply-cond-extended exp env)
  (let ((clauses (cond-clauses exp)))
    (expand-clauses-extended clauses)))

(put cond? apply-cond-extended)

