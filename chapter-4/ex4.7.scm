; Exercise 4.7: let* is similar to let, except that the bind-
; ings of the let* variables are performed sequentially from
; left to right, and each binding is made in an environment in
; which all of the preceding bindings are visible. For example
; (let* ((x 3)
;   (y (+ x 2))
;   (z (+ x y 5)))
;     (* x z))
; returns 39. Explain how a let* expression can be rewritten
; as a set of nested let expressions, and write a procedure
; let*->nested-lets that performs this transformation. If
; we have already implemented let (Exercise 4.6) and we
; want to extend the evaluator to handle let*, is it sufficient
; to add a clause to eval whose action is
; (eval (let*->nested-lets exp) env)
; or must we explicitly expand let* in terms of non-derived
; expressions?

(define (let*? exp) (tagged-list? exp 'let*))
(define (last-pair? pairs) (null? (cdr pairs)))
(define (first-pair pairs) (car pairs))
(define (rest-pairs pairs) (cdr pairs))
(define (make-let pairs body)
  (list 'let pairs body))
(define (let-var-exp-pairs exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let*->nested-lets exp env)
  (let ((pairs (let-var-exp-pairs exp))
        (body (let-body exp)))
    (if (last-pair? pairs)
        (eval (make-let pairs body) env)
        (eval (make-let (cons (first-pair pairs) '())
                (list 'let* (rest-pairs pairs) body)) env))))

; It is sufficient to evaluate this line in order to
; add support for let* expressions:
(put let*? let*->nested-lets)


