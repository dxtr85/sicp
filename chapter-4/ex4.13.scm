; Exercise 4.13: Scheme allows us to create new bindings for
; variables by means of define, but provides no way to get
; rid of bindings. Implement for the evaluator a special form
; make-unbound! that removes the binding of a given symbol
; from the environment in which the make-unbound! expres-
; sion is evaluated. This problem is not completely specified.
; For example, should we remove only the binding in the first
; frame of the environment? Complete the speciﬁcation and
; justify any choices you make.
(load "ex4.12.scm")

(define (eval-make-unbound exp env)
  (let ((var (make-unbound-variable exp)))
    (loop-over-env-and 
      (lambda (vars vals)
        (set-car! vals (cdr vals)
        (set-car! vars (cdr vars))) '()) var val env))
)
(define (make-unbound? exp) (tagged-list? exp 'make-unbound!))
(define (make-unbound-variable exp) (cadr exp))

(put make-unbound? eval-make-unbound)

; If we wanted to unbind a variable only if it is defined
; in current frame then we should replace '() with 'true
; in above lambda function.
