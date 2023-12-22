; Exercise 4.4: Recall the definitions of the special forms and
; and or from Chapter 1:
; * and: The expressions are evaluated from left to right.
; If any expression evaluates to false, false is returned;
; any remaining expressions are not evaluated. If all the
; expressions evaluate to true values, the value of the
; last expression is returned. If there are no expressions
; then true is returned.
; * or: The expressions are evaluated from left to right.
; If any expression evaluates to a true value, that value
; is returned; any remaining expressions are not evalu-
; ated. If all expressions evaluate to false, or if there are
; no expressions, then false is returned.
; Install and and or as new special forms for the evaluator by
; deﬁning appropriate syntax procedures and evaluation pro-
; cedures eval-and and eval-or. Alternatively, show how to
; implement and and or as derived expressions.
(load "ex4.3.scm")

(define (or? exp) (tagged-list? exp 'or))
(define (and? exp) (tagged-list? exp 'and))
(define (logic-expressions exp) (cdr exp))
(define (first-logic-expression exp) (car exp))
(define (rest-logic-expressions exp) (cdr exp))
(define (last-logic-expression? exp) (eq? (rest-logic-expressions exp) '()))

(define (apply-or exp env)
  (define (apply-to-first-true exp)
    (let ((first-le (first-logic-expression le))
          (rest-le (rest-logic-expressions le))
          (last-le? (last-logic-expression? le)))
      (let ((first-evaluated (eval first-le env)))
        (if last-le?
          first-evaluated
          (if (true? first-evaluated)
              first-evaluated
              (apply-to-first-true rest-le))))))
  (let ((le (logic-expressions exp)))
      (apply-to-first-true le)))

(put or? apply-or)

(define (apply-and exp env)
  (define (apply-to-first-false exp)
    (let ((first-le (first-logic-expression le))
          (rest-le (rest-logic-expressions le))
          (last-le? (last-logic-expression? le)))
      (let ((first-evaluated (eval first-le env)))
        (if last-le?
          first-evaluated
          (if (not (true? first-evaluated))
              first-evaluated
              (apply-to-first-false rest-le))))))
  (let ((le (logic-expressions exp)))
      (apply-to-first-false le)))

(put and? apply-and)
