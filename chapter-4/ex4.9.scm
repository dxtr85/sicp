; Exercise 4.9: Many languages support a variety of iteration
; constructs, such as do, for, while, and until. In Scheme,
; iterative processes can be expressed in terms of ordinary
; procedure calls, so special iteration constructs provide no
; essential gain in computational power. On the other hand,
; such constructs are often convenient. Design some itera-
; tion constructs, give examples of their use, and show how
; to implement them as derived expressions.

; It will evaluate body at least one time. After each body
; evaluation condition is being checked and if it returns
; true then another iteration is performed.
; (do <body> <condition>)

(define (do? exp) (tagged-list? exp 'do))
(define (do-body exp) (cdr exp))

(define (make-do body)
  (list 'do body))

(define (eval-do exp env)
  (define (eval-do-block dexp env)
    (define (make-if-do condition)
      (make-if condition (make-do body) 'false))
    (let 
      ((last? (last-exp? dexp))
      (first (first-exp dexp))
      (rest (rest-exps dexp)))
        (cond (last?
               (eval (make-if-do first) env))
              (else
                (begin 
                  (eval first env)
                  (eval-do-block rest env))))))

    (let* ((body (do-body exp))
           (dexp (sequence->exp seq)))
      (eval-do-block dexp env)))

(put do? eval-do)
