; Exercise 4.6: let expressions are derived expressions, be-
; cause
; (let (( <var1> <exp1>) ... (<varn> <expn>))
;   <body>)

; is equivalent to

; ((lambda (<var1> ... <varn>)
;     <body>)
;   <exp1>
;   ...
;   <expn>)

; Implement a syntactic transformation let->combination
; that reduces evaluating let expressions to evaluating com-
; binations of the type shown above, and add the appropriate
; clause to eval to handle let expressions.

(define (let? exp) (tagged-list? exp 'let))
(define (let-var-exp-pairs exp) (cadr exp))
(define (let-body exp) (cddr exp))

; (define (let->combination exp env)
;   (let ((pairs (let-var-exp-pairs exp))
;         (body (led-body exp)))
;     (let ((parameters (map car pairs))
;           (arguments (map cadr pairs)))
;       (apply (make-procedure parameters body env)
;              (list-of-values arguments env)))))

(define (let->combination exp env)
  (let ((pairs (let-var-exp-pairs exp))
        (body (let-body exp)))
    (let ((parameters (map car pairs))
          (arguments (map cadr pairs)))
      (eval (make-lambda parameters body)
             (extend-environment parameters
                                 arguments
                                 env)))))

(put let? let->combination)
