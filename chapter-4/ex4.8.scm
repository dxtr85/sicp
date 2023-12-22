; Exercise 4.8: “Named let ” is a variant of let that has the
; form
; (let ⟨var⟩ ⟨bindings⟩ ⟨body⟩)
; The ⟨bindings⟩ and ⟨body⟩ are just as in ordinary let, ex-
; cept that ⟨var⟩ is bound within ⟨body⟩ to a procedure whose
; body is ⟨body⟩ and whose parameters are the variables in
; the ⟨bindings⟩. Thus, one can repeatedly execute the ⟨body⟩
; by invoking the procedure named ⟨var⟩. For example, the
; iterative Fibonacci procedure (Section 1.2.2) can be rewrit-
; ten using named let as follows:

; (define (fib n)
;   (let fib-iter ((a 1)
;                  (b 0)
;                  (count n))
;     (if (= count 0)
;       b
;       (fib-iter (+ a b) a (- count 1)))))

; Modify let->combination of Exercise 4.6 to also support
; named let.

(define (named-let? exp) (and (let? exp) (symbol? (cadr exp))))
(define (let-var-exp-pairs exp) 
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))
(define (let-name exp)
  (cadr exl)) 
(define (let-body exp) 
  (if (named-let? exp)
      (cdddr exp)
      (cddr exp)))

(define (let->combination exp env)
  (let ((pairs (let-var-exp-pairs exp))
        (body (let-body exp)))
    (let ((parameters (map car pairs))
          (arguments (map cadr pairs)))
      (define new-env 
        (if (named-let? exp)
            (extend-environment (cons (let-name exp) '())
                                (cons (make-procedure parameters body env) '())
                                env)
            env))
          (eval (make-lambda parameters body)
             (extend-environment parameters
                                 arguments
                                 new-env)))))
