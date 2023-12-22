; Below definition needs to be the first one for metacircular evaluator to work
(define apply-in-underlying-scheme apply)

; We only import a language spec file, in order to keep it
; separate from evaluator's logic.
(load "ex4.2.scm")

(define (match expression table)
  ; (display "in match\n")
  ; (display  expression)
  ; (display "\ntable:\n")
  ; (display (cdr table))
  (let ((record (assoc expression (cdr table))))
    ; (display "in match, record: \n")
    ; (display record)
    (if record
      (cdr record)
      false)))

(define (assoc expression records)
  ; (display "\nin assoc:\n")
  ; (display  expression)
  ; (display "\ntest: ")
  ; (display  ((caar records) expression))
  (cond ((null? records) false)
    (((caar records) expression) (car records))
    (else (assoc expression (cdr records)))))

(define (insert! predicate? apply table)
  (set-cdr! table
    (cons (cons predicate? apply)
    (cdr table)))
  'ok)

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
    (eval (assignment-value exp) env)
    env)
  'ok)

(define (make-lang-spec-table)
  (list '*lang-spec*
    (cons self-evaluating? 'self-evaluating)
    (cons variable? 'lookup)
    (cons quoted? 'quoted)
    (cons assignment? eval-assignment)
    (cons definition? eval-definition)
    (cons if? eval-if)
    (cons lambda? 'lambda) 
    (cons begin? (lambda (exp env)
                   (eval-sequence (begin-actions exp) env)))
    (cons cond? (lambda (exp env)
                  (eval (cond->if exp) env)))
    (cons application? 
          (lambda (exp env)
            (apply (eval (operator exp) env)
              (list-of-values (operands exp) env))))))

(define *lang-spec* (make-lang-spec-table))

; (get expression) looks up the procedure that is a good match
;                  for given expression in *lang-spec* table
;                  If no item is found, get returns false.
(define (get expression)
  (match expression *lang-spec*))

; (put predicate? apply) installs the apply procedure in the table, 
(define (put predicate? apply)
  (insert! predicate? apply *lang-spec*))

(define (eval exp env)
  ; (display "In eval2, exp:\n")
  ; (display  exp)
  ; (display "\nIn eval2, env:\n")
  ; (display  env)
  ; (display "\nproceeding...\n")
  (let ((apply-procedure (get exp)))
        ; (display "get returned:\n")
        ; (display apply-procedure)
        ; (display "\n")
        (if (not apply-procedure) ; not found - display an error
          (error "Unknown expression type: EVAL" exp)
          (cond ((eq? 'self-evaluating apply-procedure) exp)
                ((eq? 'lookup apply-procedure) (lookup-variable-value exp env)) 
                ((eq? 'quoted apply-procedure) (text-of-quotation exp))
                ((eq? 'lambda apply-procedure) 
                  (make-procedure (lambda-parameters exp)
                    (lambda-body exp)
                    env))
                (else (apply-procedure exp env))))))

(define (apply procedure arguments)
  ; (display "In apply, procedure:\n")
  ; (display  procedure)
  ; (display "\nIn apply, args:\n")
  ; (display  arguments)
  (cond ((primitive-procedure? procedure)
      (apply-primitive-procedure procedure arguments))
    ((compound-procedure? procedure)
      (eval-sequence
        (procedure-body procedure)
        (extend-environment
          (procedure-parameters procedure)
          arguments
          (procedure-environment procedure))))
    (else
      (error
        "Unknown procedure type: APPLY" procedure))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
      (eval (first-exp exps) env))
    (else
      (eval (first-exp exps) env)
      (eval-sequence (rest-exps exps) env))))


; from left to right
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((first-operand-value (eval (first-operand exps) env)))
        (cons first-operand-value
          (list-of-values (rest-operands exps) env)))))

; from right to left
; (define (list-of-values exps env)
;   (if (no-operands? exps)
;     '()
;     (let ((rest-operands-value (list-of-values (rest-operands exps) env)))
;         (cons (eval (first-operand exps) env)
;               rest-operands-value))))

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

(define (let? exp) (tagged-list? exp 'let))
(define (let-var-exp-pairs exp) (cadr exp))
(define (let-body exp) (cddr exp))

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


(define (true? x) (not (eq? x false)))

(define (false? x) (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (make-frame variables values)
  (map cons variables values))

(define (add-binding-to-frame! var val frame)
  (set! frame (cons (cons var val) frame)))

(define (loop-over-env-and fun no-loop-into-subframes? var val env)
  ; (display "\ninside user defined loop\n")
  (define (env-loop env)
    ; (display "\ninside env-loop\n")
    (define (scan frame)
      ; (display "\nscanning:\n")
      ; (display frame)
      ; (display "\n\n")
      (cond ((null? frame)
          (if no-loop-into-subframes?
              (add-binding-to-frame! var val frame )
              (env-loop (enclosing-environment env))))
        ((eq? var (caar frame)) (fun frame))
        (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
          ; (display "\nframe:\n")
          ; (display frame)
          ; (display "\n\n")
        (scan frame))))
  (env-loop env))

(define (lookup-variable-value var env)
  ; (display "\nuser defined lookup\n")
  (loop-over-env-and 
    (lambda (frame)
      ; (display "lookup-variable-value returning\n")
      ; (display (cdar frame))
      (cdar frame)) 
    '() var '() env))

(define (set-variable-value! var val env)
  (loop-over-env-and 
    (lambda (frame)
      (set-cdr! (car frame) val))
    '() var val env))

(define (define-variable! var val env)
  ; (display "in defajn\n")
  (loop-over-env-and 
  (lambda (frame)
    (set-cdr! (car frame) val))
  'true var val env))

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

(define primitive-procedures
  (list (list 'car car)
    (list 'cdr cdr)
    (list 'cons cons)
    (list 'null? null?)
  ; ⟨ more primitives ⟩ 
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
    primitive-procedures))

(define (setup-environment)
  (let ((initial-env
        (extend-environment (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
      (define-variable! 'true true initial-env)
      (define-variable! 'false false initial-env)
      initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  ; (display "In app, procedure:\n")
  ; (display  proc)
  ; (display "\nIn app, args:\n")
  ; (display  args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input: ")
(define output-prompt ";;; M-Eval value: ")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string))
(define (announce-output string)
  (display "\b") (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
      (procedure-parameters object)
      (procedure-body object)
      '<procedure-env>))
  (display object)))

(driver-loop)

; Exercise 4.14: Eva Lu Ator and Louis Reasoner are each
; experimenting with the metacircular evaluator. Eva types
; in the deﬁnition of map , and runs some test programs that
; use it. They work fine. Louis, in contrast, has installed the
; system version of map as a primitive for the metacircular
; evaluator. When he tries it, things go terribly wrong. Ex-
; plain why Louis’s map fails even though Eva’s works.

; The first solution works since both arguments to map and map
; definition are withim the same realm of metacircular evaluator
; The second solution does not work since we try to apply
; args from metacircular evaluator's world into a map procedure
; that is defined in underlying scheme and does not know how to
; handle such arguments.
