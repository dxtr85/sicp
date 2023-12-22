; Here is the speciﬁcation of the syntax of our language:
; The only self-evaluating items are numbers and strings:
(define (self-evaluating? exp)
  (cond ((number? exp) true)
    ((string? exp) true)
    (else false)))

; Variables are represented by symbols:
(define (variable? exp) (symbol? exp))

; Quotations have the form (quote ⟨ text-of-quotation ⟩):
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

; Assignments have the form (set! ⟨ var ⟩ ⟨ value ⟩ ) :
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; Deﬁnitions have the form
; (define ⟨ var ⟩ ⟨ value ⟩ )
; or the form
; (define ( ⟨ var ⟩
; ⟨ body ⟩ )

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp) ; formal parameters
      (cddr exp))))          ; body

; lambda expressions are lists that begin with the symbol lambda :
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

; We also provide a constructor for lambda expressions, which is
; used by definition-value , above:
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; Conditionals begin with if and have a predicate, a consequent,
; and an (optional) alternative. If the expression has no alternative
; part, we provide false as the alternative. 10
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    false))

; We also provide a constructor for if expressions, to be used by
; cond->if to transform cond expressions into if expressions:
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; begin packages a sequence of expressions into a single expres-
; sion. We include syntax operations on begin expressions to ex-
; tract the actual sequence from the begin expression, as well as
; selectors that return the ﬁrst expression and the rest of the ex-
; pressions in the sequence. 11
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

; We also include a constructor sequence->exp (for use by cond-
; >if) that transforms a sequence into a single expression, using
; begin if necessary:

(define (sequence->exp seq)
  (cond ((null? seq) seq)
    ((last-exp? seq) (first-exp seq))
    (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

; A procedure application is any compound expression that is not
; one of the above expression types. The car of the expression is
; the operator, and the cdr is the list of operands:
(define (application? exp) 
  ; (display "in application?\ntesting: ")
  ; (display exp)
  ; (display "result: ")
  ; (display (pair? exp))
  ; (display "\n")
  (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; Derived expressions

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
    false             ; no else clause
    (let ((first (car clauses))
        (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last: COND->IF"
            clauses))
        (make-if (cond-predicate first)
          (sequence->exp (cond-actions first))
          (expand-clauses rest))))))

; Exercise 4.2: Louis Reasoner plans to reorder the cond clauses
; in eval so that the clause for procedure applications ap-
; pears before the clause for assignments. He argues that this
; will make the interpreter more efficient: Since programs
; usually contain more applications than assignments, def-
; initions, and so on, his modiﬁed eval will usually check
; fewer clauses than the original eval before identifying the
; type of an expression.

; a. What is wrong with Louis’s plan? (Hint: What will
;    Louis’s evaluator do with the expression (define x
;    3) ?)
; b. Louis is upset that his plan didn’t work. He is will-
;    ing to go to any lengths to make his evaluator recog-
;    nize procedure applications before it checks for most
;    other kinds of expressions. Help him by changing the
;    syntax of the evaluated language so that procedure
;    applications start with call . For example, instead of
;    (factorial 3) we will now have to write (call factorial
;    3) and instead of (+ 1 2) we will have to write (call
;    + 1 2) .

; ad a) Louis's plan is wrong since definition for application?
; only checks whether given expression is a pair. This will
; return true not only for procedure application expressions
; but also for others, like (define x 3) which also is a pair.
; In this case evaluator will try to apply a procedure stored 
; under variable define in environment, and not treat define
; as a special form extending environment.

; b)
; (define (application? exp) (tagged-list? exp 'call))
; (define (operator exp) (cadr exp))
; (define (operands exp) (cddr exp))
