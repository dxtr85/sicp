; Exercise 4.10: By using data abstraction, we were able to
; write an eval procedure that is independent of the particu-
; lar syntax of the language to be evaluated. To illustrate this,
; design and implement a new syntax for Scheme by modify-
; ing the procedures in this section, without changing eval
; or apply.

; Here is the speciﬁcation of the syntax of our language:
; The only self-evaluating items are numbers and strings:

; stays the same
; (define (self-evaluating? exp)
;   (cond ((number? exp) true)
;     ((string? exp) true)
;     (else false)))

; stays the same
; Variables are represented by symbols:
; (define (variable? exp) (symbol? exp))

; Quotations have the form (quote ⟨ text-of-quotation ⟩):
(define (quoted? exp) (tagged-list? exp 'cytat))
; (define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

; Assignments have the form (set! ⟨ var ⟩ ⟨ value ⟩ ) :
(define (assignment? exp) (tagged-list? exp 'ustaw!))
; (define (assignment-variable exp) (cadr exp))
; (define (assignment-value exp) (caddr exp))

; Deﬁnitions have the form
; (define ⟨ var ⟩ ⟨ value ⟩ )
; or the form
; (define ( ⟨ var ⟩
; ⟨ body ⟩ )

(define (definition? exp) (tagged-list? exp 'zdefiniuj))
; (define (definition-variable exp)
;   (if (symbol? (cadr exp))
;     (cadr exp)
;     (caadr exp)))
; (define (definition-value exp)
;   (if (symbol? (cadr exp))
;     (caddr exp)
;     (make-lambda (cdadr exp) ; formal parameters
;       (cddr exp))))          ; body

; lambda expressions are lists that begin with the symbol lambda :
; (define (lambda? exp) (tagged-list? exp 'lambda))
; (define (lambda-parameters exp) (cadr exp))
; (define (lambda-body exp) (cddr exp))

; We also provide a constructor for lambda expressions, which is
; used by definition-value , above:
; (define (make-lambda parameters body)
;   (cons 'lambda (cons parameters body)))

; Conditionals begin with if and have a predicate, a consequent,
; and an (optional) alternative. If the expression has no alternative
; part, we provide false as the alternative. 10
(define (if? exp) (tagged-list? exp 'jeśli))
; (define (if-predicate exp) (cadr exp))
; (define (if-consequent exp) (caddr exp))
; (define (if-alternative exp)
;   (if (not (null? (cdddr exp)))
;     (cadddr exp)
;     'false))

; We also provide a constructor for if expressions, to be used by
; cond->if to transform cond expressions into if expressions:
(define (make-if predicate consequent alternative)
  (list 'jeśli predicate consequent alternative))

; begin packages a sequence of expressions into a single expres-
; sion. We include syntax operations on begin expressions to ex-
; tract the actual sequence from the begin expression, as well as
; selectors that return the ﬁrst expression and the rest of the ex-
; pressions in the sequence. 11
(define (begin? exp) (tagged-list? exp 'początek))
; (define (begin-actions exp) (cdr exp))
; (define (last-exp? seq) (null? (cdr seq)))
; (define (first-exp seq) (car seq))
; (define (rest-exps seq) (cdr seq))

; We also include a constructor sequence->exp (for use by cond-
; >if) that transforms a sequence into a single expression, using
; begin if necessary:

; (define (sequence->exp seq)
;   (cond ((null? seq) seq)
;     ((last-exp? seq) (first-exp seq))
;     (else (make-begin seq))))
(define (make-begin seq) (cons 'początek seq))

; A procedure application is any compound expression that is not
; one of the above expression types. The car of the expression is
; the operator, and the cdr is the list of operands:
(define (application? exp) (pair? exp))
; (define (operator exp) (car exp))
; (define (operands exp) (cdr exp))
; (define (no-operands? ops) (null? ops))
; (define (first-operand ops) (car ops))
; (define (rest-operands ops) (cdr ops))

; Derived expressions

(define (cond? exp) (tagged-list? exp 'warun))
; (define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'inaczej))
; (define (cond-predicate clause) (car clause))
; (define (cond-actions clause) (cdr clause))
; (define (cond->if exp) (expand-clauses (cond-clauses exp)))
; (define (expand-clauses clauses)
;   (if (null? clauses)
;     'false             ; no else clause
;     (let ((first (car clauses))
;         (rest (cdr clauses)))
;       (if (cond-else-clause? first)
;         (if (null? rest)
;           (sequence->exp (cond-actions first))
;           (error "ELSE clause isn't last: COND->IF"
;             clauses))
;         (make-if (cond-predicate first)
;           (sequence->exp (cond-actions first))
;           (expand-clauses rest))))))

; Not adding syntax from exercises 3-9.

