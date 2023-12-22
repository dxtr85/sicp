; Exercise 4.3: Rewrite eval so that the dispatch is done
; in data-directed style. Compare this with the data-directed
; differentiation procedure of Exercise 2.73. (You may use the
; car of a compound expression as the type of the expres-
; sion, as is appropriate for the syntax implemented in this
; section.)
(load "ex4.2.scm")

(define (match expression table)
  ; (display "in match\n")
  ; (display  expression)
  ; (display "\ntable:\n")
  ; (display (cdr table))
  (let ((record (assoc expression (cdr table))))
    (display "in match, record: \n")
    (display record)
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

; This can be included to make evaluator more dynamic
; (define (replace! sample-expr predicate? apply table)
;   (let ((record (assoc sample-expr (cdr table))))
;     (if record
;       (begin
;         (let ((old-record (cons (car record) (cdr record))))
;           (set-car! record predicate?)
;           (set-cdr! record apply)
;           old-record))
;       (begin
;         (display "ERROR: replace! not found record to replace")
;         'err))))

(define (insert! predicate? apply table)
  (set-cdr! table
    (cons (cons predicate? apply)
    (cdr table)))
  'ok)

(define (make-lang-spec-table)
  (list '*lang-spec*
    (cons self-evaluating? 'self-evaluating)
    (cons variable? 'lookup)
    (cons quoted? 'quoted)
    (cons assignment? eval-assignment)
    (cons definition? eval-definition)
    (cons if? eval-if)
    (cons lambda? 'lambda) ;(lambda (exp env)
                    ; (make-procedure (lambda-parameters exp)
                    ; (lambda-body exp) env)))
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
  ; (display "in get, is pair?: ")
  ; (display (pair? expression))
  (match expression *lang-spec*))

; (put predicate? apply) installs the apply procedure in the table, 
(define (put predicate? apply)
  (insert! predicate? apply *lang-spec*))

  ; OLD EVAL 
  ; (cond ((self-evaluating? exp) exp)
  ;   ((variable? exp) (lookup-variable-value exp env))
  ;   ((quoted? exp) (text-of-quotation exp))
  ;   ((assignment? exp) (eval-assignment exp env))
  ;   ((definition? exp) (eval-definition exp env))
  ;   ((if? exp) (eval-if exp env))
  ;   ((lambda? exp) (make-procedure (lambda-parameters exp)
  ;     (lambda-body exp)
  ;     env))
  ;   ((begin? exp)
  ;     (eval-sequence (begin-actions exp) env))
  ;   ((cond? exp) (eval (cond->if exp) env))
  ;   ((application? exp)
  ;     (apply (eval (operator exp) env)
  ;     (list-of-values (operands exp) env)))
  ;   (else
  ;     (error "Unknown expression type: EVAL" exp))))
(define (eval exp env)
  (display "In eval2, exp:\n")
  (display  exp)
  (display "\nIn eval2, env:\n")
  (display  env)
  (display "\nproceeding...\n")
  (let ((apply-procedure (get exp)))
        (display "get returned:\n")
        (display apply-procedure)
        (display "\n")
        (if (not apply-procedure) ; not found - display an error
          (error "Unknown expression type: EVAL" exp)
            ; special form or built-in apply-procedure
          (cond ((eq? 'self-evaluating apply-procedure) exp)
                ((eq? 'lookup apply-procedure) (lookup-variable-value exp env)) 
                ((eq? 'quoted apply-procedure) (text-of-quotation exp))
                ((eq? 'lambda apply-procedure) 
                  (make-procedure (lambda-parameters exp)
                    (lambda-body exp)
                    env))
                (else (apply-procedure exp env))))))
              ;   (cadr apply-procedure)
              ; (apply apply-procedure
              ;   (list-of-values (operands exp) env))))))
  
