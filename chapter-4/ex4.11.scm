(define (true? x) (not (eq? x false)))

(define (false? x) (eq? x false))

; (apply-primitive-procedure ⟨proc⟩ ⟨args⟩)

; (primitive-procedure? ⟨proc⟩)

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

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (display "\norig lookup\n")
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
          (env-loop (enclosing-environment env)))
        ((eq? var (car vars)) (car vals))
        (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
        (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
          (env-loop (enclosing-environment env)))
        ((eq? var (car vars)) (set-car! vals val))
        (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable: SET!" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
        (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
          (add-binding-to-frame! var val frame))
        ((eq? var (car vars)) (set-car! vals val))
        (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

; Exercise 4.11: Instead of representing a frame as a pair of
; lists, we can represent a frame as a list of bindings, where
; each binding is a name-value pair. Rewrite the environment
; operations to use this alternative representation.


(define (make-frame variables values)
  (map cons variables values))
; (define (frame-variables frame) (map car frame))
; (define (frame-values frame) (map cdr frame))
(define (add-binding-to-frame! var val frame)
  (set! frame (cons (cons var val) frame)))

(define (lookup-variable-value var env)
  (display "\norig2 lookup\n")
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
          (env-loop (enclosing-environment env)))
        ((eq? var (caar frame)) (cdar frame))
        (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan frame))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
          (env-loop (enclosing-environment env)))
        ((eq? var (caar frame)) (set-cdr! (car frame)))
        (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable: SET!" var)
      (let ((frame (first-frame env)))
        (scan frame))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan frame)
      (cond ((null? frame)
          (add-binding-to-frame! var val frame))
        ((eq? var (caar frame)) (set-cdr! (car frame) val))
        (else (scan (cdr frame)))))
    (scan frame)))

