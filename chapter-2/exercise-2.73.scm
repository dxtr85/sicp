(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

; Exercise 2.73.  Section 2.3.2 described a program that performs symbolic
; differentiation: 

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        <more rules can be added here>
        (else (error "unknown expression type -- DERIV" exp))))

; We can regard this program as performing a dispatch on the type of the
; expression to be differentiated. In this situation the ``type tag'' of the datum
; is the algebraic operator symbol (such as +) and the operation being
; performed is deriv. We can transform this program into data-directed style
; by rewriting the basic derivative procedure as 

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; a.  Explain what was done above. Why can't we assimilate the predicates
; number? and same-variable? into the data-directed dispatch?
; For expressions other than a number or a variable we use the dispatch
; mechanism's procedure 'get to retrieve deriv procedure corresponding to 
; operator and operands in expression, and immediately call this procedure on
; those operands.
; We can't use dispatch on numbers and on variables, because get retrieves
; operator and operands' tags without performing any additional actions on
; them. For numbers we would have to define deriv for every possible number value,
; for variables - they do not have tags, so we can not use them.

; b.  Write the procedures for derivatives of sums and products, and the
; auxiliary code required to install them in the table used by the program
; above.
; c.  Choose any additional differentiation rule that you like, such as the one
; for exponents (exercise 2.56), and install it in this data-directed system.

(define (install-deriv-package)
  ;; internal procedures
  (define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

  (define (addend s) (cadr s))
  (define (augend s)
    (accumulate make-sum 0 (cddr s)))
  (define (multiplier p) (cadr p))
  (define (multiplicand p)
    (accumulate make-product 1 (cddr p)))
  
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  
  (define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**)))
  (define (base exp)
    (cadr exp))
  (define (exponent exp)
    (caddr exp))
  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          (else (list '** base exponent))))

  ;; interface to the rest of the system
  (put 'deriv '+
       (lambda (exp var) (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var))))
  (put 'deriv '*
       (lambda (exp var) (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp)))))
  
  (put 'deriv '**
       (lambda (exp var) (make-product (exponent exp)
                  (make-product
                   (make-exponentiation (base exp) (- (exponent exp) 1))
                   (deriv (base exp) var)))))
  'done)

; d.  In this simple algebraic manipulator the type of an expression is the
; algebraic operator that binds it together. Suppose, however, we indexed the
; procedures in the opposite way, so that the dispatch line in deriv looked like
; 
; ((get (operator exp) 'deriv) (operands exp) var)
; 
; What corresponding changes to the derivative system are required? 

; I did not extract operands from exp in order not to modify original derivative package too much
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get (operator exp) 'deriv) exp var))))

(define (install-deriv-package)
  ;; internal procedures
  (define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

  (define (addend s) (cadr s))
  (define (augend s)
    (accumulate make-sum 0 (cddr s)))
  (define (multiplier p) (cadr p))
  (define (multiplicand p)
    (accumulate make-product 1 (cddr p)))
  
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  
  (define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**)))
  (define (base exp)
    (cadr exp))
  (define (exponent exp)
    (caddr exp))
  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          (else (list '** base exponent))))

  ;; interface to the rest of the system
  (put '+ 'deriv
       (lambda (exp var) (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var))))
  (put '* 'deriv
       (lambda (exp var) (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp)))))
  
  (put '** 'deriv
       (lambda (exp var) (make-product (exponent exp)
                  (make-product
                   (make-exponentiation (base exp) (- (exponent exp) 1))
                   (deriv (base exp) var)))))
  'done)
