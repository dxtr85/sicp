;Exercise 2.58.  Suppose we want to modify the differentiation program so
;that it works with ordinary mathematical notation, in which + and * are infix
;rather than prefix operators. Since the differentiation program is defined in
;terms of abstract data, we can modify it to work with different
;representations of expressions solely by changing the predicates, selectors,
;and constructors that define the representation of the algebraic expressions
;on which the differentiator is to operate.
;
;a. Show how to do this in order to differentiate algebraic expressions
;presented in infix form, such as (x + (3 * (x + (y + 2)))). To simplify the
;task, assume that + and * always take two arguments and that expressions
;are fully parenthesized.

;The variables are symbols. They are identified by the primitive predicate symbol?:
 (define (variable? x) (symbol? x))

;Two variables are the same if the symbols representing them are eq?:
 (define (same-variable? v1 v2)
   (and (variable? v1) (variable? v2) (eq? v1 v2)))

;A sum is a list whose first element is the symbol +:
 (define (sum? x)
   (and (pair? x) (eq? (cadr x) '+)))

;The addend is the second item of the sum list:
 (define (addend s) (car s))

;The augend is the third item of the sum list:
 (define (augend s) (caddr s))

;A product is a list whose first element is the symbol *:
 (define (product? x)
   (and (pair? x) (eq? (cadr x) '*)))

;The multiplier is the second item of the product list:
 (define (multiplier p) (car p))

;The multiplicand is the third item of the product list:
 (define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))
(define (base exp)
  (car exp))
(define (exponent exp)
  (caddr exp))
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list base '** exponent))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product
                        (make-exponentiation (base exp) (- (exponent exp) 1))
                        (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;b. The problem becomes substantially harder if we allow standard algebraic
;notation, such as (x + 3 * (x + y + 2)), which drops unnecessary
;parentheses and assumes that multiplication is done before addition. Can
;you design appropriate predicates, selectors, and constructors for this
;notation such that our derivative program still works? 

; (a + b '())  
; (a * b '())
; (a + b + c.) (+ (deriv a) (deriv '(b + c.)))
; (a + b * c.) (+ (deriv a) (deriv '(b * c.)))
; (a * b + c.) (+ (deriv '(a * b)) (deriv '(c.)))
; (a * b * c * d. + e) (+ (deriv '(a * b * c * d.)) (deriv e))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? x) ; new definition of sum?
  (if (pair? x)
      (if (null? (cdr x))
          #f
          (or
           (eq? (cadr x) '+)
           (sum? (cddr x))))
      #f))
(define (addend s); new definition of addend
  (if (sum? (cddr s))
      (append (list (car s) (cadr s)) (list (addend (cddr s))))
      (car s)))
(define (augend s) ; new definition of augend
  (if (sum? (cddr s))
      (augend (cddr s))
      (if (null? (cdddr s))
          (caddr s)
          (cddr s))))
(define (product? x) ; new definition of product?
  (if (pair? x)
      (if (null? (cdr x))
          #f
          (eq? (cadr x) '*))
      #f))
(define (multiplier p) ; new definition of multiplier
  (if (product? (cddr p))
      (append (list (car p) (cadr p)) (list (addend (cddr p))))
      (car p)))
(define (multiplicand p) ; new definition of multiplicand
  (if (product? (cddr p))
      (multiplicand (cddr p))
      (if (null? (cdddr p))
          (caddr p)
          (cddr p))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

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

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product
                        (make-exponentiation (base exp) (- (exponent exp) 1))
                        (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))
