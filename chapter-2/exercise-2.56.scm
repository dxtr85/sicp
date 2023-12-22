;We should also be able to construct expressions from parts.
;Let us assume that we already have procedures to implement the following
;selectors, constructors, and predicates:
;
; (variable? e)   Is e a variable?  
; (same-variable? v1 v2)   Are v1 and v2 the same variable?  
; (sum? e)   Is e a sum?  
; (addend e)   Addend of the sum e.  
; (augend e)   Augend of the sum e.  
; (make-sum a1 a2)   Construct the sum of a1 and a2.  
; (product? e)   Is e a product?  
; (multiplier e)   Multiplier of the product e.  
; (multiplicand e)   Multiplicand of the product e.  
; (make-product m1 m2)   Construct the product of m1 and m2.

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
        (else
         (error "unknown expression type -- DERIV" exp))))

;The variables are symbols. They are identified by the primitive predicate symbol?:
 (define (variable? x) (symbol? x))

;Two variables are the same if the symbols representing them are eq?:
 (define (same-variable? v1 v2)
   (and (variable? v1) (variable? v2) (eq? v1 v2)))

;Sums and products are constructed as lists:
 (define (make-sum a1 a2) (list '+ a1 a2))

 (define (make-product m1 m2) (list '* m1 m2))

;A sum is a list whose first element is the symbol +:
 (define (sum? x)
   (and (pair? x) (eq? (car x) '+)))

;The addend is the second item of the sum list:
 (define (addend s) (cadr s))

;The augend is the third item of the sum list:
 (define (augend s) (caddr s))

;A product is a list whose first element is the symbol *:
 (define (product? x)
   (and (pair? x) (eq? (car x) '*)))

;The multiplier is the second item of the product list:
 (define (multiplier p) (cadr p))

;The multiplicand is the third item of the product list:
 (define (multiplicand p) (caddr p))

;(deriv '(+ x 3) 'x)
;(+ 1 0)
;(deriv '(* x y) 'x)
;(+ (* x 0) (* 1 y))
;(deriv '(* (* x y) (+ x 3)) 'x)
;(+ (* (* x y) (+ 1 0))
;   (* (+ (* x 0) (* 1 y))
;      (+  x 3)))

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

;(deriv '(+ x 3) 'x)
;1
;(deriv '(* x y) 'x)
;y
;(deriv '(* (* x y) (+ x 3)) 'x)
;(+ (* x y) (* y (+ x 3)))

;Exercise 2.56.  Show how to extend the basic differentiator to handle more
;kinds of expressions. For instance, implement the differentiation rule
;
;*
;
;by adding a new clause to the deriv program and defining appropriate
;procedures exponentiation?, base, exponent, and make-exponentiation. (You may
;use the symbol ** to denote exponentiation.) Build in the rules that anything
;raised to the power 0 is 1 and anything raised to the power 1 is the thing
;itself. 

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
