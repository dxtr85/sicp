; Exercise 2.85.  This section mentioned a method for ``simplifying'' a data
; object by lowering it in the tower of types as far as possible. Design a
; procedure drop that accomplishes this for the tower described in exercise
; 2.83. The key is to decide, in some general way, whether an object can be
; lowered. For example, the complex number 1.5 + 0i can be lowered as far
; as real, the complex number 1 + 0i can be lowered as far as integer, and
; the complex number 2 + 3i cannot be lowered at all. Here is a plan for
; determining whether an object can be lowered: Begin by defining a generic
; operation project that ``pushes'' an object down in the tower. For example,
; projecting a complex number would involve throwing away the imaginary
; part. Then a number can be dropped if, when we project it and raise the
; result back to the type we started with, we end up with something equal to
; what we started with. Show how to implement this idea in detail, by writing
; a drop procedure that drops an object as far as possible. You will need to
; design the various projection operations and install project as a generic
; operation in the system. You will also need to make use of a generic
; equality predicate, such as described in exercise 2.79. Finally, use drop to
; rewrite apply-generic from exercise 2.84 so that it ``simplifies'' its answers.

(put 'project '(complex)
     (lambda (z) (make-rational (real-part z) 0)))

(put 'project '(rational)
     (lambda (r) (make-real (/ (numer r) (denom r)))))

(put 'project '(real)
     (lambda (r) (attach-tag 'scheme-number (inexact->exact r))))

(define (drop x)
  (let ((px (project x)))
    (if (equ? x (raise-type px))
        (drop px)
        x)))
      
;; coercion
(define (integer->rational n)
  (make-rat (contents n) 1))

(define (rational->real n)
  (make-real (/ (numer (contents n)) (denom (contents n)))))

(define (real->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'integer 'rational integer->rational)
(put-coercion 'rational 'real rational->real)
(put-coercion 'real 'complex real->complex)

(define (supertype val)
  (cond ((eq? (type-tag x) 'integer) 'rational)
        ((eq? (type-tag x) 'rational) 'real)
        ((eq? (type-tag x) 'real) 'complex)
        (else '())))

(define (raise-type x)
  (if (supertype x)
      ((get-coercion
        (type-tag x)
        (supertype x)) x)
      (error "Unable to raise type" (type-tag x))))

(define (supertypes x)
    (cons (supertype x) (supertypes (raise-type x))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (if (memq (car type-tags) (supertypes (cadr args)))
              (apply-generic op (raise-type (car args)) (cadr args))
              (if (not (eq? (car type-tags) (cadr type-tags)))
                  (apply-generic op (car args) (raise-type (cadr args)))
                  (apply-generic op (raise-type (car args)) (raise-type (cadr args)))))))))
