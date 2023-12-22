;; Exercise 2.83.  Suppose you are designing a generic arithmetic system
;; for dealing with the tower of types shown in figure 2.25: integer, rational,
;; real, complex. For each type (except complex), design a procedure that
;; raises objects of that type one level in the tower. Show how to install a
;; generic raise operation that will work for each type (except complex). 

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

(define (raise-type x)
  (define (supertype val)
    (cond ((eq? (type-tag x) 'integer) 'rational)
          ((eq? (type-tag x) 'rational) 'real)
          ((eq? (type-tag x) 'real) 'complex)
          (else '())))
  (if (supertype x)
      ((get-coercion
        (type-tag x)
        (supertype x)) x)
      (error "Unable to raise type" (type-tag x))))
