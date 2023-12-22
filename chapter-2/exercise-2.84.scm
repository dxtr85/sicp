;; Exercise 2.84.  Using the raise operation of exercise 2.83, modify the
;; apply-generic procedure so that it coerces its arguments to have the same
;; type by the method of successive raising, as discussed in this section.
;; You will need to devise a way to test which of two types is higher in the
;; tower. Do this in a manner that is ``compatible'' with the rest of the
;; system and will not lead to problems in adding new levels to the tower. 

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
          (apply proc (map contents args))
          (if (memq (car type-tags) (supertypes (cadr args)))
              (apply-generic op (raise-type (car args)) (cadr args))
              (if (not (eq? (car type-tags) (cadr type-tags)))
                  (apply-generic op (car args) (raise-type (cadr args)))
                  (apply-generic op (raise-type (car args)) (raise-type (cadr args)))))))))
