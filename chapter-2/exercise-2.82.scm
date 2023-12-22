;; Exercise 2.82.  Show how to generalize apply-generic to handle coercion
;; in the general case of multiple arguments. One strategy is to attempt to
;; coerce all the arguments to the type of the first argument, then to the
;; type of the second argument, and so on. Give an example of a situation
;; where this strategy (and likewise the two-argument version given above)
;; is not sufficiently general. (Hint: Consider the case where there are some
;; suitable mixed-type operations present in the table that will not be tried.)

;; to be included in the complex package
(define (add-complex-to-schemenum z x)
  (make-from-real-imag (+ (real-part z) x)
                       (imag-part z)))
(put 'add '(complex scheme-number)
     (lambda (z x) (tag (add-complex-to-schemenum z x))))

;; coercion
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (apply-generic op . args)
  (define (apply-generic-coerced coerced-args-lists)
    (cond
     ((null? coerced-args-lists)
      (error "Failed to find matching procedure for ceorced args."))
     ((eq? (length (car coerced-args-lists)) (length args))
      (apply-generic op (car coerced-args-lists)))
     (else
      (apply-generic-coerced (cdr coerced-args-lists)))))

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((coerced-args-lists
                 (map (lambda (type-tag) (coerce-args type-tag args type-tags))
                      type-tags)))
            (apply-generic-coerced coerced-args-lists))))))

(define (coerce-args target-type args arg-types)
  (cond ((null? args) '())
        ((eq? target-type (car arg-types))
         (cons (car args) (coerce-args target-type (cdr args) (cdr arg-types))))
        (else
         (let ((coerc (get-coercion target-type (car arg-types))))
           (if coerc
               (cons (coerc (car args))
                     (coerce-args target-type (cdr args) (cdr arg-types)))
               '())))))

;; Downside of this approach is that it only works for procedures with
;; homogeneous argument types, that is all arguments are of the same type.
;; In case a procedure requires at least one argument to be of different
;; type than other arguments, this coercion will not help.
