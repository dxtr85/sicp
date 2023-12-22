; Exercise 2.86.  Suppose we want to handle complex numbers whose real parts,
; imaginary parts, magnitudes, and angles can be either ordinary numbers,
; rational numbers, or other numbers we might wish to add to the system.
; Describe and implement the changes to the system needed to accommodate this.
; You will have to define operations such as sine and cosine that are generic
; over ordinary numbers and rational numbers.

; We need to define generic operations like: atan, sin, cos for rational
;  numbers, or other numbers we might wish to add to the system

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (sin-rat x )
    (make-real (sin (/ (numer x) (denom y)))))
  (define (cos-rat x )
    (make-real (cos (/ (numer x) (denom y)))))
  (define (atan-rat x )
    (make-real (atan (/ (numer x) (denom y)))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'zero? '(rational)
       (lambda (x) (eq? 0 (numer x))))
  (put 'equ? '(rational rational)
       (lambda (x y) (eq? 0 (numer (sub-rat x y)))))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'sin 'rational
       (lambda (r) (tag (sin-rat r))))
  (put 'cos 'rational
       (lambda (r) (tag (cos-rat r))))
  (put 'atan 'rational
       (lambda (r) (tag (atan-rat r))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

;;; add into global 
 (define (sin x) (apply-generic 'sin x)) 
 (define (cos x) (apply-generic 'cos x)) 
 (define (atan x) (apply-generic 'atan x)) 
