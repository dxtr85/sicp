; Exercise 2.90.  Suppose we want to have a polynomial system that is
; efficient for both sparse and dense polynomials. One way to do this is to
; allow both kinds of term-list representations in our system. The situation is
; analogous to the complex-number example of section 2.4, where we
; allowed both rectangular and polar representations. To do this we must
; distinguish different types of term lists and make the operations on term
; lists generic. Redesign the polynomial system to implement this
; generalization. This is a major effort, not a local change. 

(define (install-polynomial-package)
  (define (install-term-list-package)
    ;; sparse
    (define (adjoin-term-s term term-list)
      (if (=zero? (coeff term))
          term-list
          (cons term term-list)))
    (define (first-term-s term-list) (car term-list))

    (define (tag-s p) (attach-tag 'sparse p))
    (put 'adjoin-term 'sparse 
         (lambda (t t-list) (tag-s (adjoin-term-s t (contents t-list)))))
    (put 'first-term 'sparse 
         (lambda (t-list) (tag-s (first-term-s t-list))))

    ;; dense
    (define (adjoin-term-d term term-list)
      (if (= (order term) (length term-list))
          (cons (coeff term) term-list)
          (adjoin-term term (cons 0 term-list))))
    (define (first-term-d term-list)
      (if (empty-termlist? term-list)
          (error "Empty termlist -- FIRST-TERM")
          (if (=zero? (car term-list))
              (first-term (rest-terms term-list))
              (cons (- (length term-list) 1) (car term-list)))))

    (define (tag-d p) (attach-tag 'dense p))
    (put 'adjoin-term 'sparse 
         (lambda (t t-list) (tag-d (adjoin-term-d t t-list))))
    (put 'first-term 'sparse 
         (lambda (t-list) (tag-d (first-term-d t-list))))

    (put 'the-empty-termlist 'sparse
         (lambda () (tag-s '())))
    (put 'the-empty-termlist 'dense
         (lambda () (tag-d '())))

    (define (rest-terms term-list) (cdr (contents term-list)))
    (define (empty-termlist? term-list) (null? (contents term-list)))
    (define (make-term order coeff) (list order coeff))
    (define (negate-term term)
      (make-term (order term) (- 0 (coeff term))))
    (define (order term) (car term))
    (define (coeff term) (cadr term))
    'done)
  ;; Install sub-package
  (install-term-list-package)
  (define (adjoin-term term term-list)
    ((get 'adjoin-term (type-tag term-list)) term term-list))
  (define (first-term)
    (apply-generic 'first-term term-list))

  ;; internal procedures
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (sub-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (sub-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     (negate-term t2) (sub-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (sub (coeff t1) (coeff t2)))
                     (sub-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ; <procedures same-variable? and variable? from section 2.3.2>
  ;; representation of terms and term lists
  ; <procedures adjoin-term ...coeff from text below>

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))
(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (sub-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- SUB-POLY"
             (list p1 p2))))
(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? 'polynomial
       (lambda (poly)
         (let ((terms (term-list poly)))
               (if (empty-termlist? terms)
                   #t
                   (if (=zero? (coeff (first-term terms)))
                       (=zero? (make-poly (variable poly) (rest-terms terms)))
                       #f)))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)


(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define (the-empty-termlist)
  ((get 'the-empty-termlist 'dense)))
(define (the-empty-sparse-termlist)
  ((get 'the-empty-termlist 'sparse)))
