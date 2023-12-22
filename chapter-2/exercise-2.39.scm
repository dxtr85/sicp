;Exercise 2.39.   Complete the following definitions of reverse (exercise
;2.18) in terms of fold-right and fold-left from exercise 2.38:

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; (define (fold-right op initial sequence)
;;   (accumulate op initial sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;What are the values of

(fold-right / 1 (list 1 2 3))
; (/ 1 (/ 2 (/ 3 1))) -> (/ 1 (/ 2 3)) -> (/ 1 2/3) -> 3/2

(fold-left / 1 (list 1 2 3))
; (/ (/ (/ 1 1) 2) 3) -> (/ (/ 1 2) 3) -> (/ 1/2 3) -> 1/6

(fold-right list nil (list 1 2 3))
; (list 1 (list 2 (list 3 (list nil)))) -> '(1 (2 (3 ())))

(fold-left list nil (list 1 2 3))
; (list (list (list nil 3) 2) 1) -> (((() 3) 2) 1)

(define (reverse sequence)
  (fold-right (lambda (elem acc) (append acc (list elem)))
              '()
              sequence))

(define (reverse sequence)
  (fold-left (lambda (acc elem) (cons elem acc))
             '()
             sequence))
