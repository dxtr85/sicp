;Exercise 2.27.  Modify your reverse procedure of exercise 2.18 to produce
;a deep-reverse procedure that takes a list as argument and returns as its
;value the list with its elements reversed and with all sublists deep-reversed
;as well. For example,

(define x (list (list 1 2) (list 3 4)))

x
;((1 2) (3 4))

(reverse x)
;((3 4) (1 2))

(deep-reverse x)
;((4 3) (2 1))
;

(define (reverse lst)
  (define (revrse rev lst )
  (if (= 0 (length lst))
      rev
      (revrse (cons (car lst) rev) (cdr lst))))
  (revrse (list) lst))

(define (deep-reverse lst)
  (define (revrse rev lst )
    (cond ((= 0 (length lst))
           rev)
          ((not (pair? (car lst)))
           (revrse (cons (car lst) rev) (cdr lst)))
          (else (revrse
                 (cons (revrse (list) (car lst)) rev)
                 (cdr lst)))))
  (revrse (list) lst))
