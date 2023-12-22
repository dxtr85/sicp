;Exercise 2.28.  Write a procedure fringe that takes as argument a tree
;(represented as a list) and returns a list whose elements are all the leaves
;of the tree arranged in left-to-right order. For example,

(define x (list (list 1 2) (list 3 4)))

(fringe x)
;(1 2 3 4)

(fringe (list x x))
;(1 2 3 4 1 2 3 4)

(define (fringe x)
  (define (inter-fringe y acc)
    (cond ((null? y) acc)  
          ((not (pair? y)) (cons y acc))
          (else (inter-fringe (cdr y) (inter-fringe (car y) acc)))))
  (reverse (inter-fringe x (list))))
