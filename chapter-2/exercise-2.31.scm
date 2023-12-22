;Exercise 2.31.  Abstract your answer to exercise 2.30 to produce a
;procedure tree-map with the property that square-tree could be defined as

(define (square x) (* x x))
(define (tree-map function tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map function sub-tree)
             (function sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))
