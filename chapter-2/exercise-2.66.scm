; Exercise 2.66.  Implement the lookup procedure for the case where the set
; of records is structured as a binary tree, ordered by the numerical values of
; the keys. 

(define (lookup given-key records-tree)
  (cond ((null? records-tree) false)
        ((equal? given-key (key (entry records-tree)))
         (data (entry records-tree)))
        ((< given-key (key (entry records-tree)))
         (lookup given-key (left-branch records-tree)))
        (else (lookup given-key (right-branch records-tree)))))
