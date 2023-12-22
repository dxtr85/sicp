;Exercise 2.22.  Louis Reasoner tries to rewrite the first square-list
;procedure of exercise 2.21 so that it evolves an iterative process:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items '()))

;Unfortunately, defining square-list this way produces the answer list
;in the reverse order of the one desired. Why?

;Louis then tries to fix his bug by interchanging the arguments to cons:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

;This doesn't work either. Explain. 

;First definition adds new squared numbers to beginning of answer.
;Second one creates a new list with previous lists and with current result.

;Correct solution is to reverse answer after items traversal is over:
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        (reverse answer)
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items '()))
