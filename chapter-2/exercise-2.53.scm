(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;For example, the value of
(memq 'apple '(pear banana prune))
;is false, whereas the value of
(memq 'apple '(x (apple sauce) y apple pear))
;is (apple pear).

;Exercise 2.53.  What would the interpreter print in response to evaluating
;each of the following expressions?

(list 'a 'b 'c)
;(a b c)
(list (list 'george))
;((george))
(cdr '((x1 x2) (y1 y2)))
;((y1 y2))
(cadr '((x1 x2) (y1 y2)))
;(y1 y2)
(pair? (car '(a short list)))
;#f
(memq 'red '((red shoes) (blue socks)))
;#f
(memq 'red '(red shoes blue socks))
;(red shoes blue socks)
