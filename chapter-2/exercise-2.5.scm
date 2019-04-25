;Exercise 2.5.  Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations if we represent the pair a and b as the integer that is the product 2a 3b. Give the corresponding definitions
;of the procedures cons, car, and cdr. 
(define (cons x y)
  (define (m base x)
    (if (> x 0)
	(* base (m base (- x 1)))
	1))
  (* (m 2 x) (m 3 y)))

(define (car z)
  (define (m curr num)
    (if (= 0 (mod num 2))
	(m (+ 1 curr) (/ num 2))
	curr))
    (m 0 z))

(define (cdr z)
  (define (m curr num)
    (if (= 0 (mod num 3))
	(m (+ 1 curr) (/ num 3))
	curr))
    (m 0 z))
