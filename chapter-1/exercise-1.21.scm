(define (smallest-divisor n)
(find-divisor n 2))

(define (square n)
(* n n))

(define (find-divisor n test-divisor)
(cond ((> (square test-divisor) n) n)
((divides? test-divisor n) test-divisor)
(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
(= (remainder b a) 0))

(display "NWD 199: ")
(display (smallest-divisor 199))
(newline)
(display "NWD 1999: ")
(display (smallest-divisor 1999))
(newline)
(display "NWD 19999: ")
(display (smallest-divisor 19999))
(newline)
