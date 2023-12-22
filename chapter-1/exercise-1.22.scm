(define (smallest-divisor n)
(find-divisor n 2))

(define (find-divisor n test-divisor)
(cond ((> (square test-divisor) n) n)
((divides? test-divisor n) test-divisor)
(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
(= (remainder b a) 0))

(define (prime? n)
(= n (smallest-divisor n)))

(define (timed-prime-test n)
(newline)
(display n)
(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
(if (prime? n)
    (report-prime (- (runtime) start-time))
    #F))

(define (report-prime elapsed-time)
(display " *** ")
(display elapsed-time)
(newline)
#T)

(define (next-odd number)
  (if (= (remainder (+ number 1) 2) 0)
      (+ number 2)
      (+ number 1)))
  

(define (search-for-primes start-from quantity-to-find)
  (if (= 0 quantity-to-find)
      (display "Done.")
      (if (timed-prime-test start-from)
	   (search-for-primes (next-odd start-from) (- quantity-to-find 1)
	   (search-for-primes (next-odd start-from) quantity-to-find))))

