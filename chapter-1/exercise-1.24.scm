(define (smallest-divisor n)
(find-divisor n 2))

(define (find-divisor n test-divisor)
(cond ((> (square test-divisor) n) n)
((divides? test-divisor n) test-divisor)
(else (find-divisor n (next test-divisor)))))

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (divides? a b)
(= (remainder b a) 0))

(define (prime? n)
(= n (smallest-divisor n)))

(define (expmod base exp m)
(cond ((= exp 0) 1)
((even? exp)
(remainder (square (expmod base (/ exp 2) m))
m))
(else
(remainder (* base (expmod base (- exp 1) m))
m)))) 

(define (fermat-test n)
(define (try-it a)
(= (expmod a n n) a))
(try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
(cond ((= times 0) true)
((fermat-test n) (fast-prime? n (- times 1)))
(else false)))

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
	   (search-for-primes (next-odd start-from) (- quantity-to-find 1))
	   (search-for-primes (next-odd start-from) quantity-to-find))))

