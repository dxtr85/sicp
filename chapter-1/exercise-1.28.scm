
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
  (define (non-trivial-square number)
    (define sq (square number))
    (cond ((= number 1) 1)
	  ((= (- m 1) number) sq)
	  (else
	   (if (= 1 (remainder sq m))
	       #f
	       sq))))
  (cond ((= exp 0) 1)
	((= exp 1) (remainder base m))
	((even? exp)
	 (let ((ex (expmod base (/ exp 2) m)))
	       (if (not ex)
		   #f
		   (let ((ntsq (non-trivial-square ex)))
		     (if (not ntsq)
			 #f
			 (remainder ntsq m))))))
	(else (let ((ex (expmod base (- exp 1) m)))
		(if (not ex)
		#f
		(remainder (* base ex) m))))))

(define (miller-rabin-test n)
  (define (try-it a)
    (define ex (expmod a (- n 1) n))
    (if (not ex)
	#f
	(= ex 1)))
(try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
(cond ((= times 0) true)
((miller-rabin-test n) (fast-prime? n (- times 1)))
(else false)))

(define (timed-prime-test n)
(newline)
(display n)
(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
(if (fast-prime? n 100)
    (report-prime (- (runtime) start-time))
    #f))

(define (report-prime elapsed-time)
(display " *** ")
(display elapsed-time)
(newline))

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
