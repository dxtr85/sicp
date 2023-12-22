; To solve this exercise, we need to change fermat-test, making it test every
; integer number from 1 to n-1, not just a few random ones. 
; 
; Then, we perform the test on Carmichael numbers and print whether the
; primality test has been fooled or not. 
  
 ;; ex 1.27 
  
 (define (square x) (* x x)) 
  
 (define (expmod base exp m) 
   (cond ((= exp 0) 1) 
         ((even? exp) 
          (remainder (square (expmod base (/ exp 2) m)) 
                     m)) 
         (else 
          (remainder (* base (expmod base (- exp 1) m)) 
                     m))))         
  
 (define (full-fermat-prime? n) 
   (define (iter a n) 
     (if (= a n) true 
         (if (= (expmod a n n) a) (iter (+ a 1) n) false))) 
   (iter 1 n)) 
  
 (define (test-fermat-prime n expected) 
   (define (report-result n result expected) 
     (newline) 
     (display n) 
     (display ": ") 
     (display result) 
     (display ": ") 
     (display (if (eq? result expected) "OK" "FOOLED"))) 
   (report-result n (full-fermat-prime? n) expected)) 
  
 (test-fermat-prime 2 true) 
 (test-fermat-prime 13 true) 
 (test-fermat-prime 14 false) 
  
 (test-fermat-prime 561 false)  ; Carmichael number 
 (test-fermat-prime 1105 false) ; Carmichael number 
 (test-fermat-prime 1729 false) ; Carmichael number 
 (test-fermat-prime 2465 false) ; Carmichael number 
 (test-fermat-prime 2821 false) ; Carmichael number 
 (test-fermat-prime 6601 false) ; Carmichael number 
  
; Both of the above implementations fail to take into account the special case
; of 1 which by definition is not a prime number. Testing to see if n is equal
; to 1 solves this minor issue. Below is my solution to the exercise (because
; of the way the algorithm was implemented it is also necessary to test
; whether n is equal to 0 in order to avoid division by zero): 
  
 (define (car-test n) 
   (define (car-test-iter n a) 
     (cond ((or (= n 1) (= n 0)) false) 
           ((= a n) true) 
           ((not (= (expmod a n n) (remainder a n))) false) 
           (else (car-test-iter n (+ a 1))))) 
   (car-test-iter n 1)) 
  
 (define (expmod base exp m) 
   (cond ((= exp 0) 1) 
         ((even? exp) 
          (remainder 
           (square (expmod base (/ exp 2) m)) 
           m)) 
         (else 
          (remainder 
           (* base (expmod base (- exp 1) m)) 
           m)))) 
  
 (define (square x) (* x x)) 
  
 ;; testing on Carmichael numbers 
 (car-test 561) 
 (car-test 1105) 
 (car-test 1729) 
 (car-test 2465) 
 (car-test 2821) 
 (car-test 6601) 
  
 ;; testing on 1 and random numbers <100,000,000 (courtesy of random.org) 
 (car-test 1) 
 (car-test 85230658) 
 (car-test 13828192) 
 (car-test 69911818) 
 (car-test 49141805) 
 (car-test 29170450) 
