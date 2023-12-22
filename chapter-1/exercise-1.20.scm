; The process generated using the normal-order evaluation is the following.
; It performs 18 remainder operations: 14 when evaluating the condition and 4
; in the final reduction phase. 
; 
;   
;  (gcd 206 40) 
;   
;  (if (= 40 0) ...) 
;   
;  (gcd 40 (remainder 206 40)) 
;   
;  (if (= (remainder 206 40) 0) ...) 
;   
;  (if (= 6 0) ...) 
;   
;  (gcd (remainder 206 40) (remainder 40 (remainder 206 40))) 
;   
;  (if (= (remainder 40 (remainder 206 40)) 0) ...) 
;   
;  (if (= 4 0) ...) 
;   
;  (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 
; ;  (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) ...) 
  
;  (if (= 2 0) ...) 
  
;  (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) 
  
;  (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) ...) 
  
;  (if (= 0 0) ...) 
;  (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 
;   
; The number 'R' of remainder operations executed by the normal-order
; evaluation process is given by the formula 
; 
;          R = SUM(i from 1 to n, fib(i) + fib(i - 1)) - 1 
; 
; where n is the number of gcd invocations required to compute the (gcd a b).
; The first invocation does not need to compute remainder thus the -1 
; 
; R is given by the following function: 

 (define (count-remainders n) 
     (define (loop n sum) 
         (if (= 0 n) (- sum 1) 
             (loop (- n 1) (+ sum (fib n) (fib (- n 1)))))) 
     (loop n 0)) 

; In this particular case that is: 
; 
;  (count-remainders 5) 
;  => 18 
; 
; The process generated using the applicative-order evaluation is the
; following. It performs 4 remainder operations. 
; 
;   
;  (gcd 206 40) 
;   
;  (gcd 40 (remainder 206 40)) 
;   
;  (gcd 40 6) 
;   
;  (gcd 6 (remainder 40 6)) 
;   
;  (gcd 6 4) 
;   
;  (gcd 4 (remainder 6 4)) 
;   
;  (gcd 4 2) 
;   
;  (gcd 2 (remainder 4 2)) 
;   
;  (gcd 2 0) 
;   
;  2 
;   
; -----------------------------------------------------------------------------
; 
; It seems that the formula 
; 
;          R = SUM(i from 1 to n, fib(i) + fib(i - 1)) - 1 
; 
; is incorrect. One can easy check this by letting n=2. The correct count
; should be 1 while the formula gives 2. 
; 
; Let b_i be the value of b at the n'th invocation of gcd(a,b). Let b(i) be the
; count of remainder procedure needed to calculate b_i. Similarly, define a_i
; and a(i). It is easy to check that 
; 
; 1. a_i=b_(i-1) and thus a(i)=b(i-1). 
; 
; 2. b(i+1) = a(i) + b(i) + 1 = b(i-1) + b(i) + 1 because b_(i+1) = a_i mod b_i
; 
; Based on my own derivation, R should be 
; 
; b(1)+b(2)+...+b(n)+b(n-1) with b(1)=0, b(2)=1 and b(n)=b(n-1)+b(n-2)+1,
; which is not equivalent with the above R formula 
; 
; -----------------------------------------------------------------------------
; 
; The correct formula is 
; 
; R(n) = SUM(i from 1 to n - 1, fib(i)) + fib(n - 2) - 1
; 
; Where n is the number of applications. 
; 
; R(2) = 1 
; 
; R(3) = 4 
; 
; R(5) = 17 
; 
; -----------------------------------------------------------------------------
; 
; The above formulas are wrong. The truly correct one should be: 
; 
; R(n) = SUM(i from 1 to n, fib(i+1)- 1) + fib(n) -1
; 
; where n is the number of times gcd(a, b) is called. 
; 
; We have, 
; 
; R(1) = 0 
; 
; R(2) = 1 
; 
; R(3) = 4 
; 
; R(4) = 9 
; 
; R(5) = 18 
; 
; Below is the derivation: 
; 
; Denote n as the number of times gcd() is called, R(n) the number of times
; remainder() is invoked. 
; 
; For (gcd a(k) b(k)), let num_a(k) be the number of remainder() in a(k),
; num_b(k) be the number of remainder() in b(k), where k=1,2,...n. 
; 
; Then we have the updating process: 
; 
; a(k+1) = b(k) 
; 
; b(k+1) = remainder(a(k), b(k)) 
; 
; Thus, 
; 
; num_a(1) = 0 
; 
; num_b(1) = 0 
; 
; num_a(k+1) = num_b(k) 
; 
; R(k+1) = num_a(k) + num_b(k) + 1 = num_b(k+1) 
; 
; By substituting num_a for num_b, we get the following for {num_b(k)},
; k=1,2,...n, 
; 
; num_b(1) = 0 
; 
; num_b(2) = 1 
; 
; num_b(k+1) = num_b(k) + num_b(k-1) + 1 
; 
; Obviously we could get 
; 
; num_b(k) = fib(k+1) - 1 
; 
; Since 
; 
; R(n) = SUM(i from 1 to n, num_b(i)) + num_a(n)
; 
; which is quite obvious following the normal-order-evaluation rule for if
; statement, we could get 
; 
; R(n) = SUM(i from 1 to n, fib(i+1)- 1) + fib(n) -1
; 
; :) Lily.X 
; 
; The correct formula above can also be written as 
; 
; R(n) = 2*fib(n+2)-n-3
