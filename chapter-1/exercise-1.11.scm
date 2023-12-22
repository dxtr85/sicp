(define (f n)
  (if (< n 3)
  n
  (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f2 n)
  (define (f-iterative f-1 f-2 f-3 counter n)
  (define f-current (+ f-1 (* 2 f-2) (* 3 f-3)))
  (if (= counter n)
      f-current
      (f-iterative f-current f-1 f-2 (+ counter 1) n)))
  (if (< n 3)
      n
      (f-iterative 2 1 0 3 n)))

; The recursive implementation of the given function is straighforward: just
; translate the definition into Scheme code. 

  
 ;; ex 1.11. Recursive implementation. 
  
 (define (f n) 
    (cond ((< n 3) n) 
         (else (+ (f (- n 1)) 
                  (* 2 (f (- n 2))) 
                  (* 3 (f (- n 3))))))) 

; Iterative procedure for Ex 1.11 

 (define (f n) 
   (define (f-i a b c count) 
     (cond ((< n 3) n) 
           ((<= count 0) a) 
           (else (f-i (+ a (* 2 b) (* 3 c)) a b (- count 1))))) 
   (f-i 2 1 0 (- n 2))) 

; This iterative version will not handle non-integer values while the
; recursive version will, but as the conditions were given as n >= 3 and not
; n >= 3.0 it is sufficient. Note the <= count 0 condition. If the condition
; used = non-integer values would cause an endless loop as count would
; never equal exactly 0. As it is decimal values evaluate to the next whole
; value. ie 3.2 -> 4 
; 
; The basic transform is given as:
; a <- (a + 2b + 3c)
; b <- a
; c <- b
; 
; with a starting condition as defined by the boundary state f(3):
; a = 2
; b = 1
; c = 0
; 
; and iterating for another n-2 times. 
; 
; Welcome to DrRacket, version 7.6 [3m].
; Language: R5RS; memory limit: 128 MB.
; > (f -1)
; -1
; > (f 0)
; 0
; > (f 5)
; 25
; > (f 4.7)
; 25
; > (f 1000)
; 1200411335581569104197621183222182410228690281055710781687044573790661709343985308756380381850406620666042607564631605876156610535933789714780132607755663854744223225249491730428647795602251203632973677695221003056803565827035107926395650932180708300409716979009255557336360673626403040863408122386349183735643342985009827495351241264386090544972951146415009560371824341466875

; The iterative implementation requires a bit of thought. Note that the
; solution presented here is somewhat wasteful, since it computes f(n+1)
; and f(n+2). 

  
 ;; ex 1.11. Iterative implementation 
  
 (define (f n) 
   (define (iter a b c count) 
     (if (= count 0) 
       a 
       (iter b c (+ c (* 2 b) (* 3 a)) (- count 1)))) 
   (iter 0 1 2 n)) 

; The above version does not terminate for n < 0. The following
; implementation does: 

 (define (f n) (fi n 0 1 2)) 
  
 (define (fi i a b c) 
   (cond ((< i 0) i) 
         ((= i 0) a) 
         (else (fi (- i 1) b c (+ c (* 2 b) (* 3 a)))))) 
  
; Another implementation, which does not calculate f(n+1) or f(n+2). 
  
 (define (foo n) 
   (define (foo-iter a b c n1) 
     ;; a = f(n1 - 1), b = f(n1 - 2), c = f(n1 - 3). 
     ;; return a + 2b + 3c 
     (if (< n1 3) 
         a 
         (foo-iter (+ a (* 2 b) (* 3 c)) a b (- n1 1)))) 
   (if (< n 3) 
       n 
       (foo-iter 2 1 0 n))) 
  
; Output 
; 
; 
; > (foo 0)
; 0
; > (foo 1)
; 1
; > (foo 2)
; 2
; > (foo 3)
; 4
; > (foo 4)
; 11
; > (foo 5)
; 25
; > (foo 6)
; 59
; > (foo 7)
; 142

; Another iterative version, similar to above, but counting up from 3 to n
; (instead of counting down). 
  
 (define (f n) 
   ;; Track previous three values. 
   ;; fi-1 is f(i-1) 
   ;; fi-2 is f(i-2) 
   ;; fi-3 is f(i-3) 
   (define (f-iter fi-1 fi-2 fi-3 i) 
     ;; Calculate value at current index i. 
     (define fi (+ fi-1 
                   (* 2 fi-2) 
                   (* 3 fi-3))) 
     (if (= i n) 
         fi 
         (f-iter fi fi-1 fi-2 (+ i 1)))) 
  
   (if (< n 3) 
       n 
       (f-iter 2 1 0 3))) ;; start index i=3, count up until reach n. 

; Here is another iterative version that the original poster called "a little bit different". 

; Another commenter pointed out that it gives wrong answers for n < 3, but
; also asked, could someone explain how this works for larger inputs? 

; I am not the original author, but after staring at this for a while, I think I can explain it and correct it for n < 3. 

; Original version: 

  
 (define (f n) 
   (define (f-iter n a b c) 
   ;; this makes f(n) = a f(2) + b f(1) + c f(0) for integer n. 
     (if (< n 4) 
     ;; N < 4. cause n-1 < 3 
       (+ (* a (- n 1) ) 
         (* b (- n 2)) 
         (* c (- n 3))) 
       (f-iter (- n 1) (+ b a) (+ c (* 2 a)) (* 3 a)))) 
   (f-iter n 1 2 3)) 

; Explanation: 

; The other iterative versions start from f(0), f(1), and f(2), and calculate the next f(i) value based on the previous values. 

; In contrast, this version tracks just the coefficients of f(n-1), f(n-2), f(n-3).
; It starts with coefficients (a, b, c) = (1, 2, 3), as given by the definition. It
; then expands f(n) in terms of f(n-1), f(n-2), f(n-3). And so on, until you get
; the equivalent value using coefficients for f(2), f(1), f(0). 

; In f-iter, n is the counter that starts at the given n and gets decremented
; until n = 3. The if statement causes execution to stop at n = 3, and return
; this expression: 
; 
;  (+ (* a (- n 1)) (* b (- n 2)) (* c (- n 3))) 
; 
; Since n is always 3 at this point (ignore the failure cases of n < 3 for now),
; that expression is basically: 
; 
;  (+ (* a (- 3 1)) (* b (- 3 2)) (* c (- 3 3))) 
; 
; which is: 
; 
;  (+ (* a 2) (* b 1) (* c 0)) 
; 
; And that satisfies the objective of giving the answer in terms of
; coefficients of f(2), f(1), f(0): 
; 
;  (+ (* a f(2)) (* b f(1)) (* c f(0))) 
; 
; So that is why inputs 3 or larger works, while n < 3 fails. For n < 3, the
; function should just return n, rather than calculate coefficients. 
; 
; Corrected version: 

  
 (define (f n) 
   ;; Given starting coefficients (a, b, c) = (1, 2, 3), 
   ;; where f(n) = 1 f(n-1) + 2 f(n-2) + 3 f(n-3), 
   ;; f-iter calculates new (a, b, c) such that 
   ;; f(n) = a f(2) + b f(1) + c f(0), 
   ;; where integer n > 3. 
   (define (f-iter n a b c) 
     (if (= n 3) 
         (+ (* a 2)  ;; f(2) = 2 
            (* b 1)  ;; f(1) = 1 ;; (* b 1) = b, and 
            (* c 0)) ;; f(0) = 0 ;; (* c 0) = 0, which can be omitted, 
                                 ;; but shown here for completeness. 
         (f-iter (- n 1)       ;; decrement counter 
                 (+ b a)       ;; new-a = a + b 
                 (+ c (* 2 a)) ;; new-b = 2a + c 
                 (* 3 a))))    ;; new-c = 3a 
   ;; main body 
   (if (< n 3) 
       n 
       (f-iter n 1 2 3))) 
  
; At each step of f-iter for n larger than 3, f-iter calls itself with new values
; for a, b, and c: 
; 
; new-a = a + b
; new-b = 2a + c
; new-c = 3a
; 
; To see where those calculations come from, consider this example of how
; (f 5) calculates 25. 
; 
; (f 5)
; 
; (f-iter 5 1 2 3)
; n=5, f(5) = 1 f(4)                       + 2 f(3) + 3 f(2)  ;; by definition
;           = 1 (1 f(3) + 2 f(2) + 3 f(1)) + 2 f(3) + 3 f(2)  ;; expand f(4)
;           = (1 + 2) f(3) + (2  + 3) f(2) + 3 f(1)           ;; combine terms
;              a + b          2a + c        3a                ;; observe pattern
;           = 3 f(3)       +       5  f(2) + 3 f(1)           ;; new a b c
; 
; (f-iter 4 3 5 3)
; n=4,      = 3 f(3)                       + 5 f(2) + 3 f(1)  ;; continued
;           = 3 (1 f(2) + 2 f(1) + 3 f(0)) + 5 f(2) + 3 (f1)  ;; expand f(3)
;           = (3 + 5) f(2) + (6  + 3) f(1) + 9 f(0)           ;; combine terms
;              a + b          2a + c        3a                ;; observe pattern
;           = 8 f(2)       +       9  f(1) + 9 f(0)           ;; new a b c
; 
; (f-iter 3 8 9 9)
; n=3,      = 8 f(2) + 9 f(1) + 9 f(0)
; 
; ;; n=3, so stop looping, and apply a, b, c:
; (+ (* a 2) (* b 1) (* c 0))
; (+ (* 8 2) (* 9 1) (* 9 0))
; (+     16       9       0)
; 25
; 
; Heres another iterative solution, counting up 

 (define (fn-iterate n) 
   (define (fn-iter count n f1 f2 f3) 
     (if (= count n) f3 (fn-iter (+ count 1) n f2 f3 (+ (* 3 f1) (* 2 f2) f3)))) 
   (if (<= n 3) n (fn-iter 3 n 1 2 3))) 

; another iterative solution 

 (define (f n) 
   (i n 2 1 0)) 
  
 (define (i n f1 f2 f3) 
   (cond ((< n 2) n) 
         ((< n 3) f1) 
         (else (i (- n 1) 
                  (+ f1 (* 2 f2) (* 3 f3)) f1 f2)))) 

; Another iterative version 

 (define (f-iterative n) 
   (define (sub1 x) (- x 1)) 
   (define (iter count n-1 n-2 n-3) 
     (define (f) 
       (+ n-1 (* 2 n-2) (* 3 n-3))) 
     (if (= count 0) 
       n-1 
       (iter (sub1 count) (f) n-1 n-2))) 
   (if (< n 3) 
     n 
     (iter (- n 2) 2 1 0))) 

; And these is how it calculates (f-iterative 7): 
; 
; (f-iterative 7)
; (iter (- 7 2) 2 1 0)
; (iter (sub1 5) 4 2 1)
; (iter (sub1 4) 11 4 2)
; (iter (sub1 3) 25 11 4)
; (iter (sub1 2) 59 25 11)
; (iter (sub1 1) 142 59 25)
; 142

; Another iterative version for all integers. Straightforward, but doesn't
; calculate unneeded values. 

 (define (fi n) 
     (define (f-iter a b c count) 
         (cond ((< count 0) count) 
               ((= count 0) a) 
               ((= count 1) b) 
               ((= count 2) c) 
               (else (f-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))) 
     (f-iter 0 1 2 n)) 

; A version with only the necessary 0 and 1 variables and a single storage
; variable based strictly on the function definition, no wasted iterations: 

 (define (fib n) 
     (define (fib-iter n a b) 
         (cond ((= n 0) a) 
               ((= n 1) b) 
               (else (fib-iter (- n 1) b (+ a b))))) 
     (fib-iter n 0 1)) 
