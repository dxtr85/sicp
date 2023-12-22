; Solution using max precision and fix for cube-root of -2 problem. There are
; still the cube-root of 0 (and cube-root of 100) convergence problem, where
; the answer approaches 0 (or 100) but never reaches it. (Based on Ex1.7
; solution) 

 (define (square guess) 
   (* guess guess)) 
  
 ;cube root improve formula used as is 
 (define (improve guess x) 
   (/ (+ (/ x (square guess)) (* 2 guess)) 3)) 
    
 ;iterates until guess and next guess are equal, 
 ;automatically produces answer to limit of system precision 
 (define (good-enough? guess x) 
   (= (improve guess x) guess)) 
  
 (define (3rt-iter guess x) 
   (if (good-enough? guess x) 
       guess 
       (3rt-iter (improve guess x) x))) 
  
 ;<<<expression entry point>>> 
 ;change initial guess to 1.1 to prevent an anomalous result for 
 ;cube root of -2 
 (define (3root x) 
   (3rt-iter 1.1 x)) 
; Welcome to DrRacket, version 7.5 [3m].
; Language: R5RS; memory limit: 128 MB.
; > (3root 5)
; 1.709975946676697
; > (3root -2)
; -1.2599210498948732
; > (3root 27)
; 3.0
; > (3root 0)
; 4.9406564584125e-324
; > (3root 100000000000000.0001)
; 46415.88833612779

; The solution presented here is based on the solution for sicp-ex-1.7 and,
; similarly, uses the alternative strategy for the good-enough? predicate. 
  
 ;; ex 1.8. Based on the solution of ex 1.7. 
  
 (define (square x) (* x x)) 
  
 (define (cube-root-iter guess prev-guess x) 
   (if (good-enough? guess prev-guess) 
       guess 
       (cube-root-iter (improve guess x) guess x))) 
  
 (define (improve guess x) 
   (average3 (/ x (square guess)) guess guess)) 
  
 (define (average3 x y z) 
   (/ (+ x y z) 3)) 
  
 ;; Stop when the difference is less than 1/1000th of the guess 
 (define (good-enough? guess prev-guess) 
   (< (abs (- guess prev-guess)) (abs (* guess 0.001)))) 
  
 (define (cube-root x) 
   (cube-root-iter 1.0 0.0 x)) 
  
 ;; Testing 
 (cube-root 1) 
 (cube-root -8) 
 (cube-root 27) 
 (cube-root -1000) 
 (cube-root 1e-30) 
 (cube-root 1e60) 
 ;; this fails for -2 due to zero division :( 
  
 ;; Fix: take absolute cuberoot and return with sign 
  
 ;;(define (cube-root x) 
 ;;  ((if (< x 0) - +)(cube-root-iter (improve 1.0 (abs x)) 1 (abs x)))) 
  
 (define (cube x) 
   (* x x x))

 (define (improve guess x) 
   (/ (+ (/ x (square guess)) (* 2 guess)) 3))

 (define (good-enough? guess x) 
   (< (abs (- (cube guess) x)) 0.001))

 (define (cube-root-iter guess x) 
   (if (good-enough? guess x) 
       guess 
       (cube-root-iter (improve guess x) 
                       x)))

 (define (cube-root x) 
   (cube-root-iter 1.0 x)) 
  
 (define (good-enough? guess x) 
   (< (relative-error guess (improve guess x)) error-threshold)) 
  
 (define (relative-error estimate reference) 
   (/ (abs (- estimate reference)) reference)) 
  
 (define (improve guess x) 
   (average3 (/ x (square guess)) guess guess)) 
  
 (define (average3 x y z) 
   (/ (+ x y z) 3)) 
  
 (define error-threshold 0.01) 

; This solution makes use of the fact that (in LISP) procedures are also data.

 (define (square x) (* x x)) 
 (define (cube x) (* x x x)) 
  
 (define (good-enough? guess x improve) 
   (< (abs (- (improve guess x) guess)) 
      (abs (* guess 0.001)))) 
  
 (define (root-iter guess x improve) 
   (if (good-enough? guess x improve) 
       guess 
       (root-iter (improve guess x) x improve))) 
  
 (define (sqrt-improve guess x) 
   (/ (+ guess (/ x guess)) 2)) 
  
 (define (cbrt-improve guess x) 
   (/ (+ (/ x (square guess)) 
         (* 2 guess)) 
      3)) 
  
 (define (sqrt x) 
   (root-iter 1.0 x sqrt-improve)) 
  
 (define (cbrt x) 
   (root-iter 1.0 x cbrt-improve)) 

; Use the improved good-enough?: 

 (define (cube-roots-iter guess prev-guess input) 
   (if (good-enough? guess prev-guess) 
       guess 
       (cube-roots-iter (improve guess input) guess input))) 
  
 (define (good-enough? guess prev-guess input) 
   (> 0.001 (/ (abs (- guess prev-guess)) 
               input))) ;; this should be (abs input) to handle negative inputs. Example: (cube-roots -1) should be -1. Before change, output was 0.33. After fix, output is corrected to -1.000000001794607. 
  
 (define (improve guess input) 
   (/ (+ (/ input (square guess)) 
      (* 2 guess)) 
    3)) 
  
 (define (square x) 
   (* x x)) 
  
 ;;to make sure the first input of guess and prev-guess does not pass the predicate accidentally, use improve here once: 
 ;;to make sure float number is implemented, use 1.0 instead of 1: 
 (define (cube-roots x) 
   (cube-roots-iter (improve 1.0 x) 1 x)) 

; Chan : I just added one procedure. (But I just made this procedure with low
; precision. I think you can fix this.) Give me a feedback please. 

 (define (cube-root-iter guess x) 
              (if (good-enough? guess x) 
                   guess 
                   (cube-root-iter (improve guess x) x))) 
  
 (define (improve guess x) 
               (average (/ x (square guess)) (* 2 guess))) 
  
 (define (average x y) 
              (/ (+ x y) 3)) 
                  
 (define (square x) (* x x)) 
  
 (define (good-enough? guess x) 
              (< (abs (- (cube guess) x)) (* guess 0.001))) 
  
 (define (cube x) (* x x x)) 
  
 (define (cube-root x)  
              (if (< x 0)  
                   (* -1 (cube-root-iter 1.0 (abs x)))  
                   (cube-root-iter 1.0 x))) 
  
 (cube-root 27) 
;  3.0000005410641766 
  
 (cube-root -27) 
;  -3.0000005410641766 
