 ;; ex 1.2 
  
 (/ (+ 5 
       4 
       (- 2 (- 3 (+ 6 (/ 4 5))))) 
    (* 3 
       (- 6 2) 
       (- 2 7))) 
  
 ;; Result is -0.24666666666666667, or -37/150 
  
  
 ;; ex 1.2 bis 
  
 (/ (+ 4 5 6 (/ 4 5) (- 2 3)) 
    (* 3 (- 6 2) (- 2 7))) 
  
 ;; The double substraction 2-(3-(6+4/5)) is simplified to 
 ;; 2-(3-6-4/5) which is then simplified to 
 ;; 2-3+6+4/5 which is better written as 
 ;; 2+6+4/5-3 
  
 ;; Now it is -37/150 too (4/5 instead of 4/3) 
