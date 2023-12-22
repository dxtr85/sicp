; The modified version of expmod computes huge intermediate results. 
; 
; Scheme is able to handle arbitrary-precision arithmetic, but arithmetic with
; arbitrarily long numbers is computationally expensive. This means that we
; get the same (correct) results, but it takes considerably longer. 
; 
; For example: 
; 
;  (define (square m)  
;    (display "square ")(display m)(newline) 
;    (* m m)) 
;   
;  => (expmod 5 101 101) 
;  square 5 
;  square 24 
;  square 71 
;  square 92 
;  square 1 
;  square 1 
;  5 
;  => (remainder (fast-expt 5 101) 101) 
;  square 5 
;  square 25 
;  square 625 
;  square 390625 
;  square 152587890625 
;  square 23283064365386962890625 
;  5 
; 
; The remainder operation inside the original expmod implementation, keeps the
; numbers being squared less than the number tested for primality m.
; fast-expt however squares huge numbers of a^m size. 
