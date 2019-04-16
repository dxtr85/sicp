(define (sum term a next b)
(define (iter a result)
(if (> a b)
result
(iter (next a) (+ result (term a)))))
(iter a 0))

(define (inc n) (+ n 1))

(define (simpsons-integral f a b n)
(let ((h (/ (- b a) n)))
(define (yk k)
  (f (+ a (* k h))))
(define (term k)
  (* (cond
      ((or (= 0 k) (= n k))
	   1)
      ((= 1 (remainder k 2))
       4)
      (else
       2))
  (yk k)))
(* (/ h 3.) (sum term 0 inc n))))
