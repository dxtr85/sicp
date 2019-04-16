(define (sum term a next b)
(if (> a b)
0
(+ (term a)
(sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
(sum cube a inc b))

(define (identity x) x)
(define (sum-integers a b)
(sum identity a inc b))

(define (pi-sum a b)
(define (pi-term x)
(/ 1.0 (* x (+ x 2))))
(define (pi-next x)
(+ x 4))
(sum pi-term a pi-next b))
(* 8 (pi-sum 1 1000))

(define (integral f a b dx)
(define (add-dx x) (+ x dx))
(* (sum f (+ a (/ dx 2.0)) add-dx b)
   dx))
(integral cube 0 1 0.01)

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
