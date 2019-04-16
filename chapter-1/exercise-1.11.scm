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
