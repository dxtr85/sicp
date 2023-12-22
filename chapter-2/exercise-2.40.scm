(define nil '())


(define (prime? n)
  (define (find-divisor n test-divisor)
    (define (next test-divisor)
      (if (= test-divisor 2)
          3
          (+ test-divisor 2)))
  
    (define (divides? a b)
      (= (remainder b a) 0))
  
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
(= n (find-divisor n 2)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(accumulate append
            nil
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)                    ; empty set?
      (list nil)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;Exercise 2.40.  Define a procedure unique-pairs that, given an integer n,
;generates the sequence of pairs (i,j) with 1< j< i< n. Use unique-pairs to
;simplify the definition of prime-sum-pairs given above.
(define (unique-pairs n)
  (flatmap
   (lambda (i)
          (map (lambda (j) (list i j))
               (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
