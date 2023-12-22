; Exercise 3.82: Redo Exercise 3.5 on Monte Carlo integra-
; tion in terms of streams. The stream version of estimate-
; integral will not have an argument telling how many tri-
; als to perform. Instead, it will produce a stream of estimates
; based on successively more trials.

; Exercise 3.5: Monte Carlo integration is a method of esti-
; mating deﬁnite integrals by means of Monte Carlo simula-
; tion. Consider computing the area of a region of space de-
; scribed by a predicate P(x, y) that is true for points (x, y)
; in the region and false for points not in the region. For
; example, the region contained within a circle of radius 3
; centered at (5, 7) is described by the predicate that tests
; whether (x − 5)^2 + (y − 7)^2 <= 3^2. To estimate the area of the
; region described by such a predicate, begin by choosing a
; rectangle that contains the region. For example, a rectangle
; with diagonally opposite corners at (2, 4) and (8, 10) con-
; tains the circle above. The desired integral is the area of
; that portion of the rectangle that lies in the region. We can
; estimate the integral by picking, at random, points (x , y)
; that lie in the rectangle, and testing P (x , y) for each point
; to determine whether the point lies in the region. If we try
; this with many points, then the fraction of points that fall
; in the region should give an estimate of the proportion of
; the rectangle that lies in the region. Hence, multiplying this
; fraction by the area of the entire rectangle should produce
; an estimate of the integral.

; Implement Monte Carlo integration as a procedure estimate-
; integral that takes as arguments a predicate P, upper and
; lower bounds x1, x2, y1, and y2 for the rectangle, and the
; number of trials to perform in order to produce the esti-
; mate. Your procedure should use the same monte-carlo
; procedure that was used above to estimate π. Use your estimate-
; integral to produce an estimate of Pi by measuring the
; area of a unit circle.
; You will ﬁnd it useful to have a procedure that returns a
; number chosen at random from a given range. The follow-
; ing random-in-range procedure implements this in terms
; of the random procedure used in Section 1.2.6, which re-
; turns a nonnegative number less than its input.

(load "ex3.53.scm")

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random (* range 1.0)))))

(define (map-successive-pairs f s)
  (cons-stream
    (f (stream-car s) (stream-car (stream-cdr s)))
    (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      (/ passed (+ passed failed))
      (monte-carlo
        (stream-cdr experiment-stream) passed failed)))
        (if (stream-car experiment-stream)
          (next (+ passed 1) failed)
          (next passed (+ failed 1))))

(define (estimate-integral x1 y1 x2 y2 pred?)
    
  (define (build-stream)
    (cons-stream (random-in-range x1 x2)
      (cons-stream (random-in-range y1 y2)
              (build-stream))))

  (define area-points-stream
    (build-stream))

  (define krol-albanii-stream
    (map-successive-pairs pred? area-points-stream))
  (stream-map
    (lambda (p) (* p (* (- x2 x1) (- y2 y1))))
    (monte-carlo krol-albanii-stream 0 0)))

(define pi-stream
  (estimate-integral -1.0 -1.0 1.0 1.0 (lambda (x y) (<= (+ (* x x) (* y y)) 1.0))))

; (define (rp x)
;   (display-line (stream-ref pi-stream x))
;   'done)
; (rp 0)
; (rp 10)
; (rp 100)
; (rp 1000)
; (rp 10000)
; (rp 100000)
