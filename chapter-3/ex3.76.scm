; Exercise 3.76: Eva Lu Ator has a criticism of Louis’s ap-
; proach in Exercise 3.75. The program he wrote is not mod-
; ular, because it intermixes the operation of smoothing with
; the zero-crossing extraction. For example, the extractor should
; not have to be changed if Alyssa finds a better way to con-
; dition her input signal. Help Louis by writing a procedure
; smooth that takes a stream as input and produces a stream
; in which each element is the average of two successive in-
; put stream elements. Then use smooth as a component to
; implement the zero-crossing detector in a more modular
; style.

(define (smooth stream)
    (stream-map (lambda (x y) (/ (+ x y) 2))
      stream
      (cons-stream (stream-car stream) stream)))

(define (make-zero-crossings input-stream input-conditioner)
  (define (sign-change-detector new old)
    (cond 
      ((and (> new 0) 
            (> old 0)) 0)
      ((and (< new 0) 
            (< old 0)) 0)
      ((and (> new 0) 
            (< old 0)) 1)
      ((and (< new 0) 
            (> old 0)) -1)
      (else 0)))

  (define (build-output-stream input-stream last-value)
    (if (null? input-stream)
      '()
      (cons-stream
        (sign-change-detector
          (stream-car input-stream)
          last-value)
        (build-output-stream
          (stream-cdr input-stream)
          (stream-car input-stream)))))

  (define conditioned-stream (input-conditioner input-stream))

  (build-output-stream 
    (stream-cdr conditioned-stream) 
    (stream-car conditioned-stream)))

