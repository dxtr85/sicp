; Exercise 3.57: How many additions are performed when
; we compute the n th Fibonacci number using the deﬁnition
; of fibs based on the add-streams procedure? Show that
; the number of additions would be exponentially greater
; if we had implemented (delay ⟨ exp ⟩ ) simply as (lambda
; () ⟨ exp ⟩ ) , without using the optimization provided by the
; memo-proc procedure described in Section 3.5.1.
; ---
; This exercise shows how call-by-need is closely related to
; ordinary memoization as described in Exercise 3.27. In that
; exercise, we used assignment to explicitly construct
; a local table. Our call-by-need stream optimization 
; effectively constructs such a table automatically,
; storing values in the previously forced parts of the stream.

; (define fibs
;   (cons-stream
;     0
;     (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

;     1 1 2 3 5  8 13 21 . . . = (stream-cdr fibs)
;     0 1 1 2 3  5  8 13 . . . = fibs
; 0 1 1 2 3 5 8 13 21 34 . . . = fibs

; When we use thi fibs stream for the first time procedure
; add-streams is called n-2 times when we want to compute
; the n-th fib number.
; Later when we want to compute m-th element the add-streams
; procedure gets called m - n - 2|0 times, depending whether
; m is greater or smaller than n.
; If we had used the non-optimized delay definition then
; add-streams procedure would have been called 
; (n - 2) * (n - 3) * ... * 1 times. This is because 
; each time we need next stream value we compute it, since
; we never store it in memory once we have computed it.
