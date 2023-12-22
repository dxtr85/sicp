; Exercise 3.68: Louis Reasoner thinks that building a stream
; of pairs from three parts is unnecessarily complicated. In-
; stead of separating the pair (S 0 , T 0 ) from the rest of the pairs
; in the first row, he proposes to work with the whole first
; row, as follows:

; (define (pairs s t)
;   (interleave
;     (stream-map (lambda (x) (list (stream-car s) x))
;       t)
;     (pairs (stream-cdr s) (stream-cdr t))))

; Does this work? Consider what happens if we evaluate (pairs
; integers integers) using Louis’s deﬁnition of pairs .

; The result would be a stream of pairs of equal integers:
; (1,1), (2,2), (3,3), (4,4), ... because interleave after
; producing first pair from stream-map calls pairs to produce
; next pair, but within pairs there is this again interleave,
; and it again first generates a pair from stream-map.
; But only if interleave was internally defined not to evaluate
; it's parameters before application (just like delay does).
; But mit-scheme throws this error:
; Aborting!: maximum recursion depth exceeded
; This is because interleave is a regular function and it has to
; evaluate it's arguments before applying that function hence
; causing pairs calling pairs calling pairs etc until max depth
; is reached.
