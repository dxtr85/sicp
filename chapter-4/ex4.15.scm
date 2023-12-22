; Exercise 4.15: Given a one-argument procedure p and an
; object a, p is said to “halt” on a if evaluating the expres-
; sion (p a) returns a value (as opposed to terminating with
; an error message or running forever). Show that it is im-
; possible to write a procedure halts? that correctly deter-
; mines whether p halts on a for any procedure p and object
; a. Use the following reasoning: If you had such a procedure
; halts?, you could implement the following program:

; (define (run-forever) (run-forever))
;   (define (try p)
;     (if (halts? p p) (run-forever) 'halted))

; Now consider evaluating the expression (try try) and
; show that any possible outcome (either halting or running
; forever) violates the intended behavior of halts?

; Although we stipulated that halts? is given a procedure
; object, notice that this reasoning still applies even if
; halts? can gain access to the procedure’s text and its
; environment. is is Turing’s celebrated Halting eorem,
; which gave the ﬁrst clear example of a non-computable 
; problem, i.e., a well-posed task that cannot be carried out
; as a computational procedure.

; We defined try as a procedure that does the opposite to what
; halts? procedure outputs for its argument. Now if we run
; halts? procedure with this try procedure it will revert
; the output of halts?
; So if halts? returns true on try procedure, then
; the try procedure will run forever, and if
; halts? returns false then try will just stop.
; This is a contradiction and such a procedure as halts?
; can not exist.
