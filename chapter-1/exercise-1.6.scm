(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (new-if predicate then-clause else-clause) (cond (predicate then-clause) (else else-clause)))

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

(define (good-enough? guess x)
  (if (< (abs (- (* guess guess) x)) 0.0001)
      #t
      #f))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
; The default if statement is a special form which means that even when an
; interpreter follows applicative substitution, it only evaluates one of its
; parameters- not both. However, the newly created new-if doesn't have this
; property and hence, it never stops calling itself due to the third parameter
; passed to it in sqrt-iter. 
; 
; To be even clearer: The act of re-defining a special form using generic
; arguments effectively "De-Special Forms" it. It then becomes subject to
; applicative-order evaluation, such that any expressions within the
; consequent or alternate portions are evaluated regardless of the predicate.
; In Ex 1.6, the iteration procedure is called without return and eventually
; overflows the stack causing an out of memory error. 
; 
;  (define (iff <p> <c> <a>) (if <p> <c> <a>)) 
;   
;  (define (tryif a) (if (= a 0) 1 (/ 1 0))) 
;   
;  (define (tryiff a) (iff (= a 0) 1 (/ 1 0))) 
; Welcome to DrRacket, version 7.5 [3m].
; Language: R5RS; memory limit: 128 MB.
; > (tryif 0)
; 1
; > (tryif 1)
; . . /: division by zero
; > (tryiff 0)
; . . /: division by zero
; > (tryiff 1)
; . . /: division by zero
; > 
; 
; (Note: comments below apply to a previous version of this solution, which
; has been changed to take them into account; please refer to revisions 1–12
; of the edit history to view the version on which they were made.) 
; 
; jsdalton
; 
; I believe this solution is incorrect. 
; 
; new-if does not use normal order evaluation, it uses applicative order
; evaluation. That is, the interpreter first evaluates the operator and
; operands and then applies the resulting procedure to the resulting
; arguments. As with Excercise 1.5, this results in an infinite recursion
; because the else-clause is always evaluated, thus calling the procedure
; again ad infinitum. 
; 
; The if statement is a special form and behaves differently. if first evalutes
; the predictate, and then evaluates either the consequent (if the predicate
; evalutes to #t) or the alternative (if the predicate evalues to #f). This is key
; difference from new-if -- only one of the two consequent expressions get
; evaluated when using if, while both of the consequent expressions get
; evaluated with new-if. 
; 
; wjm
; 
; A lenghtier explanation of Applicative Order and Normal Order is here:
; http://mitpress.mit.edu/sicp/full-text/sicp/book/node85.html 
; 
; dft
; 
; But if if works the way that you suggest, why does the very first example in
; wjm's link generate an error? 
; 
; (define (try a b)
;   (if (= a 0) 1 b))
; 
; Evaluating (try 0 (/ 1 0)) generates an error in Scheme. If if only
; evaluates the consequent or the alternative, it would never get to the
; division by zero. It seems to me - and this is what the link suggests - that
; even if uses applicative order. 
; 
; I don't have an alternative explanation - this exercise is stumping me. The
; applicative vs. normal explanation made sense until I saw the try example
; above. 
; 
; dft
; 
; Ah, I finally figured it out. You are right. I'm going to keep my question (and
; this additional response) though because maybe others will have made the
; same mistake. 
; 
; The reason the above example generates an error is because (1 / 0), the
; second parameter to try, is evaluated before the try is even called. The if
; in the body of try is actually irrelevant. An error would be generated even
; if try did not use the value of b at all. 
; 
; As you note, Scheme behaves this way in general due to applicative
; ordering - parameters are evaluated before the operation is carried out. if
; is an exception where the "parameters" are not evaluated unless needed.
; So if we say instead: 
; 
; (define (try a)
;   (if (= a 0) 1 (/ 1 0))
; 
; Calling (try 0) does not result in an error, because the else-clause is never
; evaluated. 
; 
; andersc
; 
; I agree with jsdalton. The reason why new-if runs out of memory is
; applicative order evaluation, so if the plain-old if uses applicative order
; evaluation, it should not work either. 
; 
; And I guess for a certain interpreter, maybe it should use a consistent way
; for all processes? 
; 
; emmp
; 
; I believe the above two posters are right and the given answer is wrong. 
; 
; It's stated clearly in the text that: 
; 
; "Lisp uses applicative-order evaluation, partly because of the additional
; efficiency obtained from avoiding multiple evaluations of expressions such
; as those illustrated with (+ 5 1) and (* 5 2) above and, more significantly,
; because normal-order evaluation becomes much more complicated to deal
; with when we leave the realm of procedures that can be modeled by
; substitution." 
; 
; So I don't see a reason why MIT-Scheme (which is supposedly what readers
; of the book use) would be any different. Plus, as andersc wrote, an
; interpreter would have to be consistent about the evaluation strategy it
; uses. 
; 
; As jsdalton said. new-if is a procedure, not a special-form, which means
; that all sub-expressions are evaluated before new-if is applied to the values
; of the operands. That includes sqrt-iter which is extended to new-if which
; again leads to the evaluation of all the sub-expressions including sqrt-iter
; etc. Instead, in if only one of the consequent expressions is evaluated each
; time. 
; 
; dpchrist
; 
; We can't mimic if with cond because we can't prevent the interpreter from
; evaluating specific arguments. 
; 
; If we use cond form instead of if, without wrapper it inside new-if - it'll still
; work as expected. 
; 
; >>> 
; 
; picard
; 
; Read the MIT "Don't Panic" guide to 6.001 on Open Courseware for a short
; guide on how Edwin works (started with "mit-scheme --edit").
; http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/tools/dontpanicnew.pdf
; 
; This exercise is solved by trying out the new-if statement and evaluating
; with M-p in Edwin. You will get an error in the Scheme REPL "Aborting:
; Maximum recursion depth exceeded" and can look through the debugger to
; see how sqrt-iter loops forever. 
; 
; trevoriannguyen
; 
; I believe the original solution and the comments by previous posters are
; incorrect. new-if is a procedure, and under applicative-order evaluation, all
; its arguments will be evaluated first before the procedure application is
; even started. The third argument to the new-if procedure, i.e. the recursive
; call to sqrt-iter, will always be evaluated. It is the evaluation of this third
; argument that causes an infinite loop. In particular, the else-clause
; mentioned by jsdalton is never evaluated. Indeed, the new-if procedure
; body (which contains the cond special form) is never even applied to the
; resulting 3 arguments as the 3rd argument never stops evaluating itself! 
; 
; student
; 
; jsdalton was actually referring to the 3rd argument by its name:
; else-clause. Your statements are thus equivalent. 
; 
; $hawty Low
; 
; I fail to see why sqrt-iter is infinitely evaluated in new-if but not in the old
; regular if. Haven't we defined a stopping point with good enough? Why
; should it continued infinitely? 
; 
; cypherpunkswritecode
; 
; Both cond and if are special forms. It's hard to follow, but pay close
; attention to the wording in SICP. 
; 
; Page 22: "...there is a special form in Lisp for notating such a case
; analysis. It is called cond..." 
; 
; Page 23: "This process continues until a predicate is found whose value is
; true, in which case the interpreter returns the value of the corresponding
; consequent expression..." 
; 
; Take note that it says nothing about evaluating the consequent expression
; at this point, only returning the value. I believe that the consequent
; expressions are evaluated first in the case of cond. Now look at the wording
; for if: 
; 
; Page 24: To evaluate an if expression, the interpreter starts by evaluating
; the ⟨predicate⟩ part of the expression. If the ⟨predicate⟩ evaluates to a true
; value, the interpreter then evaluates the ⟨consequent⟩ and returns its
; value. 
; 
; Take note that in the case of if, it explicitly states that the interpreter
; evaluates the consequent if (and only if) its corresponding predicate is
; true. That is quite different. 
; 
; jhenderson
; 
; I think, perhaps, the pretty-print is helping hide the elephant in the room
; here. In sqrt-iter, the call to new-if introduces an infinite recursion.
; Remember that arguments, if any, are evaluated before a function call. In
; this case, one of the arguments to new-if invokes sqrt-iter recursively and
; ad infinitum. The new-if procedure never executes. 
