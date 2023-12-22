; Instead of a linear recursion, the rewritten expmod generates a tree
; recursion, whose execution time grows exponentially with the depth of the
; tree, which is the logarithm of N. Therefore, the execution time is linear
; with N. 
