; Exercise 2.71.  Suppose we have a Huffman tree for an alphabet of n
; symbols, and that the relative frequencies of the symbols are 1, 2, 4, ...,
; 2n-1. Sketch the tree for n=5; for n=10. In such a tree (for general n) how
; many bits are required to encode the most frequent symbol? the least
; frequent symbol? 

  *
 / \
A  *
  / \
 B   *
    / \
   C  *
     / \
    D  *
      / \
     E  null

; 1 bit most frequent, 5 (or n - 1) bits least frequent.
