;Exercise 2.55.  Eva Lu Ator types to the interpreter the expression 
(car ''abracadabra)
;To her surprise, the interpreter prints back quote. Explain. 

;abracadabra - value of abracadabra
;'abracadabra - symbol of abracadabra
;''abracadabra = symbol of symbol of abracadabra, ie. a two-element list of quote and abracadabra symbols
;(car ''abracadabra) - first element of that list, quote
