;; Exercise 2.76.  As a large system with generic operations evolves, new
;; types of data objects or new operations may be needed. For each of the
;; three strategies -- generic operations with explicit dispatch, data-directed
;; style, and message-passing-style -- describe the changes that must be made
;; to a system in order to add new types or new operations. Which organization
;; would be most appropriate for a system in which new types must often be
;; added? Which would be most appropriate for a system in which new
;; operations must often be added? 

;; generic operations with explicit dispatch - require the most work.
;; When adding a new type one has to be aware of all existing types and it's
;; procedure names in order to create new procedures with nemas that are 
;; unique, also every generic procedure has to be updated in order to add
;; support for newly created data type. When adding a new operation one has to
;; take into consideration all existing types of operations in order to create
;; a procedure supporting all types defined to this date.

;; data-directed - when adding new data type one has to create an install
;; wrapper procedure that internalizes newly defined procedures - so that no
;; author does not to be aware of other's people procedure names. In that
;; install procedure he has also link his internal procedures to globally
;; accessible dispatcher framework. When adding a new procedure all existing
;; install procedures need to be updated with their versions of that procedure.

;; message-passing-style - When adding a new type, this new type has to define
;; all needed procedures in that type definition. When extending types with new definitions, the constructor procedures of all existing types have to be
;; updated with new procedures. Probably existing data objects will not support
;; recently added procedures.
