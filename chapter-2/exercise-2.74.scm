;; Exercise 2.74.  Insatiable Enterprises, Inc., is a highly decentralized
;; conglomerate company consisting of a large number of independent
;; divisions located all over the world. The company's computer facilities have
;; just been interconnected by means of a clever network-interfacing scheme
;; that makes the entire network appear to any user to be a single computer.
;; Insatiable's president, in her first attempt to exploit the ability of the
;; network to extract administrative information from division files, is dismayed
;; to discover that, although all the division files have been implemented as
;; data structures in Scheme, the particular data structure used varies from
;; division to division. A meeting of division managers is hastily called to
;; search for a strategy to integrate the files that will satisfy headquarters'
;; needs while preserving the existing autonomy of the divisions.

;; Show how such a strategy can be implemented with data-directed
;; programming. As an example, suppose that each division's personnel
;; records consist of a single file, which contains a set of records keyed on
;; employees' names. The structure of the set varies from division to division.
;; Furthermore, each employee's record is itself a set (structured differently
;; from division to division) that contains information keyed under identifiers
;; such as address and salary. In particular:

;; a.  Implement for headquarters a get-record procedure that retrieves a
;; specified employee's record from a specified personnel file. The procedure
;; should be applicable to any division's file. Explain how the individual
;; divisions' files should be structured. In particular, what type information
;; must be supplied?

;;  sockyfeet
;; I am assuming that the individual divisions' files are structured such that
;; there already exists, for each division, a get-record method. I am assuming there
;; exists an (<operator>, personnel-file) table. The reason I am not assuming there
;; exists an (<operator>, division) table is that this assumption would then require
;; the additional assumption that there exists a method that takes personnel-file as
;; argument and returns the division of that personnel-file. Since each division has
;; exactly one personnel-file, we have a 1-1 correspondence between the two pieces of
;; data. Indexing by personnel-file allows me to make one less assumption and have
;; the same amount of columns. Thus, we have:

 (define (get-record employee personnel-file) 
   ((get 'get-record personnel-file) employee)) 

;; b.  Implement for headquarters a get-salary procedure that returns the
;; salary information from a given employee's record from any division's
;; personnel file. How should the record be structured in order to make this
;; operation work?

;; Now, I just assume that each record is structured such that each division can
;; implement and has implemented their own get-salary method, that takes as argument
;; the employee's record and returns the employee's salary. Thus:

 (define (get-salary employee personnel-file) 
   ((get 'get-salary personnel-file) (get-record employee personnel-file))) 

;; c.  Implement for headquarters a find-employee-record procedure. This should
;; search all the divisions' files for the record of a given employee and return
;; the record. Assume that this procedure takes as arguments an employee's
;; name and a list of all the divisions' files.

;; I'm assuming that each individual divisions get-record method returns '() if
;; the employee is not employed in that division, and so the method that we implemented
;; in part a) of this question returns '() in that case, too. We have:

 (define (find-employee-record employee division-files) 
   (cond ((null? division-files) '()) 
         ((not (null? (get-record employee (car division-files)))) 
          (get-record employee (car division-files))) 
         (else (find-employee-record employee (cdr division-files))))) 

 (define (find-employee-record name list) 
   (if (null? list) 
       '() 
       (append (get-record (car list)) 
               (find-employee-record name (cdr list))))) 

;; d.  When Insatiable takes over a new company, what changes must be made
;; in order to incorporate the new personnel information into the central
;; system? 

;; Call the newly acquired division-file new-division-file. Entries corresponding to
;; (get-record, new-division-file) and (get-salary, new-division-file) need to be added
;; to our (<operator>, division-file) table. That is, the newly acquired company needs
;; to implement locally their own versions of get-record, get-salary, then we need to
;; add these implementations to our table, and then the methods defined in a), b), c)
;; will work. 
