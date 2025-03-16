; Section 3.3.3 - Representing tables

(define (make-table)
  (list '*table*))

(define (my-assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records))
         (car records))
        (else (my-assoc key (cdr records)))))

(define (lookup key table)
  (let ((record (my-assoc key (cdr table))))
    (if record
      (cdr record)
      #f)))

(define (insert! key value table)
  (let ((record (my-assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table (cons (cons key value) (cdr table)))))
  'ok)

(define tbl (make-table))
(insert! 'a 1 tbl)
(insert! 'b 2 tbl)
(insert! 'c 3 tbl)
(lookup 'b tbl) ; 2
(lookup 'd tbl) ; #f

; Two-dimensional tables
(define (lookup2 key1 key2 table)
  (let ((subtable (my-assoc key1 (cdr table))))
    (if subtable
      (let ((record (my-assoc key2 (cdr subtable))))
        (if record
          (cdr record)
          #f))
      #f)))

(define (insert2! key1 key2 value table)
  (let ((subtable (my-assoc key1 (cdr table))))
    (if subtable
      (let ((record (my-assoc key2 (cdr subtable))))
        (if record
          (set-cdr! record value)
          (set-cdr! subtable (cons (cons key2 value)
                                   (cdr subtable)))))
      (set-cdr! table
                (cons (list key1    ; XXX why `list` and not `cons`? see below
                            (cons key2 value))
                      (cdr table)))))
  'ok)

; > (cons 'a 'b)
; (mcons 'a 'b)
; > (list 'a 'b)
; (mcons 'a (mcons 'b '()))
; This is what the table looks like if constructed using cons (before the errors):
;
;   (display tbl2) ; (*table* (c f . 100) (b i . 10) (a x . 1))
;
; Only the insertion of the new subtable works, every subsequent insert fails.
; In contrast, this is what the proper 2D table looks like when constructing
; subtables with `list`:
;
;   (display tbl2) ; (*table* (c (h . 102) (g . 101) (f . 100))
;                             (b (k . 12) (j . 11) (i . 10))
;                             (a (z . 3) (y . 2) (x . 1)))
;
; The error that comes up is this:
;     mcar: contract violation
;       expected: mpair?
;       given: 'f
;
; (define pairs-chained (cons (cons 'key1 (cons 'key2 'val)) '())) ; Not a proper list
; (display pairs-chained) ; ((key1 key2 . val))  <- same as the "subtables" of line 62
; (my-assoc 'key1 pairs-chained) ; (mcons 'key1 (mcons 'key2 'val))
; (cdr (my-assoc 'key1 pairs-chained)) ; (mcons 'key2 'val) lookup2 would have returned
;                                      ;this as subtable, then the second lookup would
;                                      ; run assoc with key 2 on this pair.
; (my-assoc 'key2 (cdr (my-assoc 'key1 pairs-chained)))
;     mcar: contract violation
;       expected: mpair?
;       given: 'key2
; As expected. We need to always have a pair (a list node) returned as cdr of the
; subtable--the list constructor ensures the second element of the list (an actual
; pair) is pushed over into the second item in the list.

(define tbl2 (make-table))
(insert2! 'a 'x 1 tbl2)
(insert2! 'a 'y 2 tbl2)
(insert2! 'a 'z 3 tbl2)
(insert2! 'b 'i 10 tbl2)
(insert2! 'b 'j 11 tbl2)
(insert2! 'b 'k 12 tbl2)
(insert2! 'c 'f 100 tbl2)
(insert2! 'c 'g 101 tbl2)
(insert2! 'c 'h 102 tbl2)

(lookup2 'a 'z tbl2) ; 3
(lookup2 'b 'i tbl2) ; 10
(lookup2 'c 'x tbl2) ; #f

; Creating local tables
(define (make-table-object)
  (let ((local-table (list '*table*)))
    (define (lookup key1 key2)
      (let ((subtable (my-assoc key1 (cdr local-table))))
        (if subtable
          (let ((record (my-assoc key2 (cdr subtable))))
            (if record
              (cdr record)
              #f))
          #f)))
    (define (insert! key1 key2 value)
      (let ((subtable (my-assoc key1 (cdr local-table))))
        (if subtable
          (let ((record (my-assoc key2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key1
                                (cons key2 value))
                          (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define table-object (make-table-object))
(define get (table-object 'lookup-proc))
(define put (table-object 'insert-proc!))

(put 'a 'x 3)
(put 'b 'y 5)
(get 'a 'x) ; 3
(get 'b 'z) ; #f

; exercise 3.24
(define (make-table-object-pred same-key?)
  (let ((local-table (list '*table)))
    ; A generic assoc operation that uses same-key? to test for equality.
    (define (generic-assoc key records)
      (cond ((null? records) #f)
        ((same-key? key (caar records))
         (car records))
        (else (generic-assoc key (cdr records)))))
    ; Lookup using the generic-assoc
    (define (lookup key)
      (let ((record (generic-assoc key (cdr local-table))))
        (if record
          (cdr record)
          #f)))
    ; Insert using the generic-assoc
    (define (insert! key value)
      (let ((record (generic-assoc key (cdr local-table))))
        (if record
          (set-cdr! record value)
          (set-cdr! local-table (cons (cons key value)
                                      (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

; A predicate for numeric keys
(define (within-5 x y)
  (<= (abs (- x y)) 5))

(define numeric-table (make-table-object-pred within-5))
(define get (numeric-table 'lookup))
; Note that insert will overwrite only the first element whose
; key matches the predicate--there may be more than one!
(define put (numeric-table 'insert))

(put 3 'three)
(put 10 'ten)
(put 25 'twenty-five)
(get 5) ; 'ten  -- the table is not ordered; (10 'ten) was prepended
        ; in the local table, so it matches first
(get -1) ; 'three
(get 30) ; 'twenty-five
(get 31) ; #f

