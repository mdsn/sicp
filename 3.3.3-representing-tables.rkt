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

; exercise 3.25
; A n-dimensional table starts out as a regular empty table. It gains
; dimensions by way of insertions--each key in the input list of keys
; that is not present in the table results in the creation of a new subtable
; at the appropriate level. Lookup proceeds in a similar manner, with one
; further scenario: if lookup reaches a final value that is not a subtable
; but there are still keys remaining to traverse, it also returns false.
; The logic being that the input is a "path" to a value, and the given path
; did not arrive at any particular value.
; Note that a partial path to a subtable is still a valid path--the returned
; value is the corresponding subtable.

(define (insert-n! keys value table)
  (let ((record (my-assoc (car keys) (cdr table)))
        (final-key (null? (cdr keys))))
    (if record
      (if final-key
        (begin (set-cdr! record value)
               'ok)
        ; Problem: if record exists and is a pair (key value), but now needs
        ; to turn into a subtable on the way to a value, the recursive call
        ; sends the value as the `records` to `my-assoc`. This effectively is
        ; the same as if this key did not exist, for a new empty subtable needs
        ; to be allocated at this point, except that there is no need to modify
        ; the current table (the key is already in it).
        (if (list? record)
          (insert-n! (cdr keys) value record)
          (begin
            (set-cdr! record '()) ; turn the (cons key val) record into a new list
            (insert-n! (cdr keys) value record))))
      (if final-key
        (set-cdr! table (cons (cons (car keys) value)
                              (cdr table)))
        (let ((subtable (list (car keys))))
          (set-cdr! table (cons subtable (cdr table)))
          (insert-n! (cdr keys) value subtable))))))

(define n-table (make-table))
(insert-n! (list 'a 'f 'x) 1337 n-table)
(insert-n! (list 'a 'g 'x) 1234 n-table)
(display n-table) ; (*table* (a (g (x . 1234))
                  ;             (f (x . 1337))))
(insert-n! (list 'a 'f) 'bob n-table)
(display n-table) ; (*table* (a (g (x . 1234))
                  ;             (f . bob)))
(insert-n! (list 'a 'f 'y 'p) 'lol n-table)
(insert-n! (list 'a 'f 'x) 'dad n-table)
(insert-n! (list 'a 'f 'z) 'bro n-table)
(display n-table) ; (*table* (a (g (x . 1234))
                  ;             (f (y (p . lol)))))    :)

(define (lookup-n keys table)
  (let ((record (my-assoc (car keys) (cdr table))))
    (cond ((not record) #f)
          ((null? (cdr keys))
           (cdr record))
          ((list? record)
           (lookup-n (cdr keys) record))
          (else #f))))

; Equivalently, more nested:
; (if record
;   (if (null? (cdr keys))
;     (cdr record)
;     (if (list? record)
;       (lookup-n (cdr keys) record)
;       #f)) ; Reached a key/val pair, but there are keys remaining in the query
;   #f)))

(lookup-n (list 'a 'g 'x) n-table) ; 1234
(lookup-n (list 'a 'f) n-table)
; (mcons (mcons 'z 'bro) (mcons (mcons 'x 'dad) (mcons (mcons 'y (mcons (mcons 'p 'lol) '())) '())))
(display (lookup-n (list 'a 'f) n-table)) ; ((z . bro) (x . dad) (y (p . lol)))
; a->f is a table containing two pairs and a subtable

