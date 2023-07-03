#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
		((equal? x (car set)) true)
		(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
	set
	(cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
		((element-of-set? (car set1) set2)
		 (cons (car set1)
			   (intersection-set (cdr set1) set2)))
		(else (intersection-set (cdr set1) set2))))

; exercise 2.59

(define (union-set set1 set2)
  (cond ((null? set1) set2)
		((null? set2) set1)
		((element-of-set? (car set1) set2)
		 (union-set (cdr set1) set2))
		(else (cons (car set1)
					(union-set (cdr set1) set2)))))

(union-set '(1 2 3 4) '(3 4 5 6)) ; '(1 2 3 4 5 6)
(union-set '(1 2) '(3 4 5)) ; '(1 2 3 4 5)
(union-set '() '(1 2 3 4)) ; '(1 2 3 4)
(union-set '(1 2 3 4) '(4 5)) ; '(1 2 3 4 5)

; exercise 2.60

; element-of-set? remains the same. The entire list has to be traversed so it's linear.

; adjoin-set doesn't need to check for existence. It becomes O(1)
(define (adjoin-set x set)
  (cons x set))

; intersection-set remains quadratic; it still needs to check whether each element
; belongs in the result set.

; union-set just appends lists. It's linear.
(define (union-set set1 set2)
  (append set1 set2))

(adjoin-set 3 '(1 2 3)) ; '(3 1 2 3)
(union-set '(a b c d) '(c d e f)) ; '(a b c d c d e f)

; sets as ordered lists
(define (element-of-set? x set)
  (cond ((null? set) false)
		((= x (car set)) true)
		((< x (car set)) false)
		(else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
	'()
	(let ((x1 (car set1)) (x2 (car set2)))
	  (cond ((= x1 x2)
			 (cons x1
				   (intersection-set (cdr set1) (cdr set2))))
			((< x1 x2)
			 (intersection-set (cdr set1) set2))
			((< x2 x1)
			 (intersection-set set1 (cdr set2)))))))

; exercise 2.61
(define (adjoin-set x set)
  (if (null? set)
	(list x)
	(let ((head (car set)))
	  (cond ((= x head) set)
			((> x head)
			 (cons head (adjoin-set x (cdr set))))
			(else (cons x set))))))

(adjoin-set 5 '(1 2 3 8 9)) ; '(1 2 3 5 8 9)
(adjoin-set 8 '(1 2 3 8 9)) ; '(1 2 3 8 9)
(adjoin-set 3 '()) ; '(3)
(adjoin-set 10 '(1 2 3 4)) ; '(1 2 3 4 10)
(adjoin-set 1 '(5 10 15)) ; '(1 5 10 15)

; exercise 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
		((null? set2) set1)
		(else
		  (let ((x1 (car set1)) (x2 (car set2)))
			(cond ((= x1 x2)
				   (cons x1 (union-set (cdr set1) (cdr set2))))
				  ((< x1 x2)
				   (cons x1 (union-set (cdr set1) set2)))
				  ((> x1 x2)
				   (cons x2 (union-set set1 (cdr set2)))))))))

(union-set '(1 2 3 4) '(4 5 7 8)) ; '(1 2 3 4 5 7 8)
(union-set '(1 2 3) '(1 2 3)) ; '(1 2 3)
(union-set '() '(1 2 3)) ; '(1 2 3)
(union-set '(1 2 3) '(2 3 4 5 7 8)) ; '(1 2 3 4 5 7 8)
