#lang sicp

; exercise 2.24

; (list 1 (list 2 (list 3 4)))
; 		/			\
; 	   1       (list 2 (list 3 4))
; 	   			  /      \
; 	   			 2     (list 3 4)
; 	   			 		/    \
; 	   			 	   3     4

; exercise 2.25

(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
(car (cdaddr (list 1 3 (list 5 7) 9)))

(car (car (list (list 7))))
(caar (list (list 7)))

(car (cdadar (cdadar (cdadr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))

; exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ; '(1 2 3 4 5 6)
(cons x y)   ; '((1 2 3) 4 5 6)
(list x y)   ; '((1 2 3) (4 5 6))

; exercise 2.27

; from 2.18
(define (reverse xs)
  (define (inner xs acc)
	(if (null? xs)
	  acc
	  (inner (cdr xs) (cons (car xs) acc))))
  (inner xs '()))

(define x (list (list 1 2) (list 3 4)))

(reverse x) ; '((3 4) (1 2))

(define (deep-reverse xs)
  (if (not (pair? xs))
	xs
	(map deep-reverse (reverse xs))))

(deep-reverse x) ; '((4 3) (2 1))
(deep-reverse (list 1 (list 2 3 (list 4 5) 6) (list -1 -2))) ; '((-2 -1) (6 (5 4) 3 2) 1)

; exercise 2.28
(define (fringe xs)
  (cond ((and (pair? xs) (not (null? (cdr xs))))
		 (append (fringe (car xs)) (fringe (cdr xs))))
		((pair? xs) (fringe (car xs)))
		(else (list xs))))

(fringe x) ; '(1 2 3 4)
(fringe (list 1 (list 2 3 (list 4 5) 6) (list -1 -2))) ; '(1 2 3 4 5 6 -1 -2)

; exercise 2.29
(define (assert expr)
  (if (not expr)
	(error "failed assertion")
	true))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define left-branch car)
(define right-branch cadr)
(define branch-length car)
(define branch-structure cadr)
(define (mobile? branch)
  (pair? (branch-structure branch)))

(define b0 (make-branch 10 5))
(define b1
  (make-branch 7 
			   (make-mobile (make-branch 3 3) 
							(make-branch 7 5))))

(assert (mobile? b1)) ; #t
(assert (not (mobile? b0))) ; #t

(define mobile
  (make-mobile b0 b1))

(left-branch mobile) ; '(10 5)
(right-branch mobile) ; '(7 ((3 3) (7 2)))

(define (branch-weight branch)
  (if (mobile? branch)
	(weight (branch-structure branch))
	(branch-structure branch)))

(branch-weight b0) ; 5
(branch-weight b1) ; 8

(define (weight mobile) 
  (+ (branch-weight (left-branch mobile)) 
	 (branch-weight (right-branch mobile))))

(weight mobile) ; 13

(define (torque branch)
  (* (branch-length branch)
	 (branch-weight branch)))

(torque b0) ; 50
(torque b1) ; 56

(define (balanced? mobile)
  (= (torque (left-branch mobile))
	 (torque (right-branch mobile))))

(balanced? mobile) ; #f

(define b2 (make-branch 4 14))
(balanced? (make-mobile b1 b2)) ; #t

; If we change the definition of the constructors to use `cons` instead of `list`,
; all we need to change is the definitions of `right-branch` and `branch-structure`
; from `cadr` to `cdr`. The reason being that for a list, cdr returns a singleton list
; whereas for a pair, it returns the value itself already "unwrapped".
