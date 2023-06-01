#lang sicp

; exercise 2.17
(define (last-pair xs) 
  (let ((tail (cdr xs))
		(head (car xs))) 
	(if (null? tail)
	  (list head) 
	  (last-pair tail))))

(last-pair (list 1 2 3)) ; '(3)
(last-pair (list 1)) ; '(1)

; exercise 2.18
(define (reverse xs)
  (define (inner xs acc)
	(if (null? xs)
	  acc
	  (inner (cdr xs) (cons (car xs) acc))))
  (inner xs '()))

(reverse (list 1 2 3 4)) ; '(4 3 2 1)

; exercise 2.19
; from 1.2.2
(define us-coins (list 25 10 5 50 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(define (cc amount coin-values)
  (cond ((zero? amount) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount 
					 (except-first-denomination coin-values)) 
				 (cc (- amount (first-denomination coin-values)) 
					 coin-values)))))

(cc 100 us-coins) ; 292

; exercise 2.20
(define (same-parity x . xs)
  (define (parity x) (remainder x 2))
  (define (accumulate xs)
	(cond ((null? xs) '())
		  ((= (parity x) (parity (car xs)))
		   (cons (car xs) (accumulate (cdr xs))))
		  (else (accumulate (cdr xs)))))
  (cons x (accumulate xs)))

(same-parity 1 2 3 4 5 6 7) ; '(1 3 5 7)
(same-parity 2 3 4 5 6 7) ; '(2 4 6)

; exercise 2.21
(define (map f xs)
  (if (null? xs)
	'()
	(cons (f (car xs))
		  (map f (cdr xs)))))

(map abs (list -10 2.5 -11.6 17)) ; '(10 2.5 11.6 17)

(define (square x) (* x x))
(define (square-list1 xs)
  (if (null? xs)
	'()
	(cons (square (car xs)) (square-list1 (cdr xs)))))

(square-list1 (list 1 2 -3 5)) ; '(1 4 9 25)

(define (square-list xs)
  (map square xs))

(square-list (list 1 2 -3 5)) ; '(1 4 9 25)

; exercise 2.22
; The elements come out reversed in an iterative version because
; cons can only prepend items to a given list. Thus, iterating
; from the beginning of a list yields the first element at the
; end of the result, the second before that, and so on.
; Reversing the order of arguments to cons doesn't work because
; cons has type:
; 	cons :: T -> [T] -> [T]
; so reversing the order of the arguments builds up a collection
; of nested lists: (((cons 1 . '()) . 2) . 3)

; exercise 2.23
(define (for-each f xs)
  (if (null? xs)
	true
	((lambda ()
	   (f (car xs))
	   (for-each f (cdr xs))))))

(for-each (lambda (x) (newline) (display x))
		  (list 1 2 3 "a"))

