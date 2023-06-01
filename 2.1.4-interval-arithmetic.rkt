#lang sicp

; exercise 2.7
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
					(lower-bound y))
				 (+ (upper-bound x)
					(upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
			   (lower-bound y)))
		(p2 (* (lower-bound x)
			   (upper-bound y)))
		(p3 (* (upper-bound x)
			   (lower-bound y)))
		(p4 (* (upper-bound x)
			   (upper-bound y))))
	(make-interval (min p1 p2 p3 p4)
				   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
	x
	(make-interval
	  (/ 1.0 (upper-bound y))
	  (/ 1.0 (lower-bound y)))))

; exercise 3.8
(define a (make-interval 1 3))
(define b (make-interval 2 4))
(define c (add-interval a b)) ; '(3 . 7)

; (3 . 7) = (2 . 4) + a, thus a = (3 . 7) - (2 . 4).
; Then (3 . 7) - (2 . 4) = (1 . 3) = ((3-2) . (7-4))
(define (sub-interval a b)
  (make-interval (- (lower-bound a) (lower-bound b))
				 (- (upper-bound a) (upper-bound b))))

(sub-interval c b) ; '(1 . 3)

; exercise 3.9
(define (width a)
  (/ (- (upper-bound a) (lower-bound a))
	 2))

; Let a, b intervals, and x = a + b.
; Then width(x) = .5 (upper(x) - lower(x))
; 				= .5 upper(x) - .5 lower(x)
; 				= .5 (upper(a) + upper(b)) - .5 (lower(a) + lower(b))
; 				= .5 upper(a) - .5 lower(a) + .5 upper(b) - .5 lower(b)
; 				= .5 (upper(a) - lower(a)) + .5 (upper(b) - lower(b))
; 				= width(a) + width(b), and similarly for the difference of intervals.

(define a (make-interval 5 10))
(define b (make-interval 1 2))
(width a) ; 5/2
(width b) ; 1/2
(width (add-interval a b)) ; 3
(width (mul-interval a b)) ; 15/2
(width (div-interval a b)) ; 3.75 ???

; exercise 2.10
(define (div-interval-checked x y)
  (if (= 0 (width y))
	(error "division by null interval")
	(div-interval x y)))

(define c (make-interval 1 1))
(width c) ; 0
(div-interval-checked a c) ; division by null interval [,bt for context]

; exercise 2.11
; cases:
; 1. (-, -); (-, -)
; 2. (-, -); (-, +)
; 3. (-, -); (+, +)
; 4. (-, +); (-, -)
; 5. (-, +); (-, +)
; 6. (-, +); (+, +)
; 7. (+, +); (-, -) same as case 3
; 8. (+, +); (-, +) same as case 6
; 9. (+, +); (+, +) same as case 1
;
; We can assume that the lower bound of an interval is less than the upper bound.
; Thus in cases with mixed signs, it's possible to determine the minimum and maximum
; products directly, only spending the two multiplications for the bounds
; of the resulting interval.

;(define (neg x) (< x 0))
;(define (pos x) (not (neg x)))
;
;(define (mul-interval-cryptic a b)
;  (let ((la (lower-bound a))
;		(ua (upper-bound a))
;		(lb (lower-bound b))
;		(ub (upper-bound b)))
;	(cond
;	  ; cases 1 and 9
;	  ((or (and (neg la) (neg ua) (neg lb) (neg ub))
;		   (and (pos la) (pos ua) (pos lb) (pos ub))) ...)
;	  ; case 2
;	  ((and (neg la) (neg ua) (neg lb) (pos ub))
;	   (make-interval (* (min la lb) ub) (* (min la lb) ua))
;	  ; case 3
;	  ((and (neg la) (neg ua) (pos lb) (pos ub))
;	   (make-interval (* ua ub) (max (* lb ub) (* la ua)))) ; use abs?
;	  ; case 4
;	  ((and (neg la) (pos ua) (neg lb) (neg ub))
;	   (make-interval (* (min la ub) ua) (* (min la lb) ub)))
;	  ; case ... to hell with this exercise

; exercise 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i)
		(upper-bound i))
	 2))

; p = w / c
; pc = w
(define (make-center-percent c p)
  (make-center-width c (* c p)))

(define (percent i)
  (/ (width i) (center i)))

(make-center-percent 1 0.3) ; '(0.7 . 1.3)
(define i (make-center-percent 10 0.3)) ; '(7.0 . 13.0)
(percent i) ; 0.3

; exercise 2.13 .. 2.16
; We'll get back to this.
