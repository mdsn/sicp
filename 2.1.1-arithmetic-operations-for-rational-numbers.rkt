#lang sicp

; (define (gcd a b)
;   (if (= b 0)
; 	a
; 	(gcd b (remainder a b))))

; The modulo procedure is like remainder, except that it always yields a result
; that has the same sign as the divisor, whereas remainder always has the same
; sign as the dividend. In particular, this means that when the divisor is
; positive and the dividend is negative, modulo yields a positive (or zero) result.
; Source: https://curtsinger.cs.grinnell.edu/teaching/2018F/CSC151/readings/numbers.html
(define (modulo-gcd a b)
  (if (= b 0)
    a
    (gcd b (modulo a b))))

(define (make-rat n d)
  (let ((g (modulo-gcd n d)))
	(cons (/ n g) (/ d g))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
			(* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
			(* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
	 (* (numer y) (denom x))))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

(print-rat one-half)
(print-rat one-third)
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

; exercise 2.1
; cases:
; 1. both positive: already normalized
; 2. both negative: multiply both n and d by -1
; 3. negative n, positive d: already normalized
; 4. positive n, negative d: multiply both by -1
; In other words, if d < 0, we need to normalize regardless of the sign of n.
;
; This version depends on make-rat using a modulo-based gcd, rather than a remainder-based one.
(define (make-rat-signed n d)
  (define (sign x)
	(if (< x 0)
	  -1
	  1))
  (let ((s (sign d)))
	(make-rat (* s n) (* s d))))

; (make-rat-signed -1 2) ; '(-1 . 2)
; (make-rat-signed 1 -2) ; '(-1 . 2)
; (make-rat-signed -1 -2) ; '(1 . 2)
; (make-rat-signed 1 2) ; '(1 . 2)
; (make-rat-signed -3 9) ; '(-1 . 3)
; (make-rat-signed 3 -9) ; '(-1 . 3)

(define minus-one-half (make-rat-signed -1 2))
(print-rat minus-one-half)
(print-rat (add-rat minus-one-half minus-one-half)) ; -1/1
