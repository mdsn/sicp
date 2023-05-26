#lang sicp

; exercise 2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define z (cons 1 2))
(car z) ; 1
(cdr z) ; 2

; exercise 2.5

; We want to recover the original exponent to the base 'a' given to recover.
; (This is shitty naming because a and b have a different meaning in cons.)
; Do this by removing factors of 'b' repeatedly until we are left with a^x,
; then apply the following equality:
; 	a^b = e^(b log a)
; 	thus b = log (a^b) / log a.
(define (recover z a b)
  (define (iter x)
	(if (= (remainder x b) 0)
	  (iter (/ x b))
	  (inexact->exact (/ (log x) (log a)))))
  (iter z))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (recover z 2 3))

(define (cdr z)
  (recover z 3 2))

(define z (cons 3 4)) ; 648
(car z) ; 3
(cdr z) ; 4

(define z (cons 2 9))
(car z) ; 2
(cdr z) ; 9

(define z (cons 9 2))
(car z) ; 9
(cdr z) ; 2

; exercise 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x))))

; (add-1 zero)
; (lambda (f) (lambda (x) (f ((zero f) x))))
; 		(zero f)
; 		(lambda (x) x)
; (lambda (f) (lambda (x) (f ((lambda (x) x) x))))

(define one (lambda (f) (lambda (x) (f x))))

; (add-1 one)
; (lambda (f) (lambda (x) (f ((one f) x))))
; 		(one f)
; 		(lambda (x) (f x))
; (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f) (lambda (x) (a f (b f x)))))

; (add one two)
; (lambda (f) (lambda (x) (one f (two f x))))
; 		(one f (two f x))
; 			(two f x)
; 			(f (f x))
; 		(one f (f (f x)))
;		(f (f (f x)))
; (lambda (f) (lambda (x) (f (f (f x))))) = 3
;
; Addition is equivalent to the composition of two Church numerals. Given
; functions x and f, (one f x) adds one application of f to x, and (two f x)
; adds two. Thus if (two f x) = (f (f x)), then (one f (two f x)) = (f (f (f x))) = 3.
