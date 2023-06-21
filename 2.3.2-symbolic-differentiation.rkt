#lang scheme

(define (deriv exp var)
  (cond ((number? exp) 0)
		((variable? exp)
		 (if (same-variable? exp var) 1 0))
		((sum? exp)
		 (make-sum (deriv (addend exp) var)
				   (deriv (augend exp) var)))
		((product? exp)
		 (make-sum
		   (make-product
			 (multiplier exp)
			 (deriv (multiplicand exp) var))
		   (make-product
			 (deriv (multiplier exp) var)
			 (multiplicand exp))))
		((exponentiation? exp)
		 (make-product
		   (exponent exp)
		   (make-exponentiation var (- (exponent exp) 1))))
		(else (error "unknown expression type: DERIV" exp))))

; utilities

(define (distribute-right op exp)
  (let ((tail (drop 2 exp)))
	(if (= 1 (length tail))
	  (car tail)
	  (append (list op) tail))))

(define (drop n xs)
  (if (or (= n 0) (null? xs))
	xs
	(drop (- n 1) (cdr xs))))

(drop 3 '(1 2 3 4 5)) ; '(4 5)
(drop 2 '(1 2 3)) ; '(3)
(drop 5 '(1 2)) ; '()

; predicates

(define (variable? x) (symbol? x))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (same-variable? x y)
  (and (variable? x)
	   (variable? y)
	   (eq? x y)))

(define (sum? x)
  (and (pair? x)
	   (eq? (car x) '+)))

(define (product? exp)
  (and (pair? exp)
	   (eq? (car exp) '*)))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

; selectors

(define (addend x)
  (cadr x))

(define (augend exp) (distribute-right '+ exp))

(augend '(+ a b c)) ; '(+ b c)
(augend '(+ a b c d)) ; '(+ b c d)
(augend (augend '(+ a b c d))) ; '(+ c d)
(augend '(+ a b)) ; 'b

(define (multiplier exp) (cadr exp))
(define (multiplicand exp) (distribute-right '* exp))

(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))

; constructors

(define (make-sum x y)
  (cond ((=number? x 0) y)
		((=number? y 0) x)
		((and (number? x) (number? y))
		 (+ x y))
		(else (list '+ x y))))

(define (make-product x y)
  (cond ((or (=number? x 0)
			 (=number? y 0))
		 0)
		((=number? x 1) y)
		((=number? y 1) x)
		((and (number? x) (number? y))
		 (* x y))
		(else (list '* x y))))

(define (make-exponentiation x n)
  (cond ((=number? n 0) 1)
		((=number? n 1) x)
		(else (list '** x n))))

(deriv '(+ x 3) 'x) ; 1
(deriv '(* x y) 'x) ; 'y
(deriv '(* (* x y) (+ x 3)) 'x) ; '(+ (* x y) (* y (+ x 3)))

; exercise 2.56
(deriv '(** x 3) 'x) ; '(* 3 (** x 2))

; exercise 2.57
(deriv '(+ (** x 3) (* 3 x) x) 'x) ; '(+ (* 3 (** x 2)) 4)
(deriv '(+ x x x x) 'x) ; 4
(deriv '(+ x x x x x y (** x 3)) 'x) ; '(+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (* 3 (** x 2)))))))
(deriv '(* x y (+ x 3)) 'x) ; '(+ (* x y) (* y (+ x 3)))


