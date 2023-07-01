#lang sicp

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

; exercise 2.58-1
(define (binop? op exp)
  (and (pair? exp) (eq? (cadr exp) op)))

(define (sum? exp) (binop? '+ exp))
(define (product? exp) (binop? '* exp))
(define (exponentiation? exp) (binop? '** exp))

(sum? '(x + 1)) ; #t
(sum? '(2 + 2)) ; #t
(sum? '(+ 1 2)) ; #f
(sum? '(2 + (2 + 2))) ; #t
(product? '(x * 3)) ; #t
(exponentiation? '(x ** 2)) ; #t

(define (addend exp) (car exp))
(define (augend exp) (caddr exp))
(define (multiplier exp) (car exp))
(define (multiplicand exp) (caddr exp))
(define (base exp) (car exp))
(define (exponent exp) (caddr exp))

(addend '(x + 1)) ; 'x
(augend '(x + 1)) ; 1
(augend '(x + (y + 2))) ; '(y + 2)

(define (make-sum x y)
  (cond ((=number? x 0) y)
		((=number? y 0) x)
		((and (number? x) (number? y))
		 (+ x y))
		(else (list x '+ y))))

(make-sum 2 2) ; 4
(make-sum 'x 1) ; '(x + 1)
(make-sum 'x 0) ; 'x

(define (make-product x y)
  (cond ((or (=number? x 0)
			(=number? y 0))
		 0)
		((=number? x 1) y)
		((=number? y 1) x)
		((and (number? x) (number? y))
		 (* x y))
		(else (list x '* y))))

(make-product 2 2) ; 4
(make-product 3 'x) ; '(3 * x)
(make-product 2 '(x + 1)) ; '(2 * (x + 1))
(make-product 0 'x) ; 0

(define (make-exponentiation x n)
  (cond ((=number? n 0) 1)
		((=number? n 1) x)
		(else (list x '** n))))

(make-exponentiation 'x 2) ; '(x ** 2)

(deriv '(((x ** 3) + (3 * x)) + x) 'x) ; '(((3 * (x ** 2)) + 3) + 1)
(deriv '((x * y) * (x + 3)) 'x) ; '((x * y) + (y * (x + 3)))

; exercise 2.58-2
;
; (x + 3 * (x + y + 2))

(define (sum? exp)
  (and (pair? exp) (memq '+ exp)))

(sum? '(x + 1)) ; '(+ 1)
(sum? '(3 * x)) ; #f
(sum? '(3 * (x + 1))) ; #f

(define (take-while p xs)
  (cond ((null? xs) '())
		((p (car xs))
		 (cons (car xs) (take-while p (cdr xs))))
		(else '())))

(define (before-token t exp)
  (take-while
	(lambda (token) (not (eq? t token)))
	exp))

(define (unwrap-one exp)
  (if (= 1 (length exp))
	(car exp)
	exp))

(unwrap-one '(3)) ; 3
(unwrap-one '(x)) ; 'x
(unwrap-one '(x + 2)) ; '(x + 2)

(define (addend exp) 
  (unwrap-one (before-token '+ exp)))

(define (augend exp)
  (unwrap-one (cdr (memq '+ exp))))

(addend '(x + 3 * (x + y + 2))) ; 'x
(augend '(x + 3 * (x + y + 2))) ; '(3 * (x + y + 2))

(addend '(3 * x ** 2 + x + 1)) ; '(3 * x ** 2)
(augend '(3 * x ** 2 + x + 1)) ; '(x + 1)

(define (product? exp)
  (and (pair? exp) (memq '* exp)))

(product? '(x + 1)) ; #f
(product? '(3 * (x + y + 2))) ; '(* (x + y + 2))

(define (multiplier exp)
  (unwrap-one (before-token '* exp)))

(define (multiplicand exp)
  (unwrap-one (cdr (memq '* exp))))

(multiplier '(3 * (x + y + 2))) ; 3
(multiplicand '(3 * (x + y + 2))) ; '((x + y + 2))

(multiplier '(3 * x * y)) ; '(3)
(multiplicand '(3 * x * y)) ; '(x * y)

(deriv '(3 * x ** 2 + x + 1) 'x) ; '((3 * (2 * x)) + 1)
(deriv '(3 * (x + y + 1)) 'x) ; 3
(deriv '(x + y + 1) 'x) ; 1
(deriv '((x * y) * (x + 3)) 'x) ; '((x * y) + (y * (x + 3)))
