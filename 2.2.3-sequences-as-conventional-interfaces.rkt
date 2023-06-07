#lang sicp

(define (enumerate lo hi) 
  (if (> lo hi) 
	'() 
	(cons lo (enumerate (+ lo 1) hi))))

(enumerate 2 7) ; '(2 3 4 5 6 7)

(define (accumulate op zero seq)
  (if (null? seq)
	zero
	(op (car seq) (accumulate op zero (cdr seq)))))

(accumulate + 0 (enumerate 2 5)) ; 14

(define (filter pred? seq)
  (cond ((null? seq) '())
		((pred? (car seq))
		 (cons (car seq)
			   (filter pred? (cdr seq))))
		(else
		  (filter pred? (cdr seq)))))

(filter even? (enumerate 1 10)) ; '(2 4 6 8 10)

; exercise 2.33

(define (my-map f seq)
  (accumulate (lambda (x y) (cons (f x) y)) '() seq))

(my-map (lambda (x) (* x x)) (enumerate 1 5)) ; '(1 4 9 16 25)

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(my-append (enumerate 1 3) (enumerate 4 6)) ; '(1 2 3 4 5 6)

(define (length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

(length (enumerate 3 8)) ; 6

; exercise 2.34
(define (horner-eval x coefficients)
  (accumulate (lambda (coeff higher-terms)
				(+ coeff (* x higher-terms)))
			  0
			  coefficients))

; evaluate 1 + 3x + 5x^3 + x^5 at x=2
(horner-eval 2 (list 1 3 0 5 0 1)) ; 79

; exercise 2.35
; from 2.2.2
; (define (count-leaves t) 
;   (cond ((null? t) 0)
; 		((not (pair? t)) 1)
; 		(else (+ (count-leaves (car x))
; 				 (count-leaves (cdr x))))))
(define (enumerate-tree t)
  (cond ((null? t) '())
		((not (pair? t)) (list t))
		(else (append
				(enumerate-tree (car t))
				(enumerate-tree (cdr t))))))

(define (count-leaves t)
  (accumulate +
			  0
			  (map (lambda (x) 1) (enumerate-tree t))))

(define tree (list 1
				   (list 2 (list 3 4) 5)
				   (list 6 7)))

(enumerate-tree tree) ; '(1 2 3 4 5 6 7)
(count-leaves tree) ; 7

; 2.36
(define (accumulate-n op zero seqs)
  (if (null? (car seqs))
	'()
	(cons (accumulate op zero (map car seqs))
		  (accumulate-n op zero (map cdr seqs)))))

(define seq (list (list 1 2 3)
				  (list 4 5 6)
				  (list 7 8 9)
				  (list 10 11 12)))
(map car seq) ; '(1 4 7 10)
(map cdr seq) ; '((2 3) (5 6) (8 9) (11 12))

(accumulate-n + 0 seq) ; '(22 26 30)

; exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define v (list 1 2 3 4))
(define w (list 4 5 6 6))
(define u (list 6 7 8 9))
(define m (list v w u))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(matrix-*-vector m v) ; '(30 56 80)

(define (transpose m)
  (accumulate-n cons '() m))

(transpose m) ; '((1 4 6) (2 5 7) (3 6 8) (4 6 9))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
	(map (lambda (row) (matrix-*-vector cols row)) m)))

(define m
  (list (list 1 2 3)
		(list 4 5 6)
		(list 7 8 9)))

(matrix-*-matrix m m) ; '((30 36 42) (66 81 96) (102 126 150))

; exercise 2.38
(define (fold-left op zero seq)
  (define (iter result rest)
	(if (null? rest)
	  result
	  (iter (op result (car rest))
			(cdr rest))))
  (iter zero seq))

(define fold-right accumulate)

(fold-right / 1 (list 1 2 3)) ; 3/2
(fold-left / 1 (list 1 2 3)) ; 1/6
(fold-right list '() (list 1 2 3)) ; '(1 (2 (3 ())))
(fold-left list '() (list 1 2 3)) ; '(((() 1) 2) 3)

(fold-left + 0 (list 1 2 3)) ; 6
(fold-right + 0 (list 1 2 3)) ; 6
(fold-left * 1 (list 1 2 3)) ; 6
(fold-right * 1 (list 1 2 3)) ; 6

; Op needs to be commutative for foldl and foldr to give the same result.

; exercise 2.39
(define (reverse seq)
  (fold-right
	(lambda (x acc) (append acc (list x))) '() seq))

(reverse (list 1 2 3)) ; '(3 2 1)

(define (reverse seq)
  (fold-left
	(lambda (acc x) (append (list x) acc)) '() seq))

(reverse (list 1 2 3)) ; '(3 2 1)
