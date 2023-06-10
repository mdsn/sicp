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

; Nested mappings
; from 1.28
(define (dec n) (- n 1))
(define (rem-square-check x m)
  (if (and (not (or (= x 1)
                    (= x (- m 1))))
           (= (remainder (* x x) m) 1))
    0
    (remainder (* x x) m)))

(define (expmod-check base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
		 (rem-square-check
		   (expmod-check base (/ exp 2) m)
		   m))
        (else 
          (remainder (* base (expmod-check base (dec exp) m)) 
                    m))))

(define (miller-rabin-test n)
  (define (go b)
    (= (expmod-check b (dec n) n) 1))
  (go (+ 1 (random (dec n)))))

(define (prime? n)
  (define (run times)
    (cond ((= times 0) true)
          ((miller-rabin-test n) (run (dec times)))
          (else false)))
  (run 5))

; ====================

(define (flatmap f xs)
  (accumulate append '() (map f xs)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair)
		(cadr pair)
		(+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
	   (filter prime-sum?
			   (unique-pairs n))))

(prime-sum-pairs 6) ; '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

; ====================

(define (permutations s)
  (if (null? s)
	(list '())
	(flatmap (lambda (x)
			   (map (lambda (p) (cons x p))
					(permutations (remove x s))))
			 s)))

(define (remove item seq)
  (filter (lambda (x) (not (= x item)))
		  seq))

(permutations (list 1 2 3)) ; '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))

; exercise 2.40
(define (unique-pairs n)
  (flatmap
	(lambda (i)
	  (map (lambda (j)
			 (list i j))
		   (enumerate 1 (- i 1))))
	(enumerate 1 n)))

(unique-pairs 6)

; exercise 2.41
(define (sum seq)
  (fold-right + 0 seq))

; from exercise 2.32
(define (triples n s)
  (filter (lambda (t) (= (sum t) s))
		  (ordered-triples n)))

(triples 5 8)
; '((1 3 4) (1 4 3) (3 1 4) (3 4 1)
;   (4 1 3) (4 3 1) (1 2 5) (1 5 2)
;   (2 1 5) (2 5 1) (5 1 2) (5 2 1))

(define (ordered-triples n) 
  (flatmap 
	(lambda (s) (permutations s)) 
	(filter (lambda (s) 
			  (= (length s) 3)) 
			(subsets (enumerate 1 n)))))

(define (subsets s)
  (if (null? s)
	(list '())
	(let ((rest (subsets (cdr s))))
	  (append rest 
			  (map (lambda (x)
					 (cons (car s) x))
				   rest)))))

; exercise 2.42
(define (queens board-size)
  (define (queen-cols k)
	(if (= k 0)
	  (list empty-board)
	  (filter
		(lambda (positions) (safe? k positions))
		(flatmap
		  (lambda (positions) (adjoin board-size k positions))
		  (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())
(define (pos-row pos) (car pos))
(define (pos-col pos) (cadr pos))
(define (make-pos row col) (list row col))
(define (eq-pos? a b) 
  (and (= (pos-row a) (pos-row b)) 
	   (= (pos-col a) (pos-col b))))

(pos-row (make-pos 1 2)) ; 1
(pos-col (make-pos 1 2)) ; 2

(define (adjoin board-size k positions)
  (map (lambda (new-row) 
		 (adjoin-position 
		   new-row 
		   k 
		   positions)) 
	   (enumerate 1 board-size)))

(define (adjoin-position row col positions) 
  (append positions (list (make-pos row col))))

(define (safe? col positions)
  (let ((queen (car (reverse positions)))
		(others (cdr (reverse positions))))
	(and (safe-row? queen others)
		 (safe-diagonal? queen others))))

(define (safe-row? queen others)
  (not
	(any?
	  (map (lambda (other)
			 (= (pos-row other) (pos-row queen)))
		   others))))

(safe-row? (make-pos 1 3) (list (make-pos 1 1) (make-pos 1 2))) ; #f
(safe-row? (make-pos 1 3) (list (make-pos 2 1) (make-pos 3 3))) ; #t

; NB. This assumes we are going right-to-left. That is, `queen` is the rightmost
; queen, and `(car others)` is the one immediately to its left.
(define (safe-diagonal? queen others)
  (not 
	(any? 
	  (map 
		(lambda (pair) 
		  (let* ((i (car pair)) 
				 (other (cadr pair)) 
				 (upper (make-pos (- (pos-row queen) i) 
								  (- (pos-col queen) i))) 
				 (lower (make-pos (+ (pos-row queen) i)
								  (- (pos-col queen) i)))) 
			(or (eq-pos? upper other) 
				(eq-pos? lower other)))) 
		(zip (enumerate 1 (length others)) others)))))

(define (zip a b)
  (cond ((or (null? a) (null? b)) '())
		(else
		  (cons (list (car a) (car b)) 
				(zip (cdr a) (cdr b))))))

(zip (enumerate 1 3) (enumerate 4 6)) ; '((1 4) (2 5) (3 6))
(zip (enumerate 1 10) (enumerate 2 4)) ; '((1 2) (2 3) (3 4))
(zip '() (list 1 2 3)) ; '()

(define (any? seq) 
  ; why does plain `or` not work here, causes syntax error
  (fold-right (lambda (a b) (or a b)) false seq))

(any? (list false false false)) ; #f
(any? (list false true false)) ; #t

(define queen (make-pos 3 3))
(define others (list (make-pos 4 2) (make-pos 2 1)))
(safe-diagonal? queen others) ; #f

(queens 3) ; '()
(queens 4) ; '(((2 1) (4 2) (1 3) (3 4)) ((3 1) (1 2) (4 3) (2 4)))
(queens 8)
; '(((1 1) (5 2) (8 3) (6 4) (3 5) (7 6) (2 7) (4 8))
;   ((1 1) (6 2) (8 3) (3 4) (7 5) (4 6) (2 7) (5 8))
;	...
;   ((8 1) (3 2) (1 3) (6 4) (2 5) (5 6) (7 7) (4 8))
;   ((8 1) (4 2) (1 3) (3 4) (6 5) (2 6) (7 7) (5 8)))

(length (queens 8)) ; 92 :)

; exercise 2.43
; Slow version:
; (flatmap
;   (lambda (new-row)
; 	  (map (lambda (rest-of-queens)
; 		     (adjoin-position
; 			   new-row k rest-of-queens))
; 		   (queen-cols (- k 1))))
;   (enumerate 1 board-size))
;
; This version runs slower because it solves (queen-cols (- k 1)) over
; and over again at the same column for each of the new-row yielded by
; (enumerate 1 board-size), whereas the original version runs it once
; for each column and reuses the computed results for each row.
;
; In the original version there are board-size recursive calls to
; queen-cols, each of which perform board-size calls to adjoin-position.
;
; In the slow version, the call to (queen-cols board-size) results in
; board-size calls to (queen-cols (- board-size 1)), each of which have
; the same result. Say that n = board-size.
