#lang sicp

; The elements of programming

; exercise 1.1

10 ; 10
(+ 5 3 4) ; 12
(- 9 1) ; 8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ; 6
(define a 3) 
(define b (+ a 1))
(+ a b (* a b)) ; 19
(= a b) ; #f
(if (and (> b a) (< b (* a b)))
    b
    a) ; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; 16
(+ 2 (if (> b a) b a)) ; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ; 16

; exercise 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
   (* 3 (- 6 2) (- 2 7)))

; exercise 1.3
; define a procedure that takes three numbers as arguments and returns
; the sum of the squares of the two larger numbers.

(define (min-fst? a b c) (and (<= a b) (<= a c)))
(define (sum-square a b) (+ (* a a) (* b b)))
(define (sum-squares a b c)
  (cond ((min-fst? a b c) (sum-square b c))
        ((min-fst? b a c) (sum-square a c))
        ((min-fst? c a b) (sum-square a b))))

; exercise 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; b > 0 -> a + b
; b < 0 -> a - b -> a + abs(b)
; b = 0 -> a - 0 -> a

; exercise 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
; (test 0 (p))
; applicative order: evaluate test, evaluate 0, evaluate p, never return
; normal order: expand test, substitute operands, evaluate and never evaluate (p),
;  return 0.

; exercise 1.6

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if pred then-clause else-clause)
  (cond (pred then-clause)
        (else else-clause)))

;(define (new-sqrt-iter guess x)
;  (new-if (good-enough? guess x)
;          guess
;          (new-sqrt-iter (improve guess x) x)))
; if we rewrite sqrt-iter in terms of new-if, because of the applicative
; evaluation order, new-if, pred, then and else will all be evaluated in turn,
; thus causing the recursive call to be made regardless of the value of pred.
; in a way, branching doesn't happen; real if needs to be lazy in the clauses
; and only evaluate then/else once the value of pred is known. In this case,
; new-sqrt-iter never returns.

; incomplete
; exercise 1.7
; good-enough is defined as |guess^2 - x| < 0.001

(define (unfold p f g seed)
  (if (p seed) (list seed)
      (cons (f seed)
            (unfold p f g (g seed)))))

(define (sqrt-2 guess x)
  (unfold (lambda (t) (good-enough? t x))
          identity
          (lambda (t) (improve t x))
          guess))

; exercise 1.8

(define (qbrt x)
  (define (qb x) (* x x x))
  (define (qb-good? guess x)
    (< (abs (- (qb guess) x)) 0.001))
  (define (qbrt-iter guess x)
    (unfold (lambda (t) (qb-good? t x))
            identity
            (lambda (t) (/ (+ (/ x (square t))
                              (* 2 t))
                           3))
            guess))
  (qbrt-iter 1.0 x))
