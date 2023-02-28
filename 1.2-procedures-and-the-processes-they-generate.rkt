#lang sicp

; Procedures and the processes they generate

; exercise 1.9
; (define (+ a b)
;   (if (= a 0) b (inc (+ (dec a) b))))
; (+ 4 5)
; (inc (+ (dec 4) 5))
; (inc (inc (+ (dec 3) 5)))
; (inc (inc (inc (+ (dec 2) 5))))
; (inc (inc (inc (inc (+ (dec 1) 5)))))
; (inc (inc (inc (inc (5)))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9
; Linear recursive process

; (define (+ a b)
;   (if (= a 0) b (+ (dec a) (inc b))))
; (+ 4 5)
; (+ (dec 4) (inc 5))
; (+ (dec 3) (inc 6))
; (+ (dec 2) (inc 7))
; (+ (dec 1) (inc 8))
; (+ 0 9)
; 9
; Linear iterative process

; exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define
  (k n) (* 5 n n))

; (f n) = (A 0 n) = (* 2 n); f computes 2n

; (g 3)
; (A 1 3)
; (A 0 (A 1 2))
; (* 2 (A 1 2))
; (* 2 (A 0 (A 1 1)))
; (* 2 (* 2 (A 1 1)))
; (* 2 (* 2 2)); g computes 2^n

; (h 3)
; (A 2 3)
; (A 1 (A 2 2))
; (A 0 (A 1 (- (A 2 2) 1)))
; (* 2 (A 1 (- (A 2 2) 1)))
; (* 2 (A 1 (- (A 1 (A 2 1)) 1)))
; (* 2 (A 1 (- (A 1 2) 1)))
; (* 2 (A 1 (- (A 0 (A 1 1)) 1)))
; (* 2 (A 1 (- (* 2 2) 1)))
; (* 2 (A 1 3))
; (* 2 (A 0 (A 1 2)))
; (* 2 (* 2 (A 1 2)))
; (* 2 (* 2 (A 0 (A 1 1))))
; (* 2 (* 2 (* 2 (2))))
; 16
; ??

