#lang sicp

(define (identity x) x)
(define (inc x) (+ x 1))
(define (square x) (* x x))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

; 1.29 Simpson's Rule
;
; h = (b - a)/n
; y_k = f(a + kh)
; y0 + 4y1 + 2y2 + 4y3 + 2y4 + .. + 2yn-2 + 4yn-1 + yn
; y_0 + y_n + 4*sum(y_1 + y_3 + ... y_n-1) + 2*sum(y_2 + y_4 + ... y_n-2)

; n is even
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (add2 x) (+ x h h))
  (* (/ h 3)
     (+ (f a) ; y_0 = f(a + 0h)
        (f b) ; y_n = f(a + nh) = f(a + b - a)
        (* 2 (sum f a add2 b))
        (* 4 (sum f (+ a h) add2 b)))))

(define (cube x) (* x x x))
; (simpson cube 0 1.0 1000) ; 0.25000000000000083

; 1.30 Iterative Simpson

(define (sum-iter term a next b) 
  (define (iter a result) 
    (if (> a b)
      result
      (iter (next a) (+ result (term a))))) 
  (iter a 0))

; (sum-iter identity 1 inc 10) ; 55
; (sum identity 1 inc 10) ; 55

; 1.31 Product

(define (product f a next b)
  (if (> a b)
    1
    (* (f a)
       (product f (next a) next b))))

(define (prod-iter f a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (f a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (fact-iter n)
  (prod-iter identity 1 inc n))

; (factorial 5) ; 120
; (fact-iter 5) ; 120

(define (term x)
  (define d (+ 1 (* x 2)))
  (* (/ (* x 2) d)
     (/ (* (+ x 1) 2) d)))

(define (pi/4 n)
  (product term 1.0 inc n))

; (pi/4 100) ; 0.7873446182921491

; 1.32 Accumulate

(define (accumulate combine null-value f a next b)
  (if (> a b)
    null-value
    (combine (f a) 
             (accumulate combine null-value f (next a) next b))))

(define (acc-sum f a next b)
  (accumulate + 0 f a next b))

(define (acc-prod f a next b)
  (accumulate * 1 f a next b))

; (acc-sum identity 1 inc 10) ; 55
; (acc-prod identity 1 inc 5) ; 120

(define (acc-iter combine null-value f a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combine result (f a)))))
  (iter a null-value))

(define (acc-sum-iter f a next b)
  (acc-iter + 0 f a next b))

; (acc-sum-iter identity 1 inc 10) ; 55

; 1.33 Filtered accumulate 

(define (filtered-accumulate combine null-value f a next b filter)
    (if (> a b)
      null-value
      (if (filter a)
        (combine (f a)
                 (filtered-accumulate combine null-value f (next a) next b filter))
        (filtered-accumulate combine null-value f (next a) next b filter))))

(define (sum-even a b)
  (define (even x)
    (= (remainder x 2) 0))
  (filtered-accumulate + 0 identity a inc b even))

; (sum-even 2 10) ; 30

; from 1.28
(define (rem-square-check x m)
  (if (and (not (or (= x 1)
                    (= x (- m 1))))
           (= (remainder (square x) m) 1))
    0
    (remainder (square x) m)))

(define (expmod-check base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (rem-square-check (expmod-check base (/ exp 2) m) m))
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

; 1.33 a
(define (sum-prime-squares a b)
  (filtered-accumulate + 0 square a inc b prime?))

; (sum-prime-squares 2 10) ; 87

; 1.33 b

; from 1.2.5
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-coprimes n)
  (define (filter x) 
    (= (gcd x n) 1))
  (filtered-accumulate * 1 identity 1 inc n filter))

; (gcd 1 10) ; 1 <
; (gcd 2 10) ; 2
; (gcd 3 10) ; 1 <
; (gcd 4 10) ; 2
; (gcd 5 10) ; 5
; (gcd 6 10) ; 2
; (gcd 7 10) ; 1 <
; (gcd 8 10) ; 2
; (gcd 9 10) ; 1 <
; (gcd 10 10) ; 10

; (product-coprimes 10) ; 189

; 1.34
(define (f g) (g 2))

(f f)
; application: not a procedure;
;  expected a procedure that can be applied to arguments
;   given: 2
; (f f)
; = (f 2)
; = (2 2) '2' can't be applied to arguments, it's not a procedure.

