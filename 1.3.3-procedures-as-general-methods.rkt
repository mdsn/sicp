#lang sicp

; Finding roots of equations by the half-interval method

(define (average x y) (/ (+ x y) 2))
(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value)
               (search f neg-point midpoint))
              ((negative? test-value)
               (search f midpoint pos-point))
              (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
            (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0) ; 3.14111328125
(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0) ; 1.89306640625
(half-interval-method cos 0.0 1.0) ; Values are not of opposite sign 0.0 1.0 [,bt for context]

; Finding fixed points of functions

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(fixed-point cos 1.0)    ; 0.7390822985224024
(cos 0.7390822985224024) ; 0.7390870426953322
(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0) ; 1.2587315962971173

; doesn't converge!
#| (define (sqrt x) |#
#|   (fixed-point (lambda (y) (/ x y)) |#
#|                 1.0)) |#

; grep "average damping"
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))
(sqrt 4) ; 2.000000000000002

; 1.35 phi is the fixed point of x -> 1 + 1/x.
;
; From section 1.2.2, phi = (1 + sqrt(5))/2, and phi^2 = phi + 1.
; Let f(x) = 1 + 1/x. Then f (phi) = 1 + 1/phi = (phi+1)/phi = phi^2/phi = phi.

(define (phi)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

(phi) ; 1.6180327868852458

; 1.36 find a solution for x^x = 1000 by finding a
; fixed point of x -> log 1000 / log x

(define (fixed-point-dbg f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess i)
    (display i)
    (display ": ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next (+ i 1)))))
  (try first-guess 1))

(define (x-over-x value)
  (fixed-point-dbg (lambda (x) (/ (log value) (log x)))
                   2.0))

(define (x-over-x-damp value)
  (fixed-point-dbg (lambda (x) (average x (/ (log value) (log x))))
                   2.0))

(x-over-x 1000) ; 34 steps
(x-over-x-damp 1000) ; 9 steps

; 1.37 Continued fractions
;
; f = n_1 / (d_1 + n_2 / (...))

(define (cont-frac n d k)
  (define (cf i)
    (if (= i k)
      0
      (/ (n i) (+ (d i) (cf (+ i 1))))))
  (cf 0))

(define (cont-frac-iter n d k op)
  (define (f i x)
    (/ (n i)
       (op (d i) x)))
  (define (iter i result)
    (if (= i 0)
      (f i result)
      (iter (- i 1) (f i result))))
  (iter k 0))

(define (one-over-phi k)
  (cont-frac-iter (lambda (i) 1.0) 
                  (lambda (i) 1.0) 
                  k
                  +))

(one-over-phi 5) ; 0.6000000000000001
(one-over-phi 10) ; 0.6181818181818182
(one-over-phi 50) ; 0.6180339887498948
(one-over-phi 100) ; 0.6180339887498948

; 1.38 de fractionibus continuis
;
; e - 2 = cont-frac,
; n_i = 1
; d_i = 1, 2, 1, 1, 4, 1, 1, 6 ...
; i <-  1, 4, 7, 10 ...
; i-1   0  3  6  9
; i-1/3 0  1  2  3
; 2i+2  2  4  6  8

(define (e-minus-two)
  ; The d-indices that are different than 1
  (define (special? n) 
    (= 0 (remainder (- n 1) 3)))

  (define (d i) 
    (if (special? i) 
      (+ 2 (* 2 (/ (- i 1) 3))) 
      1))

  (cont-frac-iter (lambda (i) 1.0)
                  d
                  10
                  +))

(e-minus-two)  ; 0.7182818352059925

(define (e) (+ 2 (e-minus-two)))

(e) ; 2.7182818352059925

; 1.39 Continued fraction approximation to the tangent function
;
; tan x = x / (1 - (x^2 / (3 - (x^2 / 5 - (...))))),
; with x in radians.

(define (tan-cf x)
  (define (d i) (+ (* 2 i) 1))
  (define (n i) (if (= i 0)
                  x
                  (* x x)))
  (cont-frac-iter n d 10 -))

(tan-cf 0) ; 0
(tan-cf 3.14159) ; Should be 0 :S
(tan-cf (/ 3.14159 4)) ; 0.9999986732059835
