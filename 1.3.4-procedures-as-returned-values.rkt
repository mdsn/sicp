#lang sicp

; from 1.3.3 procedures as general methods
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


(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10) ; 55

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (cube x) (* x x x))

((deriv cube) 5) ; 75.00014999664018

; Newton's method

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; compute sqrt by finding a 0 of y -> y^2 - x
(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(sqrt 9) ; 3.000000000000002

; generalized fixed point of a transformed function
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

; rewrite of the average-damp transformed method, which looks for a
; fixed point of the average-damped version of y -> x / y.
(define (sqrt-avgdamp x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(sqrt-avgdamp 25) ; 5.0

; rewrite of sqrt using newton's method
(define (sqrt-newton x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

(sqrt-newton 49) ; 7.000000000000103

; 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic 1 2 3) 1.0) ; -1.2756822036498454

; 1.41
(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

((double inc) 1) ; 3

(((double (double double)) inc) 5) ; 21

(((double double) inc) 5) ; 9
(((double double) inc) 3) ; 7
(((double double) inc) 1) ; 5

; (double inc) x = 2 + x
; ((double double) inc) x = (2*2) + x 
; ((double (double double)) inc) x = (2*2)^2 + x

; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6) ; 49

; 1.43
(define (repeated f n)
  (if (= n 1)
    f
    (compose f (repeated f (- n 1)))))

((repeated square 2) 5) ; 625

; 1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))  ; ?? how to test this

; 1.45
; nth root -> fixed point of repeated average damping of y -> x/y^ (n-1).

(define (nth-average-damp n)
  (repeated average-damp n))

; nth root of x with m repeated applications of average-damp
(define (nth-root x n m)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (nth-average-damp m)
                            1.0))

(nth-root 59049 5 2) ; 8.99999900297224
(nth-root 59049 5 3) ; 9.00000287051618
(nth-root 28629151 5 2) ; 31.000000745214237
(nth-root 28629151 5 3) ; 31.00000388252769 <- less precision??
(nth-root 729 6 2) ; 2.999996785898161
(nth-root 729 6 3) ; 3.000001570305295
; (nth-root 10 10 2) ; doesnt converge, never returns
(nth-root 10 10 3) ; 1.2589247156514267

; 1.46

(define (iterative-improve good-enough? improve)
  (define (try guess)
    (if (good-enough? guess)
      guess
      (try (improve guess))))
  (lambda (guess) (try guess)))

(define (sqrt-iterative-improve x)
  (define (good-enough? y)
    (< (abs (- (square y) x)) dx))

  (define (improve y)
    (average y (/ x y)))

  ((iterative-improve good-enough? improve) x))

(sqrt-iterative-improve 4.0) ; 2.0000000929222947
(sqrt-iterative-improve 9.0) ; 3.000000001396984
(sqrt-iterative-improve 25.0) ; 5.000000000053722

(define (fixed-point-iterative-improve f first-guess)
  (define (good-enough? x)
    (< (abs (- x (f x))) dx))
  
  (define (improve x)
    (f x))

  ((iterative-improve good-enough? improve) first-guess))

(define (newtons-method-iterative-improve g guess)
  (fixed-point-iterative-improve (newton-transform g) guess))

(define (sqrt-newton-iterative-improve x)
  (newtons-method-iterative-improve (lambda (y) (- (square y) x))
                                    1.0))

(sqrt-newton-iterative-improve 9.0) ; 3.0000000015508212
(sqrt-newton-iterative-improve 16.0) ; 4.000000639575587
(sqrt-newton-iterative-improve 25.0) ; 5.0000000000769855
