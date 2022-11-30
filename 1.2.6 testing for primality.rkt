#lang sicp

(define (id x) x)

(define (square n)
  (* n n))

(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor f n)
  (f n 2))

(define (slow-find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (slow-find-divisor n (+ test-divisor 1)))))

(define (find-divisor n test-divisor)
  (define (next n)
    (if (= n 2)
        3
        (+ n 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

; fermat's little theorem
; let n be a prime, a < n an integer,
; then a^n === a (mod m).

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (dec exp) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (inc (random (dec n)))))

; primality tests

(define (fastest-prime? n)
  (define (run times)
    (cond ((= times 0) true)
          ((fermat-test n) (run (dec times)))
          (else false)))
  (run 5))

(define (fast-prime? n)
  (= n (smallest-divisor find-divisor n)))

(define (prime? n)
  (= n (smallest-divisor slow-find-divisor n)))

; exercise 1.21
; "1.2.6 testing for primality.rkt"> (smallest-divisor 199)
; 199
; "1.2.6 testing for primality.rkt"> (smallest-divisor 1999)
; 1999
; "1.2.6 testing for primality.rkt"> (smallest-divisor 19999)
; 7

; exercise 1.22

(define (elapsed start)
  (- (runtime) start))

; search for m primes larger than n
; (start-search 1000 3)
(define (start-search n m)
  (define (search n m start)
    (if (prime? n)
        (report n (dec m) start)
        (search (+ n 2) m (runtime))))

  (define (report n m start)
    (display n)
    (display " *** ")
    (display (elapsed start))
    (display "\n")
    (if (< 0 m)
        (search (+ n 2) m (runtime))))

  (if (even? n)
      (search (inc n) m (runtime))
      (search n m (runtime))))

(define (timed-test f p start)
  (f p)
  (elapsed start))

; run the timed tests for a prime p
(define (run-test p)
  (list p
        (timed-test prime? p (runtime))
        (timed-test fast-prime? p (runtime))
        (timed-test fastest-prime? p (runtime))))

(define (foldl f x xs)
  (if (null? xs) x
      (foldl f (f x (car xs)) (cdr xs))))

(define (unfold p f g seed)
  (if (p seed) (list seed)
      (cons (f seed)
            (unfold p f g (g seed)))))

(define (range n)
  (unfold (lambda (x) (= x (- n 1)))
          id
          inc
          0))

; run the prime tests for a prime p a number of times
(define (average-test-runs p times)
  (define (second xs) (car (cdr xs)))
  (define (third xs) (car (cdr (cdr xs))))
  (define (fourth xs) (car (cdr (cdr (cdr xs)))))

  ; a and b are (prime slow fast fastest)
  (define (add a b)
    (list (car a)
          (+ (second a) (second b))
          (+ (third a) (third b))
          (+ (fourth a) (fourth b))))

  (define (avg results)
    (cons (car results)
          (map (lambda (x) (exact->inexact (/ x times)))
               (cdr results))))

  (avg (foldl add
              (list p 0 0 0)
              (map (lambda (_) (run-test p)) (range times)))))

(define (run-tests)
  (define (avg p) (average-test-runs p 1000))
  (define (collect-results)
    (map avg
         '(1009 1013 1019
                10007 10009 10037
                100003 100019 100043
                1000003 1000033 1000037
                10000019 10000079 10000103
                100000007 100000037 100000039
                1000000007 1000000009 1000000021)))
  (display (collect-results)))


; exercise 1.23
; (run-tests)
; p slow fast %
; 1009 0.985 0.541 54.92385787
; 1013 0.755 0.458 60.66225166
; 1019 0.711 0.522 73.41772152
; 10007 1.694 0.925 54.60448642
; 10009 1.985 1.113 56.07052897
; 10037 1.939 1.2 61.88757091
; 100003 6.272 3.306 52.71045918
; 100019 6.378 2.814 44.12041392
; 100043 5.368 2.839 52.88748137
; 1000003 16.784 8.671 51.66229743
; 1000033 17.363 8.928 51.41968554
; 1000037 17.643 8.96 50.78501389
; 10000019 56.198 28.691 51.05341827
; 10000079 54.386 27.81 51.13448314
; 10000103 54.836 28.255 51.52636954
; 100000007 173.948 88.965 51.14459494
; 100000037 178.75 91.445 51.15804196
; 100000039 176.124 89.765 50.96693239
; 1000000007 550.502 281.62 51.15694403
; 1000000009 556.195 284.372 51.12811154
; 1000000021 567.012 289.51 51.05888412

; The third column is what percentage of the slow time the fast time represents.
; At a thousand iterations average, the code that skips the even numbers when checking divisors runs
; in roughly half the time the naive algorithm does, as expected. It is consistently ~1% slower,
; maybe due to the extra conditional and function call.

; exercise 1.24

; 1009    2.574
; 1013    3.026
; 1019    3.29
; ...
; 1000003 4.126
; 1000033 4.425
; 1000037 4.181

; 1000 = 10^3
; 1000000 = 10^6
; we're interested in the behavior of the Fermat prime test, which has logarithmic running time,
; as we test primes in the order of 10^n. log(10^n) = n log(10), so there is a linear growth
; between the different triplets of primes.

; exercise 1.25

; The old expmod.
; (define (expmod base exp m)
;   (cond ((= exp 0) 1)
;         ((even? exp)
;          (remainder (square (expmod base (/ exp 2) m))
;                     m))
;         (else
;          (remainder (* base (expmod base (dec exp) m))
;                     m))))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (alyssa-expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (alyssa-fermat-test n)
  (define (try-it a)
    (= (alyssa-expmod a n n) a))
  (try-it (inc (random (dec n)))))

(define (alyssa-prime? n)
  (define (run times)
    (cond ((= times 0) true)
          ((alyssa-fermat-test n) (run (dec times)))
          (else false)))
  (run 5))

; The difference in expmods comes down to the difference between the fast-expt and modulo-m
; exponentiation. In expmod, each recursive call returns a number modulo m, thus for any base b
; we will get the following base cases:
;   expmod b 0 m -> 1
;   expmod b 1 m -> remainder (* b (expmod b 0 m)) m    -> remainder b m = t
;   expmod b 2 m -> remainder (square (expmod b 1 m)) m -> remainder (square t) m, with t <= m
;   .. and so on, always squaring numbers <= m.

; With fast-expt the successive squaring is not kept in check. As a result, when we run Alyssa's
; prime test on e.g. p = 1000000021 we are calling Alyssa's expmod with some random base b, with
; 1 <= b < p, which results in fast-exp b p, with the following base cases:
;   fast-expt b 0 -> 1
;   fast-expt b 1 -> * b (fast-expt b 0) -> b
;   fast-expt b 2 -> square (fast-expt b 1) -> square b
;   fast-expct b 3 -> square (fast-expt b 2) -> square (square b)
;   .. and so on, constantly squaring a bigger and bigger number.

; The large integer multiplications seem to start taking an inordinate amount of time, even if the
; fast-expt function is as logarithmic as the expmod function.

; exercise 1.26

; This is the original expmod:
;   (remainder (square (expmod base (/ exp 2) m)) m))
; And this is Louis' one:
;   (remainder (* (expmod base (/ exp 2) m)
;                 (expmod base (/ exp 2) m))
;              m))

; Because of the applicative evaluation order, in the first case before going into the evaluation
; of square, the recursive expmod call is made. By the time square is being evaluated, it has a fully
; evaluated argument to multiply with itself. That is to say, a single call to expmod was made.
; In the second scenario, the explicit multiplication is again delayed but this time until the
; evaluation of each argument is finished. This means that, literally as it is displayed in the code,
; there will be two (identical) calls to expmod per recursive call. This results in exponential growth
; of the number of recursive calls, which cancels out the logarithmic gains of halving the exponent.
; As a result, the final running time ends up being linear.

; exercise 1.27

; (define (fermat-test n)
;   (define (try-it a)
;     (= (expmod a n n) a))
;   (try-it (inc (random (dec n)))))

(define (foolable-test n)
  (define (test a)
    (= (expmod a n n) a))
  (foldl (lambda (x y) (and x y))
         true
         (map test (cdr (range n)))))

; Some Carmichael numbers:
; "1.2.6 testing for primality.rkt"> (foolable-test 561)
; #t
; "1.2.6 testing for primality.rkt"> (foolable-test 1105)
; #t
; "1.2.6 testing for primality.rkt"> (foolable-test 1729)
; #t
; "1.2.6 testing for primality.rkt"> (foolable-test 2465)
; #t
; "1.2.6 testing for primality.rkt"> (foolable-test 2821)
; #t
; "1.2.6 testing for primality.rkt"> (foolable-test 6601)
; #t

; exercise 1.28 / the Miller-Rabin test

; non-trivial square root of 1 modulo n: a number t != 1 y t != n-1,
; such that t^2 === 1 mod n.

(define (miller-rabin-expmod base exp m)

  (define (check x)
    (if (and (not (or (= x 1)
                      (= x (dec m))))
             (= (remainder (square x) m) 1))
        0
        (remainder (square x) m)))


  (cond ((= exp 0) 1)
        ((even? exp)
         (check (miller-rabin-expmod base (/ exp 2) m)))
        (else
         (remainder (* base (miller-rabin-expmod base (dec exp) m))
                    m))))

(define (miller-rabin-test n)
  (define (go b)
    (= (miller-rabin-expmod b (dec n) n) 1))
  (go (+ 1 (random (dec n)))))
