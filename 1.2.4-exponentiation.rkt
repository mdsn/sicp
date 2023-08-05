#lang scheme

; exercise 1.16

(define (square n) (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expt b n)
  (define (iter b n a)
    (cond ((= n 1) a)
          ((even? n) (iter b (/ n 2) (* a (square b))))
          (else (iter b (- n 1) (* a b)))))
  (iter b n 1))

; exercise 1.17
(define (double n) (* n 2))
(define (halve n) (quotient n 2))

; 3*20 = 2*(3*10) = 2*(2*(3*5))
; 3*21 = 3 + 3*20 = 3 + 2*(2*(3*5))
;      = 3 + 2*(2*(3 + 3*4))
;      = 3 + 2*(2*(3 + 2*(3*2)))
;      = 3 + 2*(2*(3 + 2*(2*(3*1))))

(define (mul a b)
  (cond ((= b 1) a)                             ; a*1 = a
        ((even? b) (double (mul a (halve b))))  ; b=2m => a*b = a2m = 2am = 2a(b/2)
        (else (+ a (mul a (- b 1))))))          ; b-1 = 2m => a*b = a + a*(b-1) = a + a2m = a + 2a(b/2)

; a * b,
;   if b = 1, a*b=a
;   if b is even, a*b = a * double(halve b) = double(a * halve b)
;   if b is odd, a*b = a * (b-1) + a, (b-1) is even => a(b-1) = double(a * halve b-1)

; exercise 1.18, the russian peasant method
; repeatedly halve first factor, disregarding remainder
; 20 * 3
; 20 -> 10 -> 5 -> 2 -> 1
; repeatedly double second factor same number of times
; 3  -> 6  -> 12-> 24 -> 48
; paired up, remove those were first factor is even
; .. -> .. -> 12 -> .. -> 48
; add the remaining ones, 12 + 48 = 60 boom

(define (mul2 a b)
  (define (iter a b n)
    (cond ((= a 1) (+ n b))
          ((even? a) (iter (halve a) (double b) n))
          (else (iter (halve a) (double b) (+ n b)))))
  (iter a b 0))

; exercise 1.19
; Tpq (a,b) -> (bq+aq+ap, bp + aq)
;
; Tpq^2 (a,b) = Tpq . Tpq (a,b) = Tpq (bq+aq+ap, bp+aq)
;   let s = bq+aq+ap, t = bp+aq
;   then Tpq (bq+aq+ap, bp+aq) = Tpq (s, t) = (tq+sq+sp, tp+sq), with
;   tq+sq+sp    = q(bp+aq)      + q(bq+aq+ap)   + p(bq+aq+ap)
;               = bpq + aq^2    + bq^2  + aq^2  + apq   + bpq + apq + ap^2
;               = 2aq^2         + bq^2  + 2apq  + 2bpq  + ap^2
;               = b(q^2 + 2pq)  + a(q^2 + 2pq)  + a(q^2 + p^2),
;
;   tp+sq       = p(bp+aq)      + q(bq+aq+ap)
;               = bp^2  + aqp   + bq^2  + aq^2  + apq
;               = b(p^2 + q^2)  + a(q^2 + 2pq).
;
;   => p' = p^2 + q^2 and q' = q^2 + 2pq.

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (square q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
