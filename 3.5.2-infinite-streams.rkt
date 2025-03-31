(#%require racket/stream)

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(stream-ref no-sevens 100) ; 117

(define (fibgen a b)
  (stream-cons a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(define (pull s n)
  (stream->list (stream-take s n)))

(pull fibs 10) ; '(0 1 1 2 3 5 8 13 21 34)

(define (sieve s)
  (stream-cons
    (stream-first s)
    (sieve (stream-filter
             (lambda (x) (not (divisible? x (stream-first s))))
             (stream-rest s)))))

(define primes (sieve (integers-starting-from 2)))

(stream-ref primes 50) ; 233

(define ones (stream-cons 1 ones))

(pull ones 5) ; '(1 1 1 1 1)

(define (stream-map-n proc . argstreams)
  (if (stream-empty? (car argstreams)) ; Why check just the first stream?
    empty-stream
    (stream-cons
      (apply proc (map stream-first argstreams))
      (apply stream-map-n
             (cons proc (map stream-rest argstreams))))))

(define (add-streams s t)
  (stream-map-n + s t))

(define integers (stream-cons 1 (add-streams ones integers)))

(pull integers 10) ; '(1 2 3 4 5 6 7 8 9 10)

(define fibs
  (stream-cons 0
               (stream-cons 1
                            (add-streams (stream-rest fibs)
                                         fibs))))

(pull fibs 10) ; '(0 1 1 2 3 5 8 13 21 34)

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define double (stream-cons 1 (scale-stream double 2)))

(pull double 10) ; '(1 2 4 8 16 32 64 128 256 512)

(define primes
  (stream-cons 2
               (stream-filter prime? (integers-starting-from 3))))

(define (square n) (* n n))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-first ps)) n) true)
          ((divisible? n (stream-first ps)) false)
          (else (iter (stream-rest ps)))))
  (iter primes))

(pull primes 10) ; '(2 3 5 7 11 13 17 19 23 29)

; 3.53
; (define s (stream-cons 1 (add-streams s s)))
;
; s is a stream whose first element is 1, and whose second element
; is a promise to add each element of s to itself. That is
; 1, 1 + 1 = 2, 2 + 2 = 4, 4 + 4 = 8 ... => 1, 2, 4, 8 ...

(define s (stream-cons 1 (add-streams s s)))
(pull s 10) ; '(1 2 4 8 16 32 64 128 256 512)

; 3.54
; 0 => 1
; 1 => 1*2 = 2
; 2 => 1*2*3 = 6
; 3 => 1*2*3*4 = 24
(define (mul-streams s t)
  (stream-map-n * s t))

(define factorials
  (stream-cons 1
               (mul-streams factorials (stream-rest integers))))

(pull factorials 5) ; '(1 2 6 24 120)

; 3.55
; s_0, s_0 + s_1, s_0 + s_1 + s_2, ...
(define (partial-sums s)
  (define (iter t acc)
    (let ((acc (+ acc (stream-first t))))
      (stream-cons acc
                   (iter (stream-rest t) acc))))
  (iter s 0))

(pull (partial-sums integers) 10) ; '(1 3 6 10 15 21 28 36 45 55)

; 3.56
(define (merge s t)
  (cond ((stream-empty? s) t)
        ((stream-empty? t) s)
        (else
          (let ((s0 (stream-first s))
                (t0 (stream-first t)))
            (cond ((< s0 t0)
                   (stream-cons s0 (merge (stream-rest s) t)))
                  ((> s0 t0)
                   (stream-cons t0 (merge s (stream-rest t))))
                  (else
                    (stream-cons s0
                                 (merge (stream-rest s)
                                        (stream-rest t)))))))))

(define S (stream-cons 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

(pull S 20) ; '(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36)

; 3.57
; We are talking about this implementation of fibs:
;
;    (define (stream-map-n proc . argstreams)
;      (if (stream-empty? (car argstreams))
;        empty-stream
;        (stream-cons
;          (apply proc (map stream-first argstreams))
;          (apply stream-map-n
;                 (cons proc (map stream-rest argstreams))))))
;
;    (define (add-streams s t)
;      (stream-map-n + s t))
;
;    (define fibs
;      (stream-cons 0
;                   (stream-cons 1
;                                (add-streams (stream-rest fibs)
;                                             fibs))))
;
; With the supposition that stream-cons is a special form with the following
; semantics:
;
;    (define (stream-cons a b)
;      (cons a (delay b))
;
; And with delay a special form using memoization:
;
;    (define (memo-proc proc)
;      (let ((already-run? false)
;            (result false))
;        (lambda ()
;          (if (not already-run?)
;            (begin (set! result (proc))
;                   (set! already-run? true)
;                   result)
;            result))))
;
;    (define (delay <exp>)
;      (memo-proc (lambda () <exp>)))
;
;    (define (force delayed-object)
;      (delayed-object))
;
; Each call to stream-map-n forces different parts of fibs twice. With
; a memoizing delay, the second stream (the one that is further behind)
; does not need to perform an addition to yield its car, for it was forced
; immediately before. So a single addition is done for each new element
; in the stream, except for the first two, for a total of n-2 additions
; for the nth element, if n > 2.
;
; If delay is not memoizing, then stream-map-n does an addition but then
; delays the forcing of two pieces of fibs, each of which is (after the
; second element) a thunk that needs to evaluate two pieces of fibs. This
; results in handwavy O(2^n) such evaluations.

; 3.58
(define (expand num den radix)
  (stream-cons
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

(pull (expand 1 7 10) 20) ; '(1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4)
(pull (expand 3 8 10) 20) ; '(3 7 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

; expand produces the decimals expansion of the fraction num/den.

; 3.59
(define (integrate-series coefficients)
  (define (iter n as)
    (if (stream-empty? as)
      empty-stream
      (stream-cons (* (/ 1 n) (stream-first as))
                   (iter (+ n 1) (stream-rest as)))))
  (iter 1 coefficients))

(pull (integrate-series (stream 5 6 7 8 9)) 5) ; '(5 3 7/3 2 9/5)
(pull (integrate-series fibs) 10) ; '(0 1/2 1/3 1/2 3/5 5/6 8/7 13/8 7/3 17/5)

; The derivative of sin x is cos x, so integrating the series for cos x we
; should get back sin x:
(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

; The derivative of cos x is -sin x, same reasoning
(define cosine-series
  (stream-cons 1 (scale-stream (integrate-series sine-series) -1)))

(pull sine-series 10) ; '(0 1 0 -1/6 0 1/120 0 -1/5040 0 1/362880)
(pull cosine-series 10) ; '(1 0 -1/2 0 1/24 0 -1/720 0 1/40320 0)   (?)

; 3.60
; We have two infinite series of coefficients a = a_0 + ... + a_n and
; b = b_0 + ... + b_n. Their product can be computed in the same way finite
; sums are multiplied. For example
;   (a+b+c) (x+y+z) = a(x+y+z) + (b+c) (x+y+z)
;                   = ax + a(y+z) + (b+c) (x+y+z)
; We can take the first product as an element in the result stream, and the
; subsequent terms as a further stream addition of a scaling and another
; series multiplication.
(define (mul-series a b)
  (let ((a0 (stream-first a))
        (b0 (stream-first b)))
    (stream-cons (* a0 b0)
                 (add-streams (scale-stream (stream-rest b) a0)
                              (mul-series b (stream-rest a))))))


(define sin2 (mul-series sine-series sine-series))
(define cos2 (mul-series cosine-series cosine-series))
(pull sin2 10) ; '(0 0 1 0 -1/3 0 2/45 0 -1/315 0)
(pull cos2 10) ; '(1 0 -1 0 1/3 0 -2/45 0 1/315 0)
(pull (add-streams sin2 cos2) 10) ; '(1 0 0 0 0 0 0 0 0 0) (?)

; 3.61
(define (invert-unit-series s)
  (stream-cons 1
               (scale-stream (mul-series (stream-rest s) (invert-unit-series s))
                             -1)))

(pull (invert-unit-series cosine-series) 10) ; '(1 0 1/2 0 5/24 0 61/720 0 277/8064 0)
(pull (mul-series (invert-unit-series cosine-series)
                  cosine-series) 10) ; '(1 0 0 0 0 0 0 0 0 0)

