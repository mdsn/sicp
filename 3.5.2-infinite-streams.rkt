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
(define partial-sums
  (stream-cons 1
               (add-streams partial-sums (stream-rest integers))))

(pull partial-sums 5) ; '(1 3 6 10 15)

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
