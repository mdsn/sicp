#lang sicp

(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+
               (cc amount (- kinds-of-coins 1))
               (cc (- amount (first-denomination kinds-of-coins))
                   kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; exercise 1.11
; f(n) =
;      | n if n < 3,
;      | f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3

(define (f-rec n)
  (if (< n 3) n
      (+
       (f-rec (- n 1))
       (* 2 (f-rec (- n 2)))
       (* 3 (f-rec (- n 3))))))

; base cases
; n = 0 -> 0
; n = 1 -> 1
; n = 2 -> 2
; n = 3 -> f(2) + 2f(1) + 3f(0) = 2 + 2 = 4
; n = 4 -> f(3) + 2f(2) + 3f(1) = 4 + 4 + 3 = 11
(define (f n)

  ; accumulate f(n) in the c parameter
  (define (f-iter a b c m)
    (cond
      ((< n 3) n)
      ((= m n) c)
      (else (f-iter b c (+ c (* 2 b) (* 3 a)) (+ m 1)))))

  (f-iter 0 1 2 2))

; exercise 1.12 / pascal's triangle
;
(define (fact-iter i k n)
  (if (= i n) (* k n)
      (fact-iter (+ i 1) (* i k) n)))

(define (fact n)
  (fact-iter 1 1 n))

; n * (n - 1) * ... * (n - k + 1), k <= n
(define (fact-n-minus-k n k)
  (fact-iter (- n (- k 1)) 1 n))

(define (C n k)
  (if (= k 0) 1
      (/ (fact-n-minus-k n k)
         (fact k))))

(define (pascal-row n)
  (define (unfold i)
    (if (= i n) (list (C n n))
        (cons (C n i) (unfold (+ i 1)))))
  (if (= n 1) (list 1)
      (unfold 0)))

; display as a side effect of evaluating `iter`'s second argument
(define (pascal n)
  (define (iter i _)
    (if (= i n) '()
        (iter (+ i 1)
              (display (pascal-row i)))))
  (iter 1 0))