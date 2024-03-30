#lang sicp

(define (rand)
  (random 4294967087))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials
                          cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1)
                  trials-passed))))
  (iter trials 0))

; (estimate-pi 10) ; 3.1622776601683795
; (estimate-pi 100) ; 3.188964020716403
; (estimate-pi 1000) ; 3.1285664803324393

; exercise 3.5
(define (rand-range lo hi)
  (+ (* (random) (- hi lo)) lo))

(define (unit-circle-test x y)
  (<= (+ (* x x) (* y y)) 1))

(define (estimate-integral p x1 y1 x2 y2 trials)
  (define (circle-test)
    (let ([x (rand-range x1 x2)]
          [y (rand-range y1 y2)])
      (p x y)))
  (let ([rect-area (* (- x2 x1) (- y2 y1))])
    (* rect-area
       (exact->inexact (monte-carlo trials circle-test)))))

; (estimate-integral unit-circle-test
;                    -1 -1
;                    1 1
;                    100000) ; 3.14708

