(#%require racket)
(#%require racket/stream)

(define (rand)
  (random 4294967087))

(define random-numbers
  (stream-cons (rand)
               (stream-map (lambda (x) (rand)) random-numbers)))

(stream-ref random-numbers 10) ; 3137568905

(define (map-successive-pairs f s)
  (stream-cons (f (stream-first s) (stream-first (stream-rest s)))
               (map-successive-pairs f (stream-rest (stream-rest s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (stream-cons
      (/ passed (+ passed failed))
      (monte-carlo (stream-rest experiment-stream)
                   passed
                   failed)))
  (if (stream-first experiment-stream)
    (next (+ passed 1) failed)
    (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))

(stream-ref pi 10) ; 2.8722813232690143
(stream-ref pi 100) ; 3.232379288089343
(stream-ref pi 1000) ; 3.1403931227661994
(stream-ref pi 10000) ; 3.13923878790945 :o

; 3.81
(define (random-stream x0 requests)
  (define MAX 4294967087)
  (if (stream-empty? requests)
    empty-stream
    (let ((r (stream-first requests)))
      (cond ((eq? r 'generate)
             (stream-cons (random MAX)
                          (random-stream x0 (stream-rest requests))))
            ((eq? r 'reset)
             (random-seed x0)
             (random-stream x0 (stream-rest requests)))))))

(define commands (stream 'generate 'generate 'reset 'generate 'generate 'reset 'generate 'generate))
(stream->list (random-stream 1337 commands))
; '(4015026440 2546264201 <reset>
;   1247326341 768927287 <reset>
;   1247326341 768927287)

; 3.82
(define (coord) (- (* (random) 2) 1))
(define random-coords
  (stream-cons (cons (coord) (coord))
               (stream-map (lambda (x) (cons (coord) (coord)))
                           random-coords)))

(stream->list (stream-take random-coords 5))
; '((-0.030780133419266775 . 0.5633642201264757)
;   (0.21562492355005447 . 0.5778046269396699)
;   (0.34205438968430135 . -0.4968204203384573)
;   (0.2455808187557409 . 0.09706494635658092)
;   (-0.5123507940603804 . -0.48524427854707686))

(define (inside x y) (<= (+ (* x x) (* y y)) 1))
(stream->list (stream-map (lambda (p) (inside (car p) (cdr p)))
                          (stream-take random-coords 10))) ; '(#t #t #t #t #t #f #t #t #t #f)

(define unit-circle-test
  (stream-map (lambda (p) (inside (car p) (cdr p)))
              random-coords))

(stream->list (stream-take unit-circle-test 10)) ; '(#t #t #t #t #t #t #f #t #t #t)

(define (estimate-integral-stream)
  (let ((rect-area 4))
    (stream-map (lambda (result) (exact->inexact (* rect-area result)))
                (monte-carlo unit-circle-test 0 0))))

(stream-ref (estimate-integral-stream) 10) ; 3.272727272727273
(stream-ref (estimate-integral-stream) 1000) ; 3.088911088911089
(stream-ref (estimate-integral-stream) 100000) ; 3.140168598314017
