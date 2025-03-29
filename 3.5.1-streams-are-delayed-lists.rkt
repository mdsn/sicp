(require racket/stream)

; 3.50
(define (stream-map-n proc . argstreams)
  (if (stream-empty? (car argstreams)) ; Why check just the first stream?
    empty-stream
    (stream-cons
      (apply proc (map stream-first argstreams))
      (apply stream-map-n
             (cons proc (map stream-rest argstreams))))))

; 3.51
(define (stream-enumerate-interval a b)
  (if (> a b)
    empty-stream
    (stream-cons a
                 (stream-enumerate-interval (+ a 1) b))))

(define x (stream-enumerate-interval 1 10))
(display x) ; #<stream>

(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

(stream-ref x 5) ; 55
(stream-ref x 7) ; 77

(stream-first x) ; 00

(stream-first (stream-enumerate-interval 0 10)) ; 0

; 3.52
(define (display-stream s)
  (stream-for-each display-line s))

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))

(stream-ref y 7) ; 136

(display-stream y)
; 6
; 10
; 28
; 36
; 66
; 78
; 120
; 136
; 190
; 210

(display-stream z)
; 10
; 15
; 45
; 55
; 105
; 120
; 190
; 210

(display-stream seq)
; 1
; 3
; 6
; 10
; 15
; 21
; 28
; 36
; 45
; 55
; 66
; 78
; 91
; 105
; 120
; 136
; 153
; 171
; 190
; 210

