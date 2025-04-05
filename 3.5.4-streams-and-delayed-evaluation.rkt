(#%require racket/stream)

(define (stream-scale s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define (stream-map-n proc . argstreams)
  (if (stream-empty? (car argstreams))
    empty-stream
    (stream-cons
      (apply proc (map stream-first argstreams))
      (apply stream-map-n
             (cons proc (map stream-rest argstreams))))))

(define (add-streams s t)
  (stream-map-n + s t))


(define (implicit-integral delayed-integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (stream-scale integrand dt)
                                int))))
  int)

; doesn't work with #lang sicp
(define (solve integral f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve implicit-integral (lambda (y) y) 1 0.001)
            1000) ; 2.716923932235896

; 3.77
(define (explicit-integral delayed-integrand initial-value dt)
  (stream-cons initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-empty? integrand)
                   empty-stream
               (explicit-integral (delay (stream-rest integrand))
                                  (+ (* dt (stream-first integrand))
                                     initial-value)
                                  dt)))))

(stream-ref (solve explicit-integral (lambda (y) y) 1 0.001)
            1000) ; 2.716923932235896

(define (scale s t)
  (stream-map (lambda (x) (* x t))
              (force s)))

; 3.78
(define (solve-2nd a b dt y0 dy0)
  (define integral explicit-integral)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale (delay dy) a)
                           (scale (delay y) b)))
  y)

(stream-ref (solve-2nd 1 -1 0.001 1 1)
            100) ; 1.0998343799830568 (?)

; 3.79
(define (gen-solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map-n f dy y))
  y)

(define (f dy y)
  (+ (* dy 1) (* y -1)))

(stream-ref (gen-solve-2nd f 0.001 1 1)
            100) ; 1.0998343799830568


