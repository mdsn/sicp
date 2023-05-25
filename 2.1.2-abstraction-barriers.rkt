#lang sicp

; exercise 2.2

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (define (avg a b)
	(/ (+ a b) 2))
  (make-point (avg (x-point (start-segment s)) (x-point (end-segment s)))
			  (avg (y-point (start-segment s)) (y-point (end-segment s)))))

(define p0 (make-point 1 1))
(define p1 (make-point 1 5))
(define s (make-segment p0 p1))
(print-point (midpoint-segment s)) ; (1, 3)

; exercise 2.3
; To make perimeter and area agnostic to the representation of the rectangle,
; we can construct each rectangle as a pair where the second element is its
; particular selector for dimensions. This selector knows how to compute or
; retrieve its dimensions pair, which provides a nice abstraction barrier for
; the higher level calculations to be performed.
; The `dimensions` selector abstracts over the representation by blindly taking
; the concrete selector via cdr and applying it to the rectangle itself.

(define (make-dimensions w h)
  (cons w h))

(define (width-dimensions d)
  (car d))

(define (height-dimensions d)
  (cdr d))

; Rectangle representation with width, height components.
; The dimensions selector is trivial, it just pops back car
(define (make-rectangle-dimensions width height)
  (define (dimensions-rectangle r) (car r))
  (cons (make-dimensions width height) dimensions-rectangle))

; Rectangle with two-points representation.
; The dimensions selector pops each point, calculates width/height on the fly
; and constructs a dimensions pair.
(define (make-rectangle-points p0 p1)
  (define (dimensions-rectangle r)
	; let* is similar to let, but the bindings are performed sequentially from left to right.
	; Thus the second binding is done in an environment in which the first binding is visible, and so on.
	(let* ((p0 (car (car r)))
		   (p1 (cdr (car r)))
		   (width (abs (- (x-point p1) (x-point p0))))
		   (height (abs (- (y-point p1) (y-point p0)))))
	  (make-dimensions width height)))
  (cons (cons p0 p1) dimensions-rectangle))

; Generic selector for dimensions, assumes the dimensions selector is in cdr and
; is a function : r -> dimensions.
(define (dimensions r)
  ((cdr r) r))

; Representation-agnostic individual dimension selectors. At this level of
; abstraction we are already unaware of the representation of the rectangle.
(define (height-rectangle r)
  (height-dimensions (dimensions r)))

(define (width-rectangle r)
  (width-dimensions (dimensions r)))

; Representation-agnostic area and perimeter.
(define (area-rectangle r)
  (* (height-rectangle r) (width-rectangle r)))

(define (perimeter-rectangle r)
  (+ (* 2 (height-rectangle r))
	 (* 2 (width-rectangle r))))

(define r0 (make-rectangle-dimensions 2 3))
(height-rectangle r0) ; 3
(width-rectangle r0) ; 2
(area-rectangle r0) ; 6
(perimeter-rectangle r0) ; 10

(define r1 (make-rectangle-points (make-point 2 1) (make-point 4 4)))
(height-rectangle r1) ; 3
(width-rectangle r1) ; 2
(area-rectangle r1) ; 6
(perimeter-rectangle r1) ; 10
