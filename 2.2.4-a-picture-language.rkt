#lang sicp
(#%require sicp-pict)


; exercise 2.44
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit-1 painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter)
                        quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4
         (square-of-four identity flip-vert
                         identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz
                                  identity
                                  rotate180
                                  flip-vert)))
    (combine4 (corner-split painter n))))

; exercise 2.45

(define (split first second)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split first second) painter (- n 1))))
          (first painter (second smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

; ==========================

;(define (frame-coord-map frame)
;  (lambda (v)
;    (add-vect
;     (origin-frame frame)
;     (add-vect
;      (scale-vect (xcor-vect v)
;                  (edge1-frame frame))
;      (scale-vect (ycor-vect v)
;                  (edge2-frame frame))))))

; exercise 2.46
;(define (make-vect x y) (cons x y))
;(define xcor-vect car)
;(define ycor-vect cdr)
;
;(define (add-vect u v)
;  (make-vect (+ (xcor-vect u) (xcor-vect v))
;             (+ (ycor-vect u) (ycor-vect v))))
;
;(define (scale-vect v s)
;  (make-vect (* s (xcor-vect v))
;             (* s (ycor-vect v))))
;
;(define (sub-vect u v)
;  (add-vect u (scale-vect v -1)))

; exercise 2.47
;(define (make-frame-list origin edge1 edge2)
;  (list origin edge1 edge2))
;(define origin-frame-list car)
;(define edge1-frame-list cadr)
;(define edge2-frame-list caddr)
;
;(define (make-frame-cons origin edge1 edge2)
;  (cons origin (cons edge1 edge2)))
;(define origin-frame-cons car)
;(define edge1-frame-cons cadr)
;(define edge2-frame-cons cddr)

; exercise 2.48
;(define make-segment cons)
;(define start-segment car)
;(define end-segment cdr)

; exercise 2.49
(define dl (make-vect 0 0))
(define tl (make-vect 0 1))
(define tr (make-vect 1 1))
(define dr (make-vect 1 0))

(define outline
  (segments->painter
   (list (make-segment dl tl)
         (make-segment tl tr)
         (make-segment tr dr)
         (make-segment dr dl))))

(define cross
  (segments->painter
   (list (make-segment dl tr)
         (make-segment tl dr))))

(define top (make-vect 0.5 1))
(define down (make-vect 0.5 0))
(define left (make-vect 0 0.5))
(define right (make-vect 1 0.5))

(define diamond
  (segments->painter
   (list (make-segment left top)
         (make-segment top right)
         (make-segment right down)
         (make-segment down left))))

; exercise 2.50
(define (flip-horizontal painter)
  (transform-painter
   painter
   (make-vect 1 0)
   (make-vect 0 0)
   (make-vect 1 1)))

(define (rotate-180 painter)
  (rotate90 (rotate90 painter)))

(define (rotate-270 painter)
  (rotate90 (rotate90 (rotate90 painter))))

; exercise 2.51
(define (below1 a b)
  (let ((split-point (make-vect 0 0.5)))
    (let ((top (transform-painter a
                                  split-point
                                  (make-vect 1 0.5)
                                  (make-vect 0 1)))
          (down (transform-painter b
                                   (make-vect 0 0)
                                   (make-vect 1 0)
                                   split-point)))
      (lambda (frame)
        (top frame)
        (down frame)))))

(define (below2 a b)
  (rotate90
   (beside
    (rotate270 a)
    (rotate270 b))))

; exercise 2.52
; nah
