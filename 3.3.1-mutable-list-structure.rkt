#lang sicp

; 3.12
(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define x (list 'a 'b))
(define y (list 'c 'd))
; (cdr x) ; '(b)
; (define w (append! x y))
; (cdr x) ; '(b c d)

; 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; (define z (make-cycle (list 'a 'b 'c)))
; (last-pair z) ; never returns
; z = (a . (b . (c . _
;      ^------------'

; 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ([temp (cdr x)])
        (set-cdr! x y)
        (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

; first loop: x = '(a b c d), y = '()
; temp: '(b c d)
; set-cdr! -> x = '(a), then loop
;
; second loop: x = '(b c d), y = '(a)
; temp: '(c d)
; set-cdr! -> x = '(b a), then loop
;
; third loop: x = '(c d), y = '(b a)
; temp: '(d)
; set-cdr! -> x = '(c b a), then loop
;
; fourth loop: x = '(d), y = '(c b a)
; temp: '()
; set-cdr! -> x = '(d c b a)
;
; final loop: x = '(), y = '(d c b a), ta-da!

; (display w) ; '(d c b a)
