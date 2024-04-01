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

; 3.16
(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

; 3) Plain straight list
; (count-pairs '(a b c)) ; 3

; 4) Nested list ((a . (b . ())) . ()), then set the cdr
; to the cdr of the car
;  -> the car: (a b), the cdr: (b . ())
;
; (define t '((a b)))
; (set-cdr! t (cdar t))
; (display t) ; ((a b) b)
; (count-pairs t) ; 4

; 7) Start with a regular list, then point the car of the
; first pair to the car of the second pair, and point
; the car of the second pair to the car of the third pair.
;
; (define u '(a b c))
; (set-car! u (cdr u))
; (display u) ; ((b c) b c)
; (set-car! (cdr u) (cddr u))
; (display u) ; (((c) c) (c) c)
; (count-pairs u) ; 7

; Never return) set the cdr of the last pair to the first
; (define v '(a b c))
; (set-cdr! (last-pair v) v)
; (display v) ; #0=(a b c . #0#)
; I think racket detects this and displays it like so?

; 3.17
; Push each pair into a 'seen' list in the count-pairs-pls execution
; environment as we traverse the structure, and test for membership
; with memq for pointer equality to prevent double counting already
; seen pairs.
(define (count-pairs-pls x)
  (define seen '())
  (define (traverse x)
    (cond ((not (pair? x)) 0)
          ((memq x seen) 0)
          (else
            (begin (set! seen (cons x seen))    ; why not
                   (+ (traverse (car x))
                      (traverse (cdr x))
                      1)))))
  (traverse x))

; (count-pairs-pls '(a b c)) ; 3

; (define t '((a b)))
; (set-cdr! t (cdar t))
; (count-pairs-pls t) ; 3

; (define u '(a b c))
; (set-car! u (cdr u))
; (set-car! (cdr u) (cddr u))
; (count-pairs-pls u) ; 3

; :)

; 3.18
; Same idea as before but taking successive cdrs until we either
; land on an already seen pair and bail, or reach the end of the
; list, in which case the list is free of cycles. Takes space linear
; on the length of the list, since we store every visited pair.
(define (has-cycle? x)
  (define seen '())
  (define (traverse x)
    (cond ((null? x) #f)
          ((memq x seen) #t)
          (else
            (begin (set! seen (cons x seen))
                   (traverse (cdr x))))))
  (traverse x))

; (define ouroboros (make-cycle '(a b c)))
; (has-cycle? ouroboros) ; #t
; (has-cycle? '(1 2 3)) ; #f


