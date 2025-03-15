; A queue is a pair whose car points to the front of the queue
; and whose cdr points to the end of the queue.
(define (make-queue)
  (cons '() '()))

(define (front-ptr q)
  (car q))

(define (rear-ptr q)
  (cdr q))

(define (set-front-ptr! q x)
  (set-car! q x))

(define (set-rear-ptr! q x)
  (set-cdr! q x))

(define (empty-queue? q)
  (null? (front-ptr q)))

(define (front-queue q)
  (if (empty-queue? q)
    (error "front-queue: empty queue" q)
    (car (front-ptr q))))

; the rear-ptr points to the end of a list, that is, a pair
; (x nil). To push an element to the end of the queue, create
; a new pair like that and make it the cdr of the current end.
(define (insert-queue! q x)
  (let ((new-elem (cons x '())))
    (cond ((empty-queue? q)
           (set-front-ptr! q new-elem)
           (set-rear-ptr! q new-elem)
           q)
          (else
            (set-cdr! (rear-ptr q) new-elem)
            (set-rear-ptr! q new-elem)
            q))))

(define (delete-queue! q)
  (cond ((empty-queue? q)
         (error "delete-queue!: empty queue" q))
        (else
          (set-front-ptr! q (cdr (front-ptr q)))
          q)))

; (define q (make-queue))
; (insert-queue! q 'a) ; (mcons (mcons 'a '()) (mcons 'a '()))
; (insert-queue! q 'b) ; (mcons (mcons 'a (mcons 'b '())) (mcons 'b '()))
; (delete-queue! q) ; (mcons (mcons 'b '()) (mcons 'b '()))
; (insert-queue! q 'c) ; (mcons (mcons 'b (mcons 'c '())) (mcons 'c '()))
; (insert-queue! q 'd) ; (mcons (mcons 'b (mcons 'c (mcons 'd '()))) (mcons 'd '()))
; (delete-queue! q) ; (mcons (mcons 'c (mcons 'd '())) (mcons 'd '()))
; (display q) ; ((c d) d)

; exercise 3.21
(define (print-queue q)
  (display (front-ptr q)))

(define q1 (make-queue))
(print-queue q1) ; ()
(insert-queue! q1 'a)
(print-queue q1) ; (a)
(insert-queue! q1 'b)
(print-queue q1) ; (a b)
(delete-queue! q1)
(print-queue q1) ; (b)
(delete-queue! q1)
(print-queue q1) ; ()
