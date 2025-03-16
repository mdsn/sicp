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

; 3.22
; the procedural representation of a queue is a closure--instead of keeping
; the front and rear pointers in a pair, they are bound in the environment in
; which dispatch executes. The operations have direct access to them because
; they are defined in the same scope, so there is no need to abstract set-front-ptr!
; or set-rear-ptr! away.
(define (make-queue-proc)
  (let* ((front-ptr '())
         (rear-ptr '())
         (empty-queue?
          (lambda () (null? front-ptr)))
         (front-queue
          (lambda ()
            (if (empty-queue?)
              (error "front-queue: empty queue")
              (car front-ptr))))
         (insert-queue  ; how to return the modified queue here?
          (lambda (x)
            (let ((new-elem (cons x '())))
             (cond ((empty-queue?)
                    (set! front-ptr new-elem)
                    (set! rear-ptr new-elem))
                   (else
                     (set-cdr! rear-ptr new-elem)
                     (set! rear-ptr new-elem))))))
         (delete-queue!
          (lambda ()
            (cond ((empty-queue?)
                   (error "delete-queue!: empty queue"))
                  (else
                    (set! front-ptr (cdr front-ptr)))))))

    (define (dispatch m)
      (cond ((eq? m 'insert) insert-queue)
            ((eq? m 'delete) (delete-queue!))
            ((eq? m 'empty) (empty-queue?))
            ((eq? m 'front) (front-queue))))
    dispatch))

(define (empty-queue-proc? q) (q 'empty))
(define (front-queue-proc q) (q 'front))
(define (insert-queue-proc! q x) ((q 'insert) x))
(define (delete-queue-proc! q) (q 'delete))

(define q2 (make-queue-proc))
(empty-queue-proc? q2) ; #t
(insert-queue-proc! q2 'a)
(insert-queue-proc! q2 'b)
(front-queue-proc q2) ; 'a
(delete-queue-proc! q2)
(front-queue-proc q2) ; 'b
(delete-queue-proc! q2)
(empty-queue-proc? q2) ; #t

; exercise 3.23 -- deque is pronounced like "deck".
; inserting at the front amounts to prepending on a list, which is O(1).
; inserting at the rear is the same as with the queue.
; deleting at the front is the same as with the queue.
; deleting at the rear though is trickier--the final node needs to know
; who its predecessor is, so the deque can bring the rear-ptr back to it.
; Therefore we need a doubly linked list (dll) here.
;
;   dq -> [front  |  rear]
;           |           |
;           v           v
;          [a <-> b <-> c]

; A dll node is a pair ((val prev) next). This is so there is a natural
; forward list representation for the printer.
(define (make-dll-node prev next val)
  (cons (cons val prev) next))

(define (set-prev! node prev) (set-cdr! (car node) prev))
(define (set-next! node next) (set-cdr! node next))
(define (val-dll-node node) (caar node))
(define (prev-dll-node node) (cdar node))
(define (next-dll-node node) (cdr node))

; A dll is a pair of pointers to dll-nodes at the front and rear.
(define (make-dll)
  (cons '() '()))

(define (front-dll dll) (car dll))
(define (rear-dll dll) (cdr dll))

(define (set-front-dll! dll node) (set-car! dll node))
(define (set-rear-dll! dll node) (set-cdr! dll node))

(define (empty-dll? dll)
  (null? (front-dll dll)))

(define (prepend-dll! dll val)
  (let ((node (make-dll-node '() (front-dll dll) val)))
    (cond ((empty-dll? dll)
           (set-front-dll! dll node)
           (set-rear-dll! dll node))
          (else
            (set-front-dll! dll node)))))

(define (append-dll! dll val)
  (let ((node (make-dll-node (rear-dll dll) '() val)))
    (cond ((empty-dll? dll) ; XXX repeated
           (set-front-dll! dll node)
           (set-rear-dll! dll node))
          (else
            (set-rear-dll! dll node)))))

(define dll (make-dll))
(empty-dll? dll) ; #t
(prepend-dll! dll 'a)
(empty-dll? dll) ; #f
(prepend-dll! dll 'b)
(val-dll-node (front-dll dll)) ; 'b
(append-dll! dll 'c)
(val-dll-node (rear-dll dll)) ; 'c

(define (delete-front-dll! dll)
  (cond ((empty-dll? dll)
         (error "delete-front-dll!: empty dll" dll))
        (else
          (set-front-dll! dll (next-dll-node (front-dll dll)))
          (set-prev! (front-dll dll) '()))))

(define (delete-rear-dll! dll)
  (cond ((empty-dll? dll)
         (error "delete-rear-dll!: empty dll" dll))
        (else
          (set-rear-dll! dll (prev-dll-node (rear-dll dll)))
          (set-next! (rear-dll dll) '()))))

; dll = (b a c)
(delete-front-dll! dll) ; (a c)
(val-dll-node (front-dll dll)) ; 'a
(append-dll! dll 'd) ; (a c d)
(val-dll-node (rear-dll dll)) ; 'd
(delete-rear-dll! dll) ; (a c)
(val-dll-node (rear-dll dll)) ; 'c

; The dll is practically a deque, it supports prepend/append and
; deletes on both ends in constant time.
