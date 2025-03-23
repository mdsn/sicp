; the queue from 3.3.2
(define (make-queue) (cons '() '()))
(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))
(define (set-front-ptr! q x) (set-car! q x))
(define (set-rear-ptr! q x) (set-cdr! q x))
(define (empty-queue? q) (null? (front-ptr q)))
(define (front-queue q)
  (if (empty-queue? q)
    (error "front-queue: empty queue" q)
    (car (front-ptr q))))
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

; wiring
(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin ((car procedures))
           (call-each (cdr procedures)))))

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))

    ; run each of the registered actions when setting the signal.
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
        (begin (set! signal-value new-value)
               (call-each action-procedures))
        'done))

    ; register a new signal change action on the wire.
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            ((eq? m 'get-actions) action-procedures)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure) ((wire 'add-action!) action-procedure))
(define (get-actions wire) (wire 'get-actions))

; agenda
(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda)
  (list 0))

(define (current-time agenda)
  (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))

  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      (insert-queue! (segment-queue (car segments))
                     action)
      (let ((rest (cdr segments)))
        (if (belongs-before? rest)
          (set-cdr! segments
                    (cons (make-new-time-segment time action)
                          (cdr segments)))
          (add-to-segments! rest)))))

  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
      (set-segments! agenda
                     (cons (make-new-time-segment time action)
                           segments))
      (add-to-segments! segments))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty -- first-agenda-item")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

; our simulation's agenda
(define the-agenda (make-agenda))

; delays
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

; simulation
(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))

; register a probing action that prints the state of the
; wire when its signal changes.
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New value = ")
                 (display (get-signal wire)))))

; logical gates
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and s t)
  (if (and (= s 1) (= t 1))
    1
    0))

(define (logical-or s t)
  (if (or (= s 1) (= t 1))
    1
    0))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a b output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a) (get-signal b))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a and-action-procedure)
  (add-action! b and-action-procedure)
  'ok)

; exercise 3.28
(define (or-gate a b output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal a) (get-signal b))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a or-action-procedure)
  (add-action! b or-action-procedure)
  'ok)

; half adder
(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

; let's go
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

; (define out (make-wire))
; (probe 'out out)
; (or-gate input-1 input-2 out)
; (set-signal! input-1 1)
; (set-signal! input-2 0)
; (propagate) ; out 5 New value = 1'done

(probe 'sum sum) ; sum 0 New value = 0
(probe 'carry carry) ; carry 0 New value = 0
(half-adder input-1 input-2 sum carry) ; 'ok
(set-signal! input-1 1) ; 'done
(propagate) ; sum 8 New value = 1'done
(set-signal! input-2 1) ; 'done
(propagate)
; carry 11 New value = 1
; sum 16 New value = 0'done :)

; exercise 3.29
; by De Morgan, (A or B) = !(!A and !B)
; by definition of nand, (A nand B) = !(A and B)
;
; consider (A and A)    A | A and A
;                       0 | 0
;                       1 | 1
; therefore !(A and A) = (A nand A) = !A
;
; Then (A or B) = !((A nand A) nand (B nand B))
(define (nand-gate a b output)
  (define (nand-action-procedure)
    (let ((new-value
            (logical-not (logical-and (get-signal a) (get-signal b)))))
      (after-delay (+ inverter-delay and-gate-delay)
                   (lambda () (set-signal! output new-value)))))
  (add-action! a nand-action-procedure)
  (add-action! b nand-action-procedure)
  'ok)

(define (or-from-nand a b output)
  (let ((c (make-wire))
        (d (make-wire)))
    (nand-gate a a c)
    (nand-gate b b d)
    (nand-gate c d output)
    'ok))

(define the-agenda (make-agenda)) ; reset
(define u (make-wire))
(define v (make-wire))
(define w (make-wire))
(probe 'w w)
(or-from-nand u v w)
(set-signal! u 1)
(propagate) ; w 5 New value = 1
            ; w 10 New value = 0
            ; w 10 New value = 1
(set-signal! u 0)
(propagate) ; w 20 New value = 0
(set-signal! v 1)
(propagate) ; w 30 New value = 1
(set-signal! u 1)
(propagate) ; 'done     w already is 1

; exercise 3.30
(define the-agenda (make-agenda)) ; reset

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

; a ripple carry adder that assumes all three of as, bs and ss are
; the same length.
(define (ripple-carry-adder as bs ss c)
  (if (null? as)
    'ok
    (let ((a (car as))
          (b (car bs))
          (s (car ss))
          (c-in (make-wire)))
      (full-adder a b c-in s c)
      (ripple-carry-adder (cdr as) (cdr bs) (cdr ss) c-in))))

(define (make-wires xs)
  (if (null? xs)
    '()
    (let ((w (make-wire)))
      (if (= 1 (car xs))
        (begin (set-signal! w 1)
               (cons w (make-wires (cdr xs))))
        (cons w (make-wires (cdr xs)))))))

(define as (make-wires '(0 1 1 0 0 1 0 0))) ; 100
(define bs (make-wires '(1 0 1 0 0 0 1 0))) ; 162
(define ss (make-wires '(0 0 0 0 0 0 0 0))) ; expected sum = 00000110, carry = 1
(define c (make-wire))

(probe 'c c)
(map (lambda (s) (probe 's s))
     ss)

(ripple-carry-adder as bs ss c)

(propagate)
; s 8 New value = 1
; s 8 New value = 1
; s 8 New value = 1
; s 16 New value = 1
; s 16 New value = 1
; s 16 New value = 0
; s 32 New value = 0
; c 40 New value = 1
; s 48 New value = 0'done

(get-signal c) ; 1
(map (lambda (s) (get-signal s)) ss)
; (mcons 0 (mcons 0 (mcons 0 (mcons 0 (mcons 0 (mcons 1 (mcons 1 (mcons 0 '()))))))))
; result = 00000110, carry 1 :)

