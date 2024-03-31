; exercise 3.7
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

  (define (deposit amount)
    (begin (set! balance (+ balance amount))
           balance))

  (define (dispatch p m)
    (if (not (eq? p password))
      (lambda (_) "Incorrect password")
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "unknown request: make-account" m)))))

  dispatch)

(define (make-joint account password new-password)
  (lambda (p m)
    (if (not (eq? p new-password))
      (lambda (_) "Incorrect password")
      (account password m))))

; (define peter-acc (make-account 100 'hemlock))
; (define paul-acc (make-joint peter-acc 'hemlock 'bubbagump))
; ((peter-acc 'hemlock 'withdraw) 10) ; 90
; ((paul-acc 'bubbagump 'deposit) 100) ; 190
; ((paul-acc 'hemlock 'withdraw) 10) ; "Incorrect password"

; exercise 3.8
; (f 0) is a no-op, it just returns the internal state
; (f 1) increments the internal state but returns the value before the assignment.
; Thus, assuming left-to-right order of evaluation,
;   (+ (f 0) (f 1)) = (+ 0 0) = 0, end state = 1, but
;   (+ (f 1) (f 0)) = (+ 0 1) = 1, end state = 1.
(define (make-f)
  (let ([state 0])
    (lambda (x)
      (if (= x 0)
        state
        (begin (set! state (+ state 1))
               (- state 1))))))

(define f (make-f))

; (+ (f 0) (f 1)) ; 0
; (+ (f 1) (f 0)) ; 1, on a clean interpreter.
