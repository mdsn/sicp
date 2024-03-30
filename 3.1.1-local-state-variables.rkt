; exercise 3.1
(define (make-accumulator n)
  (lambda (m)
    (begin (set! n (+ n m))
           n)))

; (define A (make-accumulator 5))
; (A 10) ; 15
; (A 15) ; 30

; exercise 3.2
(define (make-monitored f)
  (let ([n 0])
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) n)
            ((eq? x 'reset-count)
             (begin (set! n 0)))
            (else
              (begin (set! n (+ n 1))
                     (f x)))))))

; (define s (make-monitored sqrt))
; (s 100) ; 10
; (s 9) ; 3
; (s 'how-many-calls?) ; 1
; (s 'reset-count) ; Empty result.
; (s 'how-many-calls?) ; 0

; exercise 3.3
; exercise 3.4
(define (make-account balance password)
  (define (call-the-cops)
    "Calling the cops EEOOEEOO")
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let ([calls 0])
    (lambda (p m)
      (if (not (eq? p password))
        (lambda (n)
          (if (< calls 7)
            (begin (set! calls (+ calls 1))
                   "Incorrect password")
            (call-the-cops)))
        (begin
          (set! calls 0)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "unknown request: make-account" m))))))))

; (define acc (make-account 100 'alaska))
; ((acc 'alaska 'deposit) 10) ; 110
; ((acc 'wrong 'withdraw) 20) ; "Incorrect password"
; ...
; ((acc 'wrong 'withdraw) 20) ; "Calling the cops EEOOEEOO"
