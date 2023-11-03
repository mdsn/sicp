#lang sicp

(define (square x) (* x x))

; https://stackoverflow.com/a/36824291
(define *op-table* (make-hash))
(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))
(define (get op type)
  (hash-ref *op-table* (list op type) '()))

; Type tags
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum -- CONTENTS" datum)))

; Generic interface
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types -- APPLY-GENERIC"
          (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ x y) (apply-generic 'equ x y))
(define (=zero? x) (apply-generic 'zero x))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put 'zero '(scheme-number)
       (lambda (x) (= 0 x)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(make-scheme-number 3) ; '(scheme-number . 3)

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ '(rational rational)
       (lambda (x y)
         (and (= (numer x) (numer y))
              (= (denom x) (denom y)))))
  (put 'zero '(rational)
       (lambda (x)
         (= 0 (numer x))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'angle '(rectangular) angle)
  (put 'magnitude '(rectangular) magnitude)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-polar-package)
(install-rectangular-package)

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag
      (+ (real-part z1) (real-part z2))
      (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
      (- (real-part z1) (real-part z2))
      (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
      (* (magnitude z1) (magnitude z2))
      (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
      (/ (magnitude z1) (magnitude z2))
      (- (angle z1) (angle z2))))
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ '(complex complex)
       (lambda (z1 z2)
         (and (= (real-part z1) (real-part z2))
              (= (imag-part z1) (imag-part z2)))))
  (put 'zero '(complex)
       (lambda (z)
         (and (= 0 (imag-part z))
              (= 0 (real-part z)))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-scheme-number-package) ; 'done
(install-rational-package) ; 'done
(install-complex-package) ; 'done

(make-scheme-number 30) ; '(scheme-number . 30)
(make-rational 5 3) ; '(rational 5 . 3)
(make-complex-from-real-imag 3 2) ; '(complex rectangular 3 . 2)
(make-complex-from-mag-ang 3 2) ; '(complex polar 3 . 2)

(define r1 (make-rational 5 3))
(define z1 (make-complex-from-real-imag 3 2))

(add r1 r1) ; '(rational 10 . 3)
(add z1 z1) ; '(complex rectangular 6 . 4)
; (add r1 z1) Naturally
; ; application: not a procedure;
; ;  expected a procedure that can be applied to arguments
; ;   given: '()
; ; [,bt for context]

; exercise 2.77
; (magnitude (make-complex-from-real-imag 2 3))
; ; application: not a procedure;
; ;  expected a procedure that can be applied to arguments
; ;   given: '()
; ; [,bt for context]

(display z1) ; (complex rectangular 3 . 2)

(type-tag z1) ; 'complex
(contents z1) ; '(rectangular 3 . 2)
(get 'magnitude '(complex)) ; #<procedure:magnitude>
(magnitude z1) ; 3.605551275463989

; magnitude z1
;   apply-generic 'magnitude (complex rectangular 3 . 2)
;       type-tag <- 'complex
;       arg      <- 'rectangular 3 2
;       op       <- magnitude (complex package)
; -> magnitude (rectangular 3 2)
;   apply-generic 'magnitude (rectangular 3 2)
;       type-tag <- 'rectangular
;       arg      <- (3 2)
;       op       <- magnitude (rectangular package)
; -> magnitude (3 2)
;       (sqrt (+ (square (real-part z))
;                (square (imag-part z)))))
; -> real-part (rectangular package) = car z
; -> imag-part (rectangular package) = cdr z

; exercise 2.78
(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(make-scheme-number 3) ; 3
(add (make-scheme-number 3) (make-scheme-number 2)) ; 5

; exercise 2.79
(equ (make-scheme-number 3)
     (sub (make-scheme-number 5) (make-scheme-number 2))) ; #t

(equ (make-rational 10 6) (make-rational 5 3)) ; #t

(define z1 (make-complex-from-real-imag 3 2))
(define z2 (make-complex-from-mag-ang
             (magnitude z1)
             (angle z1)))

(real-part z1) ; 3
(imag-part z1) ; 2
(real-part z2) ; 3.0
(imag-part z2) ; 1.9999999999999996

(equ z1 z2) ; #f >_>

; exercise 2.80
(=zero? (make-scheme-number 0)) ; #t
(=zero? (make-rational 0 3)) ; #t
(=zero? (make-complex-from-real-imag 0 0)) ; #t
