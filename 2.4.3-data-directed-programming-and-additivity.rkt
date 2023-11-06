#lang sicp

; https://stackoverflow.com/a/36824291
(define *op-table* (make-hash))
(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))
(define (get op type)
  (hash-ref *op-table* (list op type) '()))

; (put 'testop 'testype map)
; (display *op-table*) ; #hash(((testop testype) . #<procedure:map>))

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

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

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
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types -- APPLY-GENERIC"
          (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

; exercise 2.73
(define (square x) (* x x))

(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (variable? x) (symbol? x))
(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

; Define a data-directed generic `deriv` procedure that dispatches to an expression-specific instance of
; deriv for each type of expression that is not a number or variable, that is, a sum or product.
; We don't need to abstract over `number?` or `variable?` because the derivation of these expressions don't
; involve any operations, and operations are the type tag in the table.
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else
          ((get 'deriv (operator exp)) (operands exp)
                                       var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(operator '(+ x 3)) ; '+
(operands '(+ x 3)) ; '(x 3)

(define (make-sum x y)
  (cond ((=number? x 0) y)
        ((=number? y 0) x)
        ((and (number? x) (number? y))
         (+ x y))
        (else (list '+ x y))))

(define (make-product x y)
  (cond ((or (=number? x 0)
			 (=number? y 0))
		 0)
		((=number? x 1) y)
		((=number? y 1) x)
		((and (number? x) (number? y))
		 (* x y))
		(else (list '* x y))))

(define (make-exponentiation x n)
  (cond ((=number? n 0) 1)
        ((=number? n 1) x)
        (else (list '** x n))))

(define (install-derivative-sum)
  (define (addend exp) (car exp))
  (define (augend exp) (cadr exp))
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (put 'deriv '+ deriv-sum)
  'done)

(define (install-derivative-product)
  (define (multiplicand exp) (car exp))
  (define (multiplier exp) (cadr exp))
  (define (deriv-prod exp var)
    (make-sum
      (make-product (multiplier exp)
                    (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var)
                    (multiplicand exp))))
  (put 'deriv '* deriv-prod)
  'done)

(define (install-derivative-exponentiation)
  (define (base exp) (car exp))
  (define (exponent exp) (cadr exp))
  (define (deriv-exp exp var)
    (make-product
      (exponent exp)
      (make-exponentiation var (- (exponent exp) 1))))
  (put 'deriv '** deriv-exp)
  'done)

(install-derivative-sum) ; 'done
(install-derivative-product) ; 'done
(install-derivative-exponentiation) ; 'done

(deriv 'x 'x) ; 1
(deriv 1 'x) ; 0
(deriv '(+ x 3) 'x) ; 1
(deriv '(* x 3) 'x) ; 3
(deriv '(+ (* x 3) 1) 'x) ; 3
(deriv '(** x 3) 'x) ; '(* 3 (** x 2))
; This version of deriv can only derive binary expressions.
(deriv '(+ (+ (** x 3) (* x 3)) x) 'x) ; '(+ (+ (* 3 (** x 2)) 3) 1) = 3x**2 + 4

; exercise 2.74
; ... >_>

; Message passing

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
            (error "unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define z1 (make-from-real-imag 3 2))
(real-part z1) ; 3
(magnitude z1) ; 3.605551275463989
(angle z1) ; 0.5880026035475675

; exercise 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
            (error "unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define z2 (make-from-mag-ang 3.6 0.58))
(real-part z2) ; 3.011265539694673
(imag-part z2) ; 1.972886172450745
(magnitude z2) ; 3.6
(angle z2) ; 0.58

; exercise 2.76
; When dispatching on type, if a new type needs to be added that implements the
; operations, then each individual operation needs to be modified to do the
; explicit checking for the type tag and to dispatch to the particular function.
; If a new operation needs to be added to the existing types, then once it is
; implemented for each specific type, an individual generic function needs to be
; created to dispatch. The rest of the operations need not be modified.
;
; When using the data-directed methodology, if a new type needs to be added
; then a new "column" is added to the table of operations and types. Each
; operation is implemented inside the new package, these are then installed and
; automatically available. There is no need to modify any of the existing types
; or their implementation. On the other hand, if a new operation needs to be
; added to the existing types, each individual package needs to be extended with
; the implementation, and its public interface extended.
;
; When using message passing, if a new type needs to be added, a new
; constructor needs to be added that returns a dispatching procedure implementing
; each of the datatype's methods. If a new operation needs to be added, each of
; the existing constructors needs to be modified to implement it. In this sense,
; there is not much difference with the data-directed methodology.
;
; For a system where new types need to be added it is better to use
; data-directed programming or message passing. In a system where new operations
; need to be added, it may be better to use explicit dispatch.
