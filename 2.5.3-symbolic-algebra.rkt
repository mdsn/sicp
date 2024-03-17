; boilerplate
(define *op-table* (make-hash))
(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))
(define (get op type)
  (hash-ref *op-table* (list op type) #f))

(define *coercions-table* (make-hash))
(define (put-coercion t1 t2 proc)
  (hash-set! *coercions-table* (list t1 t2) proc))
(define (get-coercion t1 t2)
  (hash-ref *coercions-table* (list t1 t2) #f))

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

; Generic interface
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags)))
            (if (eq? type1 type2)
              (error "no method for these types" (list op type-tags))
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                        (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                        (else (error "no coercion for these types" (list op type-tags))))))))
          (error "no method for these types" (list op type-tags)))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ x y) (apply-generic 'equ x y))
(define (neg x) (apply-generic 'neg x))  ; 2.88
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
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  (put 'zero '(scheme-number)
       (lambda (x) (= 0 x)))
  (put 'neg '(scheme-number)
       (lambda (x) (- x)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'raise 'scheme-number        ; 2.83
       (lambda (x) (make-rational x 1)))
  'done)

; polynomials
(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ; procedures adjoin-term and coeff
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in the same var -- ADD-POLY" (list p1 p2))))

  ; 2.88
  ; To subtract polynomials, we can compute -p2 and add it with add-poly.
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (let ([minus-p2 (neg-poly p2)])
        (add-poly p1 minus-p2))
      (error "Polys not in the same var -- SUB-POLY" (list p1 p2))))

  ; 2.88
  ; Generic negation operation for polynomials. This is necessary to support
  ; subtraction of polynomials with polynomial coefficients; without it we
  ; wouldn't know how to negate each coefficient in negate-terms.
  (define (neg-poly p)
    (make-poly (variable p)
               (negate-terms (term-list p))))

  ; 2.88
  ; Negate terms by generically negating the coefficient of each term into
  ; a new term list. 'neg' is the generic negation operation from the hint.
  (define (negate-terms L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ([t (first-term L)])
        (adjoin-term
          (make-term (order t)
                     (neg (coeff t)))
          (negate-terms (rest-terms L))))))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ([t1 (first-term L1)]
                  [t2 (first-term L2)])
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in the same var -- MUL-POLY" (list p1 p2))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ([t2 (first-term L)])
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (tag p) (attach-tag 'polynomial p))
  ; exercise 2.87
  ; polynomials will be represented sparsely, as a list of non-zero terms,
  ; each of the form (order coeff). If the polynomial is 0, all its terms are 0,
  ; so the list of terms is empty.
  ; Although, why not '(polynomial x (0 0)) ? We may end up in a situation like
  ; this when adding/subtracting polynomials, unless specifically simplifying away
  ; terms with coefficient 0.
  (put 'zero '(polynomial)
       (lambda (p) (empty-termlist? (term-list p))))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  ; exercise 2.88
  (put 'neg '(polynomial)
       (lambda (p) (tag (neg-poly p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-term order coeff) (list order coeff))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))

; constructors
(define (make-scheme-number n) ((get 'make 'scheme-number) n))
(define (make-polynomial var terms) ((get 'make 'polynomial) var terms))

; set up packages
(install-scheme-number-package) ; 'done
(install-polynomial-package) ; 'done

; (define p1 (make-polynomial 'x '((100 1) (2 2) (0 1))))
; (define p2 (make-polynomial 'x '((100 3) (2 1) (1 3) (0 3))))
; (add p1 p2) ; '(polynomial x (100 4) (2 3) (1 3) (0 4))

; (define p1 (make-polynomial 'y (list (make-term 1 1) (make-term 0 1)))) ; (y + 1)
; (define p2 (make-polynomial 'y (list (make-term 2 1) (make-term 0 1)))) ; (y^2 + 1)
; (define p3 (make-polynomial 'y (list (make-term 1 1) (make-term 0 -1)))) ; (y - 1)
; (define q1 (make-polynomial 'x (list (make-term 2 p1) (make-term 1 p2) (make-term 0 p3))))
; q1 = (y+1)x^2 + (y^2+1)x + (y-1)
; (define q2 (make-polynomial 'x (list (make-term 1 p2) (make-term 0 p1))))
; q2 = (y^2+1)x + (y+1)

; (make-polynomial 'x (list (make-term 2 p1) (make-term 1 p2) (make-term 0 p3)))
; '(polynomial
;   x
;   (2 (polynomial y (1 1) (0 1)))
;   (1 (polynomial y (2 1) (0 1)))
;   (0 (polynomial y (1 1) (0 -1))))
; = (y+1)x^2 + (y^2+1)x + (y-1)

; (add q1 q2)
; '(polynomial
;   x
;   (2 (polynomial y (1 1) (0 1)))
;   (1 (polynomial y (2 2) (0 2)))
;   (0 (polynomial y (1 2))))
; = (y+1)x^2 + (2y^2+2)x + 2y :)

; 2.88
; (sub (make-polynomial 'x (list (make-term 2 2)
;                                (make-term 1 3)
;                                (make-term 0 5))) ; 2x^2 + 3x + 5
;      (make-polynomial 'x (list (make-term 2 1)
;                                (make-term 1 1)
;                                (make-term 0 3)))) ; x^2 + x + 3
; '(polynomial x (2 1) (1 2) (0 2))
;  = x^2 + 2x + 2 :)
