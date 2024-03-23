; Exercise 2.90
; To support both sparse and dense polynomials we will want to abstract the
; representation of the term lists (and terms) away. Operations on polynomials
; will need to interact with the term lists involved through generic operations.
;
; So far the operations in the polynomial package have used the following
; procedures on term lists:
;   - make-term
;   - empty-termlist?
;   - first-term
;   - rest-terms
;   - adjoin-term
;
; Additionally, the operations that implement addition, multiplication and
; negation of term lists depend on generic constructors and selectors for terms:
;   - coeff
;   - order

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
(define (neg x) (apply-generic 'neg x))
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
  (put 'raise 'scheme-number
       (lambda (x) (make-rational x 1)))
  'done)

; sparse term list data type
(define (install-sparse-package)
  (define (tag term-list) (attach-tag 'sparse term-list))

  (define (make-term order coeff) (list order coeff))
  (define (term-order term) (car term))
  (define (term-coeff term) (cadr term))

  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))

  (define (adjoin-term term term-list)
    (if (=zero? (term-coeff term))
      term-list
      (cons term term-list)))

  ; implementations
  (define (add-terms l1 l2)
    (cond ((empty-termlist? l1) l2)
          ((empty-termlist? l2) l1)
          (else
            (let ([t1 (first-term l1)]
                  [t2 (first-term l2)])
              (cond ((> (term-order t1) (term-order t2))
                     (adjoin-term t1 (add-terms (rest-terms l1) l2)))
                    ((< (term-order t1) (term-order t2))
                     (adjoin-term t2 (add-terms l1 (rest-terms l2))))
                    (else
                      (adjoin-term
                        (make-term (term-order t1)
                                   (add (term-coeff t1) (term-coeff t2)))
                        (add-terms (rest-terms l1) (rest-terms l2)))))))))

  ; typed methods
  (put 'make 'sparse
       (lambda (term-list) (tag term-list)))
  (put 'add '(sparse sparse)
       (lambda (t1 t2) (tag (add-terms t1 t2))))
  'done)

; dense term list data type
(define (install-dense-package)
  (define (tag term-list) (attach-tag 'dense term-list))

  (define (make-term order coeff) (cons coeff (build-list order (const 0))))
  (define (term-order term) (- (length term) 1))
  (define (term-coeff term) (car term))

  ; see 2.89
  (define (first-term term-list)
    (let ([coeff (car term-list)]
          [order (length (cdr term-list))])
      (make-term order coeff)))

  ; see 2.89
  (define (rest-terms term-list)
    (cdr term-list))

  (define (adjoin-term term term-list)
    (if (=zero? (term-coeff term))
      term-list
      ; the order of a term list is the same as the order of an individual term
      (let ([len (- (length term-list) 1)])
        (append (take term (- (term-order term) len))
                term-list))))
      ; example: adjoin '(1 0 0 0 0) '(3 0 3) -> append '(1 0) '(3 0 3)

  ; implementations
  ; This procedure looks exactly the same as the one in the sparse package, so how
  ; come it's repeated here verbatim? The reason is that `make-term` is not a generic
  ; procedure. In order to construct a term when adding two terms of the same order
  ; we would need to know whether we are adding sparse or dense term lists generically.
  ; But if we made this method generic, and called it via an apply-generic router, the
  ; type tag would be stripped from l1 and l2, and we wouldn't be able to tell what the
  ; type is we are operating on. Ultimately the cause is that terms themselves are not
  ; tagged with a type, and so we can't dispatch on a generic `make-term` -- each of
  ; the make-term procedures is tied to the term-list type.
  (define (add-terms l1 l2)
    (cond ((empty-termlist? l1) l2)
          ((empty-termlist? l2) l1)
          (else
            (let ([t1 (first-term l1)]
                  [t2 (first-term l2)])
              (cond ((> (term-order t1) (term-order t2))
                     (adjoin-term t1 (add-terms (rest-terms l1) l2)))
                    ((< (term-order t1) (term-order t2))
                     (adjoin-term t2 (add-terms l1 (rest-terms l2))))
                    (else
                      (adjoin-term
                        (make-term (term-order t1)
                                   (add (term-coeff t1) (term-coeff t2)))
                        (add-terms (rest-terms l1) (rest-terms l2)))))))))

  ; typed methods
  (put 'make 'dense
       (lambda (term-list) (tag term-list)))
  (put 'add '(dense dense)
       (lambda (t1 t2) (tag (add-terms t1 t2))))
  'done)

; both kinds of term lists share this empty term list constructor and query
(define (empty-termlist) '())
(define (empty-termlist? t) (null? t))

; polynomials
(define (install-polynomial-package)
  (define (tag p) (attach-tag 'polynomial p))
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ; implementations
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add (term-list p1) (term-list p2)))
      (error "Polys not in the same var -- ADD-POLY" (list p1 p2))))

  ; typed methods
  (put 'make-polynomial 'dense
       (lambda (var terms)
         (tag (make-poly var (make-dense-termlist terms)))))
  (put 'make-polynomial 'sparse
       (lambda (var terms)
         (tag (make-poly var (make-sparse-termlist terms)))))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  'done)

; constructors
(define (make-scheme-number n) ((get 'make 'scheme-number) n))
(define (make-sparse-termlist terms) ((get 'make 'sparse) terms))
(define (make-dense-termlist terms) ((get 'make 'dense) terms))
(define (make-dense-polynomial var terms)
  ((get 'make-polynomial 'dense) var terms))
(define (make-sparse-polynomial var terms)
  ((get 'make-polynomial 'sparse) var terms))

; setup
(install-scheme-number-package)
(install-sparse-package)
(install-dense-package)
(install-polynomial-package)

; (make-sparse-termlist '((2 2) (0 1))) ; '(sparse (2 2) (0 1))
; (make-dense-termlist '(1 2 0 3 -2 -5)) ; '(dense 1 2 0 3 -2 -5)
; (make-dense-polynomial 'x '(1 2 0 3 -2 -5)) ; '(polynomial x dense 1 2 0 3 -2 -5)
; (make-sparse-polynomial 'x '((2 2) (0 1))) ; '(polynomial x sparse (2 2) (0 1))

; Add sparse polynomials
; (define p1 (make-sparse-polynomial 'x '((2 2) (0 1))))
; (define p2 (make-sparse-polynomial 'x '((3 1) (0 2))))
; (add p1 p2) ; '(polynomial x sparse (3 1) (2 2) (0 3)) :)

; Add dense polynomials
; (define p1 (make-dense-polynomial 'x '(2 0 1))) ; '(polynomial x dense 2 0 1)
; (define p2 (make-dense-polynomial 'x '(1 0 0 2))) ; '(polynomial x dense 1 0 0 2)
; (add p1 p2) ; '(polynomial x dense 1 2 0 3) :)
