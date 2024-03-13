#lang sicp

(define (square x) (* x x))

; https://stackoverflow.com/a/36824291
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

; Type tags
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
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'raise 'scheme-number        ; 2.83
       (lambda (x) (make-rational x 1)))
  'done)

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
  (put 'raise 'rational             ; 2.83
       (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'project '(rational)           ; 2.85
       (lambda (x) (numer x)))
  'done)

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
  (put 'project '(rectangular)        ; 2.85
       (lambda (z) (real-part z)))
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
  ; Projecting a complex number in polar coordinates brings up a problem with
  ; the "project and raise" method of assessing droppability: when projecting
  ; a complex number we throw away the type information necessary to reconstruct
  ; it when raising. We can only reasonably project to the real component of the
  ; rectangular representation, but that turns out to be a regular scheme-number
  ; so when raising back again we don't know whether to reconstruct the complex
  ; number in rectangular or polar coordinates.
  ; The problem is worked around by the fact that "equality" for complex numbers
  ; is implemented in the complex package, that is, abstracted from the internal
  ; representation. Thus we can still compare the original complex in polar coords
  ; with the re-raised one in rectangular coordinates, and get the expected
  ; comparison.
  (put 'project '(polar)          ; 2.85
       (lambda (z) (real-part z)))
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
  (put 'project '(complex)
       (lambda (z) (project z)))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-scheme-number n) ((get 'make 'scheme-number) n))
(define (make-rational n d) ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))

(install-scheme-number-package) ; 'done
(install-rational-package) ; 'done
(install-complex-package) ; 'done

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(define n (make-scheme-number 3))
(define z (make-complex-from-real-imag 1 4))

; (scheme-number->complex n) ; '(complex rectangular 3 . 0)

(put-coercion 'scheme-number 'complex scheme-number->complex)

; (add n z) ; '(complex rectangular 4 . 4)

; exercise 2.81

; (apply-generic 'add (make-scheme-number 1) (make-scheme-number 2)) ; 3
; (apply-generic 'add (make-rational 2 3) (make-rational 2 3)) ; '(rational 4 . 3)

; apply-generic works correctly as is. There is no need to avoid coercing when
; the types are the same because we get the base operation from the type's
; package which is then applied. Only by not finding an appropriate procedure
; do we try to find a coercion between the types.

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)

(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

(define (exp x y) (apply-generic 'exp x y))

(exp (make-scheme-number 2) (make-scheme-number 3)) ; 8

; (exp (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 2 3))
; By adding the coercion complex->complex, when we fail to find an 'exp
; procedure for types 'complex, we always find a coercion and recursively call
; (apply-generic 'exp (complex->complex z1) z2). This is just the same call
; with the same effect, thus we never return from it.

; After adding a check for equal types before attempting coercion (because we
; failed to find a base implementation in the type's package):
; (exp (make-complex-from-real-imag 1 2)
;      (make-complex-from-real-imag 2 3))
; no method for these types '(exp (complex complex))

; exercise 2.82

; To implement the strategy proposed in the book we need a procedure to take
; each element of a list out of it, to try and raise the rest of the elements
; to its type:
;
; '(1 2 3 4 5)
; ->
;   (1 . 2 3 4 5)
;   (2 . 1 3 4 5)
;   (3 . 1 2 4 5)
;   (4 . 1 2 3 5)
;   (5 . 1 2 3 4)
;
; A linear complexity algorithm: take the car of the elements and try it out
; against the elements appended to the already-visited heads. To keep it linear
; we need to prepend the attempted heads, but order doesn't matter as long as
; we find the correct coercion that will cast all elements to the same type.

; (define (selections xs)
;   (let select ([head (car xs)]
;                [tail (cdr xs)]
;                [prev '()])
;     (if (null? tail)
;       (list (list head '() prev))
;       (cons (list head tail prev)
;             (select (car tail) (cdr tail) (cons head prev))))))
; 
; (selections '(1 2 3 4 5))
; ->
; '((1 (2 3 4 5) ())
;   (2 (3 4 5) (1))
;   (3 (4 5) (2 1))
;   (4 (5) (3 2 1))
;   (5 () (4 3 2 1)))

; This will make it unnecessarily complicated to find coercions, and in the end
; we'll need to get them all together into a single list of coercions anyway.
; Might as well pay the quadratic toll of successive appends with the arguments
; to simplify the coercion-finding procedure.

(define (selections xs)
  (let select ([head (car xs)]
               [tail (cdr xs)]
               [prev '()])
    (if (null? tail)
      (list (cons head prev))
      (cons (append (cons head tail) prev)
            (select (car tail) (cdr tail) (cons head prev))))))

; (selections '(1 2 3 4 5))
; ->
; '((1 2 3 4 5)
;   (2 3 4 5 1)
;   (3 4 5 2 1)
;   (4 5 3 2 1)
;   (5 4 3 2 1))

; (define args
;   (list (make-rational 2 3)
;         (make-complex-from-real-imag 5 3)
;         (make-scheme-number 7)))
; ->
; ((rational 2 . 3) (complex rectangular 5 . 3) 7)

; (selections args)
; ->
; '(((rational 2 . 3) (complex rectangular 5 . 3) 7)
;   ((complex rectangular 5 . 3) 7 (rational 2 . 3))
;   (7 (complex rectangular 5 . 3) (rational 2 . 3)))

; (define type-tags
;   (map (lambda (xs) (map type-tag xs))
;      (selections args)))
; ->
; '((rational complex scheme-number)
;   (complex scheme-number rational)
;   (scheme-number complex rational))

; Try to find a destination type to which all other argument types can be
; coerced. In this case we only have one coercion from scheme-number to
; complex.
;
; (map (lambda (tts)
;        (let ([dst (car tts)]
;              [tail (cdr tts)])
;          (cons dst
;                (map (lambda (src) (cons src (get-coercion src dst)))
;                     tail))))
;      type-tags)
; ->
; '((rational (complex . #f) (scheme-number . #f))
;   (complex
;    (scheme-number . #<procedure:scheme-number->complex>)
;    (rational . #f))
;   (scheme-number (complex . #f) (rational . #f)))

; If any of the "values" in that homemade dictionary immediately above happen
; to be all true (or rather, all not-#f), it means we have found a target type
; for all other arguments. Grab the first such (key . values) pair, and perform
; the coercions.

; Let's make new test args for which we do have a coercion strategy, for the
; sake of the exercise.
; (define args (list (make-scheme-number 3)
;                    (make-scheme-number 5)
;                    (make-complex-from-real-imag 2 3)
;                    (make-scheme-number 7)
;                    (make-complex-from-real-imag 3 2)))
; ->
; (3
;  5
;  (complex rectangular 2 . 3)
;  7
;  (complex rectangular 3 . 2))

(define (list-coercions args)
  (map (lambda (order)
         (let ([target-arg (car order)])
           (map (lambda (arg)
                  (cons (type-tag arg) (get-coercion (type-tag arg) (type-tag target-arg))))
                order)))
       (selections args)))

; (list-coercions args)
; ->
; The first result shows the attempt to coerce everything to scheme-number.
; Naturally there are no such coercions for complex numbers, so those are #f.
; '(((scheme-number . #<procedure:scheme-number->scheme-number>)
;    (scheme-number . #<procedure:scheme-number->scheme-number>)
;    (complex . #f)
;    (scheme-number . #<procedure:scheme-number->scheme-number>)
;    (complex . #f))
; ...
; This is an attempt to coerce everything to a complex number. Since there are
; both complex->complex and scheme-number->complex coercions, this is a
; successful one and the one that will be attempted.
;   ((complex . #<procedure:complex->complex>)
;    (scheme-number . #<procedure:scheme-number->complex>)
;    (complex . #<procedure:complex->complex>)
;    (scheme-number . #<procedure:scheme-number->complex>)
;    (scheme-number . #<procedure:scheme-number->complex>))

; With this naive search we keep trying types we have already seen, even though
; it would be enough to test each type as destination only once. We can turn a
; successful coercion into a map of argument types to found coercion functions.
; This will deduplicate the findings, and let us get back to the original order
; in which the arguments were provided.

(define (successful-coercion? coercions)
  (foldl
    (lambda (a b) (and (cdr a) b))
    #t
    coercions))

; (define bad-coercion (car (list-coercions args)))
; ->
; '((scheme-number . #<procedure:scheme-number->scheme-number>)
;   (scheme-number . #<procedure:scheme-number->scheme-number>)
;   (complex . #f)
;   (scheme-number . #<procedure:scheme-number->scheme-number>)
;   (complex . #f))

; (define good-coercion (caddr (list-coercions args)))
; ->
; '((complex . #<procedure:complex->complex>)
;   (scheme-number . #<procedure:scheme-number->complex>)
;   (complex . #<procedure:complex->complex>)
;   (scheme-number . #<procedure:scheme-number->complex>)
;   (scheme-number . #<procedure:scheme-number->complex>))

; (successful-coercion? (cdr bad-coercion)) ; #f
; (successful-coercion? (cdr good-coercion)) ; #t

(define (select-successful-coercion coercions)
  (cond [(null? coercions) #f]
        [(successful-coercion? (cdar coercions))
         (car coercions)]
        [else (select-successful-coercion (cdr coercions))]))

; (select-successful-coercion (list-coercions args))
;
; '((complex . #<procedure:complex->complex>)
;   (scheme-number . #<procedure:scheme-number->complex>)
;   (complex . #<procedure:complex->complex>)
;   (scheme-number . #<procedure:scheme-number->complex>)
;   (scheme-number . #<procedure:scheme-number->complex>))

; We have selected a successful coercion strategy. Simplify it into a map of
; argument types to a single destination type that can be used to coerce all
; args before applying the operation.

(define (coercion-map successful-coercion)
  (define (contains? xs x)
    (cond [(null? xs) #f]
          [(eq? (caar xs) (car x)) #t]
          [else (contains? (cdr xs) x)]))
  (define (collect remaining accum)
    (cond [(null? remaining)
           accum]
          [(contains? accum (car remaining))
           (collect (cdr remaining) accum)]
          [else (collect (cdr remaining) (cons (car remaining) accum))]))
  (collect successful-coercion '()))

; We can use this map to convert all arguments to a single type.
; (coercion-map (select-successful-coercion (list-coercions args)))
;
; '((scheme-number . #<procedure:scheme-number->complex>)
;   (complex . #<procedure:complex->complex>))

; Finally, apply the right coercion to each argument.
(define (apply-coercions coercion-map args)
  ; By construction let's assume that coercion-map is valid and thus will
  ; contain a coercion for every possible argument type.
  (define (find-coercion cmap arg-type)
    (if (eq? (caar cmap) arg-type)
      (cdar cmap)
      (find-coercion (cdr cmap) arg-type)))
  (define (coerce arg)
    (let ([cf (find-coercion coercion-map (type-tag arg))])
      (cf arg)))
  (map coerce args))

; (apply-coercions
;   (coercion-map (select-successful-coercion (list-coercions args))) args)
;
; '((complex rectangular 3 . 0)
;   (complex rectangular 5 . 0)
;   (complex rectangular 2 . 3)
;   (complex rectangular 7 . 0)
;   (complex rectangular 3 . 2))

; Now we are ready to apply the generic operation across all the arguments.
; We'll manually fold left to right with the first argument as initial value,
; since we don't have a concept of a unit for every generic operation.

(define (multi-apply-generic op . args)
  ; The arguments have all been coerced here, all to the same target type. Fold
  ; the arguments with the operation to build the result of repeated application.
  (define (apply-successively acc args)
    (if (null? args) acc
        (let* ([arg (car args)]
               [proc (get op (list (type-tag acc) (type-tag arg)))])
          (if proc
            (apply-successively
              (apply proc (list acc (contents arg)))
              (cdr args))
            (error "no method for these arguments" (list op acc arg))))))

  (let ([coercion-strategy (select-successful-coercion (list-coercions args))])
    (if coercion-strategy
      (let ([coerced-args (apply-coercions coercion-strategy args)])
        (apply-successively (car coerced-args) (cdr coerced-args)))
      (error "no coercion strategy found for these argument types" (map type-tag args)))))

; (multi-apply-generic
;   'add (make-complex-from-real-imag 3 2)
;        (make-complex-from-real-imag 1 3)
;        (make-scheme-number 1.5)
;        (make-complex-from-real-imag 0 -1)
;        (make-scheme-number -3))
; ->
; '(complex rectangular 2.5 . 4) :)

; exercise 2.83
; Here is an example of how raise is implemented in the scheme-number package:
;
;  (put 'raise 'scheme-number
;       (lambda (x) (make-rational x 1)))
;
; This is not ideal: the scheme number package now has to know about a
; different type that it can be upcasted to. One possible strategy around this
; is to break the taxonomy out into its own structure, for example into a separate
; hash map :: lower-type -> higher-type

(define (raise x) ((get 'raise (type-tag x)) x))

; (raise (make-scheme-number 3)) ; '(rational 3 . 1)
; (raise (make-rational 1 3)) ; '(complex rectangular (rational 1 . 3) . 0)

; exercise 2.84
; We need a way to know which of two types is higher than the other in the
; taxonomy. I guess in the case of scheme-number and complex, scheme-number
; would be a subtype of complex, since every scheme-number is also a complex
; number (or can be raised up to be one) and can thus be safely used where a
; complex is expected.
;
; A simple solution that will prevent the thorny situation of the polygon
; taxonomy: maintain an ordered global list of types:
;
;   ('scheme-number 'rational 'complex)
;
; When a new type is to be added, it can be inserted in the right place in the
; list, to ensure the right raising order.

(define *type-order* '(scheme-number rational complex))

; (define (find-target-type t)
;   (let iter ([xs *type-order*])
;     (cond [(or (null? xs) (null? (cdr xs))) #f]
;           [(eq? t (car xs)) (cadr xs)]
;           [else (iter (cdr xs))])))
;
; (find-target-type 'scheme-number) ; 'rational
; (find-target-type 'rational) ; 'complex
; (find-target-type 'complex) ; #f

; If the types are not the same, and assuming both types are in the taxonomy,
; then the one that shows up first when traversing the *type-order* list is a
; subtype of the other.
; (define (higher-type t u)
;   (let iter ([xs *type-order*])
;     (cond [(null? xs) (error "types not in the taxonomy" t u)]
;           [(eq? (car xs) t) u]
;           [(eq? (car xs) u) t]
;           [else (iter (cdr xs))])))
;
; (higher-type 'scheme-number 'complex) ; 'complex
; (higher-type 'scheme-number 'rational) ; 'rational
; (higher-type 'rational 'complex) ; 'complex
;
; Scenarios not considered:
; (higher-type 'complex 'complex) ; 'complex 
; (higher-type 'real 'complex) ; 'real

; Maybe a more direct result to get from this iteration is an order. With
; the same assumptions as above, we can just get both types at once in an
; ordered pair. Let's say that the car of said pair can be raised to the cdr.
; (define (order-types t u)
;   (let iter ([xs *type-order*])
;     (cond [(null? xs) (error "types not in the taxonomy" t u)]
;           [(eq? (car xs) t) (cons t u)]
;           [(eq? (car xs) u) (cons u t)]
;           [else (iter (cdr xs))])))
; 
; (order-types 'scheme-number 'complex) ; '(scheme-number . complex)
; (order-types 'complex 'scheme-number) ; '(scheme-number . complex)
; (order-types 'rational 'scheme-number) ; '(scheme-number . rational)
; (order-types 'complex 'rational) ; '(rational . complex)

; That's good, but in the new implementation of apply-generic we'll want to
; get the value as well since it's what we need to raise.
(define (order-by-type a b)
  (let ([ta (type-tag a)]
        [tb (type-tag b)])
    (let iter ([xs *type-order*])
      (cond [(null? xs)
             (error "types not in the taxonomy" ta tb)]
            [(eq? (car xs) ta) (cons a b)]
            [(eq? (car xs) tb) (cons b a)]
            [else (iter (cdr xs))]))))

; (order-by-type (make-scheme-number 3)
;                (make-complex-from-real-imag 2 3)) ; '(3 complex rectangular 2 . 3)
; (order-by-type (make-complex-from-real-imag 2 3)
;                (make-scheme-number 3)) ; '(3 complex rectangular 2 . 3)

; Raise a to type t, if necessary applying multiple raises.
(define (raise-to t a)
  (if (eq? t (type-tag a))
    a
    (raise-to t (raise a))))

; (raise-to 'complex (make-scheme-number 3)) ; '(complex rectangular (rational 3 . 1) . 0)
; (raise-to 'complex (make-rational 1 3)) ; '(complex rectangular (rational 1 . 3) . 0)

(define (raising-apply-generic op . args)
  (let* ([type-tags (map type-tag args)]
         [proc (get op type-tags)])
    (if proc
      ; This captures arguments of the same type, since the operations are
      ; directly implemented inside each package.
      (apply proc (map contents args))
      (let* ([ordered-args (order-by-type (car args) (cadr args))]
             [a (car ordered-args)]
             [b (cdr ordered-args)]
             [raised-a (raise-to (type-tag b) a)])
        (raising-apply-generic op raised-a b)))))

; Fails because since raise raises one type at a time, we always get to complex with a
; rational number in its real component, which are not supported yet.
;
; (raising-apply-generic
;   'add
;   (make-complex-from-real-imag 1 2)
;   (make-scheme-number 2))
; 
; +: contract violation
;   expected: number?
;   given: '(rational 2 . 1)

; exercise 2.85
; exercise 2.86
