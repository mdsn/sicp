(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        ; 4.4
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (definition-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)      ; (define x 1)
    (caadr exp)))   ; (define (f x) ...)

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp)
  (tagged-list exp 'begin))

(define (begin-actions exp)
  (cdr exp))

(define (last-exp? seq)
  (null? (cdr seq)))

(define (first-exp seq)
  (car seq))

(define (rest-exps seq)
  (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

; 4.5 We don't strictly need a make-application constructor
; since by definition it's any compound expression that is
; not one of the others.
(define (make-application operator operands)
  (list operator operands))

(define (application? exp)
  (pair? exp))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (no-operands? ops)
  (null? ops))

(define (first-operand ops)
  (car ops))

(define (rest-operands ops)
  (cdr ops))

(define (cond? exp)
  (tagged-list? exp 'cond))

(define (cond-clauses exp)
  (cdr exp))

; 4.5
(define (cond-arrow-clause? clause)
  (tagged-list? clause 'arrow))

(define (cond-else-clause? clause)
  (tagged-list? clause 'else))

(define (cond-default-clause? clause)
  (tagged-list? clause 'default))

(define (cond-predicate clause)
  (cadr clause))

(define (cond-actions clause)
  (cddr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

;(define (expand-clauses clauses)
;  (if (null? clauses)
;    'false
;    (let ((first (car clauses))
;          (rest (cdr clauses)))
;      (if (cond-else-clause? first)
;        (if (null? rest)
;          (sequence->exp (cond-actions first))
;          (error "ELSE clause isn't last -- COND->IF" clauses))
;        (make-if (cond-predicate first)
;                 (sequence->exp (cond-actions first))
;                 (expand-clauses rest))))))

; 4.5
(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (cond ((cond-default-clause? first)
             (make-if (cond-predicate first)
                      (sequence-exp (cond-actions first))
                      (expand-clauses rest)))
            ; 4.5 The cond-actions of an arrow clause is a lambda. We don't have
            ; a `let` yet, but it can be simulated by application to a lambda.
            ; Evaluate the predicate and extract the recipient (the car of the
            ; cond-actions), and provide them as the arguments to an application
            ; of a lambda that executes the if: if the predicate-value is truthy,
            ; it is given to the recipient procedure; otherwise the clause
            ; expansion continues.
            ((cond-arrow-clause? first)
             (make-application
               (make-lambda '(predicate-value recipient)
                            (list (make-if 'predicate-value
                                           '(recipient predicate-value) ; does this also require a
                                                                        ; make-application?
                                           (expand-clauses rest))))
               (list (eval (cond-predicate first) env)
                     (car (cond-actions first)))))
            ((cond-else-clause? first)
             (if (null? rest)
               (sequence-exp (cond-actions first))
               (error "ELSE clause isn't last -- COND->IF" clauses)))
            (else
              (error "Unknown clause type -- COND->IF" first))))))

; 4.1
(define (list-of-values-l2r exps env)
  (if (no-operands? exps)
    '()
    (let ((head '()))
      (begin (set! head (eval (first-operand exps) env))
             (cons head (list-of-values-l2r (rest-operands exps) env))))))

(define (list-of-values-r2l exps env)
  (if (no-operands? exps)
    '()
    (let ((tail '()))
      (begin (set! tail (list-of-values-r2l (rest-operands exps) env))
             (cons (eval (first-operand exps) env)
                   tail)))))

; 4.2
; Suppose Louis writes eval like this:
;
;    (define (eval exp env)
;      (cond ((application? exp) ...)
;            ((assignment? exp) ...)
;            ((definition? exp) ...)
;            ...
;
; Now consider (define x 3). With the definition we have
; of `application?`, this expression will match this cond
; clause and attempt to apply `define` to x and 3 instead
; of pushing a new definition.

; In order for applications to be checked first, something
; must call them out distinctly from other elements of the
; language. A 'call list tag will be it:
;
;    (define (application? exp)  ; ('call operator <operands>)
;      (tagged-list? exp 'call))
;    (define (operator exp) (cadr exp))
;    (define (operands exp) (cddr exp))

; 4.3
; To rewrite eval in a data directed fashion, after the style
; of the differentiation procedure of exercise 2.73, we would
; start by defining a table of evaluation rules keyed by the
; type of an expression (the type tag that answers predicates
; such as quoted? and assignment?).
;
; Instead of a long list of conditions, `eval` would assume
; the expression to evaluate possesses its own type tag--with
; it, eval would lookup the table for the evaluation procedure
; for this expression, and run it.
;
;    (define (install-eval-lambda)
;      (define (eval-rule exp env)
;        (make-procedure (lambda-parameters exp)
;                        (lambda-body exp)
;                        env))
;      (put 'eval 'lambda eval-rule)
;      'done)
;
;    (define (type-tag exp)
;      (car exp))
;
;    (define (eval exp env)
;      (...)  ; Evaluations of self-evaluating expressions
;             ; and any other expressions with no type tag.
;      ((get 'eval (type-tag exp)) exp env))

; 4.4
(define (and? exp)
  (tagged-list? exp 'and))

(define (or? exp)
  (tagged-list? exp 'or))

; Our language will reuse the operator/operand related
; accessors for logical operators as well.

(define (eval-and exp env)
  (define (evaluate exps)
    (cond ((no-operands? exps) true)            ; no operands: return true
          ((no-operands? (rest-operands exps))  ; final operand: return its value
           (eval (first-operand exps) env))
          ((not (eval (first-operand exps) env)) false)
          (else (eval-and (rest-operands exps) env))))
  (evaluate (operands exp)))

(define (eval-or exp env)
  (define (evaluate exps)
    (cond ((no-operands? exps) false)
          ((eval (first-operand exps) env) => identity)
          (else (eval-or (rest-operands exps) env))))
  (evaluate (operands exp)))

; 4.5
; Cond clauses look like this: ('cond (clauses ...)); each clause
; is either ((predicate) (actions ...)) or ('else (actions ...)).
; To support the new kind of clause, the arrow variant, a type tag
; is necessary within clauses. For lack of a better word, we will
; call the "default" kind of clause 'default:
;
;   ('default (predicate) (actions ...))
;   ('arrow (predicate) (actions ...))
;   ('else '() (actions ...))
;
; This changes the way 'else work until exercise 4.5, where it took
; the place of the predicate in the default type of clause. With this
; new syntax, 'else will still have an empty predicate to simplify
; the cond-actions accessor.
