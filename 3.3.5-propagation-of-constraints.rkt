(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception)
           (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

; connectors
(define (has-value? connector) (connector 'has-value?))
(define (get-value connector) (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (make-connector)
  (let ((value false)
        (informant false)
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
        (begin (set! informant false)
               (for-each-except retractor
                                inform-about-no-value
                                constraints))
        'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints
              (cons new-constraint constraints)))
      (if (has-value? me)
        (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR" request))))
    me))

; constraints
(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2 (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1 (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
            (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
            (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
            (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

; celsius/fahrenheit converter
(define C (make-connector))
(define F (make-connector))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(celsius-fahrenheit-converter C F)

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user)
; Probe: Celsius temp = 25
; Probe: Fahrenheit temp = 77'done

(set-value! F 212 'user)
; Contradiction (mcons 77 (mcons 212 '()))

(forget-value! C 'user)
; Probe: Celsius temp = ?
; Probe: Fahrenheit temp = ?'done

(set-value! F 212 'user)
; Probe: Fahrenheit temp = 212
; Probe: Celsius temp = 100'done

(forget-value! C 'user) ; 'ignored

; exercise 3.33
(define (averager a b c)
  (let ((sum (make-connector))
        (two (make-connector))
        (equals (make-connector)))
    (constant 2 two)
    (multiplier two c equals)
    (adder a b equals)
    'ok))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
(probe "Average" c)
(averager a b c)

(set-value! a 5 'user) ; 'done
(set-value! b 13 'user)
; Probe: Average = 9
; 'done

(set-value! a 8 'user)
; Contradiction (mcons 5 (mcons 8 '())) [,bt for context]

(forget-value! a 'user) ; Probe: Average = ?'done
(forget-value! b 'user) ; 'done
(get-value c) ; 9   can't forget manually!

; exercise 3.34
; Louis' squarer:
;
;   (define (squarer a b) (multiplier a a b))
;
; The definition of squarer aliases multiplier's arguments m1 and m2.
; The constraint can only be connected once to the connector because of
; the `memq` check.
; Now suppose we set a value on connector a. It does not have a value
; so the first clause of the cond runs, and `value` and `informant` are
; set on it. The multiplier is informed about the new value; within it,
; both m1 and m2 have a value (they are the same connector), so the square
; of a is propagated to b.

(define (squarer a b) (multiplier a a b))
(define a (make-connector))
(define b (make-connector))
(squarer a b) ; #<procedure>
(probe "Square" b) ; #<procedure>
(probe "Root" a) ; #<procedure>
(set-value! a 3 'user)
; Probe: Root = 3
; Probe: Square = 9'done

; But an expectation is that, the other way around, this connector will
; be a square root constraint box.

(forget-value! a 'user)
; Probe: Root = ?
; Probe: Square = ?'done
(set-value! b 25 'user)
; Probe: Square = 25'done
;
; Nothing on a--we have forgotten a so we only have one of the three
; necessary values the multiplier cond understands. None of the value
; handling scenarios in multiplier's `process-new-value` can handle
; the situation, and we don't get the root propagated back.

