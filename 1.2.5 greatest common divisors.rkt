#lang scheme

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; exercise 1.20, normal order
; (gcd 206 40)
; (gcd 40 (remainder 206 40)
;   (if (= (remainder 206 40) 0)
;     40
;     (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))))
; if evaluates to select a branch, 1 evaluation, remainder 206 40 = 6, else branch selected:
;
; (gcd (remainder 206 40) (remainder 40 (remainder 206 40))
;   (if (= (remainder 40 (remainder 206 40)) 0)
;     (remainder 206 40)
;     (gcd (remainder 40 (remainder 206 40))
;          (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
; if evaluates most nested reminder, (reminder 206 40) = 6, then we have
;   ... (if (= (reminder 40 6) 0)) ...  -> 2 evaluations of reminder
; and then the outer reminder is evaluated to select a branch. (reminder 40 6) = 4, so we have
;   ... (if (= 4 0)) ... -> 3 evaluations of reminder
;
; (gcd (remainder 40 (remainder 206 40))
;      (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;   (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
;       (remainder 40 (remainder 206 40))
;       (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;            (reminder (remainder 40 (remainder 206 40))
;                      (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
; if evaluates,
;   remainder 206 40 = 6,
;   remainder 40 6 = 4,
;   remainder 206 40 = 6,
;   remainder 6 4 = 2, to select branch. (That's 4 more evaluations, so 7 so far.)
;
; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;      (reminder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;   (if (= (reminder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0)
;       (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;       (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;            (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;                       (reminder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))))
; if starts evaluating.
;   remainder 206 40 = 6 -> 8 evaluations
;     (if (= (reminder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 6))) 0)
;   remainder 206 40 = 6 -> 9 evaluations
;     (if (= (reminder (remainder 40 (remainder 206 40)) (remainder 6 (remainder 40 6))) 0)
;   remainder 40 6 = 4 -> 10 evaluations
;     (if (= (reminder (remainder 40 (remainder 206 40)) (remainder 6 4)) 0)
;   remainder 206 40 = 6 -> 11 evaluations
;     (if (= (reminder (remainder 40 6) (remainder 6 4)) 0)
;   remainder 40 6 = 4 -> 12 evaluations
;     (if (= (reminder 4 (remainder 6 4)) 0)
;   remainder 6 4 = 2 -> 13 evaluations
;     (if (= (reminder 4 2) 0)
;   remainder 4 2 = 0 -> 14 evaluations, if compares and first branch is selected:
;
; (remainder (remainder 206 40) (remainder 40 (remainder 206 40))), remainder 206 40 = 6 -> 15 evaluations
; (remainder (remainder 206 40) (remainder 40 6)), remainder 206 40 = 6 -> 16 evaluations
; (remainder 6 (remainder 40 6)), remainder 40 6 = 4 -> 17 evaluations
; (remainder 6 4), remainder 2 -> 18 evaluations total.

; applicative order
; gcd 206 40
; gcd 40 (remainder 206 40), applicative order evaluates (remainder 206 40) = 6 before the call to gcd
;   call ends up being gcd 40 6
; gcd 6 (remainder 40 6), remainder is evaluated = 4 before call to gcd
;   results in call gcd 6 4
; gcd 4 (remainder 6 4), remainder is evaluated to 2,
;   call is gcd 4 2
; gcd 2 (remainder 4 2), remainder -> 0
;   final call is gcd 2 0
; -> a = 2 with 4 total evaluations of remainder.