(#%require racket/base)

; How to use Racket's thread facilities: import racket/base since running
; this file with -I sicp prevents those symbols from being in the environment.
;
; (define worker (thread (lambda ()
;                          (let loop ()
;                            (displayln "Working...")
;                            (sleep 0.2)
;                            (loop)))))
; (sleep 2.5)
; (kill-thread worker)

; 3.39
; The original five possible execution orders and their results:
; - 101: P_1 sets x to 100, P_2 increments x to 101.
; - 121: P_2 increments x to 11, then P_1 sets x to x*x.
; - 110: P_2 changes x from 10 to 11 between the two accesses to
;   x during the evaluation of (* x x)
; -  11: P_2 accesses x, then P_1 sets x to 100, then P_2 sets x.
; - 100: P_1 accesses x twice, then P_2 sets x to 11, then P_1 sets x.
;
; Now, we want to consider the following serialization and the potential
; behaviors:
;
;   (define x 10)
;   (define s (make-serializer)
;   (parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
;                     (s (lambda () (set! x (+ x 1)))))
;
; - 101 is still possible.
; - 121 is still possible.
; - 100 is still possible.
; The evaluation of (* x x) is performed atomically, but the setting of
; that value into x (in P_1) is not.

; 3.40
; First, the two "natural" orders, if each of P_1 and P_2 execute
; without preempting each other:
; - P_1 sets x to 100, then P_2 sets x to 1_000_000.
; - P_2 sets x to 1000, then P_1 sets x to 1_000_000.
; If the multiplications get fully evaluated, but the other process
; preempts the setting, the first write will be overwritten:
; - P_1 evaluates (* x x) to 100 and gets preempted, P_2 sets x to
;   1000, then P_1 sets x to 100.
; - P_2 evaluates (* x x x) to 1000, gets preempted, P_1 sets x to 100,
;   then P_2 sets x to 1000.
; Then the various ways in which the evaluation of (* x x) or (* x x x)
; themselves can be preempted. Of all the ways this could happen the only
; ones that matter are the ones where a set! is evaluated in between reads
; of x, because these change the value in between reads:
; - P_1 reads x, then P_2 evaluates (* x x x) and sets x to 1000, then
;   P_1 reads x again and ends up setting x to 10000.
; - P_2 reads x, then P_1 evaluates (* x x) and sets x to 100, then
;   P_2 evaluates x twice and sets x to 100_000.
; - P_2 evaluates x twice, then P_1 sets x to 100, then P_2 reads x
;   one final time and sets x to 10_000.
;
; Now suppose we serialize each of the procedures. Then either P_1 runs
; atomically and sets x to 100, then P_2 sets it to 1_000_000, or P_2
; runs atomically setting x to 1000, and P_1 in turn sets it to 1_000_000
; (the "natural" orders from the top).

; 3.41
; Without serializing the read (the access to balance), a process could
; be preempted at any point until, and including, the read of balance. One
; potential anomalous behavior would be if process 1 requests 'balance, the
; balance is evaluated, but before dispatch returns, process 2 preempts it
; and deposits or withdraws some balance. Process 1 runs again, and gives
; an outdated value to its caller.

; 3.42
; Evaluating (protected f) only adds f to the serialized set of procedures
; of the `protected` serializer--it does not evaluate f (or its serialized
; wrapper). Therefore this change has no observable effect in what concurrency
; is allowed.
