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

; 3.43
; Suppose exchanges run sequentially--that is, the observable behavior is as
; if each ran to completion and in sequence. Exchanges of an account with itself
; are a no-op. Every other possible exchange must be between one account and
; either of the other two. Since the exchanges are sequential both account
; balances are preserved after the exchange, which means all three account
; balances are preserved after an arbitrary sequence of exchanges.
;
; Now, the first version of the exchange procedure:
;
;    (define (xchg a b)
;      (let ((d (- (a 'balance)
;                  (b 'balance))))
;        ((a 'withdraw) d)
;        ((b 'deposit) d)))
;
; We want to show how this implementation is susceptible to race conditions
; that violate the expected consistency of the account balances.
;
;                          A           B            C           (Time flows
;                         $10         $20          $30           downward)
;
; (xcgh A B)
;    - Access balance A 10
;    - Access balance B 20
;                                       (xchg A C)
;    - Compute difference -10               - Access balance A 10
;    - Set d <- -10                         - Access balance C 30
;                                           - Compute difference -20
;                                           - Withdraw -20 from A; A = 30
;    - Withdraw -10 from A, A = 40 (!)
;                                           - Deposit -20 into C; C = 10
;    - Deposit -10 into B, B = 10
;
; Final balances: A = 40, B = 10, C = 10. Although the total amount of cash
; has been preserved, the account balances are no longer 10, 20, 30.
;
; Why is the sum of the balances preserved regardless of the (arbitrary) order
; of execution of a non-serialized set of concurrent exchanges? Assuming the
; transactions (withdraw/deposit) operations are atomic, then regardless of
; the order in which they happen they always happen in mirrored pairs with
; an equal difference. That is to say, the same amount of money is withdrawn
; and deposited in the entire system, even if transactions preempt each other
; and cause individual balances to become invalid. No money disappears from
; the system, and no new money is introduced in it; in other words, the sum
; of the balances of the accounts is preserved.
;
; Now we want to show that if we let go of the constraint that the transactions
; themselves are serialized, then even the total amount of money in the system
; is not guaranteed to be preserved. For this it will be useful to recall the
; internals of deposit and withdraw:
;
;    (define (withdraw amount)
;      (if (>= balance amount)
;        (begin (set! balance (- balance amount))
;               balance)
;        "Insufficient funds"))
;
;    (define (deposit amount)
;      (set! balance (+ balance amount))
;      balance)
;
; The new conditions mean that either of these two procedures may be preempted
; by other concurrent processes.
;
;               A=10                        B=20                        C=30
; (xchg A B)
;   Access A = 10
;   Access B = 20
;   Compute D = -10
;   Set D = -10
;   Withdraw -10 from A
;       Balance A=10 >= -10? Yes
;                                       (xchg A C)
;                                           Access A = 10
;                                           Access C = 30
;                                           Compute D = -20
;                                           Set D = -20
;                                           Withdraw -20 from A
;                                               Balance A=10 >= -20? Yes
;                                               Compute new balance A'= 30
;       Compute new balance A'= 20
;                                               Set new balance     A = 30
;       Set new balance     A = 20.   (!)
;                                           Deposit -20 into C
;                                               Compute new balance C''= 10
;                                               Set new balance     C  = 10.
;   Deposit -10 into B
;       Compute new balance B'= 10
;       Set new balance     B = 10.
;
; Final balances: A = 20, B = 10, C = 10. We effectively lost $20 due to two
; concurrent withdrawals from A stepping on each other.

