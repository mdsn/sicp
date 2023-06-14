#lang sicp

(define a 1)
(define b 2)
(list a b) ; '(1 2)
(list 'a 'b) ; '(a b)
(list 'a b) ; '(a 2)

(car '(a b c)) ; 'a
(cdr '(a b c)) ; '(b c)

(define (memq x xs)
  (cond ((null? xs) false)
		((eq? x (car xs)) xs)
		(else (memq x (cdr xs)))))

(memq '3 '(a b 2)) ; #f
(memq 'a '(1 x a)) ; '(a)
(memq 'x '(a b x d e)) ; '(x d e)

; exercise 2.53
(list 'a 'b 'c) ; '(a b c)
(list (list 'george)) ; '((george))
(cdr '((x1 x2) (y1 y2))) ; '((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; '(y1 y2)
(pair? (car '(a short list))) ; #f
(memq 'red '((red shoes) (blue socks))) ; #f
(memq 'red '(red shoes blue socks)) ; '(red shoes blue socks)

; exercise 2.54
(define (equal? a b)
  (cond ((and (symbol? a) (symbol? b))
		 (eq? a b))
		((and (equal? (car a) (car b))
			  (equal? (cdr a) (cdr b)))
		 true)
		(else false)))

(equal? '(this is a list) '(this is a list)) ; #t
(equal? '(this is a list) '(this (is a) list)) ; #f

; exercise 2.55
(car ''abracadabra) ; 'quote
(cdr ''abracadabra) ; '(abracadabra)
(cadr ''abracadabra) ; 'abracadabra
(quote abracadabra) ; 'abracadabra
(quote 'abracadabra) ; ''abracadabra
(quote (quote abracadabra)) ; ''abracadabra
(car (quote (quote abracadabra))) ; 'quote
