#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define symbol-leaf cadr)
(define weight-leaf caddr)

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define left-branch car)
(define right-branch cadr)
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)
                             (cadr pair))
                  (make-leaf-set (cdr pairs))))))

(define test-pairs '((A 4) (B 2) (C 1) (D 1)))

(make-leaf-set test-pairs) ; '((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4))

; A less manual one
(define (make-leaf-set-fn pairs)
  ; foldl op zero seq
  ; op :: Pair -> [Leaf] -> [Leaf]
  (foldl
    (lambda (pair leaf-set)
      (adjoin-set (make-leaf (car pair) (cadr pair))
                  leaf-set))
    '()
    pairs))

(make-leaf-set-fn test-pairs) ; '((leaf C 1) (leaf D 1) (leaf B 2) (leaf A 4))

(define make-leaf-set make-leaf-set-fn)

; exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))
; ((leaf A 4)
;  ((leaf B 2)
;   ((leaf D 1)
;    (leaf C 1)
;    (D C)
;    2)
;   (B D C)
;   4)
;  (A B D C)
;  8)

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree) ; '(A D A B B C A)

; exercise 2.68
(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (encode-1 symbol current-branch)
    (if (leaf? current-branch)
      '()
      (if (branch-contains (left-branch current-branch) symbol)
        (cons 0 (encode-1 symbol (left-branch current-branch)))
        (cons 1 (encode-1 symbol (right-branch current-branch))))))
  (if (not (branch-contains tree symbol))
    (error "bad symbol -- ENCODE-SYMBOL" symbol)
    (encode-1 symbol tree)))

(define (branch-contains branch symbol)
  (memq symbol (symbols branch)))

(branch-contains sample-tree 'D) ; '(D C)
(branch-contains sample-tree 'F) ; #f

(encode-symbol 'F sample-tree) ; ; bad symbol -- ENCODE-SYMBOL 'F [,bt for context]

(define encoded-sample
  (encode
    (decode sample-message sample-tree)
    sample-tree)) ; '(0 1 1 0 0 1 0 1 0 1 1 1 0)

(equal? encoded-sample sample-message) ; #t

; exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (insert-in-position node nodes)
  (cond ((null? nodes) (list node))
        ((< (weight (car nodes)) (weight node))
         (cons (car nodes) (insert-in-position node (cdr nodes))))
        (else (cons node nodes))))

(define test-leaf-set (make-leaf-set test-pairs))
(display test-leaf-set) ; ((leaf C 1) (leaf D 1) (leaf B 2) (leaf A 4))

(define test-merged-tree
  (insert-in-position (make-code-tree (car test-leaf-set) (cadr test-leaf-set))
                      (cddr test-leaf-set))) ; '(((leaf C 1) (leaf D 1) (C D) 2) (leaf B 2) (leaf A 4))

(insert-in-position (make-leaf 'F 3) test-merged-tree)
; '(((leaf C 1) (leaf D 1) (C D) 2) (leaf B 2) (leaf F 3) (leaf A 4))

(define (successive-merge leaf-set)
  (cond ((null? leaf-set) '())
        ((= 1 (length leaf-set)) (car leaf-set))
        (else (length leaf-set)
           (successive-merge
             (insert-in-position
               (make-code-tree (car leaf-set) (cadr leaf-set))
               (cddr leaf-set))))))

(define generated-tree (generate-huffman-tree test-pairs))
; '((((leaf C 1) (leaf D 1) (C D) 2) (leaf B 2) (C D B) 4)
;   (leaf A 4)
;   (C D B A)
;   8)
;
;        (CDBA|8)
;     (CDB|4)   (A|4)
; (CD|2)  (B|2)

(decode sample-message generated-tree) ; '(B A D B B A A)
(encode (decode sample-message generated-tree) generated-tree) ; '(0 1 1 0 0 1 0 1 0 1 1 1)

; exercise 2.70

(define rock-pairs
  '((NA 16)
    (YIP 9)
    (SHA 3)
    (A 2)
    (GET 2)
    (JOB 2)
    (BOOM 1)
    (WAH 1)))

(define rock-tree (generate-huffman-tree rock-pairs))

(define lyrics
  '(GET A JOB
        SHA NA NA NA NA NA NA NA NA
        GET A JOB
        SHA NA NA NA NA NA NA NA NA
        WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
        SHA BOOM))
(length lyrics) ; 36

(define lyrics-encoded (encode lyrics rock-tree))

(length lyrics-encoded) ; 84

(decode lyrics-encoded rock-tree)
; '(GET A JOB
;   SHA NA NA NA NA NA NA NA NA
;   GET A JOB
;   SHA NA NA NA NA NA NA NA NA
;   WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
;   SHA BOOM)

; For this alphabet, to encode 8 symbols we'll need 3 bits per symbol. The message is 36 symbols long,
; so 3*36 = 108 bits if we use a fixed length code, vs 84 bits with the variable length code.

; exercise 2.71
(generate-huffman-tree
  '((A 1) (B 2) (C 4) (D 8) (E 16)))
; '(((((leaf A 1)
;      (leaf B 2)
;      (A B) 3)
;     (leaf C 4)
;     (A B C)
;     7)
;    (leaf D 8)
;    (A B C D)
;    15)
;   (leaf E 16)
;   (A B C D E)
;   31)

;               (ABCDE)
;        (ABCD)         (E)
;    (ABC)   (D)
;  (AB) (C)
;(A)(B)

; To encode most frequent symbol (E): 1 bit
; To encode least frequent symbol (A): 4 bits (0000)

; exercise 2.72
; Let's assume a Huffman tree for n symbols, with frequency n_i = 2^(i-1), so powers of two starting at 1.
; Then the most frequent symbol is one step away from the root, the left-branch subset of symbols is searched
; linearly for a total of (n-1) elements.
; For the least frequent symbol, n-1 levels have to be traversed, with linear searches over increasingly
; small subsets of symbols. So (n-1) are searched, then (n-2), ... then just 1. That is to say, (n(n+1))/2.
; Thus the number of steps to reach the least frequent symbol grows quadratically with the size of the
; alphabet.
