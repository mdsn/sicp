#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
		((equal? x (car set)) true)
		(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
	set
	(cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
		((element-of-set? (car set1) set2)
		 (cons (car set1)
			   (intersection-set (cdr set1) set2)))
		(else (intersection-set (cdr set1) set2))))

; exercise 2.59

(define (union-set set1 set2)
  (cond ((null? set1) set2)
		((null? set2) set1)
		((element-of-set? (car set1) set2)
		 (union-set (cdr set1) set2))
		(else (cons (car set1)
					(union-set (cdr set1) set2)))))

(union-set '(1 2 3 4) '(3 4 5 6)) ; '(1 2 3 4 5 6)
(union-set '(1 2) '(3 4 5)) ; '(1 2 3 4 5)
(union-set '() '(1 2 3 4)) ; '(1 2 3 4)
(union-set '(1 2 3 4) '(4 5)) ; '(1 2 3 4 5)

; exercise 2.60

; element-of-set? remains the same. The entire list has to be traversed so it's linear.

; adjoin-set doesn't need to check for existence. It becomes O(1)
(define (adjoin-set x set)
  (cons x set))

; intersection-set remains quadratic; it still needs to check whether each element
; belongs in the result set.

; union-set just appends lists. It's linear.
(define (union-set set1 set2)
  (append set1 set2))

(adjoin-set 3 '(1 2 3)) ; '(3 1 2 3)
(union-set '(a b c d) '(c d e f)) ; '(a b c d c d e f)

; sets as ordered lists
(define (element-of-set? x set)
  (cond ((null? set) false)
		((= x (car set)) true)
		((< x (car set)) false)
		(else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
	'()
	(let ((x1 (car set1)) (x2 (car set2)))
	  (cond ((= x1 x2)
			 (cons x1
				   (intersection-set (cdr set1) (cdr set2))))
			((< x1 x2)
			 (intersection-set (cdr set1) set2))
			((< x2 x1)
			 (intersection-set set1 (cdr set2)))))))

; exercise 2.61
(define (adjoin-set x set)
  (if (null? set)
	(list x)
	(let ((head (car set)))
	  (cond ((= x head) set)
			((> x head)
			 (cons head (adjoin-set x (cdr set))))
			(else (cons x set))))))

(adjoin-set 5 '(1 2 3 8 9)) ; '(1 2 3 5 8 9)
(adjoin-set 8 '(1 2 3 8 9)) ; '(1 2 3 8 9)
(adjoin-set 3 '()) ; '(3)
(adjoin-set 10 '(1 2 3 4)) ; '(1 2 3 4 10)
(adjoin-set 1 '(5 10 15)) ; '(1 5 10 15)

; exercise 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
		((null? set2) set1)
		(else
		  (let ((x1 (car set1)) (x2 (car set2)))
			(cond ((= x1 x2)
				   (cons x1 (union-set (cdr set1) (cdr set2))))
				  ((< x1 x2)
				   (cons x1 (union-set (cdr set1) set2)))
				  ((> x1 x2)
				   (cons x2 (union-set set1 (cdr set2)))))))))

(union-set '(1 2 3 4) '(4 5 7 8)) ; '(1 2 3 4 5 7 8)
(union-set '(1 2 3) '(1 2 3)) ; '(1 2 3)
(union-set '() '(1 2 3)) ; '(1 2 3)
(union-set '(1 2 3) '(2 3 4 5 7 8)) ; '(1 2 3 4 5 7 8)

; sets as binary trees
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
		((= x (entry set)) true)
		((< x (entry set))
		 (element-of-set? x (left-branch set)))
		((> x (entry set))
		 (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
		((= x (entry set)) set)
		((< x (entry set))
		 (make-tree (entry set)
					(adjoin-set x (left-branch set))
					(right-branch set)))
		((> x (entry set))
		 (make-tree (entry set)
					(left-branch set)
					(adjoin-set x (right-branch set))))))

; exercise 2.63
(define (tree->list-1 tree)
  (if (null? tree)
	'()
	(append (tree->list-1 (left-branch tree))
			(cons (entry tree)
				  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
	(if (null? tree)
	  result-list
	  (copy-to-list (left-branch tree)
					(cons (entry tree)
						  (copy-to-list (right-branch tree)
										result-list)))))
  (copy-to-list tree '()))

; tree->list-1 is slower because of the append call -- it causes a linear scan of each resulting
; sublist, over and over as longer sublists are built out of subtrees.
; tree->list-2 on the other hand does only constant-time operations.

(define tree1
  (make-tree 7
			 (make-tree 3
						(make-tree 1 '() '())
						(make-tree 5 '() '()))
			 (make-tree 9
						'()
						(make-tree 11 '() '()))))

(define tree2
  (make-tree 3
			 (make-tree 1 '() '())
			 (make-tree 7
						(make-tree 5 '() '())
						(make-tree 9
								   '()
								   (make-tree 11 '() '())))))

(define tree3
  (make-tree 5
			 (make-tree 3
						(make-tree 1 '() '())
						'())
			 (make-tree 9
						(make-tree 7 '() '())
						(make-tree 11 '() '()))))

(tree->list-1 tree1) ; '(1 3 5 7 9 11)
(tree->list-1 tree2) ; '(1 3 5 7 9 11)
(tree->list-1 tree3) ; '(1 3 5 7 9 11)

(tree->list-2 tree1) ; '(1 3 5 7 9 11)
(tree->list-2 tree2) ; '(1 3 5 7 9 11)
(tree->list-2 tree3) ; '(1 3 5 7 9 11)

; They produce the same results though.

; exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

(list->tree '(1 3 5 7 9 11))
; '(5 (1 ()
;        (3 () ()))
;     (9 (7 () ())
;        (11 () ())))

;                  5
;               /    \
;            1          9
;          / \        /   \
;        ()   3     7     11
;           / \    / \   / \
;          () ()  () () () ()

; A call to partial tree for some number n of elements to insert uses n as counter, divides the counter
; by two (with a bias to the right) and recursively decreases said counter until the base case is reached,
; at which point it ensures the previous call, which will still have a "this-entry", inserts an empty leaf
; node as left tree.
; Once this-entry is popped from the list of elements pending insertion, a recursive call to build the
; right tree with the corresponding elements proceeds in the same way, first building the left trees with
; fewer elements, then taking each this-entry, then building the right sub-trees.
; Since the car of the remaining-elts is always pushed, the order of insertion can be seen by scanning the
; tree from left to right. Thus 1 is leftmost, followed by 3, followed by 5 (this-entry at the topmost call)
; then followed by 7, 9 and 11 in the right subtree of the topmost call.

; exercise 2.64 b
; Each call to partial-tree results in two recursive calls, but these are done with approximately half
; the number of elements each. Thus the exponential number of calls balances out with the logarithmic
; number of elements (?) (logarithmic decrease? exponential decrease?) at each step. Thus, list->tree
; is linear in the number of elements, which sort of makes sense because each element is visited once
; when it is popped as this-entry (besides the initial linear scanning of elements to compute the total
; length of the input list).

; (partial-tree (1 .. 11) 6)
;   left-size = 2
;   (partial-tree (1 .. 11) 2)
;       left-size = 0
;       (quotient 1 2) -> left-size = 0
;       (partial-tree (1 .. 11) 0)
;       left-result <== (cons '() (1 .. 11))
;       left-tree = '(), non-left-elts (1 .. 11)
;       right-size = 2 - 1 = 1
;       this-entry = 1
;       (partial-tree (3 .. 11) 1)
;           left-size = 0
;           left-result <== (cons '() (3 .. 11))
;           left-tree = '(), non-left-elts = (3 .. 11)
;           right-size = 1 - 1 = 0
;           this-entry = 3
;           right-result <== (partial-tree (5 .. 11) 0) = (() (5 .. 11))
;           right-tree = (), remaining-elts = (5 .. 11)
;       right-result <== (cons (make-tree 3 () () (5 .. 11)))
;                     == '((3 () ()) 5 7 9 11)
;       right-tree = (3 () ())
;       remaining-elts = (5 .. 11)
;  left-result <== (cons (make-tree 1 () (3 () ())) (5 .. 11))
;               == '((1 () (3 () ())) 5 7 9 11)
;  left-tree = (1 () (3 () ()))
;  non-left-elts = (5 .. 11)
;  right-size = 6 - 3 = 3
;  this-entry = 5
;  (partial-tree (7 9 11) 3)
;       left-size = 1
;       (partial-tree (7 9 11) 1)
;           left-size = 0
;           left-result <== (() (7 9 11))
;           left-tree = (), non-left-elts = (7 9 11)
;           right-size = 1 - 1 = 0
;           this-entry = 7
;           right-result <== (partial-tree (9 11) 0) = (() (9 11))
;           right-tree = (), remaining-elts = (9 11)
;       left-result <== (cons (make-tree 7 () ()) (9 11))
;                    == '((7 () ()) 9 11)
;       left-tree = (7 () ()), non-left-elts = (9 11)
;       right-size = 3 - 2 = 1
;       this-entry = 9
;       (partial-tree (11) 1)
;           left-size = 0
;           left-result <== (() 11)
;           left-tree = (), non-left-elts = (11)
;           right-size = 1 - 1 = 0
;           this-entry = 11
;           right-result <== '(())
;           right-tree = (), remaining-elts = ()
;       right-result <== (cons (make-tree 11 () ()) ())
;                     == '((11 () ()))
;       right-tree = (11 () ())
;       remaining-elts = ()
;  right-result <== (cons (make-tree 9 (7 () ()) (11 () ())) ())
;                == '((9 (7 () ()) (11 () ())))
;  right-tree = (9 (7 () ()) (11 () ()))
;  remaining-elts = ()
; <== (cons (make-tree 5 (1 () (3 () ()))
;                        (9 (7 () ()) (11 () ())))
;           ())
;  == '((5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))), the final tree.

; exercise 2.65

(define tree->list tree->list-2)
; Assuming tree->list-2 is linear, this is a linear operation:
(tree->list (list->tree '(1 3 5 7 9 11))) ; '(1 3 5 7 9 11)

; From 2.3.3, for a valid representation we can assume the elements in the left subtree are smaller
; and the ones in the right subtree are larger than a node's value. That is to say, tree->list will
; always output an ordered list (if given a balanced tree produced by list->tree lol).

(define tree1 (list->tree '(1 3 5 7 9))) ; (5 (1 () (3 () ())) (7 () (9 () ())))
(define tree2 (list->tree '(5 7 9 11 13))) ; (9 (5 () (7 () ())) (11 () (13 () ())))

; Merge two ordered lists. This operation is linear: it traverses each list at most once.
(define (merge xs ys)
  (cond ((null? xs) ys)
        ((null? ys) xs)
        (else (let ((x (car xs))
                    (y (car ys)))
                (cond ((> x y) (cons y (merge xs (cdr ys))))
                      ((> y x) (cons x (merge (cdr xs) ys)))
                      (else (cons x (merge (cdr xs) (cdr ys)))))))))

(merge '(1 3 5 7 9) '(1 3 5 7 9)) ; '(1 3 5 7 9)
(merge '(1 3 5 7 9) '(5 7 9 11 13)) ; '(1 3 5 7 9 11 13)
(merge '(1 3 5) '(7 9 11 13)) ; '(1 3 5 7 9 11 13)
(merge '() '(1 2 3)) ; '(1 2 3)

(define (union-set set1 set2)
  (let ((list1 (tree->list set1))           ; linear
        (list2 (tree->list set2)))          ; linear
    (list->tree (merge list1 list2))))      ; linear (linear)

(union-set tree1 tree2)
; '(7 (3                               7
;       (1 () ())                3           11
;       (5 () ()))            1     5     9     13
;     (11
;       (9 () ())
;       (13 () ())))

; Remember that for binary trees, element-of-set? is a (log n) operation.
(element-of-set? 3 tree1) ; #t
(element-of-set? 4 tree1) ; #f

; Intersect two ordered lists. This operation is linear: it traverses each list at most once.
(define (intersect xs ys)
  (if (or (null? xs) (null? ys))
    '()
    (let ((x (car xs))
          (y (car ys)))
      (cond ((< x y) (intersect (cdr xs) ys))
            ((< y x) (intersect xs (cdr ys)))
            (else (cons x (intersect (cdr xs) (cdr ys))))))))

(intersect '(1 3 5 7 9) '(1 3 5 7 9)) ; '(1 3 5 7 9)
(intersect '(1 3 5 7 9) '(5 7 9 11 13)) ; '(5 7 9)
(intersect '(1 3 5) '(7 9 11)) ; '()

(define (intersection-set set1 set2)
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (list->tree (intersect list1 list2))))

(intersection-set tree1 tree2) ; '(7 (5 () ()) (9 () ()))

; Ta-da!
