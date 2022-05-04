;s1084098 Oberhofer Julian
#lang racket

(require racket/trace)

(define (trenne) (displayln "---------------"))

#|1. Define at least five trees, i.e., nested lists. Two of them shall have at least depth 3. Use these trees as
examples to check your implementation of the further assignments.|#

(define t1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define t2 (list 1 (list 2 3) 4 (list 5 6) (list 7 8)))
(define t3 (list (list (list 1 2) (list 3 4)) (list (list 5 6) (list 7 8))))
(define t4 (list 1 2 3 4 5 6 7 8))
(define t5 (list (list (list (list (list 1 2) 3) 4 5) 6) 7))

;2. Compare the function accumulate-tree with “accumulate” (foldr):
(define (accumulate-tree tree term op init)
  (cond ((null? tree) init)
       ((not (pair? tree)) (term tree))
       (else (op (accumulate-tree (car tree) term op init)
                 (accumulate-tree (cdr tree) term op init)))))

(trenne)

;Use accumulate-tree to implement sum and count such that:

(define (count t)
  (accumulate-tree t (λ (x) 1) + 0))
(define (sum t)
  (accumulate-tree t (λ (x) x) + 0))



(count t1) ;7 the number of leafs
(count t2) ;8
(count t3) ;8
(count t4) ;8
(count t5) ;7
(sum t1) ;28 the sum of all leafs
(sum t2) ;36
(sum t3) ;36
(sum t4) ;36
(sum t5) ;28

(trenne)

#|3. Write a function leaflist that takes as argument a tree (represented as a nested list) and returns a list
whose elements are all the leaves of the tree arranged in left-to-right order. Note: list?, pair?, empty?,
append could be helpful.|#

(define (leaflist t [l null])
  (cond ((null? t) l)
        ((not (pair? t)) (list t))
        (else (append (leaflist (car t) l) (leaflist (cdr t) l) l))))
;(trace leaflist)

(define x (list (list 1 2) (list 3 4)))
(leaflist (list x x)) ;'(1 2 3 4 1 2 3 4)

(leaflist t1) ;'(1 2 3 4 5 6 7)
(leaflist t2) ;'(1 2 3 4 5 6 7 8)
(leaflist t3) ;'(1 2 3 4 5 6 7 8)
(leaflist t4) ;'(1 2 3 4 5 6 7 8)
(leaflist t5) ;'(1 2 3 4 5 6 7)

(trenne)

#|4. Fill in the missing expressions to complete the following definitions of the basic list-manipulation
operations (map, append, length) as accumulations (using foldr):
|#

(define (mymap p xs)
 (foldr (lambda (x y) (cons (p x) y)) null xs))

(define (square x) (* x x))

(map square (range 1 10))
(mymap square (range 1 10))



(define (myappend xs ys)
 (foldr cons ys xs))

(append (list 1 2 3 4 5) (list 6 7 8 9))
(myappend (list 1 2 3 4 5) (list 6 7 8 9))



(define (mylength xs)
 (foldr (λ (x y) (add1 y)) 0 xs))

(length (list 1 2 3 4 5 6 7 8 9))
(mylength (list 1 2 3 4 5 6 7 8 9))

(trenne)

;5. Write a procedure (nestingLevel xs) that determines how deeply nested xs is.

(define (nestingLevel xs (d 0))
  (cond ((null? xs) d)
        ((not (list? xs)) 0)
        (else (max (add1 (nestingLevel (car xs) d)) (nestingLevel (cdr xs) d)))))

;(trace nestingLevel)

(nestingLevel (list 1 2 3)) ;1 (no nesting, just the top-level list)
(nestingLevel (list (list 1 2) 3 4)); 2 (inner list in top-level list)
(nestingLevel (list 1 (list 2 (list 3 4)))) ;3
(nestingLevel (list (list 1 2) (list 3 4))) ;2




