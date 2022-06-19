;s1084098 Oberhofer Julian
#lang racket

(require racket/trace)

(define (trenne) (displayln "---------------"))


#|1) Define a procedure (makeHydra n) that generates/fills a data-structure to represent an n-headed Hydra.|#

(define (makeHydra n)
  (map (λ (x) n) (range n)))

#|2) Implement a procedure that counts how many cuts are required until such a beast is dead|#

(define (howManyCutts li pro (c 0))
  (displayln li)
  (if (null? li)
      c
      (howManyCutts (pro li) pro (add1 c))))

(define killFirstHead
  (λ (x) (append (map (λ (y) (sub1 (car x))) (range (sub1 (car x)))) (cdr x))))

(howManyCutts (makeHydra 3) killFirstHead)

(trenne)

#|3) Implement a procedure that counts the max. number of heads that exist concurrently|#

(define (howManyMax li pro (c 0))
  (if (null? li)
      c
      (howManyMax (pro li) pro (max c (length li)))))

(howManyMax (makeHydra 3) killFirstHead)