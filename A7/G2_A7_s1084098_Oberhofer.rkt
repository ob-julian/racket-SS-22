;s1084098 Oberhofer Julian
#lang racket

(require racket/trace)

(define (trenne) (displayln "---------------"))

#|1. Implement procedure toCelsius (assignment 3, Blatt 5) by using list comprehension instead of map.|#

;old:
(define (toCelsiusOld l)
  (map (λ (x) (* (- x 32) (/ 5 9))) l))

;new:

(define (toCelsius l)
  (for/list ((i l)) (* (- i 32) (/ 5 9))))

(toCelsius (list -40 32 50)) ;'(-40, 0, 10)

(trenne)

#|2. Consider the rainfall problem (discussed in lecture 0, slide 10). Use list comprehension to implement a
procedure (rain xs) that solves the rainfall problem.|#

(require math/statistics)
(define (rain xs)
  (mean (for/list ((l xs)  #:break (= l -999) #:when (> l 0)) l)))

(rain (list 1 2 -100 -999 1000)) ;1.5

(trenne)

#|3. Implement the function length and filter using pattern matching by splitting the list into its head and tail.|#

(define (myLength xs (l 0))
  (match xs
    ('() l)
    ((cons h t) (myLength t (add1 l)))))
 
(myLength (list 1 2 3 4 5 6 7 8 9))
(myLength (list 1 2 3 4 5 6 7 8 9))
(length null)
(length null)



(define (myFilter p xs (l null))
  (match xs
    ('() l)
    ((cons h t) (if (p h) (myFilter p t (append l (list h))) (myFilter p t l)))))

(myFilter even? (range 1 11))
(filter even? (range 1 11))

(trenne)

#|4. Compare the performance of the original tree-recursive fib and its memorized variant in terms of (a)
execution time and (b) number of procedure invocations.|#

(define c 0)

(define (fib n)
  (set! c (add1 c))
  (if (< n 2) n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define ht (make-hash))
(define (fib-mem n)
  (set! c (add1 c))
  (if (hash-has-key? ht n)
      (hash-ref ht n)
      (begin
        (hash-set! ht n (if (< n 2) n (+ (fib-mem (- n 1)) (fib-mem (- n 2)))))
        (hash-ref ht n))))

(time (void (fib 30)))
c
(set! c 0)
(time (void (fib-mem 30)))
c
(set! c 0)
(time (void (fib-mem 30)))
c
(set! c 0)

(trenne)

#|5. Imagine you want to go up a flight of stairs that has n steps. Suppose you can take either a single step or
2 steps at once. How many different way do exist to reach the top?
Example n=5: 1 1 1 1 1, 2 1 1 1, 1 2 1 1, 1 1 2 1, 1 1 1 2, 1 2 2, 2 1 2, 2 2 1  8 different ways.
Implement the procedure countstairs that solves this problem. Note: we are only interested in the number
of possibilities not in visualization.|#

(require math/number-theory)

;(define (countstairs n)
;  (foldr + 0 (map (λ (x) (binomial (- n x) x)) (range 0 (+ 1 (floor (/ n 2)))))))

(define (countstairs n)
  (fib-mem (add1 n))

(map countstairs (range 1 15)) ;'(1 2 3 5 8 13 21 34 55 89 144 233 377 610)
(map fib-mem (range 2 16))
