;s1084098 Oberhofer Julian
#lang racket

(require racket/trace)

(define (trenne) (displayln "---------------"))

#|1. Write two procedures checkProp and checkPropOneLine. Both procedures shall take a list of items
and a predicate, i.e. a procedure that returns a boolean value (e.g. even?). It shall return a new list that
contains the corresponding boolean values that indicate whether or not the property holds for the
respective element (i.e., if the predicate evaluates to #t or #f). The implementation (body) of
checkPropOneLine shall be a single line of code and may use built-in procedures such as range, filter,
map. CheckProp must not use range, filter, map, or checkPropOneLine.|#

(define (checkProp1 op l)
  (if (null? l)
      null
      (cons (op (car l)) (checkProp1 op (cdr l)))))

(define (checkProp2 op l)
  (if (null? l)
      null
      (cons (if(op (car l)) #t #f) (checkProp2 op (cdr l)))))

(checkProp1 even? (list 1 2 3 4)) ;'(#f #t #f #t)
(checkProp2 even? (list 1 2 3 4)) ;'(#f #t #f #t)
(checkProp1 + (list 1 2 3 4)) ;'(1 2 3 4) -> not boolean
(checkProp2 + (list 1 2 3 4)) ;'(#t #t #t #t)

'-------------

(define (checkPropOneLine1 op l)
  (map op l))

(define (checkPropOneLine2 op l)
  (map (λ (x) (if (op x) #t #f)) l))

(checkPropOneLine1 even? (list 1 2 3 4)) ;'(#f #t #f #t)
(checkPropOneLine2 even? (list 1 2 3 4)) ;'(#f #t #f #t)
(checkPropOneLine1 + (list 1 2 3 4)) ;'(1 2 3 4) -> not boolean
(checkPropOneLine2 + (list 1 2 3 4)) ;'(#t #t #t #t)

(trenne)

#|2. Write a procedure doAll that takes a list of procedures and an item as arguments and returns a list of
the results of applying each procedure to the item.|#

(define (doAll1 l i)
  (map (λ (x) (x i)) l))

(define (doAll2 l i)
  (if (null? l)
      null
      (cons ((car l) i) (doAll2 (cdr l) i))))

(doAll1 (list sqrt (λ (x) (* x x)) (λ (x) (* x x x))) 4) ;'(2 16 64)
(doAll1 (list length car cdr) (list 1 2 3)) ;'(3 1 (2 3))
(doAll2 (list sqrt (λ (x) (* x x)) (λ (x) (* x x x))) 4) ;'(2 16 64)
(doAll2 (list length car cdr) (list 1 2 3)) ;'(3 1 (2 3))

(trenne)

#|3. Given a list of numbers that represent temperature values in Fahrenheit, write a procedure toCelsius
that converts this list into Celsius scale. Use map and λ-Expressions.|#

(define (toCelsius l)
  (map (λ (x) (* (- x 32) (/ 5 9))) l))

(toCelsius (list -40 32 50)) ;'(-40, 0, 10)

(trenne)

#|4. You are allowed to use any procedure you want:
a. Write a procedure countEvenGreater20 takes a list of values and returns the number of values that
are even and greater than 20.|#

(define (countEvenGreater20 l)
  (foldr (λ (x y) (add1 y)) 0 (filter even? (filter (λ (x) (>= x 20)) l))))

(countEvenGreater20 (list 1 4 22 24 26 33 -44)) ;3

#|b. Write a procedure sumEvenGreater20 takes a list of values and returns the sum of the values that are
even and greater than 20.|#

(define (sumEvenGreater20 l)
  (foldr + 0 (filter even? (filter (λ (x) (>= x 20)) l))))

(sumEvenGreater20 (list 1 4 22 24 26 33 -44)) ;72

(trenne)

#|5.
a. Use the conventional interfaces (e.g., map, filter, foldr, ...) discussed in the lecture and lambda
expressions to calculate the following:|#

(foldr + 0 (map (λ (x) (* x x x))(filter even? (range 10 100))))

#|b. Write a recursive procedure (getFromIdx idx xs) that returns a list with the elements of list xs starting
from index idx.|#

(define (getFromIdx idx xs)
  (cond ((null? xs)
         null)
        ((<= idx 0)
         xs)
        (else (getFromIdx (sub1 idx) (cdr xs)))))

(getFromIdx 2 (list 0 1 2 3 4 5 6)) ; ‘(2 3 4 5 6)
(getFromIdx 2 (list 5 7 4)) ; ‘(4)