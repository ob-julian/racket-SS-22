;s12105875 Oberhofer Julian
#lang racket

(require racket/trace)

(define (trenne) (displayln "---------------"))

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (inc x) (+ x 1))

#|1. a) Write a procedure min-fx-gx that takes two numerical procedures f and g and a number x as
input, and that returns the minimum of applying f to x and g to x. Note: you may use min).|#

(define (min-fx-gx f g x)
  (min (f x) (g x)))

(min-fx-gx square cube -1) ;-1
(min-fx-gx square cube 2) ;4
(min-fx-gx (λ (x) (* x x)) (λ (x) (+ x x)) -1)

#|b) Then generalize these examples so that the procedure you apply to the results of f and g is a
parameter too. The name of the procedure shall be combine-fx-gx.|#


(define (combine-fx-gx a f g x)
  (a (f x) (g x)))

(trenne)

;2. Consider the following procedure:

(define (f g)
 (g 5))


;First think, then try:

(f +) ;solle 5 ausgeben, da (+ 5) ausgeführt wird
(f square) ; (square 5) = 25
(f (lambda (x) (* x (+ x 2)))) ;(* 5 (+ 5 2)) = (* 5 7) = 35
;(f f) ;error, da (f 5) versuchen würde (5 5) auszuführen, was nicht geht

(trenne)

#|3. Consider the sumrec function below. Rewrite the function such that the sum calculation results in
an iterative process; the new function name shall be sumiter. Do NOT use any Racket feature that we
haven’t discussed in the lecture! Test your code with different term-functions. Compare the
performance between sumrec and sumiter.|#

(define (sumrec term a b)
 (if (> a b)
 0
 (+ (term a)
 (sumrec term (+ a 1) b))))

(trace sumrec)
(sumrec * 10 15)

(define (sumiter term a b [c 0])
  (if (> a b)
      c
      (sumiter term (+ a 1) b (+ (term a) c))))

(trace sumiter)
(sumiter * 10 15)

(trenne)

#|4. Hint: Make use of the ability to have functions as return values. Use lambda.
a) Define a procedure twice that takes a procedure of one argument and returns a procedure that
applies the original procedure twice.|#

(define (twice p)
  (λ (x) (p (p x))))

((twice square) 4) ; 256

#|b) Define a procedure comp that implements composition: The composition f after g is defined to be
the function )). The functions f and g shall be functions that have one argument only. |#

(define (comp f g)
  (λ (x) (f (g x))))

((comp cube inc) 2) ;27
((comp inc cube) 2) ;9

(trenne)

#|5. Be sure to understand the self-made mycons, mycar, mycdr functions. Define a data abstraction
for representing complex numbers and implement a function (add-complex c1 c2) that returns the
sum of two complex numers. Do not use built-in pairs, but use the self-made mycons/mycar/mycdr
from the lecture. Also write a print-complex function that outputs a nice representation of a complex
number.|#

; konstruktor und 
;also datenstucktur reicht ein cons aus weil man mint (cons x y) die Komplexe Zahl als: x+yi darstellen kann.

(define (mycons x y)
  (λ (m) (m x y)))

(define (mycar z)
  (z (λ (p q) p)))

(define (mycdr z)
  (z (λ (p q) q)))

(define (add-complex c1 c2)
  (mycons (+ (mycar c1) (mycar c2)) (+ (mycdr c1) (mycdr c2))))

(define (print c)
  (if (= 0 (mycar c)) (void) (display (mycar c))) (cond ((= 0 (mycdr c)) (void)) (else (if (and (> (mycdr c) 0) (not (= 0 (mycar c)))) (display '+) (void)) (display (mycdr c)) (display 'i))))

(print (add-complex (mycons 1 4) (mycons 2 3)))
(println void )
(print (mycons 0 1))



