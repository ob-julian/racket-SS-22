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
  (for/list ((i l))
    (λ (i) (* (- i 32) (/ 5 9)))))

(toCelsius (list -40 32 50)) ;'(-40, 0, 10)

