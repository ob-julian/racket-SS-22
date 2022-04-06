;s1084098 Oberhofer Julian
#lang racket

(require racket/trace)

(define (trenne) (displayln "---------------"))

#|1.
Implement the procedure (mylast xs). It shall return the last element in the given list xs.|#

(define (mylast xs) ;genau wie bei last firft mylast bei aufruf mit null eine fehler
  (if (null? (rest xs))
      (first xs)
      (mylast (rest xs))))

(mylast (list 1 2 3)) ;3

(trenne)

#|2. Implement procedure tailLength that returns the length of a list. It shall be a tail-recursive version of
(my)length-procedure discussed in the lecture.|#

(define (tailLength x  (a 0)) ;wei bei length ist null = 0 in länge
  (if (null? x)
      a
      (tailLength (rest x) (add1 a))))

(tailLength (list 1 2 3)) ;3

(trenne);


#|Write a function (combine op xs ys) that takes a function op and two lists xs and ys as input. It
returns a new list by applying op on the elements with matching index. We can assume |xs| = |ys|.|#

(define (combine os xs ys)
  (if (null? xs)
      null
      (append (list (os (first xs) (first ys))) (combine os (rest xs) (rest ys))))) ; (append (list x) (y z)) kann einfacher als (cons x (y z) geschrieben werden


(define (combineIter os xs ys (i null))
  (if (null? xs)
      i
      (combineIter os (rest xs) (rest ys) (append i (list (os (first xs) (first ys)))))))

(trace combine)
(combine + (list 1 2 3) (list 4 5 6)) ; ‘(5 7 9)

(trace combineIter)
(combineIter + (list 1 2 3) (list 4 5 6)) ; ‘(5 7 9)

(trenne)

#|4.Write a recursive procedure (isSame xs ys) that returns #t iff (if and only if ) the two lists xs and ys are
the same (they contain exactly the same elements in the same order). |#


(define (isSame xs ys)
  (cond ((and (null? xs) (null? ys))
          #t)
         ((or (null? xs) (null? ys) (not (= (first xs) (first ys))))
          #f)
         (else
          (isSame (rest xs) (rest ys)))))




(isSame (list 1 2) (list 1))   ; #f
(isSame (list 1 2) (list 2 1)) ; #f
(isSame (list 1 2) (list 1 2)) ; #t
(isSame null null) ; #t

(trenne)

#|5. Take a look at the procedure minbad. It shall return the minimum value of a list xs, but it has the
reputation of performing very inefficiently.|#

(define (minbad xs)
 (cond ((null? (cdr xs))
          (car xs))
       ((< (car xs)(minbad (cdr xs)))
          (car xs))
       (else
          (minbad (cdr xs)))))

;a) show that it performs pretty bad in the average case.

;(trace minbad)

(define (machMalSoVielBestCase k (l null))
  (if (< k 1)
      l
      (machMalSoVielBestCase (sub1 k) (append (list k) l))))

#|(define (machMalSoVielWorstCase k (l null));achtung lange laufzeit bei großen werten
  (if (< k 1)
      l
      (machMalSoVielWorstCase (sub1 k) (append l (list k)))))|#

(define (machMalSoVielWorstCase k)
  (reverse (machMalSoVielBestCase k)))


(define (machMalSoVielEtwaAverageCase k (l null))
  (if (< k 1)
      l
      (if (> (random) 0.5)
          (machMalSoVielEtwaAverageCase (sub1 k) (append (list k) (reverse l)))
          (machMalSoVielEtwaAverageCase (sub1 k) (append (list k) l)))))

;(time (void (minbad (machMalSoVielBestCase 1000000))));1M ~1/3 sec

;(time (void (minbad (machMalSoVielWorstCase 25)))); 25 ~1sec

;(time (void (minbad (machMalSoVielEtwaAverageCase 25)))); 25 ~zwischen 200msec und 1sec


;b) explain why this is the case

;für das herausfinden von min wird die erste stelle der list verglichen mit dem imimun der restlichen liste. D.h. für jeden eintrag in der liste der nicht das minimum ist muss vorher die gesamte liste verglichen werden um das minimum danach zu finden


;c) implement a more efficient version

(define (minNotSoBad xs)
 (cond ((null? (cdr xs))
          (car xs))
          (else
           (define-values (x) (minNotSoBad (cdr xs)))
           (if (< (car xs) x)
               (car xs)
               x))))

(define (myMin x (y +inf.0));besser (y (car x))
             (cond ((null? x)
                    y)
                   ((< (first x) y)
                    (myMin (rest x) (first x)))
                   (else
                    (myMin (rest x) y))))

(time (void (myMin (machMalSoVielWorstCase 1000000)))) ;~200msce
(time (void (myMin (machMalSoVielBestCase 1000000))))  ;~200msce

(time (void (minNotSoBad (machMalSoVielWorstCase 1000000)))) ;~250msce
(time (void (minNotSoBad (machMalSoVielBestCase 1000000))))  ;~250msce