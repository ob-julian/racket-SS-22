;s1084098 Oberhofer Julian
#lang racket

(require racket/trace)

(define (trenne) (displayln "---------------"))

;Aus der VO:

(define-syntax-rule (s-delay exp)
  (λ() exp))

(define (s-force delayedObject)
  (delayedObject))

(define empty-s 'S-EMPTY-STREAM)

(define (s-empty? s)
  (eq? s empty-s))

(define-syntax-rule (s-cons a b)
  (cons a (s-delay b)))

(define (s-first s)
  (car s))

(define (s-rest s)
  (s-force (cdr s)))

(define (s-display s)
  (if (s-empty? s)
      ""
      (~a (s-first s) "," (s-display (s-rest s)))))

(define (s-display-limit s limit)
  (if (or (= limit 0) (s-empty? s))
      "...?"
      (~a (s-first s) "," (s-display-limit (s-rest s) (- limit 1)))))

;list-ref
(define (s-ref s n)
  (if (= n 0)
      (s-first s)
      (s-ref (s-rest s) (- n 1))))

;map
(define (s-map proc s)
  (if (s-empty? s)
      empty-s
      (s-cons (proc (s-first s)) (s-map proc (s-rest s)))))

;filter
(define (s-filter p s)
  (cond ((s-empty? s) empty-s)
        ((p (s-first s))
         (s-cons (s-first s)
               (s-filter p (s-rest s))))
        (else (s-filter p (s-rest s)))))

;range (enumerate)
(define (s-range low high)
  (if (>= low high)
      empty-s
      (s-cons
       low
       (s-range (+ low 1) high))))

#|1.
a) Implement s-length that returns the length of a stream.|#



(define (s-length s (l 0))
  (if (s-empty? s)
      l
      (s-length (s-rest s) (add1 l))))

(s-length (s-range 0 100)) ;100


#|b) Use s-map to define a procedure toCelsius-stream that takes a stream of temperatures in Fahrenheit
and returns a stream of corresponding temperatures in Celsius, using the formula C = 5/9 (F – 32). Process
the stream in two stages (i.e., produce an intermediate stream!): first subtract 32 from each temperature
then multiply the results by 5/9. |#


(define (toCelsius-stream s)
  (s-display (s-map (λ (x) (* (/ 5 9) x)) (s-map (λ (x) (- x 32)) s))))

(toCelsius-stream (s-cons -40 (s-cons 32 (s-cons 50 empty-s)))) ; "-40, 0, 10"

(trenne)

#|2. Define two procedures list2s and s2list to convert from a list to a stream and vice versa.|#

(define (list2s l)
  (if(null? l)
     empty-s
     (s-cons (car l) (list2s (cdr l)))))

(define (s2list s)
  (if (s-empty? s)
      null
      (cons (s-first s) (s2list (s-rest s)))))

(list2s (range 0 10))
(s2list (s-range 0 10))

(trenne)

#|3. Define an infinite stream of powers of two starting from a certain number.|#

(define (powersOf2 n)
  (s-cons n (powersOf2 (* 2 n))))

(s-display-limit (powersOf2 64) 5) ; stream: 64,128,256,512,1024,...

(trenne)

#|4. Define a procedure (s-add s1 s2) that creates a new stream by performing element-wise addition of s1
and s2.|#

(define (s-add s1 s2)
  (if (or (s-empty? s1) (s-empty? s2))
      empty-s
      (s-cons (+ (s-first s1) (s-first s2)) (s-add (s-rest s1) (s-rest s2)))))

(define integers (s-range 1 +inf.0))

(s-display-limit (s-add integers integers) 5); stream: 2, 4, 6, 8, ...

#|5. Define a procedure (mySum s) that takes a stream s and returns the stream whose elements are s0,
s0+s1, s0+s1+s2, ...|#

(define (mySum s (old 0))
  (if (s-empty? s)
      empty-s
      (let ((x (+ (s-first s) old)))
        (s-cons x (mySum (s-rest s) x)))))

(s-display-limit (mySum integers) 4) ; stream: 1, 3, 6, 10, ...