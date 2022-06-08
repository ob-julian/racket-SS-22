;s1084098 Oberhofer Julian
#lang racket

(require racket/trace)

(define (trenne) (displayln "---------------"))


#|1. Study the interpreter1 (primitive) discussed in the lecture. You may want to use the
debugger to step through the code and/or tracing. Additionally, use well-placed debugoutput statements (e.g. using displayln to see what’s happening). Be sure to understand
the general principle and also what is going on in each line. Start with small example
expressions. You shall submit your code examples (meaningful tests!) and the interpreter
(with the inserted debug statements).|#


;From Lecture:

(define (mc-eval exp [env null])      ;input: an expression and an (maybe empty) environment; goal: evaluate the expression in the environment
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup-variable-value exp env))  ;to resolve variables and primitive functions
        ((pair? exp) (mc-apply (mc-eval (car exp) env) (list-of-values (cdr exp) env))) ;application: (operation operand1 operand2..)
        (else (error "Unknown expression type -- EVAL" exp))))


(define (mc-apply procedure arguments)     ;input: a procedure and its arguments; goal: apply the procedure to its arguments
  ;;;(displayln arguments)
  (cond ((tagged-list? procedure 'primitive) (apply-primitive-procedure procedure arguments))
        (else (error "Unknown procedure type -- APPLY" procedure))))




(define (lookup-variable-value var env)       ;input: a variable and an environment (i.e., a list of pairs); goal: get the value for this variable (since we have only primitive procedures, only one scope is required)
      (define val (assq var env))
      (if (eq? val false)
          (error "unbound variable" var)
          (cdr val)))

(define (list-of-values exps env)             ;input: a list of expressions and an environment; goal: turn the list of expressions into a list of values (by evaluating each expression in the environment) 
  (if (null? exps)
      '()
      (cons (mc-eval (car exps) env) (list-of-values (cdr exps) env))))

(define (tagged-list? exp tag)                ;input: an expression (a list that might start with tag) and a tag; goal: determine if the expression is a list that starts whose first element is tag
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (apply-primitive-procedure proc args)  ;input a procedure {e.g., '(primitive #<procedure:*>)} and arguments {e.g., '(1 2) }; goal: apply the procedure to the arguments
  (apply-in-underlying-racket (car (cdr proc)) args))

(define apply-in-underlying-racket apply) ;redirect to implementation language

; Tests:

(mc-eval 5) ;5
(mc-eval 5 5) ;5 /environment not from importance because number
(mc-eval '5) ;5 /still a number 

'----

(define e1 (list (cons 'a 1) (cons 'b 2)))
(define e2 (list (cons 'a 1) (cons 'a 2)))

;;;;;;;;(trace mc-eval)
;;;;;;;;(trace lookup-variable-value)

(mc-eval 'a e1) ;1 /after lokking up variable
(lookup-variable-value 'a e1);1
(mc-eval 5 e1) ;5
(mc-eval 'a e2) ;1 because 1 is earlier

'----

(define e3 (list (cons '+ (list 'primitive +)) (cons '- (list 'primitive -))))

;;;;;;;;(trace mc-apply)

(mc-eval '(+ 1 2) e3) ;3
(mc-eval '(+ (- 9 5) (+ 10 2)) e3);16


(trenne)

;2
#|a) Write an efficient procedure (findFirst pred lst noMatch) that returns the first
element in list lst for which the predicate pred produces a true value. It shall return
noMatch if no such element was found. You can make noMatch an optional argument; find a
reasonable default value.|#

(define (findFirst2 pred lst (noMatch '404_Not_Found))
  (let ((x (assf pred lst)))
    (if x x noMatch))) ; Ok just kidding

(define (findFirst pred lst (noMatch '404_Not_Found))
  (cond ((null? lst)
         noMatch)
        ((pred (first lst))
         (first lst))
        (else (findFirst pred (rest lst) noMatch))))

 (findFirst2 (lambda (arg) (> arg 2))(list (list 1 2) (list 3 4) (list 5 6)))
 (findFirst (lambda (arg) (> (car arg) 2))(list (list 1 2) (list 3 4) (list 5 6)))

#|b) Use your findFirst procedure from above to re-implement lookup-variable-value for
interpreter1 and interpreter2. (You must not use assq anymore). Submit the two procedures
as lookup-variable-value1 and lookup-variable-value2, respectively.|#

#|(define (lookup-variable-value-primitive var env)
      (define val (assq var env))
      (if (eq? val false)
          (error "unbound variable" var)
          (cdr val)))|#

(define (lookup-variable-value1 var env)
      (define val (findFirst (λ (x) (eq? (car x) var)) env #f))
      (if (eq? val false)
          (error "unbound variable" var)
          (cdr val)))

(lookup-variable-value1 'a e1) ;1

#|(define (lookup-variable-value-compound var env)
  (cond ((null? env) (error "unbound variable" var))
        (else
         (define binding (assq var (car env)))
         (if (eq? binding false)
             (lookup-variable-value var (cdr env))
             (cdr binding)))))|#

(define (lookup-variable-value2 var env)
  (cond ((null? env) (error "unbound variable" var))
        (else
         (define binding (findFirst (λ (x) (eq? (car x) var)) (car env) #f))
         (if (eq? binding false)
             (lookup-variable-value2 var (cdr env))
             (cdr binding)))))

(lookup-variable-value2 'c (list e1 e2 (list (cons 'c 100)))) ;100

(trenne)

#|3. Extend the interpreter1 in order to support the special forms and and or. Both operations
should only work for two arguments (e.g. (mc-eval '(and true true) myenv) ).
Of course, nesting should be allowed:(mc-eval '(and true (and true false)) myenv).
Find out (and document it), if your approach is using short-circuit evaluation or not?
Implement the features by extending the interpreter, not by adding and/or to the
environment (true and false should be provided via the environment). Optional: Try to
make two versions: a) with, b) without short-circuit evaluation.
|#

#|(define (mc-eval exp [env null])
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup-variable-value exp env))
        ((pair? exp) (mc-apply (mc-eval (car exp) env) (list-of-values (cdr exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))|#

;Ansatz 2

(define (mc-eval-logic exp [env null])
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup-variable-value exp env))
        ((pair? exp)(let ((x (mc-eval-logic (car exp) env)))
                      (cond
                        ((eq? x 'and) (if (eq? (mc-eval-logic (car (cdr exp)) env) #t) (if (eq? (mc-eval-logic (car (cdr (cdr exp))) env) #t) #t #f) #f))
                        ((eq? x 'or) (if (eq? (mc-eval-logic (car (cdr exp)) env) #t) #t (if (eq? (mc-eval-logic (car (cdr (cdr exp))) env) #t) #t #f)))
                        ((mc-apply x (list-of-values (cdr exp) env))))))
        (else (error "Unknown expression type -- EVAL" exp))))

(define myenv (list (cons 'false #f) (cons 'true #t) (cons 'and 'and) (cons 'or 'or)))

(mc-eval 'false myenv)
(mc-eval-logic '(and true true) myenv) ;#t
(mc-eval-logic '(and true (and true false)) myenv) ;#f

'----
;Ansatz 2

(define (mc-apply-logik procedure arguments)
  (cond ((tagged-list? procedure 'primitive) (apply-primitive-procedure procedure arguments))
        ((tagged-list? procedure 'logik) (apply-logik-procedure procedure arguments))
        (else (error "Unknown procedure type -- APPLY" procedure))))
(set! mc-apply mc-apply-logik)

(define (apply-logik-procedure proc arg)
  ;;;(displayln arg)
  (cond ((eq? (car (cdr proc)) 'and) (if (eq? (car arg) #t) (if (eq? (car (cdr arg)) #t) #t #f) #f))
        ((eq? (car (cdr proc)) 'or) (if (eq? (car arg) #t) #t (if (eq? (car (cdr arg)) #t) #t #f)))))

(define myenv2 (list (cons 'false #f) (cons 'true #t) (cons 'and (list 'logik 'and)) (cons 'or (list 'logik 'or))))

(mc-eval '(and true true) myenv2) ;#t
(mc-eval '(and true (and true false)) myenv2) ;#f

'----
;test for Short circut

;1: short-circuit yes
(mc-eval-logic '(and false x) myenv) ;#f
(mc-eval-logic '(or true x) myenv) ;#f

;2: short-circuit no
;(mc-eval '(and false x) myenv2) ;error
;(mc-eval '(or true x) myenv2) ;error

(trenne)

#|4. With interpreter2: Find the most simple expression ex such that (mc-eval ex null) will not
terminate (i.e., it executes an endless loop). Explain why. (Note: the lambda-calculus
slides/examples might be helpful)|#

;'((lambda (x) (x x)) (lambda (x) (x x)))

;Because (lambda (x) (x x)) gets 1 arg and that is (lambda (x) (x x)). so if you apply, the outcome is: ((lambda (x) (x x)) (lambda (x) (x x))) and this ist just the starting argument again


#|5. Consider the function add3: (Integer x Integer x Integer) -> Integer that returns the sum of
all its arguments. Write a curried function add3_curried without using the function curry.
Note: add3_curried should demonstrate currying and not partial application. Give a small
example.|#

(define (add3_curried a b c)
  ((curried-add a) ((curried-add b) c)))

(define curried-add (lambda (x)
    (lambda (y)
        (+ x y))))

(add3_curried 1 2 3)
