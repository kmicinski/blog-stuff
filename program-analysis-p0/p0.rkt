;; CIS 700 -- Metacircular interpreter and Homework 1
;; 
;; Kris Micinski, Fall '19, Syracuse University
#lang racket

;; Definition of core scheme
(define (expr? e)
  (match e
    [(? symbol? var) #t]
    [(? lit?) #t]
    [`(lambda (,xs ...) ,body) #t]
    [`(if ,(? expr? guard)
          ,(? expr? etrue)
          ,(? expr? efalse)) #t]
    [`(let* ([,(? symbol? xs) ,(? expr? x-bodies)] ...)
        ,(? expr? body)) #t]
    [`(letrec ([,(? symbol?) (lambda (,xs ...) ,(? expr?))] ...)
        ,(? expr? body)) #t]
    [`(,(? builtin?) ,(? expr? args) ...) #t]
    [`(,(? expr? e0) ,(? expr? e1) ...) #t]))

(define (lit? e)
  (match e
    [(? number?) #t]
    [(? string?) #t]
    [(? boolean?) #t]
    [else #f]))

(define (builtin? x)
  (set-member? (set '+ '- '* '/ 'cons 'car 'cdr 'list 'list? '= 'empty? 'number? 'string? 'string-append 'string-length 'length) x))

; Mapping from symbols to the primitive built-in functions within
; Racket.
(define builtins 
  (make-hash (list (cons '+ +)
                   (cons '- -)
                   (cons '* *)
                   (cons '/ /)
                   (cons 'cons cons)
                   (cons 'car car)
                   (cons 'cdr cdr)
                   (cons 'list list)
                   (cons '= equal?)
                   (cons 'empty? empty)
                   (cons 'number? number?)
                   (cons 'string? string?)
                   (cons 'string-append string-append)
                   (cons 'string-length string-length)
                   (cons 'length length)
                   (cons 'list? list?))))

;; Interpreter

;; Attempt 1
(define (interp e env)
  (match e
    [(? lit? l) l]
    [(? symbol? var) 'undefined]
    [`(lambda (,var) ,body)
     'undefined]
    ;; Builtins!
    ;; [`(,(? builtin? op) ,(? expr? builtin-args) ...) 
    ;;  ;; Evaluate each argument and then apply the builtin denotation
    ;;  ;; of the function.
    ;;  (let ([evaluated-args (map (lambda (arg) (interp arg env)) builtin-args)])
    ;;    ;; Take care to undersand this one!
    ;;    (apply (hash-ref builtins op) evaluated-args))]
    [`(,(? expr? e0) ,(? expr? e1)) 
     'undefined]
    [`(if ,(? expr? guard)
          ,(? expr? etrue)
          ,(? expr? efalse)) 
     'undefined]
    [`(let* ([,(? symbol?) ,(? expr?)] ...)
        ,(? expr? body))
     'undefined]
    [`(letrec ([,(? symbol? f) (lambda (,f-arg ...) ,f-body)]))
     'undefined]))

;; Examples for Attempt 1
(interp '((lambda (x) x) "Hello, World!") (hash))
(interp '(((lambda (x) (x x)) (lambda (x) x)) 1) (hash))
(interp '((lambda (x) (if (= x 0) 1
                          (+ ((lambda (y) (if (= y 3) 1 2)) x) x)))
          (* 1 (if #f 1 4)))
        (hash))

;; Letrec examples
(letrec ([f
          (lambda (x) (if (= x 0)
                          1
                          (* x (f (- x 1)))))])
  (f 10))

;; Implementing letrec using set!
(define f (lambda (x) x))
(set! f (lambda (x) (if (= x 0)
                        1
                        (* x (f (- x 1))))))
;(f 10)

;; Attempt 2 at interpreter: also adds letrec and many other things.
(define (interp-letrec e env)
  (match e
    [(? symbol? var) (hash-ref env var)]
    [(? lit?) e]
    [`(lambda (,var) ,body)
     (let ([env-copy (make-hash
                      (hash-map env
                                (lambda (x y) (cons x y))))])
       (lambda (x) (begin (hash-set! env-copy var x)
                          (interp-letrec body env-copy))))]
    [`(let* ([,(? symbol?) ,(? expr?)] ...)
        ,(? expr? body))
     'undefined]
    [`(letrec ([,(? symbol? f) (lambda (,f-arg) ,f-body)])
        ,(? expr? body))
     ;; Make a mutable copy of the hash table
     (let ([env-copy (make-hash
                      (hash-map env
                                (lambda (x y) (cons x y))))])
       ;; Mutably set f to be a lambda that points at itself
       (hash-set! env-copy
                  f
                  (lambda (x) (interp-letrec
                               f-body
                               (begin
                                 (hash-set! env-copy f-arg x)
                                 env-copy))))
       ;; Now interpret the body with this updated (mutable) env
       (interp-letrec body env-copy))]
    ;; Builtins!
    [`(,(? builtin? op) ,(? expr? builtin-args) ...) 
     ;; Evaluate each argument and then apply the builtin denotation
     ;; of the function.
     (let ([evaluated-args (map (lambda (arg) (interp-letrec arg env)) builtin-args)])
       ;; Take care to undersand this one!
       (apply (hash-ref builtins op) evaluated-args))]
    [`(,(? expr? e0) ,(? expr? e1)) 
     ((interp-letrec e0 env) (interp-letrec e1 env))]
    [`(if ,(? expr? guard)
          ,(? expr? etrue)
          ,(? expr? efalse)) 
     (if (interp-letrec guard env)
         (interp-letrec etrue env)
         (interp-letrec efalse env))]))

(define letrec-example-1
  '(letrec ([f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))])
     (f 10)))
(interp-letrec letrec-example-1 (hash))

;;
;;
;; BEGIN HOMEWORK 1
;;
;; 

;; Homework 1 has the following tasks. For each of the tests, you
;; should write tests alongside the ones I have provided. Your
;; homework will also be tested with tests I don't give here.
;; 
;; - Multi-argument lambdas. These must be implemented correctly. You
;; should build examples yourself demonstrating that they work. 
;;
;; - Let*.
;;
;; - Multi-argument letrec. Ensure that the function bound by the
;; letrec can accept and properly be applied to multiple arguments.
;; 
;; - Example that demonstrates multi-argument letrec.
;; 
;; - Match statement compilation.
;; 
;; To complete the homework, make sure you fill in each place marked
;; with TODO (including the tests for letrec down below).

;; Core-Scheme grammar extended to allow match statements
(define (mexpr? e)
  (match e
    [(? symbol? var) #t]
    [(? lit?) #t]
    [`(lambda (,xs ...) ,body) #t]
    [`(if ,(? expr? guard)
          ,(? expr? etrue)
          ,(? expr? efalse)) #t]
    [`(let* ([,(? symbol? xs) ,(? expr? x-bodies)] ...)
        ,(? expr? body)) #t]
    [`(letrec ([,(? symbol?) (lambda (,xs ...) ,(? expr?))] ...)
        ,(? expr? body)) #t]
    [`(,(? builtin?) ,(? expr? args) ...) #t]
    [`(,(? expr? e0) ,(? expr? e1) ...) #t]
    [`(match ,(? expr? match-expr)
        [,(? match-pattern? patterns) ,(? expr? bodies)] ...) #t]))

;; Match patterns in *our* language. Note: these are *not* the same as
;; Racket's match patterns. We are implementing a subset of match
;; patterns for our language. If you are confused about this, please
;; make sure you understand this predicate and write out some
;; examples.
(define (match-pattern? mp)
  (match mp
    ;; Something that looks like (? predicate? x) (Note: predicates
    ;; don't have to end w/ ?, but do by convention).
    [`(? ,predicate ,binding-var) #t]
    ;; Matches a cons pattern where the cons matches pattern
    ;; car-pattern, and cdr matches cdr-pattern. For example: (cons (?
    ;; (lambda (x) #t) x) (? (lambda (x) #t) y)) matches any cons
    ;; pattern. Note that--in our language--the base patern *must* be
    ;; something like (? predicate? x).
    [`(cons ,(? match-pattern? car-pattern) ,(? match-pattern? cdr-pattern)) #t]
    ;; Matches a finite list of elements.
    [`(list ,(? match-pattern? subpatterns) ...) #t]))

(define sum-example
  '((lambda (id) 
      (letrec
          ([sum
            (lambda (lst)
              (match lst
                [(? empty? l) 0]
                [(cons (? id fst) (? id rst)) (+ id (sum rst))]))]))
      (sum (list 1 2 3 5)))
    (lambda (x) x)))

;; Interpreter for HW1
(define (interp-hw1 e env)
  ; Might want to turn this on for debugging
  ;(displayln (format "~a ; ~a" e env))
  (match e
    ;; Variable lookup
    [(? symbol? var) (hash-ref env var)]
    ;; Literals
    [(? lit?) e]
    ;; TODO: implement multi-argument lambda. Right now this only
    ;; handles single-argument lambda (throws away rest of arguments).
    [`(lambda (,vars ...) ,body)
     ;; Copy the environment mutably
     (lambda (x) 
       (let ([env-copy (make-hash
                        (hash-map env
                                  (lambda (x y) (cons x y))))])
         (hash-set! env-copy (first vars) x)         
         (interp-hw1 body env-copy)))]
    ;; TODO: implement let* correctly
    [`(let* ([,(? symbol?) ,(? expr?)] ...)
        ,(? expr? body))
     'undefined]
    ;; TODO: handle multi-argument letrec (still only one f)
    [`(letrec ([,(? symbol? f) (lambda (,f-args ...) ,f-body)])
        ,(? expr? body))
     ;; Make a mutable copy of the hash table
     (let ([env-copy (make-hash
                      (hash-map env
                                (lambda (x y) (cons x y))))])
       ;; NOTE: this implementation is *WRONG*. It throws away the
       ;; rest of the arguments past the first...
       (hash-set! env-copy
                  f
                  (lambda (x) (interp-hw1
                               f-body
                               (begin
                                 (hash-set! env-copy (first f-args) x)
                                 env-copy))))
       ;; Now interpret the body with this updated (mutable) env
       (interp-hw1 body env-copy))]
    ;; Builtins! (Once you get multi-argument application to work, you
    ;; should be able to delete this branch and the code should work
    ;; the same.)
    [`(,(? builtin? op) ,(? expr? builtin-args) ...)
     ;; Evaluate each argument and then apply the builtin denotation
     ;; of the function.
     (let ([evaluated-args (map (lambda (arg) (interp-hw1 arg env)) builtin-args)])
       ;; Take care to undersand this: it is the hint for how to
       ;; understand multi-arg evaluation.
       (apply (hash-ref builtins op) evaluated-args))]
    ;; TODO. Compile a match statement. To implement this you should
    ;; need only to change the implementation of `compile-match-stmts`
    ;; below.
    [`(match ,(? expr? e) [,match-patterns ,match-bodies] ...)
     (eval-match-statements match-patterns match-bodies (interp-hw1 e env) env)]
    ;; TODO. Application: right now this only handles single-argument
    ;; application (throws away the rest of e-args).
    [`(,(? expr? e0) ,(? expr? e-args) ...)
     ((interp-hw1 e0 env) (interp-hw1 (first e-args) env))]
    ;; If
    [`(if ,(? expr? guard)
          ,(? expr? etrue)
          ,(? expr? efalse)) 
     (if (interp-hw1 guard env)
         (interp-hw1 etrue env)
         (interp-hw1 efalse env))]))

;; TODO: extend this function.  Basic idea: translate each match
;; pattern as a lambda that returns *either* an updated environment
;; that correctly binds the bodies (if it is successful) or returns
;; #f. Translate each match pattern into this function and then
;; perform application. If this results in #f, move on to try the next
;; match pattern (unless there are none left, in which case throw an
;; error).
(define (eval-match-statements patterns bodies matching-value env)
  ;; Compiles an individual match statement to a lambda which will
  ;; return either an updated environment (capturing variables as
  ;; appropriate).
  (define (compile-match-statement pattern env)
    (match pattern
      [`(? ,predicate? ,var)
       (lambda (x) (if (begin
                         (hash-set! env var x)
                         ((interp-hw1 predicate? env) x))
                       env
                       #f))]
      [`(cons ,car-pattern ,cdr-pattern)
       (lambda (x)
         (if (cons? x)
             (let ([env-after-car ((compile-match-statement car-pattern env) (car x))])
               (if env-after-car
                   ((compile-match-statement cdr-pattern env-after-car) (cdr x))
                   #f))
             #f))]
      ;; Special case to handle matching on the empty list. (A good
      ;; solution in the next part should cover this case!)
      [`(list)
       (lambda (x) (and (list? x) (equal? (length x) 0)))]
      ;; TODO. Fill in your answer here. This can nicely be done as
      ;; follows.
      ;;
      ;; HINTS:
      ;; - Create a lambda (as in the case of cons) that accepts a list x
      ;;
      ;; - Check immediately whether list? is true of x, and whether
      ;; the length of x is equal to the length of patterns (else this
      ;; can't match).
      ;; 
      ;; - For each index i between 0 <= i < (length patterns), check
      ;; that (compile-match-statement) applies to the pattern at
      ;; index i and ith element of the list x. If it does, return
      ;; that updated environment. You can do this easily using foldl.
      [`(list ,patterns ...)
       (lambda (x) #t)]))

  ;; Recursively try each pattern in order until we find one that
  ;; applies. If there are no more patterns, throw an error.
  (define (try-each-pattern patterns bodies matching-value env)
    (if (empty? patterns)
        ;; Error, no patterns left
        (error "No more possible patterns to match")
        ;; Otherwise, compile the first pattern into a lambda whose
        ;; job it is it to return either an updated environment (which
        ;; will be used to evaluate the body) or #f (indicating that
        ;; the match failed and we should move on to the next).
        (let* ([compiled-match-statement
                (compile-match-statement (first patterns) env)]
               [updated-env (compiled-match-statement matching-value)])
          (if updated-env 
              (interp-hw1 (car bodies) updated-env)
              (try-each-pattern (cdr patterns) (cdr bodies) matching-value env)))))

  ;; Finally, try each in turn
  (try-each-pattern patterns bodies matching-value env))

;; All of the tests (as Racket data). These will be run by the driver below.
(define tests
  `((multi-arg-lambda-0 ((lambda (x y z) (+ x y)) 1 2 3) 3)
    (multi-arg-lambda-1 ((lambda (x y z) (+ x y ((lambda (x z) (+ x z)) y (+ 1 z)))) 1 2 3) 9)
    (let*-0 (let* ([x 0] [y 1]) (+ x y)) 1)
    (let*-1 (let* ([x 0] [y (+ x 1)]) (+ x y)) 1)
    (let*-2 (let* ([x 0] [y (+ x 1)] [z (+ x y)]) (+ x y z)) 2)
    (let*-3 (let* ([x (lambda (y) y)] [x (lambda (z) 5)]) (x 4)) 5)

    ;; TODO: Write two different examples here that use letrec with more
    ;; than one argument. These should be small (but interesting)
    ;; recursive functions. You should write them in plain Racket first.
    (letrec-multiarg-0 #t #t)
    (letrec-multiarg-1 #t #t)
    
    ;; Match statement examples    
    (match-0 (match 5
               [(? number? x) (+ x 1)]
               [(? string? x) (string-append "hello-" x)]
               [(cons (? (lambda (x) #t) x) (? (lambda (x) #t) y)) (+ x y)])
             6)
    (match-1 (match (cons 1 2)
               [(? number? x) (+ x 1)]
               [(? string? x) (string-append "hello-" x)]
               [(cons (? (lambda (x) #t) x) (? (lambda (x) #t) y)) (+ x y)])
             3)
    (match-2 (match "hello"
               [(? number? x) (+ x 1)]
               [(? string? x) (string-append "hello-" x)]
               [(cons (? (lambda (x) #t) x) (? (lambda (x) #t) y)) (+ x y)])
             "hello-hello")
    (match-3 (match (list 1 2 "e")
               [(list (? number? x) (? number? y) (? string? e)) (+ x y)])
             3)

    ;; The following examples are tricky!
    (letrec-plus-match-1
     (letrec ([f (lambda (e)
                   (match e
                     [(? number? x) (+ x 1)]
                     ;; Match a list of length two where first element
                     ;; matches number?, second element matches number?.
                     [(list (? number? x) (? number? y)) (+ x y)]
                     [(list) 0]
                     [(cons (? string? x) (? list? l)) (+ (string-length x) (f l))]))])
       (f (list "hello" "hello" "hello")))
     15)

    (letrec-plus-match-2
     (letrec ([f (lambda (e)
                   (match e
                     [(? number? x) (+ x 1)]
                     ;; Match a list of length two where first element
                     ;; matches number?, second element matches number?.
                     [(list (? number? x) (? number? y)) (+ x y)]
                     [(list) 0]
                     [(cons (? string? x) (? list? l)) (+ (string-length x) (f l))]))])
       (f (list 2 3)))
     5)

    (letrec-plus-match-3
     (letrec ([f (lambda (e)
                   (match e
                     [(? number? x) (+ x 1)]
                     ;; Match a list of length two where first element
                     ;; matches number?, second element matches number?.
                     [(list (? number? x) (? number? y)) (+ x y)]
                     [(list) 0]
                     [(cons (? string? x) (? list? l)) (+ (string-length x) (f l))]))])
       (f (list)))
     0)
    
    (letrec-plus-match-4
     (letrec ([f (lambda (e)
                   (match e
                     [(? number? x) (+ x 1)]
                     ;; Match a list of length two where first element
                     ;; matches number?, second element matches number?.
                     [(list (? number? x) (? number? y)) (+ x y)]
                     [(list) 0]
                     [(cons (? string? x) (? list? l)) (+ (string-length x) (f l))]))])
       (f (cons "hello" (list 1 2))))
     8)

    ;#()
    ))

;; Running the tests
(for ([test tests])
  (match-define `(,test-name ,test-source ,expected-result) test)
  (displayln (format "Performing test ~a" test-name))
  (define actual-result
    (with-handlers ([exn:fail? (lambda (v) 'error)])
      (interp-hw1 test-source builtins)))
  (if (not (equal? actual-result expected-result))
      (begin
        (displayln (format "Error in test ~a. Expected ~a but got ~a. Here is the program source:"
                           test-name expected-result actual-result))
        (pretty-print test-source))
      (void)))

;;
;; GRADING
;;

;; Grading rubric for HW1:
;; [ ] - Multi-argument lambdas.
;; [ ] - Let*
;; [ ] - Multi-argument letrec.
;; [ ] - Multi-argument letrec performs application correctly.
;; [ ] - Example that demonstrates multi-argument letrec.
;; [ ] - Match statement compilation.
