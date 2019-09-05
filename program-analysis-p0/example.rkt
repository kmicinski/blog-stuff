;; Notes from class Wednesday, Sep 4
#lang racket

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
    [`(,(? expr? e0) ,(? expr? e1) ...) #t]))

(define (lit? e)
  (match e
    [(? number?) #t]
    [(? string?) #t]
    [(? boolean?) #t]
    [else #f]))

(expr? '(lambda (x) x))

;; (expr? '((lambda (x) x) 2))
;; (expr? '(if ((lambda (x) x) #t)
;;             1
;;             2))
;; (expr? '(if ((lambda (x) x) #f)
;;             1
;;             ((lambda (x) (x 2)) (lambda (y) #f))))

(define (interp e env)
  (match e
    [(? lit? l) l]
    [(? symbol? var) (hash-ref
                      env
                      var
                      (lambda () (error "No such variable")))]
    [`(lambda (,formal-var) ,body)
     (lambda (runtime-var) (interp body (hash-set env formal-var runtime-var)))]
    [`(,(? expr? e0) ,(? expr? e1)) 
     ((interp e0 env) (interp e1 env))]
    ;; Builtins!
    ;; [`(,(? builtin? op) ,(? expr? builtin-args) ...) 
    ;;  ;; Evaluate each argument and then apply the builtin denotation
    ;;  ;; of the function.
    ;;  (let ([evaluated-args (map (lambda (arg) (interp arg env)) builtin-args)])
    ;;    ;; Take care to undersand this one!
    ;;    (apply (hash-ref builtins op) evaluated-args))]
    [`(if ,(? expr? guard)
          ,(? expr? etrue)
          ,(? expr? efalse)) 
     'undefined]
    [`(let* ([,(? symbol?) ,(? expr?)] ...)
        ,(? expr? body))
     'undefined]
    [`(letrec ([,(? symbol? f) (lambda (,f-arg ...) ,f-body)]))
     'undefined]))

(interp '((lambda (x) x) "Hello, World!") (hash))

(interp '(((lambda (x) (x x)) (lambda (x) x)) 1) (hash))
