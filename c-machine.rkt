; C machine (term rewriting based on Redexes)
;
; This is a small-step machine with only a single component: the
; control string (i.e., the expression itself). Application must be
; performed by explicit substitution, which must (if necessary)
; perform capture-avoiding substitution. The upshot of this is that
; our interpreter is quite slow: it has to traverse the term on each
; step to (a) find the next redex and (b) perform capture-avoiding
; substitution.

#lang racket

; The "C Machine" (or "control machine") is a small-step
; term-rewriting abstract machine for the lambda calculus.
; 
; The machine is called the "C Machine" because its only component is
; the control string (current expression)
; 
; Σ = Exp
;
; Stepping takes expressions to expressions
; ↝ : Exp → Exp
; 
; The step function's rules are defined by the following (for the
; Call-by-Value lambda calculus):
; 
; ((lambda (x) body) v) ↝ body [x |-> e]  << Capture avoiding
; e₀ ↝ e' ⇒ (e₀ e₁) ↝ (e' e₁)
; e₁ ↝ e' ⇒ (v e₁) ↝ (v e₁)
; 
; Where the metavariable v quantifies over values. In our case (this
; being the pure lambda calculus), values are just lambdas:
;    v, (lambda (x) e) ∈ Val = Lam
; 
; Injection is just the identity (as Σ = Exp)
; inj(e) = e

; Values (irreducible expressions) are just lambdas
(define (value? e)
  (match e
    [`(λ (,x) ,body) #t]
    [else #f]))

; Free variables (need this for capture-avoiding substitution)
(define (free-vars e)
  (match e
    [`(λ (,x) ,body) (set-subtract (free-vars body) (set x))]
    [(? symbol? x) (set x)]
    [`(,e0 ,e1) (set-union (free-vars e0) (free-vars e1))]))

; Return a fresh symbol that lies outside of the set s.
(define (fresh-var s)
  (let ([x (gensym)])
    (if (not (set-member? s x)) x
        (fresh-var s))))

; Perform capture-avoiding substitution from x to e1 within e. Perfrom
; α-conversion as necessary to avoid captures. This uses the fresh-var
; function above to perform α-renaming
(define (capture-avoiding-subst e x e1)
  (match e
    [`(λ (,y) ,body)
     (if (equal? x y)
         ; Need to convert y to be something else
         ;
         ; Note that we use capture-avoiding-subst twice here: the
         ; innermost call is to perform the necessary α conversion.
         (let ([z (fresh-var (free-vars body))])
           `(λ (,z) ,(capture-avoiding-subst
                           (capture-avoiding-subst body y z)
                           x
                           e1)))
         `(λ (,y) ,(capture-avoiding-subst body x e1)))]
     [(? symbol? y) (if (equal? x y) e1 y)]
     [`(,e2 ,e3) `(,(capture-avoiding-subst e2 x e1)
                   ,(capture-avoiding-subst e3 x e1))]))

; The step function
(define (↝ state)
  (match state
    [`((λ (,x) ,body) ,(? value? v))
      (capture-avoiding-subst body x v)]
    [`(,(? value? v) ,e1)
     `(,v ,(↝ e1))]
    [`(,e0 ,e1)
      `(,(↝ e0) ,e1)]))

; Evaluate an entire expression (just the transitive closure of ↝)
(define (eval-c expr)
  (let loop ([e expr])
    (if (value? e) e (loop (↝ e)))))

; If you have the church encoder, you can build examples by replicating the following 
#;(with-output-to-file
  "/Users/kmicinski/out.scm"
  (lambda () (pretty-print (church-encode
                            '(letrec [(f (lambda (x) (if (zero? x) 0 (f (sub1 x)))))] (f 23))))))
; 
; I'm not releasing the church encoder as it's a project for my
; course. If you would like me to church encode a small term for you
; too test out the interpreter, feel free to ask.

; Read in the example
(define example-input (read (open-input-file "big-loop.scm")))

(time (eval-c example-input))
