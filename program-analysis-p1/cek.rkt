;; Project 1 -- CIS 700
#lang racket

;; Expressions
(define (expr? e)
  (match e
    [(? symbol? x) #t]
    [(? number? n) #t]
    [(? boolean? n) #t]
    [`(,(? builtin? e0) ,aes ...) #t]
    [`(,(? expr? e0) ,(? expr? e1)) #t]
    [`(lambda (,x) ,(? expr? e-body)) #t]
    [`(if (? expr? e0) ,(? expr? e-true) ,(? expr? e-false)) #t]))



