#;
(exn:pred (lambda (e) (regexp-match? "Expected 0 type variables" e)))
#lang typed/racket

;; Testing polydots arity check

(: f (All (b ...) (b ... b -> Integer)))
(define f
  (plambda: (a b ...) ([x : b]) 0))