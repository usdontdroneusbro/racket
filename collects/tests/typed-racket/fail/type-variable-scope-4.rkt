#;
(exn:pred (lambda (e) (regexp-match? "Mutation only allowed" e)))
#lang typed/racket

(: f (All (a b ...) (a b ... b -> a)))
(define f
  (lambda: ([x : a] y : b ... b)
    (: g (All (a) (a -> a)))
    (define (g z) (set! z x) z)
    (g x)))
