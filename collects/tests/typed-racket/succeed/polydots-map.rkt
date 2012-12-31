#lang typed/racket

;; Test polydots for special map rule

(: f (All (b ...) (b ... b -> (List b ... b))))
(define (f . bs)
  ;; note the single 'b' without an index
  ;; it should be bound by the polydots above
  (map (λ: ([x : b]) x) bs))

(: g (All (b ...) (b ... b -> (List b ... b))))
(define (g . bs)
  (: f (b ... b -> (List b ... b)))
  (define (f . bs) (map (λ: ([x : b]) x) bs))

  ;; make sure List constructor gets the right index
  (apply f bs))

