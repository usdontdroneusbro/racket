#lang racket/base

(require "common.rkt")

(provide timer%/c)

(define timer%/c
 (class/c
  (init [notify-callback (-> any)]
        [interval (or/c (integer-in 0 1000000000) #f)]
        [just-once? any/c])
  (interval (->m (integer-in 0 1000000000)))
  (notify (->m any))
  (override
    (notify (->m any)))
  (start (->*m ((integer-in 0 1000000000)) (any/c) any))
  (stop (->m any))))
