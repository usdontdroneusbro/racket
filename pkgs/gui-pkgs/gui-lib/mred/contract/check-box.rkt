#lang racket/base

(require "common.rkt"
         "interface/control.rkt")

(provide check-box%/c)

(define check-box%/c
 (and/c
  control<%>/c
  (class/c
   (init
    [label label/c]
    [parent parent/c]
    [callback (callback/c check-box%)]
    [style (listof (one-of/c 'deleted))]
    [value any/c]
    [font (is-a?/c font%)]
    [enabled any/c]
    [vert-margin (integer-in 0 1000)]
    [horiz-margin (integer-in 0 1000)]
    [min-width dimension-non-zero/c]
    [min-height dimension-non-zero/c]
    [stretchable-width any/c]
    [stretchable-height any/c])
   (get-value
    (->m boolean?))
   (set-label
    (->m label/c any))
   (set-value
    (->m any/c any)))))
