#lang racket/base

(require "common.rkt"
         "interface/control.rkt")

(provide gauge%/c)

(define gauge%/c
 (and/c
  control<%>/c
  (class/c
   (init
    [label (or/c label-string? false/c)]
    [range (integer-in 1 10000)]
    [parent parent/c]
    [style (listof (one-of/c 'horizontal 'vertical
                             'vertical-label 'horizontal-label))]
    [font (is-a?/c font%)]
    [enabled any/c]
    [vert-margin (integer-in 0 1000)]
    [horiz-margin (integer-in 0 1000)]
    [min-width dimension-non-zero/c]
    [min-height dimension-non-zero/c]
    [stretchable-width any/c]
    [stretchable-height any/c])
   (get-range
    (->m (integer-in 1 10000)))
   (get-value
    (->m dimension-non-zero/c))
   (set-range
    (->m (integer-in 1 10000) any))
   (set-value
    (->m dimension-non-zero/c any)))))

