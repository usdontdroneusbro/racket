#lang racket/base

(require "common.rkt"
         "interface/list-control.rkt")

(provide choice%/c)

(define choice%/c
 (and/c
  list-control<%>/c
  (class/c
   (init
    [label (or/c label-string? false/c)]
    [choice (listof label-string?)]
    [parent parent/c]
    [callback (callback/c choice%)]
    [style (listof (one-of/c 'horizontal-label 'vertical-label
                             'deleted))]
    [selection exact-nonnegative-integer?]
    [font (is-a?/c font%)]
    [enabled any/c]
    [vert-margin (integer-in 0 1000)]
    [horiz-margin (integer-in 0 1000)]
    [min-width dimension-non-zero/c]
    [min-height dimension-non-zero/c]
    [stretchable-width any/c]
    [stretchable-height any/c]))))

