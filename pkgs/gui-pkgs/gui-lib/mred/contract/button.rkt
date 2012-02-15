#lang racket/base

(require "common.rkt"
         "interface/control.rkt")

(provide button%/c)

(define button%/c
 (and/c
  control<%>/c
  (class/c
   (init
    [label (or/c label-string?
                 (is-a?/c bitmap%)
                 (list/c (is-a?/c bitmap%)
                         label-string?
                         (one-of/c 'left 'top 'right 'bottom)))]
    [parent parent/c]
    [callback (callback/c button%)]
    [style (listof (one-of/c 'border 'deleted))]
    [font (is-a?/c font%)]
    [enabled any/c]
    [vert-margin (integer-in 0 1000)]
    [horiz-margin (integer-in 0 1000)]
    [min-width dimension-non-zero/c]
    [min-height dimension-non-zero/c]
    [stretchable-width any/c]
    [stretchable-height any/c])
   [set-label (->m (or/c label-string? (is-a?/c bitmap%)) any)])))
