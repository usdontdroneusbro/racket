#lang racket/base

(require "common.rkt"
         "interface/control.rkt")

(provide slider%/c)

(define slider%/c
 (and/c
  control<%>/c
  (class/c
   (init
    [label (or/c label-string? false/c)]
    [min-value (integer-in -10000 10000)]
    [max-value (integer-in -10000 10000)]
    [parent parent/c]
    [callback (callback/c slider%)]
    [style (listof (one-of/c 'horizontal 'vertical 'plain
                             'vertical-label 'horizontal-label
                             'deleted))]
    [font (is-a?/c font%)]
    [enabled any/c]
    [vert-margin (integer-in 0 1000)]
    [horiz-margin (integer-in 0 1000)]
    [min-width min-width/c]
    [min-height min-height/c]
    [stretchable-width any/c]
    [stretchable-height any/c])
   (get-value
    (->m (integer-in -10000 10000)))
   (set-value
    (->m (integer-in -10000 10000) any)))))
