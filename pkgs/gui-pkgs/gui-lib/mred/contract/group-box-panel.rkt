#lang racket/base

(require "common.rkt")

(provide group-box-panel%/c)

(define group-box-panel%/c
 (class/c
  (init
   [label label-string?]
   [parent parent/c]
   [style (listof (one-of/c 'deleted))]
   [font (is-a?/c font%)]
   [enabled any/c]
   [vert-margin (integer-in 0 1000)]
   [horiz-margin (integer-in 0 1000)]
   [border (integer-in 0 1000)]
   [spacing (integer-in 0 1000)]
   [alignment (list/c (one-of/c 'left 'center 'right)
                      (one-of/c 'top 'center 'bottom))]
   [min-width dimension-non-zero/c]
   [min-height dimension-non-zero/c]
   [stretchable-width any/c]
   [stretchable-height any/c])))
