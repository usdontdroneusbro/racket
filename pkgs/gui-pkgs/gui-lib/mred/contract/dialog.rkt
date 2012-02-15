#lang racket/base

(require "common.rkt"
         "interface/top-level-window.rkt")

(provide dialog%/c)

(define dialog%/c
 (and/c
  top-level-window<%>/c
  (class/c
   (init
    [label label-string?]
    [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c)]
    [width (or/c dimension-non-zero/c false/c)]
    [height (or/c dimension-non-zero/c false/c)]
    [x (or/c dimension-non-zero/c false/c)]
    [y (or/c dimension-non-zero/c false/c)]
    [style (listof (one-of/c 'no-caption 'resize-border
                             'no-sheet 'close-button))]
    [enabled any/c]
    [border (integer-in 0 1000)]
    [spacing (integer-in 0 1000)]
    [alignment (list/c (one-of/c 'left 'center 'right)
                       (one-of/c 'top 'center 'bottom))]
    [min-width dimension-non-zero/c]
    [min-height dimension-non-zero/c]
    [stretchable-width any/c]
    [stretchable-height any/c])
   (on-subwindow-char
    (->m
     (is-a?/c window<%>)
     (is-a?/c key-event%)
     boolean?))
   (show
    (->m any/c any)))))
