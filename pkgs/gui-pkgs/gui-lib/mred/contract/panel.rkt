#lang racket/base

(require "common.rkt"
         "interface/area-container-window.rkt"
         "interface/subwindow.rkt")

(provide panel%/c
         horizontal-panel%/c
         vertical-panel%/c
         tab-panel%/c)

(define panel%/c
 (and/c
  area-container-window<%>/c
  subwindow<%>/c
  (class/c
   (init
    [parent parent/c]
    [style (listof (one-of/c 'border 'deleted))]
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
    [stretchable-height any/c]))))

(define horizontal-panel%/c
 (and/c
  area-container-window<%>/c
  subwindow<%>/c
  (class/c
   (init
    [parent parent/c]
    [style (listof (one-of/c 'border 'deleted))]
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
    [stretchable-height any/c])
   (set-orientation
    (->m boolean? any))
   (get-orientation
    (->m boolean?)))))

(define vertical-panel%/c
 (and/c
  area-container-window<%>/c
  subwindow<%>/c
  (class/c
   (init
    [parent parent/c]
    [style (listof (one-of/c 'border 'deleted))]
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
    [stretchable-height any/c])
   (set-orientation
    (->m boolean? any))
   (get-orientation
    (->m boolean?)))))

(define tab-panel%/c
 (class/c
  (init
   [choices (listof label-string?)]
   [parent parent/c]
   [callback
    ((is-a?/c tab-panel%) (is-a?/c control-event%) . -> . any)]
   [style (listof (one-of/c 'no-border 'deleted))]
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
   [stretchable-height any/c])
  (append
   (->m label-string? any))
  (delete
   (->m exact-nonnegative-integer? any))
  (get-item-label
   (->m exact-nonnegative-integer? string?))
  (get-number
   (->m exact-nonnegative-integer?))
  (get-selection
   (->m (or/c exact-nonnegative-integer? false/c)))
  (set
   (->m (listof label-string?) any))
  (set-item-label
   (->m exact-nonnegative-integer? label-string? any))
  (set-selection
   (->m exact-nonnegative-integer? any))))
