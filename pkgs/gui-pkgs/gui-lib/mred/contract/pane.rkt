#lang racket/base

(require "common.rkt"
         "interface/area-container.rkt" 
         "interface/subarea.rkt")

(provide (all-defined-out))

(define pane%/c
 (and/c
  area-container<%>/c
  subarea<%>/c
  (class/c
   (init
    [parent parent/c]
    [vert-margin (integer-in 0 1000)]
    [horiz-margin (integer-in 0 1000)]
    [border (integer-in 0 1000)]
    [spacing (integer-in 0 1000)]
    [alignment (list/c (one-of/c 'left 'center 'right)
                       (one-of/c 'top 'center 'bottom))]
    [min-width min-width/c]
    [min-height min-height/c]
    [stretchable-width any/c]
    [stretchable-height any/c]))))

(define horizontal-pane%/c
 (and/c
  area-container<%>/c
  subarea<%>/c
  (class/c
   (init
    [parent parent/c]
    [vert-margin (integer-in 0 1000)]
    [horiz-margin (integer-in 0 1000)]
    [border (integer-in 0 1000)]
    [spacing (integer-in 0 1000)]
    [alignment (list/c (one-of/c 'left 'center 'right)
                       (one-of/c 'top 'center 'bottom))]
    [min-width min-width/c]
    [min-height min-height/c]
    [stretchable-width any/c]
    [stretchable-height any/c]))))

(define vertical-pane%/c
 (and/c
  area-container<%>/c
  subarea<%>/c
  (class/c
   (init
    [parent parent/c]
    [vert-margin (integer-in 0 1000)]
    [horiz-margin (integer-in 0 1000)]
    [border (integer-in 0 1000)]
    [spacing (integer-in 0 1000)]
    [alignment (list/c (one-of/c 'left 'center 'right)
                       (one-of/c 'top 'center 'bottom))]
    [min-width min-width/c]
    [min-height min-height/c]
    [stretchable-width any/c]
    [stretchable-height any/c]))))

(define grow-box-spacer-pane%/c
 (and/c
  area-container<%>/c
  subarea<%>/c
  (class/c
   (init
    [parent parent/c]
    [vert-margin (integer-in 0 1000)]
    [horiz-margin (integer-in 0 1000)]
    [border (integer-in 0 1000)]
    [spacing (integer-in 0 1000)]
    [alignment (list/c (one-of/c 'left 'center 'right)
                       (one-of/c 'top 'center 'bottom))]
    [min-width min-width/c]
    [min-height min-height/c]
    [stretchable-width any/c]
    [stretchable-height any/c]))))
