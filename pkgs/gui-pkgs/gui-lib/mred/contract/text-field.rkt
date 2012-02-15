#lang racket/base

(require "common.rkt"
         "interface/control.rkt")

(provide text-field%/c
         combo-field%/c)

(define text-field%/c
 (and/c
  control<%>/c
  (class/c
   (init
    [label (or/c label-string? false/c)]
    [parent parent/c]
    [callback (callback/c text-field%)]
    [style (listof (one-of/c 'single 'multiple 'hscroll 'password
                             'vertical-label 'horizontal-label
                             'deleted))]
    [font (is-a?/c font%)]
    [enabled any/c]
    [vert-margin (integer-in 0 1000)]
    [horiz-margin (integer-in 0 1000)]
    [min-width dimension-non-zero/c]
    [min-height dimension-non-zero/c]
    [stretchable-width any/c]
    [stretchable-height any/c])
   (get-editor
    (->m (is-a?/c text%)))
   (get-field-background
    (->m (is-a?/c color%)))
   (get-value
    (->m string?))
   (set-field-background
    (->m (is-a?/c color%) any))
   (set-value
    (->m string? any)))))

(define combo-field%/c
 (and/c
  control<%>/c
  (class/c
   (init
    [label (or/c label-string? false/c)]
    [choices (listof label-string?)]
    [parent parent/c]
    [callback (callback/c combo-field%)]
    [style (listof (one-of/c 'horizontal-label 'vertical-label
                             'deleted))]
    [font (is-a?/c font%)]
    [enabled any/c]
    [vert-margin (integer-in 0 1000)]
    [horiz-margin (integer-in 0 1000)]
    [min-width dimension-non-zero/c]
    [min-height dimension-non-zero/c]
    [stretchable-width any/c]
    [stretchable-height any/c])
   (append
     (->m label-string? (is-a?/c menu-item%)))
   (get-menu
    (->m (is-a?/c popup-menu%)))
   (on-popup
     (->m (is-a?/c control-event%) any))
   (override
     (on-popup
      (->m (is-a?/c control-event%) any))))))
