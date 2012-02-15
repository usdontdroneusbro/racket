#lang racket/base

(require "common.rkt"
         "interface/control.rkt")

(provide list-box%/c)

(define list-box%/c
 (and/c
  control<%>/c
  ;; contracts from list-control<%> except append
  (class/c
   (clear (->m any))
   (find-string (->m string? (or/c exact-nonnegative-integer? false/c)))
   (get-number (->m exact-nonnegative-integer?))
   (get-selection (->m (or/c exact-nonnegative-integer? false/c)))
   (get-string (->m exact-nonnegative-integer? (and/c immutable? label-string?)))
   (get-string-selection (->m (or/c (and/c immutable? label-string?) false/c)))
   (set-selection (->m exact-nonnegative-integer? any))
   (set-string-selection (->m string? any)))
  (class/c
   (init
    [label (or/c label-string? false/c)]
    [choices (listof label-string?)]
    [parent parent/c]
    [callback
     ((is-a?/c list-box%) (is-a?/c control-event%) . -> . any)]
    [style (listof (one-of/c 'single 'multiple 'extended
                             'vertical-label 'horizontal-label
                             'deleted))]
    [selection (or/c exact-nonnegative-integer? false/c)]
    [font (is-a?/c font%)]
    [enabled any/c]
    [vert-margin (integer-in 0 1000)]
    [horiz-margin (integer-in 0 1000)]
    [min-width dimension-non-zero/c]
    [min-height dimension-non-zero/c]
    [stretchable-width any/c]
    [stretchable-height any/c])
   (append
    (->*m
     (string?)
     (any/c)
     any))
   (delete
    (->m exact-nonnegative-integer? any))
   (get-data
    (->m exact-nonnegative-integer? any/c))
   (get-first-visible-item
    (->m exact-nonnegative-integer?))
   (get-label-font
    (->m (is-a?/c font%)))
   (get-selections
    (->m (listof exact-nonnegative-integer?)))
   (is-selected?
    (->m exact-nonnegative-integer? boolean?))
   (number-of-visible-items
    (->m exact-positive-integer?))
   (select
    (->*m
     (exact-nonnegative-integer?)
     (any/c)
     any))
   (set
    (->m (listof label-string?) any))
   (set-data
    (->m exact-nonnegative-integer? any/c any))
   (set-first-visible-item
    (->m exact-nonnegative-integer? any))
   (set-string
    (->m exact-nonnegative-integer? label-string? any)))))
