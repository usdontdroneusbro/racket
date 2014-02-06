#lang racket/base

(require "common.rkt"
         "interface/control.rkt")

(provide message%/c)

(define message%/c
 (and/c
  control<%>/c
  (class/c
   (init
    [label (or/c label-string? (is-a?/c bitmap%)
           (one-of/c 'app 'caution 'stop))]
    [parent parent/c]
    [style (listof (one-of/c 'deleted))]
    [font (is-a?/c font%)]
    [enabled any/c]
    [vert-margin (integer-in 0 1000)]
    [horiz-margin (integer-in 0 1000)]
    [min-width min-width/c]
    [min-height min-height/c]
    [stretchable-width any/c]
    [stretchable-height any/c]
    [auto-resize any/c])
   (auto-resize
    (case->m
     (-> boolean?)
     (-> any/c any)))
   (set-label
    (->m (or/c label-string? (is-a?/c bitmap%)) any)))))
