#lang racket/base

(require "common.rkt"
         "interface/canvas.rkt")

(provide canvas%/c)

(define canvas%/c
 (and/c
  canvas<%>/c
  (class/c
   (init
    [parent parent/c]
    [style
     (listof (one-of/c 'border 'control-border 'combo
                       'vscroll 'hscroll 'resize-corner
                       'gl 'no-autoclear 'transparent
                       'no-focus 'deleted))]
    [paint-callback
     ((is-a?/c canvas%) (is-a?/c dc<%>) . -> . any)]
    [label (or/c label-string? false/c)]
    [gl-config (or/c (is-a?/c gl-config%) false/c)]
    [enabled any/c]
    [vert-margin (integer-in 0 1000)]
    [horiz-margin (integer-in 0 1000)]
    [min-width dimension-non-zero/c]
    [min-height dimension-non-zero/c]
    [stretchable-width any/c]
    [stretchable-height any/c])
   (get-scroll-page
    (->m (one-of/c 'horizontal 'vertical) (integer-in 1 1000000000)))
   (get-scroll-pos
    (->m (one-of/c 'horizontal 'vertical) (integer-in 0 1000000000)))
   (get-scroll-range
    (->m (one-of/c 'horizontal 'vertical) (integer-in 0 1000000000)))
   (get-view-start
    (->m (values dimension-non-zero/c dimension-non-zero/c)))
   (get-virtual-size
    (->m (values dimension-non-zero/c dimension-non-zero/c)))
   (init-auto-scrollbars
    (->m
     (or/c (integer-in 1 1000000000) false/c)
     (or/c (integer-in 1 1000000000) false/c)
     (real-in 0.0 1.0)
     (real-in 0.0 1.0)
     any))
   (init-manual-scrollbars
    (->m (or/c (integer-in 0 1000000000) false/c)
         (or/c (integer-in 0 1000000000) false/c)
         (integer-in 1 1000000000)
         (integer-in 1 1000000000)
         (integer-in 0 1000000000)
         (integer-in 0 1000000000)
         any))
   (on-paint
    (->m any))
   (on-scroll
    (->m (is-a?/c scroll-event%) any))
   (scroll
    (->m (or/c (real-in 0.0 1.0) false/c)
         (or/c (real-in 0.0 1.0) false/c)
         any))
   (set-scroll-page
    (->m
     (one-of/c 'horizontal 'vertical)
     (integer-in 1 1000000000)
     any))
   (set-scroll-pos
    (->m
     (one-of/c 'horizontal 'vertical)
     (integer-in 0 1000000000)
     any))
   (set-scroll-range
    (->m
     (one-of/c 'horizontal 'vertical)
     (integer-in 0 1000000000)
     any))
   (show-scrollbars
    (->m any/c any/c any))
   (swap-gl-buffers
    (->m any))
   (with-gl-context
    (->m (-> any) any)))))
