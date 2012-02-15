#lang racket/base

(require "common.rkt")

(provide editor-canvas%/c)

(define editor-canvas%/c
 (class/c
  (init (parent (or/c (is-a?/c frame%) 
                      (is-a?/c dialog%) 
                      (is-a?/c panel%) 
                      (is-a?/c pane%)))
        (editor (or/c (or/c (is-a?/c text%) 
                            (is-a?/c pasteboard%)) 
                      false/c))
        (style
         (listof
          (one-of/c
           'no-border
           'control-border
           'combo
           'no-hscroll
           'no-vscroll
           'hide-hscroll
           'hide-vscroll
           'auto-vscroll
           'auto-hscroll
           'resize-corner
           'deleted
           'transparent)))
        (scrolls-per-page (integer-in 1 10000))
        (label (or/c label-string? false/c))
        (wheel-step (or/c (integer-in 1 10000) false/c))
        (line-count (or/c (integer-in 1 1000) false/c))
        (horizontal-inset (integer-in 0 1000))
        (vertical-inset (integer-in 0 1000))
        (enabled any/c)
        (vert-margin (integer-in 0 1000))
        (horiz-margin (integer-in 0 1000))
        (min-width dimension-non-zero/c)
        (min-height dimension-non-zero/c)
        (stretchable-width any/c)
        (stretchable-height any/c))
  (accept-tab-focus (case->m (-> boolean?) (-> any/c any)))
  (allow-scroll-to-last (case->m (-> boolean?) (-> any/c any)))
  (allow-tab-exit (case->m (-> boolean?) (-> any/c any)))
  (call-as-primary-owner (->m (-> any) any))
  (force-display-focus (case->m (-> boolean?) (-> any/c any)))
  (get-canvas-background (->m (or/c (is-a?/c color%) false/c)))
  (get-dc (->m (is-a?/c dc<%>)))
  (get-editor (->m (or/c (or/c (is-a?/c text%) (is-a?/c pasteboard%)) false/c)))
  (get-line-count (->m (or/c (integer-in 1 1000) false/c)))
  (horizontal-inset (case->m (-> (integer-in 1 10000)) (-> (integer-in 1 10000) any)))
  (lazy-refresh (case->m (-> boolean?) (-> any/c any)))
  (min-client-height (case->m (-> (integer-in 0 10000)) (-> (integer-in 0 10000) any)))
  (min-client-width (case->m (-> (integer-in 0 10000)) (-> (integer-in 0 10000) any)))
  (on-char (->m (is-a?/c key-event%) any))
  (on-event (->m (is-a?/c mouse-event%) any))
  (on-focus (->m any/c any))
  (on-paint (->m any))
  (on-size (->m (integer-in 0 10000) (integer-in 0 10000) any))
  (on-tab-in (->m any))
  (override
    (on-char (->m (is-a?/c key-event%) any))
    (on-event (->m (is-a?/c mouse-event%) any))
    (on-focus (->m any/c any))
    (on-paint (->m any))
    (on-size (->m (integer-in 0 10000) (integer-in 0 10000) any))
    (on-tab-in (->m any)))
  (scroll-to
   (->*m
    (real? real? 
     (and/c real? (not/c negative?)) 
     (and/c real? (not/c negative?)) any/c)
    ((one-of/c 'start 'end 'none))
    boolean?))
  (scroll-with-bottom-base (case->m (-> boolean?) (-> any/c any)))
  (set-canvas-background (->m (is-a?/c color%) any))
  (set-editor (->*m ((or/c (or/c (is-a?/c text%) (is-a?/c pasteboard%)) false/c)) (any/c) any))
  (set-line-count (->m (or/c (integer-in 1 1000) false/c) any))
  (set-resize-corner (->m any/c any))
  (vertical-inset (case->m (-> (integer-in 1 10000)) (-> (integer-in 1 10000) any)))
  (warp-pointer (->m (integer-in 0 10000) (integer-in 0 10000) any))
  (wheel-step
   (case->m (-> (or/c (integer-in 1 10000) false/c)) (-> (or/c (integer-in 1 10000) false/c) any)))))
