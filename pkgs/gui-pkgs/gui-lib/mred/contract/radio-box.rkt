#lang racket/base

(require "common.rkt"
         "interface/subarea.rkt")

(provide radio-box%/c)

(define radio-box%/c
 (and/c
  subarea<%>/c
  ;; contracts from window<%> except enable and is-enabled?
  (class/c
   (accept-drop-files
    (case->m
     (-> boolean?)
     (-> any/c any)))
   (client->screen
    (->m
     dimension/c
     dimension/c
     (values dimension/c
             dimension/c)))
   (focus
    (->m any))
   (get-client-size
    (->m (values dimension-non-zero/c dimension-non-zero/c)))
   (get-cursor
    (->m (or/c (is-a?/c cursor%) false/c)))
   (get-handle
    (->m exact-integer?))
   (get-height
    (->m dimension-non-zero/c))
   (get-label
    (->m (or/c label-string? (is-a?/c bitmap%)
               (one-of/c 'app 'caution 'stop) false/c)))
   (get-plain-label
    (->m (or/c string? false/c)))
   (get-size
    (->m (values dimension-non-zero/c dimension-non-zero/c)))
   (get-width (->m dimension-non-zero/c))
   (get-x (->m dimension/c))
   (get-y (->m dimension/c))
   (has-focus? (->m boolean?))
   (is-shown? (->m boolean?))
   (on-drop-file (->m path? any))
   (on-focus (->m any/c any))
   (on-move (->m dimension/c dimension/c any))
   (on-size (->m dimension/c dimension/c any))
   (on-subwindow-char (->m (is-a?/c window<%>) (is-a?/c key-event%) boolean?))
   (on-subwindow-event (->m (is-a?/c window<%>) (is-a?/c mouse-event%) boolean?))
   (on-superwindow-enable (->m any/c any))
   (on-superwindow-show (->m any/c any))
   (popup-menu (->m (is-a?/c popup-menu%)
                    dimension-non-zero/c
                    dimension-non-zero/c
                    any))
   (refresh (->m any))
   (screen->client (->m dimension/c dimension/c (values dimension/c dimension/c)))
   (set-cursor (->m (or/c (is-a?/c cursor%) false/c) any))
   (set-label (->m label-string? any))
   (show (->m any/c any)))
  ;; contracts from control<%>
  (class/c
   (command (->m (is-a?/c control-event%) any)))
  (class/c
   (init
    [label (or/c label-string? false/c)]
    [choices (or/c (listof label-string?) (listof (is-a?/c bitmap%)))]
    [parent parent/c]
    [callback
     ((is-a?/c tab-panel%) (is-a?/c control-event%) . -> . any)]
    [style (listof (one-of/c 'horizontal 'vertical
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
   (enable
    (case->m
     (-> any/c any)
     (-> exact-nonnegative-integer? any/c any)))
   (get-item-label
    (->m exact-nonnegative-integer? string?))
   (get-item-plain-label
    (->m exact-nonnegative-integer? string?))
   (get-number
    (->m exact-nonnegative-integer?))
   (get-selection
    (->m (or/c exact-nonnegative-integer? false/c)))
   (is-enabled?
    (case->m
     (-> boolean?)
     (-> exact-nonnegative-integer? boolean?)))
   (set-selection
    (->m (or/c exact-nonnegative-integer? false/c) any)))))

