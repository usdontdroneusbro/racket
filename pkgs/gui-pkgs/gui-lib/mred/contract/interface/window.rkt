#lang racket/base

(require "area.rkt"
         "common.rkt")

(provide (prefix-out 
           window<%>-
           (except-out (all-defined-out) window<%>/c))
          window<%>/c)

;; methods
(define accept-drop-files/c 
  (case->m (-> boolean?)
           (-> any/c any)))

(define client-/c>screen
  (->m dimension/c dimension/c
       (values dimension/c dimension/c)))

(define enable/c (->m any/c any))

(define focus/c  (->m any))

(define get-client-size/c 
  (->m (values dimension-non-zero/c dimension-non-zero/c)))

(define get-cursor/c (->m (maybe/c (is-a?/c cursor%))))

(define get-handle/c (->m exact-integer?))

(define get-height/c (->m dimension-non-zero/c))

(define get-label/c
  (->m (or/c label-string? (is-a?/c bitmap%)
             (one-of/c 'app 'caution 'stop) 
             (list/c (is-a?/c bitmap%)
                     label-string?
                     (one-of/c 'left 'top 'right 'bottom))
             #f)))

(define get-plain-label/c (->m (or/c string? false/c)))

(define get-size/c 
  (->m (values dimension-non-zero/c dimension-non-zero/c)))

(define get-width/c (->m dimension-non-zero/c))

(define get-x/c (->m dimension/c))
(define get-y/c (->m dimension/c))

(define has-focus?/c (->m boolean?))

(define is-enabled?/c (->m boolean?))
(define is-shown?/c (->m boolean?))

(define on-drop-file/c (->m path? any))
(define on-focus/c (->m any/c any))
(define on-move/c (->m dimension/c dimension/c any))
(define on-size/c (->m dimension/c dimension/c any))
(define on-subwindow-char/c 
  (->m (is-a?/c window<%>) (is-a?/c key-event%) boolean?))
(define on-subwindow-event/c 
  (->m (is-a?/c window<%>) (is-a?/c mouse-event%) boolean?))
(define on-superwindow-enable/c (->m any/c any))
(define on-superwindow-show/c (->m any/c any))

(define popup-menu/c (->m (is-a?/c popup-menu%)
                                  dimension-non-zero/c
                                  dimension-non-zero/c
                                  any))

(define refresh/c (->m any))

(define screen-/c>client 
  (->m dimension/c dimension/c 
       (values dimension/c dimension/c)))

(define set-cursor/c (->m (or/c (is-a?/c cursor%) false/c) any))
(define set-label/c (->m label-string? any))

(define show/c (->m any/c any))

;; interface
(define window<%>/c
  (and/c
   area<%>/c
   (class/c
    (accept-drop-files       accept-drop-files/c)
    (client->screen          client-/c>screen)
    (enable                  enable/c)
    (focus                   focus/c)
    (get-client-size         get-client-size/c)
    (get-cursor              get-cursor/c)
    (get-handle              get-handle/c)
    (get-height              get-height/c)
    (get-label               get-label/c)
    (get-plain-label         get-plain-label/c)
    (get-size                get-size/c)
    (get-width               get-width/c)
    (get-x                   get-x/c)
    (get-y                   get-y/c)
    (has-focus?              has-focus?/c)
    (is-enabled?             is-enabled?/c)
    (is-shown?               is-shown?/c)
    (on-drop-file            on-drop-file/c)
    (on-focus                on-focus/c)
    (on-move                 on-move/c)
    (on-size                 on-size/c)
    (on-subwindow-char       on-subwindow-char/c)
    (on-subwindow-event      on-subwindow-event/c)
    (on-superwindow-enable   on-superwindow-enable/c)
    (on-superwindow-show     on-superwindow-show/c)
    (override
      (on-drop-file          on-drop-file/c)
      (on-focus              on-focus/c)
      (on-move               on-move/c)
      (on-size               on-size/c)
      (on-subwindow-char     on-subwindow-char/c)
      (on-subwindow-event    on-subwindow-event/c)
      (on-superwindow-enable on-superwindow-enable/c)
      (on-superwindow-show   on-superwindow-show/c))
    (popup-menu              popup-menu/c)
    (refresh                 refresh/c)
    (screen->client          screen-/c>client)
    (set-cursor              set-cursor/c)
    ;; Behavioral subtypes override this with incompatible contracts
    ;; (set-label               window<%>-set-label/c)
    (show                    show/c))))
