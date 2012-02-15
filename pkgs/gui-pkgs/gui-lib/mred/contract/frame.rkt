#lang racket/base

(require "common.rkt"
         "interface/top-level-window.rkt")

(provide (prefix-out
           frame%-
           (except-out (all-defined-out)
                       frame%/c))
         frame%/c)
  
;; methods
(define create-status-line/c (->m any))

(define get-menu-bar/c 
  (->m (or/c (is-a?/c menu-bar%) false/c)))

(define has-status-line?/c (->m boolean?))

(define iconize/c (->m any/c any))

(define is-iconized?/c (->m boolean?))
  
(define is-maximized?/c (->m boolean?))

(define maximize/c (->m any/c any))

(define modified/c
  (case->m (-> boolean?)
           (-> any/c any)))

(define on-menu-char/c (->m (is-a?/c key-event%) boolean?))

(define on-subwindow-char/c
 (->m (is-a?/c window<%>)
      (is-a?/c key-event%)
      boolean?))

(define on-toolbar-button-click/c
 (->m any))

(define set-icon/c
 (->*m
  ((is-a?/c bitmap%))
  ((is-a?/c bitmap%)
   (one-of/c 'small 'large 'both))
  any))

(define set-status-text/c
 (->m string? any))

;; class
(define frame%/c
 (and/c
  top-level-window<%>/c
  (class/c
   (init
    [label label-string?]
    [parent (or/c (is-a?/c frame%) false/c)]
    [width (or/c dimension-non-zero/c false/c)]
    [height (or/c dimension-non-zero/c false/c)]
    [x (or/c dimension/c false/c)]
    [y (or/c dimension/c false/c)]
    [style (listof (one-of/c 'no-resize-border 'no-caption
                             'no-system-menu 'hide-menu-bar
                             'mdi-parent 'mdi-child
                             'toolbar-button 'float 'metal))]
    [enabled any/c]
    [border (integer-in 0 1000)]
    [spacing (integer-in 0 1000)]
    [alignment (list/c (one-of/c 'left 'center 'right)
                       (one-of/c 'top 'center 'bottom))]
    [min-width dimension-non-zero/c]
    [min-height dimension-non-zero/c]
    [stretchable-width any/c]
    [stretchable-height any/c])
   [create-status-line       create-status-line/c]
   [get-menu-bar             get-menu-bar/c]
   [has-status-line?         has-status-line?/c]
   [iconize                  iconize/c]
   [is-iconized?             is-iconized?/c]
   [is-maximized?            is-maximized?/c]
   [maximize                 maximize/c]
   [modified                 modified/c]
   [on-menu-char             on-menu-char/c]
   [on-subwindow-char        on-subwindow-char/c]
   [on-toolbar-button-click  on-toolbar-button-click/c]
   [set-icon                 set-icon/c]
   [set-status-text          set-status-text/c]
   (override
     [maximize                 maximize/c]
     [on-menu-char             on-menu-char/c]
     [on-subwindow-char        on-subwindow-char/c]
     [on-toolbar-button-click  on-toolbar-button-click/c]))))
