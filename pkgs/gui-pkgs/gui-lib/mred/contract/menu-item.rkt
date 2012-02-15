#lang racket/base

(require "common.rkt"
         "interface/menu-item.rkt")

(provide menu-item%/c
         checkable-menu-item%/c
         separator-menu-item%/c
         menu%/c
         menu-bar%/c
         popup-menu%/c)

(define menu-item%/c
 (and/c
  selectable-menu-item<%>/c
  (class/c
   (init
    [label label-string?]
    [parent (or/c (is-a?/c menu%) (is-a?/c popup-menu%))]
    [callback (callback/c menu-item%)]
    [shortcut (or/c char? false/c)]
    [help-string (or/c label-string? false/c)]
    [demand-callback ((is-a?/c menu-item%) . -> . any)]
    [shortcut-prefix (listof (one-of/c 'alt 'cmd 'meta 'ctl
                                       'shift 'option))]))))

(define checkable-menu-item%/c
 (and/c
  selectable-menu-item<%>/c
  (class/c
   (init
    [label label-string?]
    [parent (or/c (is-a?/c menu%) (is-a?/c popup-menu%))]
    [callback (callback/c checkable-menu-item%)]
    [shortcut (or/c char? false/c)]
    [help-string (or/c label-string? false/c)]
    [demand-callback ((is-a?/c menu-item%) . -> . any)]
    [checked any/c]
    [shortcut-prefix (listof (one-of/c 'alt 'cmd 'meta 'ctl
                                       'shift 'option))])
   (check
    (->m any/c any))
   (is-checked?
    (->m boolean?)))))

(define separator-menu-item%/c
 (and/c
  menu-item<%>/c
  (class/c
   (init
    [parent (or/c (is-a?/c menu%) (is-a?/c popup-menu%))]))))

(define menu%/c
 (and/c
  menu-item-container<%>/c
  labelled-menu-item<%>/c
  (class/c
   (init
    [label label-string?]
    [parent (or/c (is-a?/c menu%) (is-a?/c popup-menu%)
                  (is-a?/c menu-bar%))]
    [help-string (or/c label-string? false/c)]
    [demand-callback ((is-a?/c menu%) . -> . any)]))))

(define menu-bar%/c
 (and/c
  menu-item-container<%>/c
  (class/c
   (init
    [parent (or/c (is-a?/c frame%) (one-of/c 'root))]
    [demand-callback ((is-a?/c menu-bar%) . -> . any)])
   (enable
    (->m any/c any))
   (get-frame
    (->m (is-a?/c frame%)))
   (is-enabled?
    (->m boolean?)))))

(define popup-menu%/c
 (and/c
  menu-item-container<%>/c
  (class/c
   (init
    [title (or/c label-string? false/c)]
    [popdown-callback (callback/c popup-menu%)]
    [demand-callback ((is-a?/c popup-menu%) . -> . any)]
    [font (is-a?/c font%)])
   (get-font
    (->m (is-a?/c font%)))
   (get-popup-target
    (->m (or/c (is-a?/c window<%>) (is-a?/c editor<%>) false/c)))
   (set-min-width
    (->m dimension-non-zero/c any)))))
