#lang racket/base

(require "common.rkt")

(provide (all-defined-out))

;; menu-item<%> methods
(define menu-item<%>-delete/c (->m any))

(define menu-item<%>-get-parent/c
  (->m (or/c (is-a?/c menu%)
             (is-a?/c popup-menu%)
             (is-a?/c menu-bar%))))

(define menu-item<%>-is-deleted?/c (->m boolean?))
(define menu-item<%>-restore/c     (->m any))

;; menu-item<%> interface
(define menu-item<%>/c
  (class/c
   (delete       menu-item<%>-delete/c)
   (get-parent   menu-item<%>-get-parent/c)
   (is-deleted?  menu-item<%>-is-deleted?/c)
   (restore      menu-item<%>-restore/c)))

;; labelled-menu-item<%> methods
(define labelled-menu-item<%>-enable/c          (->m any/c any))
(define labelled-menu-item<%>-get-help-string/c (->m (maybe/c label-string?)))
(define labelled-menu-item<%>-get-label/c       (->m label-string?))
(define labelled-menu-item<%>-get-plain-label/c (->m label-string?))
(define labelled-menu-item<%>-is-enabled?/c     (->m boolean?))
(define labelled-menu-item<%>-on-demand/c       (->m any))
(define labelled-menu-item<%>-set-help-string/c (->m (maybe/c label-string?) any))
(define labelled-menu-item<%>-set-label/c       (->m label-string? any))

;; labelled-menu-item<%> interface
(define labelled-menu-item<%>/c
  (and/c
   menu-item<%>/c
   (class/c
    (enable            labelled-menu-item<%>-enable/c)
    (get-help-string   labelled-menu-item<%>-get-help-string/c)
    (get-label         labelled-menu-item<%>-get-label/c)
    (get-plain-label   labelled-menu-item<%>-get-plain-label/c)
    (is-enabled?       labelled-menu-item<%>-is-enabled?/c)
    (on-demand         labelled-menu-item<%>-on-demand/c)
    (override
      (on-demand       labelled-menu-item<%>-on-demand/c))
    (set-help-string   labelled-menu-item<%>-set-help-string/c)
    (set-label         labelled-menu-item<%>-set-label/c))))

;; selectable-menu-item<%> methods
(define selectable-menu-item<%>-command/c
  (->m (is-a?/c control-event%) any))

(define selectable-menu-item<%>-get-shortcut/c 
  (->m (or/c char? symbol? false/c)))

(define selectable-menu-item<%>-get-shortcut-prefix/c
  (->m (listof (one-of/c 'alt 'cmd 'meta 'ctl 'shift 'option))))

(define selectable-menu-item<%>-set-shortcut/c 
  (->m (or/c char? symbol? false/c) any))

(define selectable-menu-item<%>-set-shortcut-prefix/c
  (->m (listof (one-of/c 'alt 'cmd 'meta 'ctl 'shift 'option)) any))

;; selectable-menu-item<%> interface
(define selectable-menu-item<%>/c
  (and/c
   labelled-menu-item<%>/c
   (class/c
    (command              selectable-menu-item<%>-command/c)
    (get-shortcut         selectable-menu-item<%>-get-shortcut/c)
    (get-shortcut-prefix  selectable-menu-item<%>-get-shortcut-prefix/c)
    (set-shortcut         selectable-menu-item<%>-set-shortcut/c)
    (set-shortcut-prefix  selectable-menu-item<%>-set-shortcut-prefix/c))))

;; menu-item-container<%> methods
(define menu-item-container<%>-get-items/c 
  (->m (listof (is-a?/c menu-item<%>))))

(define menu-item-container<%>-on-demand/c 
  (->m any))

;; menu-item-container<%> interface
(define menu-item-container<%>/c
  (class/c
   (get-items     menu-item-container<%>-get-items/c)
   (on-demand     menu-item-container<%>-on-demand/c)
   (override
     (on-demand   menu-item-container<%>-on-demand/c))))
