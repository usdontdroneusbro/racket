#lang racket/base

(require "area-container-window.rkt"
         "common.rkt")

(provide (prefix-out 
           top-level-window<%>-
           (except-out (all-defined-out)
                       top-level-window<%>/c))
         top-level-window<%>/c)

;; methods
(define can-close?/c (->m boolean?))
(define can-exit?/c  (->m boolean?))

(define center/c
  (->m (one-of/c 'horizontal 'vertical 'both)
       any))

(define get-edit-target-object/c
  (->m (maybe/c (or/c (is-a?/c window<%>)
                      (is-a?/c editor<%>)))))

(define get-edit-target-window/c
  (->m (maybe/c (is-a?/c window<%>))))

(define get-eventspace/c (->m eventspace?))

(define get-focus-object/c
  (->m (maybe/c (or/c (is-a?/c window<%>)
                      (is-a?/c editor<%>)))))

(define get-focus-window/c
  (->m (maybe/c (is-a?/c window<%>))))

(define move/c
  (->m dimension/c
       dimension/c
       any))

(define on-activate/c (->m any/c any))
(define on-close/c    (->m any))
(define on-exit/c     (->m any))
(define on-message/c  (->m any/c any/c))

(define on-traverse-char/c 
  (->m (is-a?/c key-event%) boolean?))

(define on-system-menu-char/c
  (->m (is-a?/c key-event%) boolean?))

(define resize/c
  (->m dimension-non-zero/c dimension-non-zero/c any))

(define show/c
  (->m any/c any))

;; interface
(define top-level-window<%>/c
  (and/c
   area-container-window<%>/c
   (class/c
    (can-close?               can-close?/c)
    (can-exit?                can-exit?/c)
    (override
      (can-exit?              can-exit?/c))
    (center                   center/c)
    (get-edit-target-object   get-edit-target-object/c)
    (get-edit-target-window   get-edit-target-window/c)
    (get-eventspace           get-eventspace/c)
    (get-focus-object         get-focus-object/c)
    (get-focus-window         get-focus-window/c)
    (move                     move/c)
    (on-activate              on-activate/c)
    (on-close                 on-close/c)
    (on-exit                  on-exit/c)
    (on-message               on-message/c)
    (on-traverse-char         on-traverse-char/c)
    (on-system-menu-char      on-system-menu-char/c)
    (inner
      (on-close               on-close/c))
    (override
      (on-activate            on-activate/c)
      (on-exit                on-exit/c)
      (on-message             on-message/c)
      (on-traverse-char       on-traverse-char/c)
      (on-system-menu-char    on-system-menu-char/c))
    (resize                   resize/c)
    (show                     show/c))))
