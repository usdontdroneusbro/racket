#lang racket/base

(require "common.rkt"
         "subwindow.rkt")

(provide (prefix-out 
           canvas<%>-
           (except-out (all-defined-out)
                       canvas<%>/c))
         canvas<%>/c)

;; canvas<%> methods
(define accept-tab-focus/c
  (case->m (-> boolean?)
           (-> any/c any)))

(define get-canvas-background/c 
  (->m (maybe/c (is-a?/c color%))))

(define get-dc/c 
  (->m (is-a?/c dc<%>)))

(define min-client-height/c
  (case->m (-> dimension-non-zero/c)
           (-> dimension-non-zero/c any)))

(define min-client-width/c
  (case->m (-> dimension-non-zero/c)
           (-> dimension-non-zero/c any)))

(define on-char/c   (->m (is-a?/c key-event%) any))
(define on-event/c  (->m (is-a?/c mouse-event%) any))
(define on-paint/c  (->m any))
(define on-tab-in/c (->m any))

(define set-canvas-background/c (->m (is-a?/c color%) any))
(define set-resize-corner/c     (->m any/c any))

(define warp-pointer/c
  (->m dimension-non-zero/c dimension-non-zero/c any))

;; interface
(define canvas<%>/c
  (and/c
   subwindow<%>/c
   (class/c
    (accept-tab-focus        accept-tab-focus/c)
    (get-canvas-background   get-canvas-background/c)
    (get-dc                  get-dc/c)
    (min-client-height       min-client-height/c)
    (min-client-width        min-client-width/c)
    (on-char                 on-char/c)
    (on-event                on-event/c)
    (on-paint                on-paint/c)
    (on-tab-in               on-tab-in/c)
    (override
      (on-char               on-char/c)
      (on-event              on-event/c)
      (on-paint              on-paint/c)
      (on-tab-in             on-tab-in/c))
    (set-canvas-background   set-canvas-background/c)
    (set-resize-corner       set-resize-corner/c)
    (warp-pointer            warp-pointer/c))))
