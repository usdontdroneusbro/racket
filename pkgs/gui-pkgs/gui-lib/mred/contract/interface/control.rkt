#lang racket/base

(require "common.rkt"
         "subarea.rkt"
         "window.rkt")

(provide (prefix-out control<%>- command/c)
         control<%>/c)

;; control<%> methods
(define command/c
  (->m (is-a?/c control-event%) any))

;; interface
(define control<%>/c
  (and/c
   subarea<%>/c
   window<%>/c
   (class/c
    (command command/c))))
