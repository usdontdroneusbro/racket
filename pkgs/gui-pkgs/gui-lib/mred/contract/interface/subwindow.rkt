#lang racket/base

(require "common.rkt"
         "subarea.rkt"
         "window.rkt")

(provide subwindow<%>/c)

(define subwindow<%>/c
  (and/c subarea<%>/c window<%>/c))
