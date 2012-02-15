#lang racket/base

(require "common.rkt"
         "area-container.rkt"
         "subarea.rkt"
         "window.rkt")

(provide area-container-window<%>/c)

(define area-container-window<%>/c
  (and/c area-container<%>/c window<%>/c))
