#lang racket/base

(require "area.rkt"
         "common.rkt")

(provide (prefix-out subarea<%>-
                     (combine-out horiz-margin/c
                                  vert-margin/c))
         subarea<%>/c)

;; methods
(define horiz-margin/c 
  (case->m (-> (integer-in 0 1000))
           (-> (integer-in 0 1000) any)))

(define vert-margin/c 
  (case->m (-> (integer-in 0 1000))
           (-> (integer-in 0 1000) any)))

;; interface
(define subarea<%>/c
  (and/c
   area<%>/c
   (class/c
    (horiz-margin horiz-margin/c)
    (vert-margin  vert-margin/c))))
