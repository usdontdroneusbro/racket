#lang racket/base

(require "common.rkt")

(provide area<%>/c
         (prefix-out 
           area<%>- 
           (except-out (all-defined-out) area<%>/c)))

;; methods
(define get-graphical-min-size/c
  (->m (values dimension-non-zero/c dimension-non-zero/c)))

(define get-parent/c
  (->m (or/c (is-a?/c area-container<%>) false/c)))

(define get-top-level-window/c
  (->m (or/c (is-a?/c frame%) (is-a?/c dialog%))))

(define min-width/c
  (case->m
    (-> dimension-non-zero/c)
    (-> dimension-non-zero/c any)))

(define min-height/c
  (case->m
    (-> dimension-non-zero/c)
    (-> dimension-non-zero/c any)))

(define stretchable-height/c
  (case->m
    (-> boolean?)
    (-> any/c any)))

(define stretchable-width/c
  (case->m
    (-> boolean?)
    (-> any/c any)))

;; interface
(define area<%>/c
  (class/c
   (get-graphical-min-size get-graphical-min-size/c)
   (get-parent             get-parent/c)
   (get-top-level-window   get-top-level-window/c)
   (min-width              min-width/c)
   (min-height             min-height/c)
   (stretchable-height     stretchable-height/c)
   (stretchable-width      stretchable-width/c)))
