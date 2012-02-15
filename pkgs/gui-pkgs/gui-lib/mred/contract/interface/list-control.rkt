#lang racket/base

(require "common.rkt"
         "control.rkt")

(provide (prefix-out 
           list-control<%>-
           (except-out (all-defined-out)
                       list-control<%>/c))
         list-control<%>/c)

;; list-control<%> methods
(define append/c        (->m string? any))
(define clear/c         (->m any))
(define find-string/c   (->m string? 
                           (maybe/c exact-nonnegative-integer?)))

(define get-number/c    (->m exact-nonnegative-integer?))
(define get-selection/c (->m (maybe/c exact-nonnegative-integer?)))
(define get-string/c    (->m exact-nonnegative-integer? 
                                           (and/c immutable? label-string?)))
(define get-string-selection/c 
  (->m (maybe/c (and/c immutable? label-string?))))

(define set-selection/c (->m exact-nonnegative-integer? any))
(define set-string-selection/c (->m string? any))

;; interface
(define list-control<%>/c
  (and/c
   control<%>/c
   (class/c
    (append                 append/c)
    (clear                  clear/c)
    (find-string            find-string/c)
    (get-number             get-number/c )
    (get-selection          get-selection/c )
    (get-string             get-string/c )
    (get-string-selection   get-string-selection/c )
    (set-selection          set-selection/c )
    (set-string-selection   set-string-selection/c ))))
