#lang racket

(require (only-in racket/class make-object)
         (only-in racket/draw color% bitmap%))

;; This module exists only as a helper for the
;; "insert-large-letters.rkt" module due to TR limitations

;; TR can't deal with the constructor of `color%` because
;; it uses `init-rest` arguments. So use this wrapper
;; function and import it with a sensible type.
(define (make-color:mut r g b [a 1.0])
  (make-object color% r g b a))

;; same issue as color%
(define (make-bitmap:mono width height)
  (make-object bitmap% width height #t))

(provide make-color:mut make-bitmap:mono)

