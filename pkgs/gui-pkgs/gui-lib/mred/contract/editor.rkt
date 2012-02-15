#lang racket/base

(require "common.rkt")

(provide editor-stream-in%/c
         editor-stream-in-base%/c
         editor-wordbreak-map%/c
         editor-data%/c
         editor-admin%/c)

(define editor-stream-in%/c
 (class/c
  (init [base (is-a?/c editor-stream-in-base%)])
  (get
   (->m
    (or/c (box/c exact-integer?)
          (box/c real?))
    (is-a?/c editor-stream-in%)))
  (get-bytes
   (->*m
    ()
    ((or/c (box/c exact-nonnegative-integer?) false/c))
    (or/c bytes? false/c)))
  (get-exact
   (->m exact-integer?))
  (get-fixed
   (->m
    (box/c exact-integer?)
    (is-a?/c editor-stream-in%)))
  (get-fixed-exact
   (->m exact-integer?))
  (get-inexact
   (->m real?))
  (get-unterminated-bytes
   (->*m
    ()
    ((or/c (box/c exact-nonnegative-integer?) false/c))
    (or/c bytes? false/c)))
  (jump-to
   (->m exact-nonnegative-integer? any))
  (ok?
   (->m boolean?))
  (remove-boundary
   (->m any))
  (set-boundary
   (->m exact-nonnegative-integer? any))
  (skip
   (->m exact-nonnegative-integer? any))
  (tell
   (->m exact-nonnegative-integer?))))

(define editor-stream-in-base%/c
 (class/c
  (bad?
   (->m boolean?))
  (read
   (->m
    (and/c vector? (not immutable?))
    exact-nonnegative-integer?))
  (read-bytes
   (->m
    (and/c bytes? (not immutable?))
    exact-nonnegative-integer?))
  (read-byte
   (->m (or/c byte? false/c)))
  (seek
   (->m
    exact-nonnegative-integer?
    any))
  (skip
   (->m
    exact-nonnegative-integer?
    any))
  (tell
   (->m exact-nonnegative-integer?))))

(define editor-wordbreak-map%/c
 (class/c
  (init)
  (get-map (->m char? (listof symbol?)))
  (override (get-map (->m char? (listof symbol?))))
  (set-map (->m char? (listof symbol?) any))
  (override (set-map (->m char? (listof symbol?) any)))))

(define editor-data%/c
 (class/c
  (init)
  (get-dataclass (->m (or/c (is-a?/c editor-data-class%) false/c)))
  (override (get-dataclass (->m (or/c (is-a?/c editor-data-class%) false/c))))
  (get-next (->m (or/c (is-a?/c editor-data%) false/c)))
  (override (get-next (->m (or/c (is-a?/c editor-data%) false/c))))
  (set-dataclass (->m (is-a?/c editor-data-class%) any))
  (override (set-dataclass (->m (is-a?/c editor-data-class%) any)))
  (set-next (->m (or/c (is-a?/c editor-data%) false/c) any))
  (override (set-next (->m (or/c (is-a?/c editor-data%) false/c) any)))
  (write (->m (is-a?/c editor-stream-out%) boolean?))
  (override (write (->m (is-a?/c editor-stream-out%) boolean?)))))

(define editor-admin%/c
 (class/c
  (init)
  (get-dc
   (->*m
    ()
    ((or/c (box/c real?) false/c) (or/c (box/c real?) false/c))
    (or/c (is-a?/c dc<%>) false/c)))
  (override (get-dc
             (->*m
              ()
              ((or/c (box/c real?) false/c) (or/c (box/c real?) false/c))
              (or/c (is-a?/c dc<%>) false/c))))
  (get-max-view
   (->*m
    ((or/c (box/c real?) false/c)
     (or/c (box/c real?) false/c)
     (or/c (box/c (>=/c 0)) false/c)
     (or/c (box/c (>=/c 0)) false/c))
    (any/c)
    any))
  (override (get-max-view
             (->*m
              ((or/c (box/c real?) false/c)
               (or/c (box/c real?) false/c)
               (or/c (box/c (>=/c 0)) false/c)
               (or/c (box/c (>=/c 0)) false/c))
              (any/c)
              any)))
  (get-view
   (->*m
    ((or/c (box/c real?) false/c)
     (or/c (box/c real?) false/c)
     (or/c (box/c (>=/c 0)) false/c)
     (or/c (box/c (>=/c 0)) false/c))
    (any/c)
    any))
  (override (get-view
             (->*m
              ((or/c (box/c real?) false/c)
               (or/c (box/c real?) false/c)
               (or/c (box/c (>=/c 0)) false/c)
               (or/c (box/c (>=/c 0)) false/c))
              (any/c)
              any)))
  (grab-caret (->*m () ((one-of/c 'immediate 'display 'global)) any))
  (override (grab-caret (->*m () ((one-of/c 'immediate 'display 'global)) any)))
  (modified (->m any/c any))
  (override (modified (->m any/c any)))
  (needs-update
   (->m real? real? (>=/c 0) (>=/c 0) any))
  (override (needs-update
             (->m
              real?
              real?
              (>=/c 0)
              (>=/c 0)
              any)))
  (popup-menu (->m (is-a?/c popup-menu%) real? real? boolean?))
  (override (popup-menu (->m (is-a?/c popup-menu%) real? real? boolean?)))
  (refresh-delayed? (->m boolean?))
  (override (refresh-delayed? (->m boolean?)))
  (resized (->m any/c any))
  (override (resized (->m any/c any)))
  (scroll-to
   (->*m
    (real? real? (>=/c 0) (>=/c 0))
    (any/c (one-of/c 'start 'end 'none))
    boolean?))
  (override (scroll-to
             (->*m
              (real? real? (>=/c 0) (>=/c 0))
              (any/c (one-of/c 'start 'end 'none))
              boolean?)))
  (update-cursor (->m any))
  (override (update-cursor (->m any)))))

