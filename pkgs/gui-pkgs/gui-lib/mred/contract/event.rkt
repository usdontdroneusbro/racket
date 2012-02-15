#lang racket/base

(require "common.rkt")

(provide control-event%/c
         mouse-event%/c
         key-event%/c)

(define control-event%/c
 (class/c
  (init [event-type event-type/c]
        [time-stamp exact-integer?])
  (get-event-type (->m event-type/c))
  (set-event-type (->m event-type/c any))))

(define mouse-event%/c
 (class/c
  (init (event-type
         (or/c
          'enter
          'leave
          'left-down
          'left-up
          'middle-down
          'middle-up
          'right-down
          'right-up
          'motion))
        (left-down any/c)
        (middle-down any/c)
        (right-down any/c)
        (x exact-integer?)
        (y exact-integer?)
        (shift-down any/c)
        (control-down any/c)
        (meta-down any/c)
        (alt-down any/c)
        (time-stamp exact-integer?)
        (caps-down any/c))
  (button-changed? (->*m () ((or/c 'left 'middle 'right 'any)) boolean?))
  (override (button-changed? (->*m () ((or/c 'left 'middle 'right 'any)) boolean?)))
  (button-down? (->*m () ((or/c 'left 'middle 'right 'any)) boolean?))
  (override (button-down? (->*m () ((or/c 'left 'middle 'right 'any)) boolean?)))
  (button-up? (->*m () ((or/c 'left 'middle 'right 'any)) boolean?))
  (override (button-up? (->*m () ((or/c 'left 'middle 'right 'any)) boolean?)))
  (dragging? (->m boolean?))
  (override (dragging? (->m boolean?)))
  (entering? (->m boolean?))
  (override (entering? (->m boolean?)))
  (get-alt-down (->m boolean?))
  (override (get-alt-down (->m boolean?)))
  (get-caps-down (->m boolean?))
  (override (get-caps-down (->m boolean?)))
  (get-control-down (->m boolean?))
  (override (get-control-down (->m boolean?)))
  (get-event-type
   (->m
    (or/c 'enter 'leave 'left-down 'left-up 'middle-down 'middle-up 'right-down 'right-up 'motion)))
  (override (get-event-type
             (->m
              (or/c
               'enter
               'leave
               'left-down
               'left-up
               'middle-down
               'middle-up
               'right-down
               'right-up
               'motion))))
  (get-left-down (->m boolean?))
  (override (get-left-down (->m boolean?)))
  (get-meta-down (->m boolean?))
  (override (get-meta-down (->m boolean?)))
  (get-middle-down (->m boolean?))
  (override (get-middle-down (->m boolean?)))
  (get-right-down (->m boolean?))
  (override (get-right-down (->m boolean?)))
  (get-shift-down (->m boolean?))
  (override (get-shift-down (->m boolean?)))
  (get-time-stamp (->m exact-integer?))
  (override (get-time-stamp (->m exact-integer?)))
  (get-x (->m exact-integer?))
  (override (get-x (->m exact-integer?)))
  (get-y (->m exact-integer?))
  (override (get-y (->m exact-integer?)))
  (leaving? (->m boolean?))
  (override (leaving? (->m boolean?)))
  (moving? (->m boolean?))
  (override (moving? (->m boolean?)))
  (set-alt-down (->m any/c any))
  (override (set-alt-down (->m any/c any)))
  (set-caps-down (->m any/c any))
  (override (set-caps-down (->m any/c any)))
  (set-control-down (->m any/c any))
  (override (set-control-down (->m any/c any)))
  (set-event-type
   (->m
    (or/c 'enter 'leave 'left-down 'left-up 'middle-down 'middle-up 'right-down 'right-up 'motion)
    any))
  (override (set-event-type
             (->m
              (or/c
               'enter
               'leave
               'left-down
               'left-up
               'middle-down
               'middle-up
               'right-down
               'right-up
               'motion)
              any)))
  (set-left-down (->m any/c any))
  (override (set-left-down (->m any/c any)))
  (set-meta-down (->m any/c any))
  (override (set-meta-down (->m any/c any)))
  (set-middle-down (->m any/c any))
  (override (set-middle-down (->m any/c any)))
  (set-right-down (->m any/c any))
  (override (set-right-down (->m any/c any)))
  (set-shift-down (->m any/c any))
  (override (set-shift-down (->m any/c any)))
  (set-time-stamp (->m exact-integer? any))
  (override (set-time-stamp (->m exact-integer? any)))
  (set-x (->m exact-integer? any))
  (override (set-x (->m exact-integer? any)))
  (set-y (->m exact-integer? any))
  (override (set-y (->m exact-integer? any)))))

(define key-event%/c
 (class/c
  (init (key-code (or/c char? key-code-symbol?))
        (shift-down any/c)
        (control-down any/c)
        (meta-down any/c)
        (alt-down any/c)
        (x exact-integer?)
        (y exact-integer?)
        (time-stamp exact-integer?)
        (caps-down any/c))
  (get-alt-down (->m boolean?))
  (override (get-alt-down (->m boolean?)))
  (get-caps-down (->m boolean?))
  (override (get-caps-down (->m boolean?)))
  (get-control-down (->m boolean?))
  (override (get-control-down (->m boolean?)))
  (get-key-code (->m (or/c char? key-code-symbol?)))
  (override (get-key-code (->m (or/c char? key-code-symbol?))))
  (get-key-release-code (->m (or/c char? key-code-symbol?)))
  (override (get-key-release-code (->m (or/c char? key-code-symbol?))))
  (get-meta-down (->m boolean?))
  (override (get-meta-down (->m boolean?)))
  (get-other-altgr-key-code (->m (or/c char? key-code-symbol? false/c)))
  (override (get-other-altgr-key-code (->m (or/c char? key-code-symbol? false/c))))
  (get-other-caps-key-code (->m (or/c char? key-code-symbol? false/c)))
  (override (get-other-caps-key-code (->m (or/c char? key-code-symbol? false/c))))
  (get-other-shift-altgr-key-code (->m (or/c char? key-code-symbol? false/c)))
  (override (get-other-shift-altgr-key-code (->m (or/c char? key-code-symbol? false/c))))
  (get-other-shift-key-code (->m (or/c char? key-code-symbol? false/c)))
  (override (get-other-shift-key-code (->m (or/c char? key-code-symbol? false/c))))
  (get-shift-down (->m boolean?))
  (override (get-shift-down (->m boolean?)))
  (get-time-stamp (->m exact-integer?))
  (override (get-time-stamp (->m exact-integer?)))
  (get-x (->m exact-integer?))
  (override (get-x (->m exact-integer?)))
  (get-y (->m exact-integer?))
  (override (get-y (->m exact-integer?)))
  (set-alt-down (->m any/c any))
  (override (set-alt-down (->m any/c any)))
  (set-caps-down (->m any/c any))
  (override (set-caps-down (->m any/c any)))
  (set-control-down (->m any/c any))
  (override (set-control-down (->m any/c any)))
  (set-key-code (->m (or/c char? key-code-symbol?) any))
  (override (set-key-code (->m (or/c char? key-code-symbol?) any)))
  (set-key-release-code (->m (or/c char? key-code-symbol?) any))
  (override (set-key-release-code (->m (or/c char? key-code-symbol?) any)))
  (set-meta-down (->m any/c any))
  (override (set-meta-down (->m any/c any)))
  (set-other-altgr-key-code (->m (or/c char? key-code-symbol? false/c) any))
  (override (set-other-altgr-key-code (->m (or/c char? key-code-symbol? false/c) any)))
  (set-other-caps-key-code (->m (or/c char? key-code-symbol? false/c) any))
  (override (set-other-caps-key-code (->m (or/c char? key-code-symbol? false/c) any)))
  (set-other-shift-altgr-key-code (->m (or/c char? key-code-symbol? false/c) any))
  (override (set-other-shift-altgr-key-code (->m (or/c char? key-code-symbol? false/c) any)))
  (set-other-shift-key-code (->m (or/c char? key-code-symbol? false/c) any))
  (override (set-other-shift-key-code (->m (or/c char? key-code-symbol? false/c) any)))
  (set-shift-down (->m any/c any))
  (override (set-shift-down (->m any/c any)))
  (set-time-stamp (->m exact-integer? any))
  (override (set-time-stamp (->m exact-integer? any)))
  (set-x (->m exact-integer? any))
  (override (set-x (->m exact-integer? any)))
  (set-y (->m exact-integer? any))
  (override (set-y (->m exact-integer? any)))))
