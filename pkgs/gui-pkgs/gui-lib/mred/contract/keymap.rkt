#lang racket/base

(require "common.rkt")

(provide (prefix-out 
           keymap%-
           (except-out (all-defined-out)
                       keymap%/c))
         keymap%/c)

;; method contracts
(define add-function/c 
  (->m string? (-> any/c (is-a?/c event%) any/c) any))

(define break-sequence/c 
  (->m any))

(define call-function/c
  (->*m (string? any/c (is-a?/c event%)) 
        (any/c) 
        boolean?))

(define chain-to-keymap/c 
  (->m (is-a?/c keymap%) any/c any))

(define get-double-click-interval/c 
  (->m exact-nonnegative-integer?))

(define handle-key-event/c 
  (->m any/c (is-a?/c key-event%) boolean?))

(define handle-mouse-event/c 
  (->m any/c (is-a?/c mouse-event%) boolean?))

(define map-function/c 
  (->m string? string? any))

(define remove-chained-keymap/c 
  (->m (is-a?/c keymap%) any))

(define remove-grab-key-function/c 
  (->m any))

(define remove-grab-mouse-function/c 
  (->m any))

(define set-break-sequence-callback/c 
  (->m (-> any) any))

(define set-double-click-interval/c 
  (->m exact-nonnegative-integer? any))

(define set-grab-key-function/c
   (->m (-> (or/c string? false/c) 
            (is-a?/c keymap%) 
            any/c 
            (is-a?/c key-event%) 
            any) 
        any))

(define set-grab-mouse-function/c
   (->m (-> (or/c string? false/c) 
            (is-a?/c keymap%) 
            any/c 
            (is-a?/c mouse-event%) 
            any) 
        any))

;; class contract
(define keymap%/c
  (class/c
    (add-function                 add-function/c)
    (break-sequence               break-sequence/c)
    (call-function                call-function/c)
    (chain-to-keymap              chain-to-keymap/c)
    (get-double-click-interval    get-double-click-interval/c)
    (handle-key-event             handle-key-event/c)
    (handle-mouse-event           handle-mouse-event/c)
    (map-function                 map-function/c)
    (remove-chained-keymap        remove-chained-keymap/c)
    (remove-grab-key-function     remove-grab-key-function/c)
    (remove-grab-mouse-function   remove-grab-mouse-function/c)
    (set-break-sequence-callback  set-break-sequence-callback/c)
    (set-double-click-interval    set-double-click-interval/c)
    (set-grab-key-function        set-grab-key-function/c)
    (set-grab-mouse-function      set-grab-mouse-function/c)))
