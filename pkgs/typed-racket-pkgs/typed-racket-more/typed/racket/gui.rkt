#lang typed/racket/base

(require "gui-types.rkt"
         "gui-env.rkt"
         "draw.rkt"
         "snip.rkt")

(provide (all-from-out "gui-types.rkt")
         (all-from-out "gui-env.rkt")
         (all-from-out "draw.rkt")
         (all-from-out "snip.rkt"))

(require/typed
 racket/gui
 ;; This would be better to have in 4.2 below
 ;; but the `require/typed/provide` macro does not
 ;; allow it
 [#:opaque Eventspace eventspace?])

(provide Eventspace eventspace?)

(require/typed/provide
 racket/gui
 [make-eventspace (-> Eventspace)]
 [current-eventspace (Parameterof Eventspace)]
 [event-dispatch-handler (Parameterof (Eventspace -> Any))]
 [eventspace-event-evt
  (case-> (-> (Evtof Eventspace))
          (Eventspace -> (Evtof Eventspace)))]
 [eventspace-shutdown? (Eventspace -> Boolean)]
 [eventspace-handler-thread (Eventspace -> (Option Thread))])

