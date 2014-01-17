#lang typed/racket/base

(require "racket/gui-types.rkt"
         "framework-types.rkt"
         "framework-env.rkt")

(provide (all-from-out "framework-types.rkt")
         (all-from-out "framework-env.rkt"))

(require/typed/provide
 framework
 ;; 11 Editor
 [editor:get-standard-style-list
  (-> (Instance Style-List%))]
 ;; 16
 [gui-utils:ok/cancel-buttons
  ((Instance Horizontal-Panel%)
   ((Instance Button%) (Instance Event%) -> Void)
   ((Instance Button%) (Instance Event%) -> Void)
   -> (Values Any Any))]
 ;; 27
 [preferences:get (Symbol -> Sexp)]
 [preferences:set (Symbol Sexp -> Void)]
 [preferences:set-default (Symbol Sexp (Any -> Boolean) -> Void)])

