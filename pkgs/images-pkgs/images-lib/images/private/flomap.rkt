#lang typed/racket/base

(require (only-in typed/racket/draw Bitmap% DC<%>)
         "flomap-struct.rkt"
         "flomap-stats.rkt"
         "flomap-pointwise.rkt"
         "flomap-transform.rkt"
         "flomap-gradient.rkt"
         "flomap-effects.rkt"
         "flomap-blur.rkt"
         "flomap-composite.rkt"
         "flomap-resize.rkt")

(require/typed
 "flomap-convert.rkt"
 [bitmap->flomap  ((Instance Bitmap%) -> flomap)]
 [flomap->bitmap  (flomap -> (Instance Bitmap%))]
 [draw-flomap     (((Instance DC<%>) -> Any) Integer Integer -> flomap)])

(provide (all-from-out "flomap-struct.rkt"
                       "flomap-stats.rkt"
                       "flomap-pointwise.rkt"
                       "flomap-transform.rkt"
                       "flomap-gradient.rkt"
                       "flomap-effects.rkt"
                       "flomap-blur.rkt"
                       "flomap-composite.rkt"
                       "flomap-resize.rkt")
         bitmap->flomap flomap->bitmap draw-flomap)
