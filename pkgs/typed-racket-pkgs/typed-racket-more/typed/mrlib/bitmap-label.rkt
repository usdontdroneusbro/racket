#lang typed/racket

(require typed/racket/gui)

(require/typed/provide
 mrlib/bitmap-label
 [make-bitmap-label
  (case->
   (String (U (Instance Bitmap%) Path-String) -> (Instance Bitmap%))
   (String (U (Instance Bitmap%) Path-String)
    (Instance Font%) -> (Instance Bitmap%)))]
 [bitmap-label-maker
  (String (U (Instance Bitmap%) Path-String)
   -> (Any -> (Instance Bitmap%)))])

