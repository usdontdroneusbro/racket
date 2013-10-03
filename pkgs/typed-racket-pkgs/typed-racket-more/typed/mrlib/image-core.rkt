#lang typed/racket

(require (only-in typed/2htdp/image Image)
         typed/racket/draw)

(require/typed/provide
 mrlib/image-core
 ;; FIXME: allow image-snip% too
 [render-image ((U (Instance Bitmap%) Image) (Instance DC<%>) Real Real -> Void)]
 ;; FIXME: should the domain be more flexible here?
 [un/cache-image (Image Any -> Image)]
 [compute-image-cache (Image -> Void)])

