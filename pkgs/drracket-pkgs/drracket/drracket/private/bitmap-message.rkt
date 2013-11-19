#lang typed/racket

(require typed/racket/gui)
(provide bitmap-message%)

(define bitmap-message%
  (class canvas%
    (inherit min-width min-height get-dc refresh)
    (: bm (Option (Instance Bitmap%)))
    (define bm #f)
    (define/override (on-paint)
      (define bm* bm)
      (when bm*
        (let ([dc (get-dc)])
          (send dc draw-bitmap bm* 0 0)))
      (void))
    (: set-bm ((Instance Bitmap%) -> Void))
    (define/public (set-bm b)
      (set! bm b)
      (define bm* bm)
      (when bm*
        (min-width (send bm* get-width))
        (min-height (send bm* get-height)))
      (refresh))
    (super-new (stretchable-width #f)
               (stretchable-height #f)
               (style '(no-focus)))))
