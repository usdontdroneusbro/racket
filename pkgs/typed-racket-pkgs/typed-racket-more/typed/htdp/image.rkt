#lang typed/racket

(define-type Mode
  (U 'solid "solid" 'outline "outline"))

(define-type Image-Color
  (U String Symbol))

(require/typed/provide
 htdp/image
 [#:opaque Image image?]
 ;; This cannot be supported right now because htdp/image does
 ;; not export the `color` binding
 #|
 [#:struct color ([red : Byte] [green : Byte] [blue : Byte])
           #:extra-constructor-name make-color]
 |#
 ;; 1.1.1
 [image=? (Image Image -> Boolean)]
 ;; 1.1.3
 [rectangle (Nonnegative-Real Nonnegative-Real Mode Image-Color -> Image)]
 [circle (Nonnegative-Real Mode Image-Color -> Image)]
 [ellipse (Nonnegative-Real Natural Mode Image-Color -> Image)]
 [triangle (Real Mode Image-Color -> Image)]
 [star (Positive-Real Positive-Real Positive-Real Mode Image-Color -> Image)]
 [regular-polygon
  (case-> (Positive-Integer Real Mode Image-Color -> Image)
          (Positive-Integer Real Mode Image-Color Real -> Image))]
 [line (Real Real Image-Color -> Image)]
 [text (String Positive-Integer Image-Color -> Image)]
 ;; 1.1.4
 [image-width (Image -> Natural)]
 [image-height (Image -> Natural)]
 [pinhole-x (Image -> Integer)]
 [pinhole-y (Image -> Integer)]
 [put-pinhole (Image Real Real -> Image)]
 [move-pinhole (Image Real Real -> Image)]
 ;; 1.1.5
 [add-line (Image Real Real Real Real Image-Color -> Image)]
 [overlay (Image Image Image * -> Image)]
 [overlay/xy (Image Real Real Image -> Image)]
 [image-inside? (Image Image -> Boolean)]
 #;
 [find-image (Image Image -> Posn)]
 ;; 1.1.6
 [shrink-tl (Image Real Real -> Image)]
 [shrink-tr (Image Real Real -> Image)]
 [shrink-bl (Image Real Real -> Image)]
 [shrink-br (Image Real Real -> Image)]
 [shrink (Image Real Real Real Real -> Image)]
 ;; 1.1.7
 [empty-scene (Natural Natural -> Image)]
 [place-image (Image Real Real Image -> Image)]
 [nw:rectangle (Natural Natural Mode Image-Color -> Image)]
 [scene+line (Image Real Real Real Real Image-Color -> Image)]
 ;; 1.1.8
 [image->color-list (Image -> (Listof Image-Color))]
 [color-list->image ((Listof Image-Color) Natural Natural Natural Natural -> Image)]
 ;; Does not work for the same reason as color
 #|
 [#:struct alpha-color ([red : Byte] [green : Byte] [blue : Byte] [alpha : Byte])
           #:extra-constructor-name make-alpha-color]
 [image->alpha-color-list (Image -> (Listof alpha-color))]
 [alpha-color-list->image ((Listof alpha-color) Natural Natural Natural Natural -> Image)]
 |#
 )

