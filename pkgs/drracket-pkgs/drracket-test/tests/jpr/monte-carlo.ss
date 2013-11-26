;;; Simulation graphique a la Monte Carlo
;;; ----> Some red points are outside the circle on the bottom right ???

#lang typed/racket

#;
(require racket/require)
#;
(require (subtract-in racket/gui/base racket/draw)
         racket/draw/private/bitmap
         racket/draw/private/pen
         racket/draw/private/brush)
(require typed/racket/gui)

;(define RED-PEN (make-object pen% "red" 2 'solid))
;(define BLACK-PEN (make-object pen% "black" 2 'solid))
;(define BLUE-PEN (make-object pen% "blue" 2 'solid))
;(define YELLOW-BRUSH (make-object brush% "yellow" 'solid))
(define RED-PEN (new pen% [color "red"] [width 2] [style 'solid]))
(define BLACK-PEN (new pen% [color "black"] [width 2] [style 'solid]))
(define BLUE-PEN (new pen% [color "blue"] [width 2] [style 'solid]))
(define YELLOW-BRUSH (new brush% [color "yellow"] [style 'solid]))

(define FRAME 
  (new frame% (label "Monte-Carlo") (stretchable-width #f) (stretchable-height #f)))

(define VPANEL 
  (new vertical-panel% (parent FRAME)))

(: CANVAS (Instance Canvas%))

(define TEXT-FIELD 
  (new text-field% (parent VPANEL) 
       (label "Nombre de points N =") 
       (init-value "5000")
       (callback (lambda (t e)
                   (when (eq? (send e get-event-type) 'text-field-enter)
                     (send CANVAS refresh))))))

(define MSG (new message% (parent VPANEL) (label "?") (min-width 50)))

(define CANVAS 
  (new canvas% (parent VPANEL) 
       (min-width 300) (min-height 300) (style '(border))
       (paint-callback 
        (lambda: ([obj : (Instance Canvas%)] [evt : Any])   ; c est le canvas et e est l'evenement
          (let ((dc (send obj get-dc)))
            (send dc clear)
            (send dc set-pen BLUE-PEN)          ; le bord du disque
            (send dc set-brush YELLOW-BRUSH)    ; l'interieur du disque
            (send dc draw-ellipse 0 0 299 299)
            (let ((s 0) (N (assert (string->number (send TEXT-FIELD get-value)))))
              (do ((i 0 (+ i 1)))
                ((= i N) (send MSG set-label (number->string (* 4.0 (/ s N)))))
                (let ((x (random 300)) (y (random 300)))
                  (if (< (+ (sqr (- x 150)) (sqr (- y 150))) (sqr 150))
                      (begin (send dc set-pen RED-PEN) (set! s (+ s 1)))
                      (send dc set-pen BLACK-PEN))
                  (send dc draw-point x y)))))))))

(define BUTTON 
  (new button% (parent VPANEL) (label "Go !")
       (callback (lambda (obj evt)
                   (send CANVAS on-paint)))))

(send FRAME show #t)
(for ([i 10])
  (send TEXT-FIELD set-value "50000")
  (send BUTTON command (new control-event% [event-type 'button])))
(send FRAME show #f)

