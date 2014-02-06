#lang racket/base

(require racket/class
         racket/contract
         racket/draw
         "../private/mred.rkt")

(provide (all-defined-out))

;; ============= Contract utilities =============

(define nonnegative-real?/c 
  (and/c real? (not/c negative?)))

(define (maybe/c contract) 
  (or/c contract #f))

(define dimension-non-zero/c (integer-in 0 10000))
(define dimension/c (integer-in -10000 10000))

(define min-width/c  (or/c dimension-non-zero/c #f))
(define min-height/c (or/c dimension-non-zero/c #f))

(define label/c (or/c label-string? (is-a?/c bitmap%)))
(define parent/c (or/c (is-a?/c frame%) (is-a?/c dialog%)
                       (is-a?/c panel%) (is-a?/c pane%)))
(define (callback/c class)
  ((is-a?/c class) (is-a?/c control-event%) . -> . any))

(define edit-operation/c
  (one-of/c 'undo 'redo 'clear 'cut 'copy 'paste
            'kill 'select-all 'insert-text-box
            'insert-pasteboard-box 'insert-image))

(define format/c
  (one-of/c 'guess 'same 'copy 'standard
            'text 'text-force-cr))

(define caret-threshold/c
  (one-of/c 'no-caret 'show-inactive-caret 'show-caret))

(define image-type/c
  (one-of/c 'unknown 'unknown/mask 'unknown/alpha
            'gif 'gif/mask 'gif/alpha
            'jpeg 'png 'png/mask 'png/alpha
            'xbm 'xpm 'bmp 'pict))

(define event-type/c
  (one-of/c 'button 'check-box 'choice
            'list-box 'list-box-dclick 'text-field
            'text-field-enter 'menu 'slider 'radio-box
            'menu-popdown 'menu-popdown-none 'tab-panel))


