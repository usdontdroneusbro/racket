#lang racket/base

(require "common.rkt"
         racket/snip/private/contract)

(provide (prefix-out 
           editor-snip%-
           (except-out (all-defined-out)
                       editor-snip%/c))
          editor-snip%/c)

;; methods
(define border-visible?/c 
  (->m boolean?))

(define get-align-top-line/c 
  (->m boolean?))

(define get-editor/c 
  (->m (or/c (or/c (is-a?/c text%) (is-a?/c pasteboard%)) false/c)))

(define get-inset/c 
  (->m (box/c exact-nonnegative-integer?)
       (box/c exact-nonnegative-integer?)
       (box/c exact-nonnegative-integer?)
       (box/c exact-nonnegative-integer?)
       any))

(define get-margin/c 
  (->m (box/c exact-nonnegative-integer?)
       (box/c exact-nonnegative-integer?)
       (box/c exact-nonnegative-integer?)
       (box/c exact-nonnegative-integer?)
       any))

(define get-max-height/c 
  (->m (or/c (and/c real? (not/c negative?)) 
             (one-of/c 'none))))

(define get-max-width/c 
  (->m (or/c (and/c real? (not/c negative?)) 
             (one-of/c 'none))))

(define get-min-height/c 
  (->m (or/c (and/c real? (not/c negative?)) 
             (one-of/c 'none))))

(define get-min-width/c 
  (->m (or/c (and/c real? (not/c negative?)) 
             (one-of/c 'none))))

(define get-tight-text-fit/c 
  (->m boolean?))

(define resize/c 
  (->m (and/c real? (not/c negative?))
       (and/c real? (not/c negative?))
       boolean?))

(define set-align-top-line/c 
  (->m any/c any))

(define set-editor/c 
  (->m (or/c (or/c (is-a?/c text%) (is-a?/c pasteboard%)) #f)
       any))

(define set-inset/c 
  (->m exact-nonnegative-integer? exact-nonnegative-integer?
       exact-nonnegative-integer? exact-nonnegative-integer?
       any))

(define set-max-height/c 
  (->m (or/c (and/c real? (not/c negative?)) 
             (one-of/c 'none)) 
       any))

(define set-max-width/c 
  (->m (or/c (and/c real? (not/c negative?)) 
             (one-of/c 'none)) 
       any))

(define set-min-height/c 
  (->m (or/c (and/c real? (not/c negative?)) 
             (one-of/c 'none)) 
       any))

(define set-min-width/c 
  (->m (or/c (and/c real? (not/c negative?)) 
             (one-of/c 'none)) 
       any))

(define set-tight-text-fit/c 
  (->m any/c any))

(define show-border/c 
  (->m any/c any))

(define style-background-used?/c 
  (->m boolean?))

(define use-style-background/c 
  (->m any/c any))

;; class
(define editor-snip%/c
  (and/c snip%/c
         (class/c
           (init [editor (or/c (is-a?/c editor<%>) false/c)]
                 [with-border? any/c]
                 [left-margin exact-nonnegative-integer?]
                 [top-margin exact-nonnegative-integer?]
                 [right-margin exact-nonnegative-integer?]
                 [bottom-margin exact-nonnegative-integer?]
                 [left-inset exact-nonnegative-integer?]
                 [top-inset exact-nonnegative-integer?]
                 [right-inset exact-nonnegative-integer?]
                 [bottom-inset exact-nonnegative-integer?]
                 [min-width (or/c (and/c real? (not/c negative?)) (one-of/c 'none))]
                 [max-width (or/c (and/c real? (not/c negative?)) (one-of/c 'none))]
                 [min-height (or/c (and/c real? (not/c negative?)) (one-of/c 'none))]
                 [max-height (or/c (and/c real? (not/c negative?)) (one-of/c 'none))])
           (border-visible?         border-visible?/c)
           (get-align-top-line      get-align-top-line/c)
           (get-editor              get-editor/c)
           (get-inset               get-inset/c)
           (get-margin              get-margin/c)
           (get-max-height          get-max-height/c)
           (get-max-width           get-max-width/c)
           (get-min-height          get-min-height/c)
           (get-min-width           get-min-width/c)
           (get-tight-text-fit      get-tight-text-fit/c)
           (resize                  resize/c)
           (set-align-top-line      set-align-top-line/c)
           (set-editor              set-editor/c)
           (set-inset               set-inset/c)
           (set-max-height          set-max-height/c)
           (set-max-width           set-max-width/c)
           (set-min-height          set-min-height/c)
           (set-min-width           set-min-width/c)
           (set-tight-text-fit      set-tight-text-fit/c)
           (show-border             show-border/c)
           (style-background-used?  style-background-used?/c)
           (use-style-background    use-style-background/c))))
