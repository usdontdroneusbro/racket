#lang racket/base

(require "area.rkt"
         "common.rkt")

(provide (prefix-out area-container<%>-
                     (except-out (all-defined-out) 
                                 area-container<%>/c))
         area-container<%>/c)

;; methods
(define add-child/c 
  (->m (is-a?/c subwindow<%>) any))

(define after-new-child/c 
  (->m (is-a?/c subarea<%>) any))

(define begin-container-sequence/c (->m any))

(define border/c
  (case->m
    (-> (integer-in 0 1000))
    (-> (integer-in 0 1000) any)))

(define change-children/c
  (->m
    ((listof (is-a?/c subarea<%>))
     . -> . (listof (is-a?/c subarea<%>)))
    any))

(define container-flow-modified/c (->m any))

(define container-size/c
  (->m (listof (list/c dimension-non-zero/c
                       dimension-non-zero/c
                       any/c
                       any/c))
       (values dimension-non-zero/c dimension-non-zero/c)))

(define delete-child/c (->m (is-a?/c subwindow<%>) any))
(define end-container-sequence/c (->m any))

(define get-alignment/c
  (->m (values (symbols 'right 'center 'left)
               (symbols 'bottom 'center 'top))))

(define get-children/c (->m (listof (is-a?/c subarea<%>))))

(define place-children/c
  (->m
    (listof (list/c dimension-non-zero/c
                    dimension-non-zero/c
                    any/c
                    any/c))
    dimension-non-zero/c
    dimension-non-zero/c
    (listof (list/c dimension-non-zero/c
                    dimension-non-zero/c
                    dimension-non-zero/c
                    dimension-non-zero/c))))

(define reflow-container/c (->m any))

(define set-alignment/c
  (->m
    (symbols 'right 'center 'left)
    (symbols 'bottom 'center 'top)
    any))

(define spacing/c
  (case->m
    (-> (integer-in 0 1000))
    (-> (integer-in 0 1000) any)))

;; interface
(define area-container<%>/c
  (and/c
   area<%>/c
   (class/c
    (add-child                 add-child/c)
    (after-new-child           after-new-child/c)
    (override
      (after-new-child         after-new-child/c))
    (begin-container-sequence  begin-container-sequence/c)
    (border                    border/c)
    (change-children           change-children/c)
    (container-flow-modified   container-flow-modified/c)
    (container-size            container-size/c)
    (delete-child              delete-child/c)
    (end-container-sequence    end-container-sequence/c)
    (get-alignment             get-alignment/c)
    (get-children              get-children/c)
    (place-children            place-children/c)
    (reflow-container          reflow-container/c)
    (set-alignment             set-alignment/c)
    (spacing                   spacing/c))))
