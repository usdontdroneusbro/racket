#lang racket/base

(require "common.rkt"
         "interface/editor.rkt")

(provide (prefix-out 
           text%-
           (except-out (all-defined-out)
                       text%/c))
         text%/c)

;; text% methods
(define after-change-style/c 
  (->m natural-number/c
       natural-number/c 
       any))

(define after-delete/c 
  (->m exact-nonnegative-integer? 
       exact-nonnegative-integer? 
       any))

(define after-insert/c 
  (->m exact-nonnegative-integer? 
       exact-nonnegative-integer? 
       any))

(define after-merge-snips/c 
  (->m exact-nonnegative-integer? any))

(define after-set-position/c 
  (->m any))

(define after-set-size-constraint/c 
  (->m any))

(define after-split-snip/c 
  (->m exact-nonnegative-integer? any))

(define call-clickback/c 
  (->m exact-nonnegative-integer? 
       exact-nonnegative-integer? 
       any))

(define can-change-style?/c 
  (->m exact-nonnegative-integer? 
       exact-nonnegative-integer? 
       boolean?))

(define can-delete?/c 
  (->m exact-nonnegative-integer? 
       exact-nonnegative-integer? 
       boolean?))

(define can-insert?/c 
  (->m exact-nonnegative-integer? 
       exact-nonnegative-integer? 
       boolean?))

(define can-set-size-constraint?/c 
  (->m boolean?))

(define caret-hidden?/c 
  (->m boolean?))

(define change-style/c
  (->*m ((maybe/c (or/c (is-a?/c style-delta%)
                        (is-a?/c style<%>))))
        ((or/c exact-nonnegative-integer? 'start)
         (or/c exact-nonnegative-integer? 'end)
         any/c)
        any))

(define delete/c 
  (->*m () 
        ((or/c exact-nonnegative-integer? 'start)
         (or/c exact-nonnegative-integer? 'back)
         any/c)
        any))

(define do-copy/c 
  (->m exact-nonnegative-integer? 
       exact-nonnegative-integer? 
       exact-integer? 
       any/c 
       any))

(define do-paste/c 
  (->m exact-nonnegative-integer? 
       exact-integer? 
       any))

(define do-paste-x-selection/c 
  (->m exact-nonnegative-integer? 
       exact-integer? 
       any))

(define erase/c 
  (->m any))

(define find-line/c 
  (->*m (real?) 
        ((maybe/c (box/c any/c))) 
        exact-nonnegative-integer?))

(define find-newline/c 
  (->*m ()
        ((or/c 'forward 'backward)
         (or/c exact-nonnegative-integer? 'start)
         (or/c exact-nonnegative-integer? 'eof))
        (maybe/c exact-nonnegative-integer?)))

(define find-next-non-string-snip/c 
  (->m (maybe/c (is-a?/c snip%)) 
       (maybe/c (is-a?/c snip%))))

(define find-position/c 
  (->*m (real? real?)
        ((maybe/c (box/c any/c))
         (maybe/c (box/c any/c))
         (maybe/c (box/c real?)))
        exact-nonnegative-integer?))

(define find-position-in-line/c 
  (->*m (exact-nonnegative-integer? real?)
        ((maybe/c (box/c any/c))
         (maybe/c (box/c any/c))
         (maybe/c (box/c real?)))
        exact-nonnegative-integer?))

(define find-snip/c 
  (->*m (exact-nonnegative-integer? 
          (or/c 'before-or-none 'before 'after 'after-or-none))
        ((maybe/c (box/c exact-nonnegative-integer?)))
        (maybe/c (is-a?/c snip%))))

(define find-string/c 
  (->*m (string?)
        ((or/c 'forward 'backward)
         (or/c exact-nonnegative-integer? 'start)
         (or/c exact-nonnegative-integer? 'eof)
         any/c
         any/c)
        (maybe/c exact-nonnegative-integer?)))

(define find-string-all/c 
  (->*m (string?)
        ((or/c 'forward 'backward)
         (or/c exact-nonnegative-integer? 'start)
         (or/c exact-nonnegative-integer? 'eof)
         any/c
         any/c)
        (listof exact-nonnegative-integer?)))

(define find-wordbreak/c 
  (->m (maybe/c (box/c exact-nonnegative-integer?))
       (maybe/c (box/c exact-nonnegative-integer?))
       (or/c 'caret 'line 'selection 'user1 'user2)
       any))

(define flash-off/c 
  (->m any))

(define flash-on/c 
  (->*m (exact-nonnegative-integer? exact-nonnegative-integer?)
        (any/c any/c exact-nonnegative-integer?)
        any))

(define get-anchor/c 
  (->m boolean?))

(define get-between-threshold/c 
  (->m (>=/c 0)))

(define get-character/c 
  (->m exact-nonnegative-integer? char?))

(define get-end-position/c 
  (->m exact-nonnegative-integer?))

(define get-file-format/c 
  (->m (or/c 'standard 'text 'text-force-cr)))

(define get-line-spacing/c 
  (->m (>=/c 0)))

(define get-overwrite-mode/c 
  (->m boolean?))

(define get-padding/c 
  (->m (values (>=/c 0) 
               (>=/c 0) 
               (>=/c 0)
               (>=/c 0))))

(define get-position/c 
  (->*m ((maybe/c (box/c exact-nonnegative-integer?)))
        ((maybe/c (box/c exact-nonnegative-integer?)))
        any))

(define get-region-data/c 
  (->m exact-nonnegative-integer? 
       exact-nonnegative-integer?
       (maybe/c (is-a?/c editor-data%))))

(define get-revision-number/c 
  (->m (>=/c 0)))

(define get-snip-position/c 
  (->m (is-a?/c snip%) (maybe/c exact-nonnegative-integer?)))

(define get-snip-position-and-location/c 
  (->*m ((is-a?/c snip%) (maybe/c (box/c exact-nonnegative-integer?)))
        ((maybe/c (box/c real?)) (maybe/c (box/c real?)))
        boolean?))

(define get-start-position/c 
  (->m exact-nonnegative-integer?))

(define get-styles-sticky/c 
  (->m boolean?))

(define get-tabs/c 
  (->*m () 
        ((maybe/c (box/c exact-nonnegative-integer?))
         (maybe/c (box/c real?))
         (maybe/c (box/c any/c)))
        (listof real?)))

(define get-text/c 
  (->*m ()
        (exact-nonnegative-integer?
          (or/c exact-nonnegative-integer? 'eof)
          any/c any/c)
        string?))

(define get-top-line-base/c 
  (->m (>=/c 0)))

(define get-visible-line-range/c 
  (->*m ((maybe/c (box/c exact-nonnegative-integer?))
         (maybe/c (box/c exact-nonnegative-integer?)))
        (any/c)
        any))

(define get-visible-position-range/c 
  (->*m ((maybe/c (box/c exact-nonnegative-integer?))
         (maybe/c (box/c exact-nonnegative-integer?)))
        (any/c)
        any))

(define get-wordbreak-map/c 
  (->m (maybe/c (is-a?/c editor-wordbreak-map%))))

(define hide-caret/c 
  (->m any/c any))

(define last-line/c 
  (->m exact-nonnegative-integer?))

(define last-paragraph/c 
  (->m exact-nonnegative-integer?))

(define last-position/c 
  (->m exact-nonnegative-integer?))

(define line-end-position/c 
  (->*m (exact-nonnegative-integer?) 
        (any/c) 
        exact-nonnegative-integer?))

(define line-length/c 
  (->m exact-nonnegative-integer? 
       exact-nonnegative-integer?))

(define line-location/c 
  (->*m (exact-nonnegative-integer?) (any/c) real?))

(define line-paragraph/c 
  (->m exact-nonnegative-integer? 
       exact-nonnegative-integer?))

(define line-start-position/c 
  (->*m (exact-nonnegative-integer?) 
        (any/c) 
        exact-nonnegative-integer?))

(define move-position/c 
  (->*m ((or/c 'home 'end 'right 'left 'up 'down))
        (any/c (or/c 'simple 'word 'page 'line))
        any))

(define on-change-style/c 
  (->m exact-nonnegative-integer? exact-nonnegative-integer? any))

(define on-default-char/c 
  (->m (is-a?/c key-event%) any))

(define on-default-event/c 
  (->m (is-a?/c mouse-event%) any))

(define on-delete/c 
  (->m exact-nonnegative-integer? exact-nonnegative-integer? any))

(define on-insert/c 
  (->m exact-nonnegative-integer? exact-nonnegative-integer? any))

(define on-new-string-snip/c 
  (->m (is-a?/c string-snip%)))

(define on-new-tab-snip/c 
  (->m (is-a?/c tab-snip%)))

(define on-reflow/c 
  (->m any))

(define on-set-size-constraint/c 
  (->m any))

(define paragraph-end-line/c 
  (->m exact-nonnegative-integer? exact-nonnegative-integer?))

(define paragraph-end-position/c 
  (->*m (exact-nonnegative-integer?) 
        (any/c) 
        exact-nonnegative-integer?))

(define paragraph-start-line/c 
  (->m exact-nonnegative-integer? 
       exact-nonnegative-integer?))

(define paragraph-start-position/c 
  (->*m (exact-nonnegative-integer?) 
        (any/c) 
        exact-nonnegative-integer?))

(define paste-next/c 
  (->m any))

(define position-line/c 
  (->*m (exact-nonnegative-integer?) 
        (any/c) 
        exact-nonnegative-integer?))

(define position-location/c 
  (->*m (exact-nonnegative-integer?)
        ((maybe/c (box/c real?))
         (maybe/c (box/c real?))
         any/c any/c any/c)
        any))

(define position-locations/c 
  (->*m (exact-nonnegative-integer?)
        ((maybe/c (box/c real?))
         (maybe/c (box/c real?))
         (maybe/c (box/c real?))
         (maybe/c (box/c real?))
         any/c any/c)
        any))

(define position-paragraph/c 
  (->*m (exact-nonnegative-integer?) 
        (any/c) 
        exact-nonnegative-integer?))

(define remove-clickback/c 
  (->m exact-nonnegative-integer? exact-nonnegative-integer? any))

(define scroll-to-position/c 
  (->*m (exact-nonnegative-integer?)
        (any/c
         (or/c exact-nonnegative-integer? 'same)
         (or/c 'start 'end 'none))
        boolean?))

(define set-anchor/c 
  (->m any/c any))

(define set-autowrap-bitmap/c 
  (->m (maybe/c (is-a?/c bitmap%)) 
       (maybe/c (is-a?/c bitmap%))))

(define set-between-threshold/c 
  (->m (>=/c 0) any))

(define set-clickback/c 
  (->*m (exact-nonnegative-integer? 
          exact-nonnegative-integer?
          (-> (is-a?/c text%)
              exact-nonnegative-integer? 
              exact-nonnegative-integer?
              any))
        ((maybe/c (is-a?/c style-delta%))
         any/c)
        any))

(define set-file-format/c 
  (->m (or/c 'standard 'text 'text-force-cr) any))

(define set-line-spacing/c 
  (->m (>=/c 0) any))

(define set-overwrite-mode/c 
  (->m any/c any))

(define set-padding/c 
  (->m (>=/c 0)
       (>=/c 0)
       (>=/c 0)
       (>=/c 0)
       any))

(define set-paragraph-alignment/c 
  (->m exact-nonnegative-integer? 
       (or/c 'left 'center 'right) 
       any))

(define set-paragraph-margins/c 
  (->m exact-nonnegative-integer?
       (>=/c 0)
       (>=/c 0)
       (>=/c 0)
       any))

(define set-position/c 
  (->*m (exact-nonnegative-integer?)
        ((or/c exact-nonnegative-integer? 'same)
         any/c any/c
         (or/c 'default 'x 'local))
        any))

(define set-position-bias-scroll/c 
  (->*m ((or/c 'start-only 'start 'none 'end 'end-only)
         exact-nonnegative-integer?)
        ((or/c exact-nonnegative-integer? 'same)
         any/c any/c
         (or/c 'default 'x 'local))
        any))

(define set-region-data/c 
  (->m exact-nonnegative-integer? exact-nonnegative-integer?
       (is-a?/c editor-data%)
       any))

(define set-styles-sticky/c 
  (->m any/c any))

(define set-tabs/c 
  (->*m ((listof real?)) 
        (real? any/c) 
        any))

(define set-wordbreak-func/c 
  (->m (-> (is-a?/c text%) 
           (maybe/c (box/c exact-nonnegative-integer?))
           (maybe/c (box/c exact-nonnegative-integer?))
           symbol?
           any)
       any))

(define set-wordbreak-map/c 
  (->m (maybe/c (is-a?/c editor-wordbreak-map%)) any))

(define split-snip/c 
  (->m exact-nonnegative-integer? any))


;; text% class
(define text%/c
  (and/c
   editor<%>/c
   (class/c
    (init [line-spacing (>=/c 0)]
          [tabstops (listof real?)]
          [auto-wrap any/c])
    (after-change-style              after-change-style/c)
    (after-delete                    after-delete/c)
    (after-insert                    after-insert/c)
    (after-merge-snips               after-merge-snips/c)
    (after-set-position              after-set-position/c)
    (after-set-size-constraint       after-set-size-constraint/c)
    (after-split-snip                after-split-snip/c)
    (inner
      (after-change-style            after-change-style/c)
      (after-delete                  after-delete/c)
      (after-insert                  after-insert/c)
      (after-merge-snips             after-merge-snips/c)
      (after-set-position            after-set-position/c)
      (after-set-size-constraint     after-set-size-constraint/c)
      (after-split-snip              after-split-snip/c))
    (call-clickback                  call-clickback/c)
    (can-change-style?               can-change-style?/c)
    (can-delete?                     can-delete?/c)
    (can-insert?                     can-insert?/c)
    (can-set-size-constraint?        can-set-size-constraint?/c)
    (inner
      (can-change-style?             can-change-style?/c)
      (can-delete?                   can-delete?/c)
      (can-insert?                   can-insert?/c)
      (can-set-size-constraint?      can-set-size-constraint?/c))
    (caret-hidden?                   caret-hidden?/c)
    (change-style                    change-style/c)
    (delete                          delete/c)
    (do-copy                         do-copy/c)
    (do-paste                        do-paste/c)
    (do-paste-x-selection            do-paste-x-selection/c)
    (override
      (do-copy                       do-copy/c)
      (do-paste                      do-paste/c)
      (do-paste-x-selection          do-paste-x-selection/c))
    (erase                           erase/c)
    (find-line                       find-line/c)
    (find-newline                    find-newline/c)
    (find-next-non-string-snip       find-next-non-string-snip/c)
    (find-position                   find-position/c)
    (find-position-in-line           find-position-in-line/c)
    (find-snip                       find-snip/c)
    (find-string                     find-string/c)
    (find-string-all                 find-string-all/c)
    (find-wordbreak                  find-wordbreak/c)
    (flash-off                       flash-off/c)
    (flash-on                        flash-on/c)
    (get-anchor                      get-anchor/c)
    (get-between-threshold           get-between-threshold/c)
    (get-character                   get-character/c)
    (get-end-position                get-end-position/c)
    (get-file-format                 get-file-format/c)
    (get-line-spacing                get-line-spacing/c)
    (get-overwrite-mode              get-overwrite-mode/c)
    (get-padding                     get-padding/c)
    (get-position                    get-position/c)
    (get-region-data                 get-region-data/c)
    (get-revision-number             get-revision-number/c)
    (get-snip-position               get-snip-position/c)
    (get-snip-position-and-location  get-snip-position-and-location/c)
    (get-start-position              get-start-position/c)
    (get-styles-sticky               get-styles-sticky/c)
    (get-tabs                        get-tabs/c)
    (get-text                        get-text/c)
    (get-top-line-base               get-top-line-base/c)
    (get-visible-line-range          get-visible-line-range/c)
    (get-visible-position-range      get-visible-position-range/c)
    (get-wordbreak-map               get-wordbreak-map/c)
    (hide-caret                      hide-caret/c)
    ; Need translucent contract and case->m
    ;  with sub ->*m contracts
    ;(insert                          text%-insert/c)
    (last-line                       last-line/c)
    (last-paragraph                  last-paragraph/c)
    (last-position                   last-position/c)
    (line-end-position               line-end-position/c)
    (line-length                     line-length/c)
    (line-location                   line-location/c)
    (line-paragraph                  line-paragraph/c)
    (line-start-position             line-start-position/c)
    (move-position                   move-position/c)
    (on-change-style                 on-change-style/c)
    (on-default-char                 on-default-char/c)
    (on-default-event                on-default-event/c)
    (on-delete                       on-delete/c)
    (on-insert                       on-insert/c)
    (on-new-string-snip              on-new-string-snip/c)
    (on-new-tab-snip                 on-new-tab-snip/c)
    (on-reflow                       on-reflow/c)
    (on-set-size-constraint          on-set-size-constraint/c)
    (override
      (on-default-char               on-default-char/c)
      (on-default-event              on-default-event/c)
      (on-new-string-snip            on-new-string-snip/c)
      (on-new-tab-snip               on-new-tab-snip/c))
    (inner
      (on-change-style               on-change-style/c)
      (on-delete                     on-delete/c)
      (on-insert                     on-insert/c)
      (on-reflow                     on-reflow/c)
      (on-set-size-constraint        on-set-size-constraint/c))
    (paragraph-end-line              paragraph-end-line/c)
    (paragraph-end-position          paragraph-end-position/c)
    (paragraph-start-line            paragraph-start-line/c)
    (paragraph-start-position        paragraph-start-position/c)
    (paste-next                      paste-next/c)
    (position-line                   position-line/c)
    (position-location               position-location/c)
    (position-locations              position-locations/c)
    (position-paragraph              position-paragraph/c)
    (remove-clickback                remove-clickback/c)
    (scroll-to-position              scroll-to-position/c)
    (set-anchor                      set-anchor/c)
    (set-autowrap-bitmap             set-autowrap-bitmap/c)
    (set-between-threshold           set-between-threshold/c)
    (set-clickback                   set-clickback/c)
    (set-file-format                 set-file-format/c)
    (set-line-spacing                set-line-spacing/c)
    (set-overwrite-mode              set-overwrite-mode/c)
    (set-padding                     set-padding/c)
    (set-paragraph-alignment         set-paragraph-alignment/c)
    (set-paragraph-margins           set-paragraph-margins/c)
    (set-position                    set-position/c)
    (set-position-bias-scroll        set-position-bias-scroll/c)
    (set-region-data                 set-region-data/c)
    (override
      (set-region-data               set-region-data/c))
    (set-styles-sticky               set-styles-sticky/c)
    (set-tabs                        set-tabs/c)
    (set-wordbreak-func              set-wordbreak-func/c)
    (set-wordbreak-map               set-wordbreak-map/c)
    (split-snip                      split-snip/c))))
