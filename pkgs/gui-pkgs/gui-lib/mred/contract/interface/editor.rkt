#lang racket/base

(require "common.rkt")

(provide (prefix-out
           editor<%>-
           (except-out (all-defined-out)
                       editor<%>/c))
         editor<%>/c)

;; methods
(define add-canvas/c (->m (is-a?/c editor-canvas%) any))
(define add-undo/c   (->m (-> any) any))

(define adjust-cursor/c
  (->m (is-a?/c mouse-event%) (or/c (is-a?/c cursor%) #f)))

(define after-edit-sequence/c (->m any))
(define after-load-file/c     (->m any/c any))
(define after-save-file/c     (->m any/c any))

(define auto-wrap/c
  (case->m (-> boolean?)
           (-> any/c any)))

(define begin-edit-sequence/c
  (->*m () (any/c any/c) any))

(define begin-write-header-footer-to-file/c
  (->m (is-a?/c editor-stream-out%)
       string?
       (box/c exact-integer?)
       any))

(define blink-caret/c (->m any))
(define can-do-edit-operation?/c
  (->*m (edit-operation/c) (any/c) boolean?))

(define can-load-file?/c  (->m path? format/c boolean?))
(define can-save-file?/c  (->m path? format/c boolean?))

(define clear/c       (->m any))
(define clear-undos/c (->m any))

;; Need translucent contract
;(copy
; (->*m () (any/c exact-integer?) any))

(define copy-self/c
  (->m (or/c (is-a?/c text%) (is-a?/c pasteboard%))))
(define copy-self-to/c
  (->m (or/c (is-a?/c text%) (is-a?/c pasteboard%)) any))

(define cut/c
  (->*m () (any/c exact-integer?) any))

(define dc-location-to-editor-location/c
  (->m real? real? (values real? real?)))

(define default-style-name/c
  (->m string?))

(define do-edit-operation/c
  (->*m (edit-operation/c) (any/c exact-integer?) any))

(define editor-location-to-dc-location/c
  (->m real? real? (values real? real?)))

(define end-edit-sequence/c (->m any))

(define end-write-header-footer-to-file/c
  (->m (is-a?/c editor-stream-out%) exact-integer? any))

(define find-first-snip/c
  (->m (or/c (is-a?/c snip%) #f)))

(define find-scroll-line/c
  (->m real? exact-nonnegative-integer?))

(define get-active-canvas/c
  (->m (or/c (is-a?/c editor-canvas%) #f)))

(define get-admin/c
  (->m (or/c (is-a?/c editor-admin%) #f)))

(define get-canvas/c
  (->m (or/c (is-a?/c editor-canvas%) #f)))

(define get-canvases/c
  (->m (listof (is-a?/c editor-canvas%))))

(define get-dc/c
  (->m (or/c (is-a?/c dc<%>) #f)))

(define get-descent/c
  (->m (and/c real? (not/c negative?))))

(define get-extent/c
  (->m (or/c (box/c (and/c real? (not/c negative?))) #f)
       (or/c (box/c (and/c real? (not/c negative?))) #f)
       any))

(define get-file/c
  (->m (or/c path? #f) (or/c path-string? #f)))

(define get-filename/c
  (->*m () ((box/c (or/c any/c #f))) (or/c path-string? #f)))

(define get-flattened-text/c
  (->m string?))

(define get-focus-snip/c
  (->m (or/c (is-a?/c snip%) #f)))

(define get-inactive-caret-threshold/c
  (->m caret-threshold/c))

(define get-keymap/c
  (->m (or/c (is-a?/c keymap%) #f)))

(define get-load-overwrites-styles/c
  (->m boolean?))

(define get-max-height/c
  (->m (or/c nonnegative-real?/c 'none)))

(define get-max-undo-history/c
  (->m (or/c (integer-in 0 100000) 'forever)))

(define get-max-view-size/c
  (->m (values real? real?)))

(define get-max-width/c
  (->m (or/c (and/c real? (not/c negative?)) 'none)))

(define get-min-height/c
  (->m (or/c (and/c real? (not/c negative?)) 'none)))

(define get-min-width/c
  (->m (or/c (and/c real? (not/c negative?)) 'none)))

(define get-paste-text-only/c
  (->m boolean?))

(define get-snip-data/c
  (->m (is-a?/c snip%) (or/c (is-a?/c editor-data%) #f)))

(define get-snip-location/c
  (->*m ((is-a?/c snip%))
        ((or/c (box/c real?) #f) (or/c (box/c real?) #f) any/c)
        boolean?))

(define get-space/c
  (->m (and/c real? (not/c negative?))))

(define get-style-list/c (->m (is-a?/c style-list%)))

(define get-view-size/c
  (->m (or/c (box/c (and/c real? (not/c negative?))) #f)
       (or/c (box/c (and/c real? (not/c negative?))) #f)
       any))

(define global-to-local/c
  (->m (or/c (box/c real?) #f) (or/c (box/c real?) #f) any))

(define in-edit-sequence?/c  (->m boolean?))

;; Need translucent contract:
;(insert
; (->m (is-a?/c snip%) any))

(define insert-box/c
  (->*m () ((or/c 'text 'pasteboard)) any))

(define insert-file/c
  (->*m (path-string?) (format/c any/c) boolean?))

(define insert-image/c
  (->*m () ((or/c path-string? #f) image-type/c any/c any/c) any))

(define insert-port/c
  (->*m (input-port?)
        ((format/c any/c))
        (one-of/c 'standard 'text 'text-force-cr)))

(define invalidate-bitmap-cache/c
  (->*m ()
        (real?
          real?
          (or/c real? (not/c negative?) 'end 'display-end)
          (or/c real? (not/c negative?) 'end 'display-end))
        any))

(define is-locked?/c    (->m boolean?))
(define is-modified?/c  (->m boolean?))
(define is-printing?/c  (->m boolean?))

(define kill/c
  (->*m () (exact-integer?) any))

(define load-file/c
  (->*m () ((or/c path-string? #f) format/c any/c) boolean?))

(define local-to-global/c
  (->m (or/c (box/c real?) #f) (or/c (box/c real?) #f) any))

(define locations-computed?/c
  (->m boolean?))

(define lock/c
  (->m any/c any))

(define locked-for-flow?/c   (->m boolean?))
(define locked-for-read?/c   (->m boolean?))
(define locked-for-write?/c  (->m boolean?))

(define needs-update/c
  (->m (is-a?/c snip%)
       real?
       real?
       (and/c real? (not/c negative?))
       (and/c real? (not/c negative?))
       any))

(define num-scroll-lines/c (->m exact-nonnegative-integer?))

(define on-change/c (->m any))
(define on-char/c   (->m (is-a?/c key-event%) any))
(define on-default-char/c (->m (is-a?/c key-event%) any))
(define on-default-event/c (->m (is-a?/c mouse-event%) any))
(define on-display-size/c (->m any))
(define on-display-size-when-ready/c (->m any))
(define on-edit-sequence/c (->m any))
(define on-event/c (->m (is-a?/c mouse-event%) any))
(define on-focus/c (->m any/c any))
(define on-load-file/c (->m path? format/c any))
(define on-local-char/c (->m (is-a?/c key-event%) any))
(define on-local-event/c (->m (is-a?/c mouse-event%) any))
(define on-new-box/c (->m (or/c 'text 'pasteboard) (is-a?/c snip%)))
(define on-new-image-snip/c (->m path? image-type/c any/c any/c (is-a?/c image-snip%)))

(define on-paint/c
  (->m any/c (is-a?/c dc<%>) real? real? real? real? real? real?
       (or/c caret-threshold/c
             (cons/c exact-nonnegative-integer? exact-nonnegative-integer?))
       any))

(define on-save-file/c (->m path? format/c any))
(define on-snip-modified/c (->m (is-a?/c snip%) any/c any))

(define own-caret/c
  (->m any/c any))

(define paste/c
  (->*m () (exact-integer?) any))
(define paste-x-selection/c
  (->*m () (exact-integer?) any))

(define print/c
  (->*m ()
        (any/c any/c (or/c 'standard 'postscript)
               (or/c (or/c (is-a?/c frame%) (is-a?/c dialog%)) #f)
               any/c any/c)
        any))

(define print-to-dc/c
  (->*m ((is-a?/c dc<%>)) (exact-integer?) any))

(define put-file/c
  (->m (maybe/c path?) (maybe/c path?) (maybe/c path-string?)))

(define read-footer-from-file/c
  (->m (is-a?/c editor-stream-in%) string? boolean?))

(define read-from-file/c
  (->*m ((is-a?/c editor-stream-in%)) (any/c) boolean?))

(define read-header-from-file/c
  (->m (is-a?/c editor-stream-in%) string? boolean?))

(define redo/c (->m any))

(define refresh/c
  (->m real? real? nonnegative-real?/c nonnegative-real?/c
       (or/c caret-threshold/c
             (cons/c exact-nonnegative-integer? exact-nonnegative-integer?))
       (maybe/c (is-a?/c color%))
       any))

(define refresh-delayed?/c  (->m boolean?))

(define release-snip/c (->m (is-a?/c snip%) boolean?))

(define remove-canvas/c
  (->m (is-a?/c editor-canvas%) any))

(define resized/c
  (->m (is-a?/c snip%) any/c any))

(define save-file/c
  (->*m () ((maybe/c path-string?) format/c any/c) boolean?))

(define save-port/c
  (->*m (output-port?) (format/c any/c) boolean?))

(define scroll-editor-to/c
  (->m real? real?
       nonnegative-real?/c nonnegative-real?/c
       any/c
       (or/c 'start 'end 'none)
       boolean?))

(define scroll-line-location/c
  (->m exact-nonnegative-integer?
       nonnegative-real?/c))

(define scroll-to/c
  (->*m ((is-a?/c snip%)
         real? real?
         nonnegative-real?/c nonnegative-real?/c
         any/c)
        ((or/c 'start 'end 'none))
        boolean?))

(define select-all/c (->m any))

(define set-active-canvas/c
  (->m (is-a?/c editor-canvas%) any))

(define set-admin/c
  (->m (maybe/c (is-a?/c editor-admin%)) any))

(define set-caret-owner/c
  (->*m ((maybe/c (is-a?/c snip%)))
        ((or/c 'immediate 'display 'global))
        any))

(define set-cursor/c
  (->*m ((maybe/c (is-a?/c cursor%))) (any/c) any))

(define set-filename/c
  (->*m ((maybe/c path-string?)) (any/c) any))

(define set-inactive-caret-threshold/c
  (->m caret-threshold/c any))

(define set-keymap/c
  (->*m () ((maybe/c (is-a?/c keymap%))) any))

(define set-load-overwrites-styles/c
  (->m any/c any))

(define set-max-height/c
  (->m (or/c nonnegative-real?/c 'none) any))

(define set-max-undo-history/c
  (->m (or/c exact-nonnegative-integer? 'forever) any))

(define set-max-width/c
  (->m (or/c nonnegative-real?/c 'none) any))

(define set-min-height/c
  (->m (or/c nonnegative-real?/c 'none) any))

(define set-min-width/c
  (->m (or/c nonnegative-real?/c 'none) any))

(define set-modified/c
  (->m any/c any))

(define set-paste-text-only/c
  (->m any/c any))

(define set-snip-data/c
  (->m (is-a?/c snip%) (is-a?/c editor-data%) any))

(define set-style-list/c
  (->m (is-a?/c style-list%) any))

(define size-cache-invalid/c (->m any))

(define style-has-changed/c
  (->m (maybe/c (is-a?/c style<%>)) any))

(define undo/c (->m any))

(define use-file-text-mode/c
  (case->m (-> boolean?)
           (-> any/c any)))

(define write-footers-to-file/c
  (->m (is-a?/c editor-stream-out%) boolean?))

(define write-headers-to-file/c
  (->m (is-a?/c editor-stream-out%) boolean?))

(define write-to-file/c
  (->m (is-a?/c editor-stream-out%) boolean?))


;; editor<%> interface
(define editor<%>/c
  (class/c
   (add-canvas                          add-canvas/c)
   (add-undo                            add-undo/c)
   (adjust-cursor                       adjust-cursor/c)
   (override
     (adjust-cursor                     adjust-cursor/c))
   (after-edit-sequence                 after-edit-sequence/c)
   (after-load-file                     after-load-file/c)
   (after-save-file                     after-save-file/c)
   (inner
     (after-edit-sequence               after-edit-sequence/c)
     (after-load-file                   after-load-file/c)
     (after-save-file                   after-save-file/c))
   (auto-wrap                           auto-wrap/c)
   (begin-edit-sequence                 begin-edit-sequence/c)
   (override
     (begin-edit-sequence               begin-edit-sequence/c))
   (begin-write-header-footer-to-file   begin-write-header-footer-to-file/c)
   (blink-caret                         blink-caret/c)
   (override
     (blink-caret                       blink-caret/c))
   (can-do-edit-operation?              can-do-edit-operation?/c)
   (can-load-file?                      can-load-file?/c)
   (can-save-file?                      can-save-file?/c)
   (override
     (can-do-edit-operation?            can-do-edit-operation?/c))
   (inner
     (can-load-file?                    can-load-file?/c)
     (can-save-file?                    can-save-file?/c))
   (clear                               clear/c)
   (clear-undos                         clear-undos/c)
   (copy-self                           copy-self/c)
   (copy-self-to                        copy-self-to/c)
   (cut                                 cut/c)
   (dc-location-to-editor-location      dc-location-to-editor-location/c)
   (default-style-name                  default-style-name/c)
   (do-edit-operation                   do-edit-operation/c)
   (editor-location-to-dc-location      editor-location-to-dc-location/c)
   (end-edit-sequence                   end-edit-sequence/c)
   (end-write-header-footer-to-file     end-write-header-footer-to-file/c)
   (find-first-snip                     find-first-snip/c)
   (find-scroll-line                    find-scroll-line/c)
   (get-active-canvas                   get-active-canvas/c)
   (get-admin                           get-admin/c)
   (get-canvas                          get-canvas/c)
   (get-canvases                        get-canvases/c)
   (get-dc                              get-dc/c)
   (get-descent                         get-descent/c)
   (get-extent                          get-extent/c)
   (get-file                            get-file/c)
   (override
     (get-file                          get-file/c))
   (get-filename                        get-filename/c)
   (get-flattened-text                  get-flattened-text/c)
   (get-focus-snip                      get-focus-snip/c)
   (get-inactive-caret-threshold        get-inactive-caret-threshold/c)
   (get-keymap                          get-keymap/c)
   (get-load-overwrites-styles          get-load-overwrites-styles/c)
   (get-max-height                      get-max-height/c)
   (get-max-undo-history                get-max-undo-history/c)
   (get-max-view-size                   get-max-view-size/c)
   (get-max-width                       get-max-width/c)
   (get-min-height                      get-min-height/c)
   (get-min-width                       get-min-width/c)
   (get-paste-text-only                 get-paste-text-only/c)
   (get-snip-data                       get-snip-data/c)
   (override
     (get-snip-data                     get-snip-data/c))
   (get-snip-location                   get-snip-location/c)
   (get-space                           get-space/c)
   (get-style-list                      get-style-list/c)
   (get-view-size                       get-view-size/c)
   (global-to-local                     global-to-local/c)
   (in-edit-sequence?                   in-edit-sequence?/c)
   (insert-box                          insert-box/c)
   (insert-file                         insert-file/c)
   (insert-image                        insert-image/c)
   (insert-port                         insert-port/c)
   (invalidate-bitmap-cache             invalidate-bitmap-cache/c)
   (is-locked?                          is-locked?/c)
   (is-modified?                        is-modified?/c)
   (is-printing?                        is-printing?/c)
   (kill                                kill/c)
   (load-file                           load-file/c)
   (local-to-global                     local-to-global/c)
   (locations-computed?                 locations-computed?/c)
   (lock                                lock/c)
   (locked-for-flow?                    locked-for-flow?/c)
   (locked-for-read?                    locked-for-read?/c)
   (locked-for-write?                   locked-for-write?/c)
   (needs-update                        needs-update/c)
   (num-scroll-lines                    num-scroll-lines/c)
   (on-change                           on-change/c)
   (on-char                             on-char/c)
   (on-default-char                     on-default-char/c)
   (on-default-event                    on-default-event/c)
   (on-display-size                     on-display-size/c)
   (on-display-size-when-ready          on-display-size-when-ready/c)
   (on-edit-sequence                    on-edit-sequence/c)
   (on-event                            on-event/c)
   (on-focus                            on-focus/c)
   (on-load-file                        on-load-file/c)
   (on-local-char                       on-local-char/c)
   (on-local-event                      on-local-event/c)
   (on-new-box                          on-new-box/c)
   (on-new-image-snip                   on-new-image-snip/c)
   (on-paint                            on-paint/c)
   (on-save-file                        on-save-file/c)
   (on-snip-modified                    on-snip-modified/c)
   (override
     (on-char                           on-char/c)
     (on-default-char                   on-default-char/c)
     (on-default-event                  on-default-event/c)
     (on-display-size-when-ready        on-display-size-when-ready/c)
     (on-event                          on-event/c)
     (on-focus                          on-focus/c)
     (on-local-char                     on-local-char/c)
     (on-local-event                    on-local-event/c)
     (on-new-box                        on-new-box/c)
     (on-new-image-snip                 on-new-image-snip/c)
     (on-paint                          on-paint/c))
   (inner
     (on-change                         on-change/c)
     (on-display-size                   on-display-size/c)
     (on-edit-sequence                  on-edit-sequence/c)
     (on-load-file                      on-load-file/c)
     (on-save-file                      on-save-file/c)
     (on-snip-modified                  on-snip-modified/c))
   (own-caret                           own-caret/c)
   (override
     (own-caret                         own-caret/c))
   (paste                               paste/c)
   (paste-x-selection                   paste-x-selection/c)
   (print                               print/c)
   (print-to-dc                         print-to-dc/c)
   (put-file                            put-file/c)
   (override
     (put-file                          put-file/c))
   (read-footer-from-file               read-footer-from-file/c)
   (read-from-file                      read-from-file/c)
   (read-header-from-file               read-header-from-file/c)
   (redo                                redo/c)
   (refresh                             refresh/c)
   (refresh-delayed?                    refresh-delayed?/c)
   (release-snip                        release-snip/c)
   (remove-canvas                       remove-canvas/c)
   (resized                             resized/c)
   (save-file                           save-file/c)
   (save-port                           save-port/c)
   (scroll-editor-to                    scroll-editor-to/c)
   (scroll-line-location                scroll-line-location/c)
   (scroll-to                           scroll-to/c)
   (select-all                          select-all/c)
   (set-active-canvas                   set-active-canvas/c)
   (set-admin                           set-admin/c)
   (set-caret-owner                     set-caret-owner/c)
   (set-cursor                          set-cursor/c)
   (set-filename                        set-filename/c)
   (set-inactive-caret-threshold        set-inactive-caret-threshold/c)
   (set-keymap                          set-keymap/c)
   (set-load-overwrites-styles          set-load-overwrites-styles/c)
   (set-max-height                      set-max-height/c)
   (set-max-undo-history                set-max-undo-history/c)
   (set-max-width                       set-max-width/c)
   (set-min-height                      set-min-height/c)
   (set-min-width                       set-min-width/c)
   (set-modified                        set-modified/c)
   (set-paste-text-only                 set-paste-text-only/c)
   (set-snip-data                       set-snip-data/c)
   (set-style-list                      set-style-list/c)
   (size-cache-invalid                  size-cache-invalid/c)
   (style-has-changed                   style-has-changed/c)
   (undo                                undo/c)
   (use-file-text-mode                  use-file-text-mode/c)
   (write-footers-to-file               write-footers-to-file/c)
   (write-headers-to-file               write-headers-to-file/c)
   (write-to-file                       write-to-file/c)))

