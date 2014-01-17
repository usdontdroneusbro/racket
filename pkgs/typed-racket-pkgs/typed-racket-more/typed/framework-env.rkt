#lang s-exp typed-racket/base-env/extra-env-lang

;; This module provides the GUI framework with trusted types

(require framework
         (for-syntax (only-in (rep type-rep)
                              make-Instance))
         "racket/gui-types.rkt"
         (for-syntax (submod "racket/gui-types.rkt" #%type-decl))
         "framework-types.rkt" 
         (for-syntax (submod "framework-types.rkt" #%type-decl)))

;; 8 Canvas
[canvas:basic% (parse-type #'Canvas:Basic%)]
[canvas:wide-snip-mixin (parse-type #'Canvas:Wide-Snip-Mixin)]

;; 14 Frame
[frame:basic-mixin (parse-type #'Frame:Basic-Mixin)]
[frame:focus-table-mixin (parse-type #'Frame:Focus-Table-Mixin)]
[frame:size-pref-mixin (parse-type #'Frame:Size-Pref-Mixin)]
[frame:register-group-mixin (parse-type #'Frame:Register-Group-Mixin)]
[frame:status-line-mixin (parse-type #'Frame:Status-Line-Mixin)]
;; 28
[racket:text% (parse-type #'Text:Basic<%>)]

