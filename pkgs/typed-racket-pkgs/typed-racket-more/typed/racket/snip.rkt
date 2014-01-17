#lang s-exp typed-racket/base-env/extra-env-lang

;; This module provides a base type environment including
;; racket/snip bindings

(begin
 (require racket/snip
          "snip-contracted.rkt"
          (for-syntax (only-in (rep type-rep) make-Instance))
          "gui-types.rkt"
          (for-syntax (submod "gui-types.rkt" #%type-decl)))

 (provide (all-from-out "snip-contracted.rkt")
          (all-from-out racket/snip)
          Snip%
          Snip-Admin%
          Snip-Class%
          String-Snip%
          Style<%>
          Style-Delta%
          Style-List%))

