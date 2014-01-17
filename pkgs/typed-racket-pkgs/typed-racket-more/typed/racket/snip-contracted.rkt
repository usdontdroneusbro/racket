#lang s-exp typed-racket/base-env/extra-env-lang

;; This module provides base types for contracted racket/snip
;; identifiers that need special handling

(require racket/snip/private/snip
         racket/snip/private/snip-admin
         racket/snip/private/style
         (for-syntax (only-in (rep type-rep) make-Instance))
         "gui-types.rkt"
         (for-syntax (submod "gui-types.rkt" #%type-decl)))

#:no-provide

[snip% (parse-type #'Snip%)
       #:template-id-from 'racket/snip/private/snip]
[snip-admin% (parse-type #'Snip-Admin%)
             #:template-id-from 'racket/snip/private/snip-admin]
[snip-class% (parse-type #'Snip-Class%)
             #:template-id-from 'racket/snip/private/snip]
[string-snip% (parse-type #'String-Snip%)
              #:template-id-from 'racket/snip/private/snip]
[style-delta% (parse-type #'Style-Delta%)
              #:template-id-from 'racket/snip/private/style]
[style-list% (parse-type #'Style-List%)
             #:template-id-from 'racket/snip/private/style]

