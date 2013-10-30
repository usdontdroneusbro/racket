#lang racket/base

;; This module defines a #lang for use in defining extra base
;; type environments that will only be included on a `require`
;; (unlike the monolithic base type environment in base-env.rkt)

(require "../utils/utils.rkt"
         (for-syntax racket/base syntax/parse)
         (private parse-type))

(provide (rename-out [-#%module-begin #%module-begin])
         require
         (except-out (all-from-out racket/base) #%module-begin))

;; Also see env-lang.rkt, where some of this code was stolen from
(define-syntax (-#%module-begin stx)
  (define-syntax-class clause
    #:description "[id type]"
    (pattern [id:identifier ty]))
  (syntax-parse stx #:literals (require provide begin)
    [(mb (~optional
          (~and extra (~or (begin . _)
                           (require . args)
                           (provide . args))))
         ~! :clause ...)
     #'(#%plain-module-begin
        extra
        (require (for-syntax typed-racket/env/env-req))
        (begin-for-syntax
         (module* #%type-decl #f
           (#%plain-module-begin ;; avoid top-level printing and config
            (require typed-racket/types/numeric-tower typed-racket/env/type-name-env
                     typed-racket/env/global-env typed-racket/env/type-alias-env
                     typed-racket/types/struct-table typed-racket/types/abbrev
                     typed-racket/private/parse-type
                     (rename-in racket/private/sort [sort raw-sort]))
            ;; FIXME: add a switch to turn contracts on for testing
            (register-type (quote-syntax id) (parse-type (quote-syntax ty)))
            ...)))
        (begin-for-syntax (add-mod! (variable-reference->module-path-index
                                     (#%variable-reference))))
        (provide id ...))]
    [(mb . rest)
     #'(mb (begin) . rest)]))

