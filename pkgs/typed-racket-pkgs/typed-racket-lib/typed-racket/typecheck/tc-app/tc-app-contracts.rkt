#lang racket/unit

;; This module provides custom type-checking rules for the expansion
;; of contracted values

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse syntax/stx racket/match
         racket/dict
         racket/format
         (env global-env)
         (typecheck signatures)
         (types base-abbrev resolve subtype union utils)
         (rep type-rep)
         (utils tc-utils)
         (for-template racket/base))

(import tc-expr^)
(export tc-app-contracts^)

(define-tc/app-syntax-class (tc/app-contracts expected)
  #:literals (#%plain-app list cons quote)
  (pattern (ac ctc:id orig-value:id mod-src:id
               mod-name (quote orig-value-name:id) srcloc)
     #:declare ac (id-from 'apply-contract 'racket/contract/private/base)
     (check-apply-contract #'orig-value))
  (pattern (ac . args)
     #:declare ac (id-from 'apply-contract 'racket/contract/private/base)
     (int-err "unexpected arguments to apply-contract")))

(define (check-apply-contract orig-value-id)
  (ret (lookup-type orig-value-id)))

