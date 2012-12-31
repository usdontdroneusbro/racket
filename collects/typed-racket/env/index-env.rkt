#lang racket/base

;; this implements the Theta environment from the TOPLAS paper

;; this environment maps type variables names (symbols)
;; to types representing the type variable
;; technically, the mapped-to type is unnecessary, but it's convenient to have it around? maybe?

(require "../utils/tc-utils.rkt"
         "tvar-env-helper.rkt"
         (only-in racket/dict dict-keys))

(provide initial-index-env
         current-indexes
         extend-indexes
         extend-indexes/new
         bound-index?
         lookup-index
         infer-index)

(define-tvar-ids
  initial-index-env
  current-indexes
  extend-indexes*
  extend-indexes/new*
  bound-index?
  lookup-index
  extend
  extend/many)

;; another set of macros since `extend-indexes` is supposed to
;; only take a single index despite its name
(define-syntax-rule (extend-indexes index . body)
  (extend-indexes* (list index) . body))

(define-syntax-rule (extend-indexes/new index new-index . body)
  (extend-indexes/new* (list index) (list new-index) . body))

(define (infer-index stx)
  (define bounds (dict-keys (current-indexes)))
  (when (null? bounds)
    (tc-error/stx stx "No type variable bound with ... in scope for ... type"))
  (unless (null? (cdr bounds))
    (tc-error/stx stx "Cannot infer bound for ... type"))
  (car bounds))
