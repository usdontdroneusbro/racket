#lang racket/base

;; This module contains helper functions related to Name types

(require "../utils/utils.rkt"
         (rep type-rep rep-utils))

(provide free-names)

;; free-names : Type -> (Listof Identifier)
;; Returns a list of the free names in a type. Memoizes the results
;; for each type encountered in the traversal.
(define cache (make-hash))
(define (free-names type)
  (define free-names null)
  (define (fn type)
    (define result
      (type-case (#:Type free-names
                  #:Filter (sub-f free-names)
                  #:Object (sub-o free-names))
                 type
        [#:Name n _ _ _
         (let ()
           (set! free-names (cons n free-names))
           type)]))
    (hash-set! cache type free-names)
    result)
  (cond [(hash-ref cache type #f)]
        [else
         (fn type)
         free-names]))
