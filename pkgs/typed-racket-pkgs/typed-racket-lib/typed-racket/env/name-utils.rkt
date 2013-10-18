#lang racket/base

;; This module provides helper functions for working with
;; `Name` types, which are often used for type aliases

(require "../utils/utils.rkt"
         (rep type-rep rep-utils)
         (types resolve)
         syntax/id-table)

(provide has-name-free?)

(define name-free-cache (make-hash))

;; has-name-free? : Symbol Type -> Boolean
;;
;; Check if a type has a Name type free in it. This helps
;; determine if a contract should be cached for the type or
;; not.
(define (has-name-free? name type)
  (define key (cons name (Type-seq type)))
  (cond [(hash-has-key? name-free-cache key)
         (hash-ref name-free-cache key)]
        [else
         (define result (has-name-free?/core name type))
         (hash-set! name-free-cache key result)
         result]))

(define (has-name-free?/core name type)
  (define seen-set (make-free-id-table))
  (let/ec escape
    (define (free-type? type)
     (type-case
      (#:Type free-type?)
      type
      [#:Name
       id deps arg struct?
       (cond [struct? type]
             [(free-id-table-ref seen-set id #f) type]
             [(or (eq? (syntax-e id) name)
                  (ormap (λ (id) (eq? (syntax-e id) name)) deps))
              (escape #t)]
             [else
              (free-id-table-set! seen-set id #t)
              (free-type? (resolve-once type))
              type])]))
    (free-type? type)
    #f))

