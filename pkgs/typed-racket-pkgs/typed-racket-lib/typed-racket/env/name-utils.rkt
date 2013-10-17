#lang racket/base

;; This module provides helper functions for working with
;; `Name` types, which are often used for type aliases

(require "../utils/utils.rkt"
         (rep type-rep rep-utils)
         (types resolve)
         syntax/id-table)

(provide has-name-free?)

;; has-name-free? : Symbol Type -> Boolean
;;
;; Check if a type has a Name type free in it. This helps
;; determine if a contract should be cached for the type or
;; not.
(define (has-name-free? name type)
  (define seen-set (make-free-id-table))
  (let/ec escape
    (define (free-type? type)
     (type-case
      (#:Type free-type?)
      type
      [#:Name
       id deps arg struct?
       (cond [(free-id-table-ref seen-set id #f)
              (make-Value #f)]
             [(or (eq? (syntax-e id) name)
                  (ormap (λ (id) (eq? (syntax-e id) name)) deps))
              (escape #t)]
             [else
              (free-id-table-set! seen-set id #t)
              (free-type? (resolve-once type))
              (make-Value #f)])]))
    (free-type? type)
    #f))

