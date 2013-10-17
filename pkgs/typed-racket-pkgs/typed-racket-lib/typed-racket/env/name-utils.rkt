#lang racket/base

;; This module provides helper functions for working with
;; `Name` types, which are often used for type aliases

(require "../utils/utils.rkt"
         (rep type-rep rep-utils)
         (types resolve)
         syntax/id-table)

(provide has-names-free?)

(define name-free-cache (make-hash))

;; has-names-free? : (Listof Symbol) Type -> Boolean
;;
;; Check if a type has a Name type free in it. This helps
;; determine if a contract should be cached for the type or
;; not.
(define (has-names-free? names type)
  (define seq (Type-seq type))
  (define all-in-cache?
    (for/and ([name names])
      (hash-has-key? name-free-cache (cons name seq))))
  (define any-true?
    (for/and ([name names])
      (hash-ref name-free-cache (cons name seq) #f)))
  (cond [any-true?]
        [(and all-in-cache? (not any-true?)) #f]
        [else
         (define result (has-names-free?/core names type))
         (cond [result
                (hash-set! name-free-cache (cons result seq) #t)]
               [else
                (for ([name names])
                  (hash-set! name-free-cache (cons name seq) #f))])
         result]))

(define (has-names-free?/core names type)
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
             [(or (ormap (λ (name) (and (eq? (syntax-e id) name) name))
                         names)
                  #;
                  (ormap (λ (id) (eq? (syntax-e id) name)) deps))
              =>
              (λ (name) (escape name))]
             [else
              (free-id-table-set! seen-set id #t)
              (free-type? (resolve-once type))
              (make-Value #f)])]))
    (free-type? type)
    #f))

