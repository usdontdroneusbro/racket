#lang racket/base

;; This module provides helper functions for working with
;; `Name` types, which are often used for type aliases

(require "../utils/utils.rkt"
         (rep type-rep rep-utils)
         (types resolve)
         racket/control
         syntax/id-table)

(provide has-name-free?)

(define name-free-cache (make-hash))

;; has-name-free? : Symbol Type -> Boolean
;;
;; Check if a type has a Name type free in it. This helps
;; determine if a contract should be cached for the type or
;; not.
(define (has-name-free? name type)
  (define seen-set (make-free-id-table))
  (define (free-type? type)
    (define key (cons name (Type-seq type)))
    (define has-key? (hash-has-key? name-free-cache key))
    (%
     (cond [(and has-key? (hash-ref name-free-cache key))
            ;; whenever we have a #t answer, jump away
            (fcontrol #t)]
           [else
            (type-case
             (#:Type free-type?)
             type
             [#:Name
              id deps arg struct?
              (cond [(free-id-table-ref seen-set id #f)]
                    [(or (eq? (syntax-e id) name)
                         (ormap (λ (id) (eq? (syntax-e id) name)) deps))
                     (fcontrol #t)]
                    [else
                     (free-id-table-set! seen-set id #t)
                     (free-type? (resolve-once type))])])
            (hash-set! name-free-cache key #f)
            ;; return dummy type for `type-case`
            (make-Value #f)])
     (λ (answer _)
       (unless has-key?
         (hash-set! name-free-cache key answer))
       (fcontrol answer))))
  (% (begin (free-type? type) #f)
     (λ (v _) v)))

