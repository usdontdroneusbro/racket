#lang racket/base

;; This module provides helper syntax classes and functions for
;; working with class/object types and rows

(require "../utils/utils.rkt"
         (rep type-rep)
         (except-in racket/class private)
         racket/list
         racket/match
         racket/set
         syntax/parse
         syntax/stx
         (only-in unstable/list check-duplicate)
         (for-template racket/class))

(provide row-constraints
         infer-row-constraints)

;; Data definitions
;;
;; A RowConstraint is a
;;   List(Set<Id>, Set<Id>, Set<id>)
;;
;; A Row is a
;;   List(Dict<Name, Type>, Dict<Name, Type>, Dict<Name,Type>)
;;

;; Syntax -> Syntax
;; Turn into datums and then flatten
(define (flatten/datum stx)
  (flatten (syntax->datum stx)))

;; Syntax classes for rows
(define-splicing-syntax-class row-constraints
  #:literals (init init-field field)
  (pattern (~seq (~or (init iname:id ...)
                      (init-field ifname:id ...)
                      (field fname:id ...)
                      mname:id)
                 ...)
            #:attr init-names (flatten/datum #'((iname ...) ...))
            #:attr init-field-names (flatten/datum #'((ifname ...) ...))
            #:attr field-names (flatten/datum #'((fname ...) ...))
            #:attr method-names (syntax->datum #'(mname ...))
            #:attr all-field-names (append (attribute init-field-names)
                                           (attribute field-names))
            #:attr all-init-names (append (attribute init-field-names)
                                          (attribute init-names))
            #:fail-when
            (check-duplicate (attribute all-init-names))
            "duplicate init or init-field clause"
            #:fail-when
            (check-duplicate (attribute all-field-names))
            "duplicate field or init-field clause"
            #:attr constraints
            (list (list->set (attribute all-init-names))
                  (list->set (attribute all-field-names))
                  (list->set (attribute method-names)))))

;; Type -> RowConstraint
;; Infer constraints on a row for a row polymorphic function
;; TODO
(define (infer-row-constraints type)
  (list (set) (set) (set)))

