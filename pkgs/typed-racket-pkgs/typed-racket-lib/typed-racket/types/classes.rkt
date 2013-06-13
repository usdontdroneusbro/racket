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
         infer-row-constraints
         object-type-clauses
         class-type-clauses
         (struct-out class-row))

;; Data definitions
;;
;; A RowConstraint is a
;;   List(Set<Id>, Set<Id>, Set<id>)
;;
;; A Row is a
;;   (class-row Dict<Name, Type> Dict<Name, Type> Dict<Name,Type>)
;;
(struct class-row (inits fields methods))

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

;; Syntax -> Syntax
;; removes two levels of nesting
(define (flatten-class-clause stx)
  (flatten (map stx->list (stx->list stx))))

;; Syntax class for object type parsing
(define-splicing-syntax-class object-type-clauses
  #:description "Object type clause"
  #:attributes (field-names field-types method-names method-types)
  #:literals (field)
  (pattern (~seq (~or (field field-clause:field-or-method-type ...)
                      method-clause:field-or-method-type)
                 ...)
           #:with field-names (flatten-class-clause #'((field-clause.label ...) ...))
           #:with field-types (flatten-class-clause #'((field-clause.type ...) ...))
           #:with method-names #'(method-clause.label ...)
           #:with method-types #'(method-clause.type ...)
           #:fail-when
           (check-duplicate-identifier (syntax->list #'field-names))
           "duplicate field or init-field clause"
           #:fail-when
           (check-duplicate-identifier (syntax->list #'method-names))
           "duplicate method clause"))

;; Syntax class for class type parsing
(define-splicing-syntax-class class-type-clauses
  #:description "Class type clause"
  #:attributes (row-var self extends-types
                init-names init-types init-optional?s
                init-field-names init-field-types
                init-field-optional?s
                field-names field-types
                method-names method-types)
  #:literals (init init-field field)
  (pattern (~seq (~or (~optional (~seq #:row-var row-var:id))
                      (~seq #:implements extends-type:expr)
                      (~optional (~seq #:self self:id))
                      (init init-clause:init-type ...)
                      (init-field init-field-clause:init-type ...)
                      (field field-clause:field-or-method-type ...)
                      method-clause:field-or-method-type)
                 ...)
           ;; FIXME: improve these somehow
           #:with init-names (flatten-class-clause #'((init-clause.label ...) ...))
           #:with init-types (flatten-class-clause #'((init-clause.type ...) ...))
           #:attr init-optional?s (flatten (attribute init-clause.optional?))
           #:with init-field-names (flatten-class-clause #'((init-field-clause.label ...) ...))
           #:with init-field-types (flatten-class-clause #'((init-field-clause.type ...) ...))
           #:attr init-field-optional?s (flatten (attribute init-field-clause.optional?))
           #:with field-names (flatten-class-clause #'((field-clause.label ...) ...))
           #:with field-types (flatten-class-clause #'((field-clause.type ...) ...))
           #:with method-names #'(method-clause.label ...)
           #:with method-types #'(method-clause.type ...)
           #:with extends-types #'(extends-type ...)
           #:fail-when
           (check-duplicate-identifier
            (append (syntax->list #'init-names)
                    (syntax->list #'init-field-names)))
           "duplicate init or init-field clause"
           #:fail-when
           (check-duplicate-identifier
            (append (syntax->list #'field-names)
                    (syntax->list #'init-field-names)))
           "duplicate field or init-field clause"
           #:fail-when
           (check-duplicate-identifier (syntax->list #'method-names))
           "duplicate method clause"))

(define-syntax-class init-type
  #:description "Initialization argument label and type"
  #:attributes (label type optional?)
  (pattern
   (label:id type:expr
    (~optional (~and #:optional (~bind [optional? #t]))))))

(define-syntax-class field-or-method-type
  #:description "Pair of field or method label and type"
  #:attributes (label type)
  (pattern (label:id type:expr)))

