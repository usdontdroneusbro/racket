#lang racket/base

;; This module provides object contracts that prevent access
;; to unmentioned methods and fields.
;;
;; These correspond to `object/c` and `OG` guards from
;; "Gradual Typing for First-Class Classes"

;; TODO: this may not be strong enough if the object
;;       reflection API doesn't give us enough information
;;       to reliably block off the extra fields/methods.

(require racket/class
         racket/match
         racket/contract/base
         racket/contract/combinator
         (for-syntax racket/base
                     syntax/parse))

(provide object/c-strict)

;; projection for base-object/c-strict
(define ((object/c-strict-proj ctc) blame)
  (λ (obj)
    (match-define (base-object/c-strict
                   methods method-ctcs
                   fields field-ctcs)
                  ctc)
    (define actual-fields (field-names obj))
    (define actual-methods
      (interface->method-names (object-interface obj)))
    (define remaining-fields
      (remove* fields actual-fields))
    (define remaining-methods
      (remove* methods actual-methods))
    (define guard/c
      (make-object/c (append methods remaining-methods)
                     (append method-ctcs
                             (for/list ([m remaining-methods])
                               (reject->/c)))
                     (append fields remaining-fields)
                     (append field-ctcs
                             ;; FIXME: use a custom contract instead
                             ;;        of `none/c` to fix blame
                             (for/list ([m remaining-fields]) none/c))))
    (((contract-projection guard/c) blame) obj)))

(struct base-object/c-strict
  (method-names method-ctcs field-names field-ctcs)
  #:property prop:contract
  (build-contract-property
   #:projection object/c-strict-proj))

(begin-for-syntax
 (define-syntax-class object/c-clause
   #:attributes (method-names method-ctcs field-names field-ctcs)
   (pattern (field [name:id ctc:expr] ...)
            #:with field-names #'(list (quote name) ...)
            #:with field-ctcs #'(list ctc ...)
            #:with method-names #'null
            #:with method-ctcs #'null)
   (pattern [name:id ctc:expr]
            #:with field-names #'null
            #:with field-ctcs #'null
            #:with method-names #'(list (quote name))
            #:with method-ctcs #'(list ctc))))

(define-syntax (object/c-strict stx)
  (syntax-parse stx
   [(_ ?clause:object/c-clause ...)
    (syntax/loc stx
      (base-object/c-strict
       (append ?clause.method-names ...)
       (append ?clause.method-ctcs ...)
       (append ?clause.field-names ...)
       (append ?clause.field-ctcs ...)))]))

;; this contract combinator prevents the function
;; from being called in any way and blames the caller
(define ((reject->/c-proj ctc) blame)
  (λ (fun)
    (chaperone-procedure
     fun
     (make-keyword-procedure
      (λ (kws kw-args . rest)
        (raise-blame-error
         (blame-swap blame)
         fun
         "cannot call method hidden by Typed Racket"))))))

(struct reject->/c ()
        #:property prop:chaperone-contract
        (build-chaperone-contract-property
         #:name (λ (ctc) '<hidden-method>)
         #:projection reject->/c-proj))

