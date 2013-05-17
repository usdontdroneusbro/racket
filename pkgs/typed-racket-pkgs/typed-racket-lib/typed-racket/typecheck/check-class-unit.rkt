#lang racket/unit

;; This module provides a unit for type-checking classes

(require "../utils/utils.rkt"
         racket/dict
         racket/match
         racket/pretty ;; DEBUG ONLY
         syntax/parse
         "signatures.rkt"
         "tc-metafunctions.rkt"
         "tc-funapp.rkt"
         "tc-subst.rkt"
         (private type-annotation)
         (env lexical-env)
         (types utils abbrev union subtype resolve)
         (utils tc-utils)
         (rep type-rep)
         (for-template racket/base
                       (base-env class-prims)))

(import tc-if^ tc-lambda^ tc-app^ tc-let^ tc-expr^)
(export check-class^)

;; Syntax TCResults -> Void
;; Type-check a class form by trawling its innards
;;
;; Assumptions:
;;  by the time this is called, we can be sure that
;;  init, field, and method presence/absence is guaranteed
;;  by the local-expansion done by class:
;;
;;  we know by this point that #'form is an actual typed
;;  class produced by class: due to the syntax property
(define (check-class form expected)
  (match expected
    [(tc-result1: (and self-class-type (Class: _ inits fields methods)))
     (syntax-parse form
       #:literals (let-values #%plain-lambda quote-syntax begin
                   #%plain-app values class:-internal letrec-syntaxes+values)
       ;; Inspect the expansion of the class macro for the pieces that
       ;; we need to type-check like superclass, methods, top-level
       ;; expressions and so on
       [(let-values ()
          (letrec-syntaxes+values ()
                                  ((()
                                    ;; residual class: data
                                    ;; FIXME: put in syntax class
                                    (begin
                                      (quote-syntax
                                       (class:-internal
                                        (init internal-init-names ...)
                                        (public internal-public-names ...)))
                                      (#%plain-app values))))
                                  (let-values (((superclass) superclass-expr)
                                               ((interfaces) interface-expr))
                                    (?#%app compose-class
                                            internal ...
                                            (#%plain-lambda (local-accessor local-mutator ??? ...)
                                                            (let-values ([(field-name) accessor-or-mutator]
                                                                         ...)
                                                              body))
                                            ????))))
        ;; Make sure the superclass is a class
        ;; FIXME: maybe should check the property on this expression
        ;;        as a sanity check too
        (define super-type (tc-expr #'superclass-expr))
        (define-values (super-inits super-fields super-methods)
          (match super-type
            ;; FIXME: should handle the case where the super class is
            ;;        polymorphic
            [(tc-result1: (Class: _ super-inits super-fields super-methods))
             (values super-inits super-fields super-methods)]
            [(tc-result1: t)
             (tc-error/expr "expected a superclass but got ~a" t
                            #:stx #'superclass-expr)
             ;; FIXME: is this the right thing to do?
             (values null null null)]))
        ;; Use the internal class: information to do some basic checks
        (define (missing-name? this super required)
          (ormap (λ (m) (and (not (or (member m super)
                                      (member m this)))
                             m))
                 required))
        (define (check-names this super required msg)
          (define missing?
            (missing-name? this super required))
          (when missing?
            ;; FIXME: make this a delayed error? Do it for every single
            ;;        name separately?
            (tc-error/expr "class definition missing ~a ~a" msg missing?)))
        (check-names (syntax->datum #'(internal-init-names ...))
                     (dict-keys super-inits)
                     (dict-keys inits)
                     "initialization argument")
        (check-names (syntax->datum #'(internal-public-names ...))
                     (dict-keys super-methods)
                     (dict-keys methods)
                     "public method")
        ;; trawl the body and find methods and type-check them
        (define (trawl-for-methods form)
          (syntax-parse form
            #:literals (let-values letrec-values #%plain-app
                        letrec-syntaxes+values)
            [stx
             #:when (syntax-property form 'tr:class:method)
             (list form)]
            [(let-values (b ...)
               body)
             (trawl-for-methods #'body)]
            [(letrec-values (b ...)
               body)
             (trawl-for-methods #'body)]
            [(letrec-syntaxes+values (sb ...) (vb ...)
               body)
             (trawl-for-methods #'body)]
            [(#%plain-app e ...)
             (apply append (map trawl-for-methods (syntax->list #'(e ...))))]
            [_ '()]))
        (define meths (trawl-for-methods #'body))
        (with-lexical-env/extend (syntax->list #'(internal-public-names ...))
                                 ;; FIXME: the types we put here are fine in the expected
                                 ;;        case, but not if the class doesn't have an annotation.
                                 ;;        Then we need to hunt down annotations in a first pass.
                                 ;;        (should probably do this in expected case anyway)
                                 ;; FIXME: this doesn't work because the names of local methods
                                 ;;        are obscured and need to be reconstructed somehow
                                 (map (λ (m) (car (dict-ref methods m)))
                                      (syntax->datum #'(internal-public-names ...)))
         (for ([meth meths])
           (pretty-print (syntax->datum meth))
           (define method-name (syntax-property meth 'tr:class:method))
           (define self-type (make-Instance self-class-type))
           (define method-type
             (fixup-method-type
              (car (dict-ref methods method-name))
              self-type))
           (define expected (ret method-type))
           (define annotated (annotate-method meth self-type))
           (tc-expr/check annotated expected)))
        ;; trawl the body for top-level expressions too
        ])]))

;; fixup-method-type : Function Type -> Function
;; Fix up a method's arity from a regular function type
(define (fixup-method-type type self-type)
  (match type
    [(Function: (list arrs ...))
     (define fixed-arrs
       (for/list ([arr arrs])
         (match-define (arr: doms rng rest drest kws) arr)
         (make-arr (cons self-type doms) rng rest drest kws)))
     (make-Function fixed-arrs)]
    [_ (tc-error "fixup-method-type: internal error")]))

;; annotate-method : Syntax Type -> Syntax
;; Adds a self type annotation for the first argument
(define (annotate-method stx self-type)
  (syntax-parse stx
    #:literals (let-values #%plain-lambda)
    [(let-values ([(meth-name:id)
                   (#%plain-lambda (self-param:id id:id ...)
                     body ...)])
       m)
     (define annotated-self-param
       (syntax-property #'self-param type-ascrip-symbol self-type))
     #`(let-values ([(meth-name)
                     (#%plain-lambda (#,annotated-self-param id ...)
                       body ...)])
         m)]
    [_ (tc-error "annotate-method: internal error")]))

;; I wish I could write this
#;
(module+ test
  (check-equal? (fixup-method-type (parse-type #'(Integer -> Integer)))
                (parse-type #'(Any Integer -> Integer))))

