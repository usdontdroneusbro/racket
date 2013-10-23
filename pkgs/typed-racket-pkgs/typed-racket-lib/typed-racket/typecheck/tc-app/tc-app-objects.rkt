#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse syntax/stx racket/match unstable/sequence unstable/syntax
         racket/dict
         (typecheck signatures)
         (types resolve union utils)
         (rep type-rep)
         (utils tc-utils)

         (for-template racket/base))


(import tc-expr^)
(export tc-app-objects^)

;; type-check object initialization and field operations
(define-tc/app-syntax-class (tc/app-objects expected)
  #:literals (#%plain-app list cons quote)
  (pattern (dmo b cl
            (#%plain-app list . pos-args)
            (#%plain-app list (#%plain-app cons (quote names) named-args) ...))
     #:declare dmo (id-from 'do-make-object 'racket/private/class-internal)
     (check-do-make-object #'b #'cl #'(names ...) #'(named-args ...)))
  (pattern (dmo . args)
     #:declare dmo (id-from 'do-make-object 'racket/private/class-internal)
     (int-err "unexpected arguments to do-make-object"))
  (pattern (gf field obj)
     #:declare gf (id-from 'get-field/proc 'racket/private/class-internal)
     (check-get-field #'field #'obj))
  (pattern (gf . args)
     #:declare gf (id-from 'get-field/proc 'racket/private/class-internal)
     (int-err "unexpected arguments to get-field/proc")))

;; check-do-make-object : Syntax Syntax Listof<Syntax> Listof<Syntax> -> TCResult
;; do-make-object now takes blame as its first argument, which isn't checked
;; (it's just an s-expression)
(define (check-do-make-object b cl names named-args)
  (define datum-names (stx-map syntax-e names))
  (define name-assoc (stx-map cons datum-names named-args))
  (match (resolve (tc-expr/t cl))
    [(Union: '()) (ret (Un))]
    [(and c (Class: _ (list (and inits (list init-names _ _)) ...)
                    _ _ _))
     (for ([name datum-names]
           #:unless (memq name init-names))
       (tc-error/delayed
        "unknown named argument ~a for class\nlegal named arguments are ~a"
        name (stringify init-names)))
     (for ([init inits])
       (match-define (list init-name init-type opt?) init)
       ;; stx if argument was provided, #f if it was
       ;; not provided (and if mandatory, it errors)
       (define maybe-stx
         (dict-ref
          name-assoc init-name
          (λ ()
            (unless opt?
              (tc-error/delayed "value not provided for named init arg ~a"
                                init-name))
            #f)))
       (when maybe-stx
         (tc-expr/check maybe-stx (ret init-type))))
     (ret (make-Instance c))]
    [t
     (tc-error/expr #:return (ret (Un))
                    "expected a class value for object creation, got: ~a" t)]))

;; check-get-field : Syntax Syntax -> TCResult
;; type-check the `get-field` operation on objects
(define (check-get-field field obj)
  (define maybe-field-sym
    (syntax-parse field [(quote f:id) (syntax-e #'f)] [_ #f]))
  (unless maybe-field-sym
    (tc-error/expr #:return (ret (Un))
                   "expected a symbolic field name, but got ~a" field))
  (define obj-type (tc-expr/t obj))
  (define (check obj-type)
    (match (resolve obj-type)
      ;; FIXME: handle unions
      [(and ty (Instance: (Class: _ _ (list fields ...) _ _)))
       (cond [(assq maybe-field-sym fields) =>
              (λ (field-entry) (ret (cadr field-entry)))]
             [else
              (tc-error/expr #:return (ret (Un))
                             "expected an object with field ~a, but got ~a"
                             maybe-field-sym ty)])]
      [(Instance: type)
       (check (make-Instance (resolve type)))]
      [type
       (tc-error/expr #:return (ret (Un))
                      "expected an object value for get-field, got ~a"
                      type)]))
  (check obj-type))

