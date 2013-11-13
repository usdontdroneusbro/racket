#lang racket/base

(require (rename-in "../utils/utils.rkt" [infer infer-in]))
(require racket/match
         unstable/list
         (contract-req)
         (infer-in infer)
         (rep type-rep filter-rep object-rep)
         (utils tc-utils)
         (types resolve subtype remove-intersect union)
         (only-in (env type-env-structs lexical-env)
                  env? update-type/lexical env-map env-props replace-props)
         (rename-in (types abbrev)
                    [-> -->]
                    [->* -->*]
                    [one-of/c -one-of/c])
         (typecheck tc-metafunctions))

;(trace replace-nth)

(define/cond-contract (update t lo)
  (Type/c Filter/c . -> . Type/c)
  (match* ((resolve t) lo)
    ;; pair ops
    [((Pair: t s) (TypeFilter: u (list rst ... (CarPE:)) x))
     (make-Pair (update t (-filter u x rst)) s)]
    [((Pair: t s) (NotTypeFilter: u (list rst ... (CarPE:)) x))
     (make-Pair (update t (-not-filter u x rst)) s)]
    [((Pair: t s) (TypeFilter: u (list rst ... (CdrPE:)) x))
     (make-Pair t (update s (-filter u x rst)))]
    [((Pair: t s) (NotTypeFilter: u (list rst ... (CdrPE:)) x))
     (make-Pair t (update s (-not-filter u x rst)))]

    ;; syntax ops
    [((Syntax: t) (TypeFilter: u (list rst ... (SyntaxPE:)) x))
     (make-Syntax (update t (-filter u x rst)))]
    [((Syntax: t) (NotTypeFilter: u (list rst ... (SyntaxPE:)) x))
     (make-Syntax (update t (-not-filter u x rst)))]

    ;; promise op
    [((Promise: t) (TypeFilter: u (list rst ... (ForcePE:)) x))
     (make-Promise (update t (-filter u x rst)))]
    [((Promise: t) (NotTypeFilter: u (list rst ... (ForcePE:)) x))
     (make-Promise (update t (-not-filter u x rst)))]

    ;; struct ops
    [((Struct: nm par flds proc poly pred)
      (TypeFilter: u (list rst ... (StructPE: (? (lambda (s) (subtype t s)) s) idx)) x))
     (make-Struct nm par
                  (list-update flds idx
                            (match-lambda [(fld: e acc-id #f)
                                           (make-fld
                                            (update e (-filter u x rst))
                                            acc-id #f)]
                                          [_ (int-err "update on mutable struct field")]))
                  proc poly pred)]
    [((Struct: nm par flds proc poly pred)
      (NotTypeFilter: u (list rst ... (StructPE: (? (lambda (s) (subtype t s)) s) idx)) x))
     (make-Struct nm par (list-update flds idx
                                      (match-lambda [(fld: e acc-id #f)
                                                     (make-fld
                                                      (update e (-not-filter u x rst))
                                                      acc-id #f)]
                                          [_ (int-err "update on mutable struct field")]))
                  proc poly pred)]

    ;; class field ops
    ;;
    ;; This is a bit of an abuse of filters/objects. Normally, the
    ;; object in an expression like (car? x) is the `x` identifier.
    ;; However, for an application of a field accessor (accessor obj)
    ;; the id `accessor` is the object. We need this so that we can
    ;; refine the *range* of accessor, not the object's type.
    [((Function: (list (arr: doms (Values: (list (Result: rng _ _))) _ _ _)))
      (TypeFilter: u (list rst ... (FieldPE:)) x))
     (make-Function
       (list (make-arr* doms (update rng (-filter u x rst)))))]
    [((Function: (list (arr: doms (Values: (list (Result: rng _ _))) _ _ _)))
      (NotTypeFilter: u (list rst ... (FieldPE:)) x))
     (make-Function
       (list (make-arr* doms (update rng (-not-filter u x rst)))))]

    ;; otherwise
    [(t (TypeFilter: u (list) _))
     (restrict t u)]
    [(t (NotTypeFilter: u (list) _))
     (remove t u)]
    [((Union: ts) lo)
     (apply Un (map (lambda (t) (update t lo)) ts))]
    [(t* lo)
     #;
     (int-err "update along ill-typed path: ~a ~a ~a" t t* lo)
     t]))

;; sets the flag box to #f if anything becomes (U)
(define/cond-contract (env+ env fs flag)
  (([e env?] [fs (listof Filter/c)] [bx (box/c boolean?)])
   #:pre (bx) (unbox bx) . ->i . [_ env?])
  (define-values (props atoms) (combine-props fs (env-props env) flag))
  (for/fold ([Γ (replace-props env (append atoms props))]) ([f (in-list atoms)])
    (match f
      [(Bot:) (set-box! flag #f) (env-map (lambda (k v) (Un)) Γ)]
      [(or (TypeFilter: _ _ x) (NotTypeFilter: _ _ x))
       (update-type/lexical (lambda (x t) (let ([new-t (update t f)])
                                            (when (type-equal? new-t (Un))
                                              (set-box! flag #f))
                                            new-t))
                            x Γ)]
      [_ Γ])))

(provide/cond-contract
 [env+ (([e env?] [fs (listof Filter/c)] [bx (box/c boolean?)])
        #:pre (bx) (unbox bx) . ->i . [_ env?])])
