#lang racket/base

;; Contract generation for Typed Racket

(provide type->contract define/fixup-contract? change-contract-fixups
         type->contract-fail
         ;; FIXME: instead of exposing these, it would be better to
         ;;        provide an abstraction for this instead
         current-contract-cache current-contract-types)

(require
 "../utils/utils.rkt"
 syntax/parse
 (rep type-rep filter-rep object-rep rep-utils)
 (utils tc-utils)
 (env type-alias-env type-name-env name-utils)
 (types resolve utils)
 (prefix-in t: (types abbrev numeric-tower))
 (private parse-type syntax-properties)
 racket/match syntax/stx racket/syntax racket/list
 racket/format
 unstable/list
 racket/dict racket/set
 syntax/id-table
 (only-in racket/set set-intersect set-subtract)
 unstable/sequence
 (contract-req)
 (for-template racket/base racket/contract racket/set
               (utils any-wrap object-guard)
               (prefix-in t: (types numeric-predicates))
               (only-in unstable/contract sequence/c)
               (only-in racket/class object% is-a?/c subclass?/c
                        object-contract class/c class?
                        instanceof/c new-seal/c
                        init override inherit super augment
                        field inherit-field
                        augride inner)))

;; These check if either the define form or the body form has the syntax
;; property. Normally the define form will have the property but lifting an
;; expression to the module level will put the property on the body.
(define-values (typechecker:contract-def
                typechecker:flat-contract-def
                typechecker:contract-def/maker)
  (let ()
    (define ((get-contract-def property) stx)
      (or (property stx)
          (syntax-case stx (define-values)
            ((define-values (name) body)
             (property #'body))
            (_ #f))))
    (values
      (get-contract-def contract-def-property)
      (get-contract-def flat-contract-def-property)
      (get-contract-def contract-def/maker-property))))

;; define/fixup-contract? : Syntax -> Boolean
;; Check if the argument is a definition that needs to be contracted
(define (define/fixup-contract? stx)
  (or (typechecker:contract-def stx)
      (typechecker:flat-contract-def stx)
      (typechecker:contract-def/maker stx)))

;; type->contract-fail : Syntax Type #:ctc-str String
;;                       -> #:reason (Option String) -> Void
;; Curried function that produces a function to report
;; type->contract failures
(define ((type->contract-fail to-check to-report
                              #:ctc-str [ctc-str "contract"])
         #:reason [reason #f])
  (tc-error/stx
   to-report
   (~a "Type ~a could not be converted to a "
       ctc-str
       (if reason (~a ": " reason) "."))
   to-check))

;; The following two parameters should be parameterized together

;; current-contract-cache : Parameter<Option<Dict<Id,Stx>>>
;; This parameter controls whether contracts should be cached
;; (non-#f) and if so stores a mutable dictionary from identifiers
;; to contracts. Used to make serialization efficient.
(define current-contract-cache (make-parameter #f))

;; current-contract-ids :  Parameter<Option<Box<List<Type>>>>
;; Records a list of types that need to be written out
;; in a particular order for the cache above
(define current-contract-types (make-parameter #f))

;; generate-contract-def : Syntax -> Syntax
;; Construct a contracted definition for `require/typed`
(define (generate-contract-def stx #:name-mapping [name-mapping (make-hash)])
  (define prop (define/fixup-contract? stx))
  (define maker? (typechecker:contract-def/maker stx))
  (define flat? (typechecker:flat-contract-def stx))
  (define typ (parse-type prop))
  (define kind (if flat? 'flat 'impersonator))
  (syntax-parse stx #:literals (define-values)
    [(define-values (n) _)
     (let ([typ (if maker?
                    ((map fld-t (Struct-flds (lookup-type-name (Name-id typ)))) #f . t:->* . typ)
                    typ)])
       (define/with-syntax cnt
         (type->contract
          typ
          ;; this is for a `require/typed', so the value is not from the typed side
          #:typed-side #f
          #:kind kind
          #:name-mapping name-mapping
          (type->contract-fail typ prop)))
       (quasisyntax/loc
        stx
        (define-values (n)
          (recursive-contract
           cnt
           #,(contract-kind->keyword kind)))))]
    [_ (int-err "should never happen - not a define-values: ~a"
		(syntax->datum stx))]))

;; change-contract-fixups : Syntax -> Listof<Syntax>
;; Some type->contract translations are deferred, so resolve
;; those here and leave everything else as is.
(define (change-contract-fixups forms)
  (define name-mapping (make-hash))
  (define seen-types (mutable-set))
  (parameterize ([current-contract-cache (make-hash)]
                 [current-contract-types (box '())])
    (let loop ([results '()] [exprs (syntax->list forms)])
      (cond [(empty? exprs) (reverse results)]
            [(not (define/fixup-contract? (car exprs)))
             (loop (cons (car exprs) results) (cdr exprs))]
            [else
             (define fixed-up-e
               (generate-contract-def (car exprs) #:name-mapping name-mapping))
             ;; use these two parameters to collect contracts and
             ;; name them to reduce the size of the resulting syntax
             (define cache (current-contract-cache))
             (define type-box (current-contract-types))
             (define types (reverse (remove-duplicates (reverse (unbox type-box)))))
             (define defs
               (for/list ([type+side (in-list types)]
                          #:unless (set-member? seen-types type+side))
                 (define name+ctc (dict-ref cache type+side))
                 (match-define (list name ctc) name+ctc)
                 #`(define #,name #,(syntax-property ctc 'inferred-name (void)))))
             ;; reset before the next loop so that each expr only
             ;; generates contracts for types it refers to
             ;; (this prevents unbound id errors due to require
             ;;  depdendencies, e.g., from Opaque types)
             (set-union! seen-types (list->mutable-set (unbox type-box)))
             (set-box! type-box '())
             (loop (append (cons fixed-up-e defs) results)
                   (cdr exprs))]))))

;; To avoid misspellings
(define impersonator-sym 'impersonator)
(define chaperone-sym 'chaperone)
(define flat-sym 'flat)

(define (contract-kind-max i . args)
  (define (contract-kind-max2 x y)
    (cond
      ((equal? flat-sym x) y)
      ((equal? flat-sym y) x)
      ((equal? chaperone-sym x) y)
      ((equal? chaperone-sym y) x)
      (else impersonator-sym)))
  (for/fold ((acc i)) ((v (in-list args)))
    (contract-kind-max2 v acc)))

(define (contract-kind-min i . args)
  (define (contract-kind-min2 x y)
    (cond
      ((equal? flat-sym x) x)
      ((equal? flat-sym y) y)
      ((equal? chaperone-sym x) x)
      ((equal? chaperone-sym y) y)
      (else impersonator-sym)))
  (for/fold ((acc i)) ((v (in-list args)))
    (contract-kind-min2 v acc)))


(define (contract-kind->keyword sym)
  (string->keyword (symbol->string sym)))

(define (from-typed? side)
  (case side
   [(typed both) #t]
   [(untyped) #f]))

(define (from-untyped? side)
  (case side
   [(untyped both) #t]
   [(typed) #f]))

(define (flip-side side)
  (case side
   [(typed) 'untyped]
   [(untyped) 'typed]
   [(both) 'both]))

(define (type->contract ty fail #:typed-side [typed-side #t] #:kind [kind 'impersonator]
                        #:name-mapping [name-mapping (make-hash)])
  (define vars (make-parameter '()))
  (define dont-cache (make-parameter '()))
  (define current-contract-kind (make-parameter flat-sym))
  (define (increase-current-contract-kind! kind)
    (current-contract-kind (contract-kind-max (current-contract-kind) kind)))
  (let/ec exit
    (let loop ([ty ty] [typed-side (if typed-side 'typed 'untyped)] [structs-seen null] [kind kind])
      (define (t->c t #:seen [structs-seen structs-seen] #:kind [kind kind])
        (loop t typed-side structs-seen kind))
      (define (t->c/neg t #:seen [structs-seen structs-seen] #:kind [kind kind])
        (loop t (flip-side typed-side) structs-seen kind))
      (define (t->c/both t #:seen [structs-seen structs-seen] #:kind [kind kind])
        (loop t 'both structs-seen kind))
      (define (t->c/fun f #:method [method? #f])
        (match f
          [(Function: (list (top-arr:)))
           (set-chaperone!)
           #'(case->)]
          [(Function: arrs)
           (set-chaperone!)
           ;; Try to generate a single `->*' contract if possible.
           ;; This allows contracts to be generated for functions with both optional and keyword args.
           ;; (and don't otherwise require full `case->')
           (define conv (match-lambda [(Keyword: kw kty _) (list kw (t->c/neg kty))]))
           (define (partition-kws kws) (partition (match-lambda [(Keyword: _ _ mand?) mand?]) kws))
           (define (process-dom dom*)  (if method? (cons #'any/c dom*) dom*))
           (define (process-rngs rngs*)
             (match rngs*
               [(list r) r]
               [_ #`(values #,@rngs*)]))
           (cond
            ;; To generate a single `->*', everything must be the same for all arrs, except for positional
            ;; arguments which can increase by at most one each time.
            ;; Note: optional arguments can only increase by 1 each time, to avoid problems with
            ;;  functions that take, e.g., either 2 or 6 arguments. These functions shouldn't match,
            ;;  since this code would generate contracts that accept any number of arguments between
            ;;  2 and 6, which is wrong.
            ;; TODO sufficient condition, but may not be necessary
            [(and
              (> (length arrs) 1)
              ;; Keyword args, range and rest specs all the same.
              (let* ([xs (map (match-lambda [(arr: _ rng rest-spec _ kws)
                                             (list rng rest-spec kws)])
                              arrs)]
                     [first-x (first xs)])
                (for/and ([x (in-list (rest xs))])
                  (equal? x first-x)))
              ;; Positionals are monotonically increasing by at most one.
              (let-values ([(_ ok?)
                            (for/fold ([positionals (arr-dom (first arrs))]
                                       [ok-so-far?  #t])
                                ([arr (in-list (rest arrs))])
                              (match arr
                                [(arr: dom _ _ _ _)
                                 (define ldom         (length dom))
                                 (define lpositionals (length positionals))
                                 (values dom
                                         (and ok-so-far?
                                              (or (= ldom lpositionals)
                                                  (= ldom (add1 lpositionals)))
                                              (equal? positionals (take dom lpositionals))))]))])
                ok?))
             (match* ((first arrs) (last arrs))
               [((arr: first-dom (Values: (list (Result: rngs (FilterSet: (Top:) (Top:)) (Empty:)) ...)) rst #f kws)
                 (arr: last-dom _ _ _ _)) ; all but dom is the same for all
                (with-syntax
                    ([(dom* ...)
                      ;; Mandatory arguments are positionals of the first arr
                      ;; (smallest set, since postitionals are monotonically increasing)
                      ;; and mandatory kw args.
                      (let*-values ([(mand-kws opt-kws) (partition-kws kws)])
                        (process-dom (append (map t->c/neg first-dom)
                                             (append-map conv mand-kws))))]
                     [(opt-dom* ...)
                      (let-values ([(mand-kws opt-kws) (partition-kws kws)])
                        (append (map t->c/neg (drop last-dom (length first-dom)))
                                (append-map conv opt-kws)))]
                     [rng* (process-rngs (map t->c rngs))]
                     [(rst-spec ...) (if rst #`(#:rest (listof #,(t->c/neg rst))) #'())])
                  #'((dom* ...) (opt-dom* ...) rst-spec ... . ->* . rng*))])]
            [else
             (define ((f [case-> #f]) a)
               (define-values (dom* opt-dom* rngs* rst)
                 (match a
                   ;; functions with no filters or objects
                   [(arr: dom (Values: (list (Result: rngs (FilterSet: (Top:) (Top:)) (Empty:)) ...)) rst #f kws)
                    (let-values ([(mand-kws opt-kws) (partition-kws kws)])
                      (values (append (map t->c/neg dom) (append-map conv mand-kws))
                              (append-map conv opt-kws)
                              (map t->c rngs)
                              (and rst (t->c/neg rst))))]
                   ;; functions with filters or objects
                   [(arr: dom (Values: (list (Result: rngs _ _) ...)) rst #f '())
                    (if (not (from-untyped? typed-side))
                        (values (map t->c/neg dom)
                                null
                                (map t->c rngs)
                                (and rst (t->c/neg rst)))
                        (exit (fail #:reason
                                    (~a "cannot generate contract for function type"
                                        " with filters or objects."))))]
                   [_ (exit (fail))]))
               (with-syntax*
                ([(dom* ...)     (process-dom dom*)]
                 [(opt-dom* ...) opt-dom*]
                 [rng*           (process-rngs rngs*)]
                 [rst*           rst]
                 [(rst-spec ...) (if rst #'(#:rest (listof rst*)) #'())])
                ;; Garr, I hate case->!
                (if (and (pair? (syntax-e #'(opt-dom* ...))) case->)
                    (exit (fail))
                    (if (or rst (pair? (syntax-e #'(opt-dom* ...))))
                        (if case->
                            #'(dom* ... rst-spec ... . -> . rng*)
                            #'((dom* ...) (opt-dom* ...) rst-spec ... . ->* . rng*))
                        #'(dom* ... . -> . rng*)))))
             (define arities (for/list ([t (in-list arrs)])
                               (match t
                                 [(arr: dom _ _ _ _) (length dom)]
                                 ;; is there something more sensible here?
                                 [(top-arr:) (int-err "got top-arr")])))
             (define maybe-dup (check-duplicate arities #:same? =))
             (when maybe-dup
               (define reason
                 (~a "function type has two cases of arity " maybe-dup))
               (exit (fail #:reason reason)))
             (match (map (f (not (= 1 (length arrs)))) arrs)
               [(list e) e]
               [l #`(case-> #,@l)])])]
          [_ (int-err "not a function ~a" f)]))

      ;; Helpers for contract requirements
      (define (set-impersonator!)
        (when (not (equal? kind impersonator-sym))
          (exit (fail #:reason
                      (~a "required a chaperone or flat contract but could"
                          " only generate an impersonator contract."))))
        (increase-current-contract-kind! impersonator-sym))
      (define (set-chaperone!)
        (when (equal? kind flat-sym)
          (exit (fail #:reason
                      (~a "required a first-order contract but could"
                          " only generate a higher-order contract."))))
        (increase-current-contract-kind! chaperone-sym))

      ;; t->c/poly-row : PolyRow -> Syntax
      ;; Helper function to create a contract for row poly types
      (define (t->c/poly-row type #:method [method? #f])
        (match-define (PolyRow: vs _ b) type)
        (cond [(not (from-untyped? typed-side))
               ;; see Poly
               (parameterize ([vars (append (for/list ([v (in-list vs)])
                                              (list v #'any/c))
                                            (vars))])
                 (t->c/fun b #:method method?))]
              [else
               ;; extend row constraints and let Class contract
               ;; generation figure out whether to use sealing or
               ;; unsealing
               (match-define (PolyRow-names: vs-nm constraints _) ty)
               (define/with-syntax seal/c (generate-temporary))
               (set-impersonator!)
               (parameterize ([vars (cons (list (car vs) #'seal/c) (vars))])
                 #`(let ([seal/c (new-seal/c #,(car constraints)
                                             #,(cadr constraints)
                                             #,(caddr constraints))])
                     #,(t->c/fun b #:method method?)))]))

      (define (t->c/method t)
        (cond [(PolyRow? t) (t->c/poly-row t #:method #t)]
              [else (t->c/fun t #:method #t)]))

      (define cache (current-contract-cache))
  (cond
   [(and cache
         ;; FIXME: remove or refactor
         (let ([cache-item (dict-ref cache (cons (Type-seq ty) typed-side) #f)])
           (and cache-item
                (andmap (λ (var-pair)
                          (define var (car var-pair))
                          (not (member var (fv ty))))
                        (vars))
                cache-item))) => car]
   [else
    (define ctc
      (match ty
        ;; Applications of polymorphic recursive type aliases
        ;; turn into contract function applications to prevent
        ;; infinite resolution of the application.
        [(App: (and rator (Name: _ _ _ #f)) rands _)
         #`(#,(t->c rator) #,@(map t->c rands))]
        [(Name: id pre-deps args #f)
         (define name (syntax-e id))
         (define poly? (and args (not (null? args))))
         (cond [;; Recursive references get indirected to an id
                (dict-has-key? name-mapping (cons name typed-side))
                (define ctc (dict-ref name-mapping (cons name typed-side)))
                (if poly? ctc #`(recursive-contract #,ctc))]
               [poly?
                (dict-set! name-mapping (cons name typed-side) (generate-temporary))
                (define resolved (resolve-once ty))
                (define params-id (generate-temporaries (or args null)))
                (define params-sym (map syntax-e params-id))
                (define new-vars (map list params-sym params-id))
                (define ctc
                  (parameterize ([vars (append new-vars (vars))]
                                 [dont-cache (cons (cons ty typed-side) (dont-cache))])
                    (t->c (if poly? (Poly-body params-sym resolved) resolved))))
                (with-syntax ([(param ...) params-id])
                  #`(λ (param ...) (recursive-contract #,ctc)))]
               [else
                (dict-set! name-mapping (cons name typed-side) (generate-temporary))
                (define ctc
                  (parameterize ([dont-cache (cons (cons ty typed-side) (dont-cache))])
                    (t->c (resolve-once ty))))
                #`(recursive-contract #,ctc)])]
        [(or (App: _ _ _) (Name: _ _ _ #t))
         (t->c (resolve-once ty))]
        ;; any/c doesn't provide protection in positive position
        [(Univ:)
         (cond [(from-typed? typed-side)
                (set-chaperone!)
                #'any-wrap/c]
               [else #'any/c])]
        ;; we special-case lists:
        [(Mu: var (Union: (list (Value: '()) (Pair: elem-ty (F: var)))))
         (if (and (not (from-typed? typed-side)) (type-equal? elem-ty t:Univ))
             #'list?
             #`(listof #,(t->c elem-ty)))]
        [(? (lambda (e) (eq? t:Any-Syntax e))) #'syntax?]

        ;; numeric special cases
        ;; since often-used types like Integer are big unions, this would
        ;; generate large contracts.
        [(== t:-PosByte type-equal?) #'(flat-named-contract 'Positive-Byte (and/c byte? positive?))]
        [(== t:-Byte type-equal?) #'(flat-named-contract 'Byte byte?)]
        [(== t:-PosIndex type-equal?) #'(flat-named-contract 'Positive-Index (and/c t:index? positive?))]
        [(== t:-Index type-equal?) #'(flat-named-contract 'Index t:index?)]
        [(== t:-PosFixnum type-equal?) #'(flat-named-contract 'Positive-Fixnum (and/c fixnum? positive?))]
        [(== t:-NonNegFixnum type-equal?) #'(flat-named-contract 'Nonnegative-Fixnum (and/c fixnum? (lambda (x) (>= x 0))))]
        ;; -NegFixnum is a base type
        [(== t:-NonPosFixnum type-equal?) #'(flat-named-contract 'Nonpositive-Fixnum (and/c fixnum? (lambda (x) (<= x 0))))]
        [(== t:-Fixnum type-equal?) #'(flat-named-contract 'Fixnum fixnum?)]
        [(== t:-PosInt type-equal?) #'(flat-named-contract 'Positive-Integer (and/c exact-integer? positive?))]
        [(== t:-Nat type-equal?) #'(flat-named-contract 'Natural (and/c exact-integer? (lambda (x) (>= x 0))))]
        [(== t:-NegInt type-equal?) #'(flat-named-contract 'Negative-Integer (and/c exact-integer? negative?))]
        [(== t:-NonPosInt type-equal?) #'(flat-named-contract 'Nonpositive-Integer (and/c exact-integer? (lambda (x) (<= x 0))))]
        [(== t:-Int type-equal?) #'(flat-named-contract 'Integer exact-integer?)]
        [(== t:-PosRat type-equal?) #'(flat-named-contract 'Positive-Rational (and/c t:exact-rational? positive?))]
        [(== t:-NonNegRat type-equal?) #'(flat-named-contract 'Nonnegative-Rational (and/c t:exact-rational? (lambda (x) (>= x 0))))]
        [(== t:-NegRat type-equal?) #'(flat-named-contract 'Negative-Rational (and/c t:exact-rational? negative?))]
        [(== t:-NonPosRat type-equal?) #'(flat-named-contract 'Nonpositive-Rational (and/c t:exact-rational? (lambda (x) (<= x 0))))]
        [(== t:-Rat type-equal?) #'(flat-named-contract 'Rational t:exact-rational?)]
        [(== t:-FlonumZero type-equal?) #'(flat-named-contract 'Float-Zero (and/c flonum? zero?))]
        [(== t:-NonNegFlonum type-equal?) #'(flat-named-contract 'Nonnegative-Float (and/c flonum? (lambda (x) (not (< x 0)))))]
        [(== t:-NonPosFlonum type-equal?) #'(flat-named-contract 'Nonpositive-Float (and/c flonum? (lambda (x) (not (> x 0)))))]
        [(== t:-NegFlonum type-equal?) #'(flat-named-contract 'Negative-Float (and/c flonum? (lambda (x) (not (>= x 0)))))]
        [(== t:-PosFlonum type-equal?) #'(flat-named-contract 'Positive-Float (and/c flonum? (lambda (x) (not (<= x 0)))))]
        [(== t:-Flonum type-equal?) #'(flat-named-contract 'Float flonum?)]
        [(== t:-SingleFlonumZero type-equal?) #'(flat-named-contract 'Single-Flonum-Zero (and/c single-flonum? zero?))]
        [(== t:-InexactRealZero type-equal?) #'(flat-named-contract 'Inexact-Real-Zero (and/c inexact-real? zero?))]
        [(== t:-PosSingleFlonum type-equal?) #'(flat-named-contract 'Positive-Single-Flonum (and/c single-flonum? (lambda (x) (not (<= x 0)))))]
        [(== t:-PosInexactReal type-equal?) #'(flat-named-contract 'Positive-Inexact-Real (and/c inexact-real? (lambda (x) (not (<= x 0)))))]
        [(== t:-NonNegSingleFlonum type-equal?) #'(flat-named-contract 'Nonnegative-Single-Flonum (and/c single-flonum? (lambda (x) (not (< x 0)))))]
        [(== t:-NonNegInexactReal type-equal?) #'(flat-named-contract 'Nonnegative-Inexact-Real (and/c inexact-real? (lambda (x) (not (< x 0)))))]
        [(== t:-NegSingleFlonum type-equal?) #'(flat-named-contract 'Negative-Single-Flonum (and/c single-flonum? (lambda (x) (not (>= x 0)))))]
        [(== t:-NegInexactReal type-equal?) #'(flat-named-contract 'Negative-Inexact-Real (and/c inexact-real? (lambda (x) (not (>= x 0)))))]
        [(== t:-NonPosSingleFlonum type-equal?) #'(flat-named-contract 'Nonpositive-Single-Flonum (and/c single-flonum? (lambda (x) (not (> x 0)))))]
        [(== t:-NonPosInexactReal type-equal?) #'(flat-named-contract 'Nonpositive-Inexact-Real (and/c inexact-real? (lambda (x) (not (> x 0)))))]
        [(== t:-SingleFlonum type-equal?) #'(flat-named-contract 'Single-Flonum single-flonum?)]
        [(== t:-InexactReal type-equal?) #'(flat-named-contract 'Inexact-Real inexact-real?)]
        [(== t:-RealZero type-equal?) #'(flat-named-contract 'Real-Zero (and/c real? zero?))]
        [(== t:-PosReal type-equal?) #'(flat-named-contract 'Positive-Real (and/c real? (lambda (x) (not (<= x 0)))))]
        [(== t:-NonNegReal type-equal?) #'(flat-named-contract 'Nonnegative-Real (and/c real? (lambda (x) (not (< x 0)))))]
        [(== t:-NegReal type-equal?) #'(flat-named-contract 'Negative-Real (and/c real? (lambda (x) (not (>= x 0)))))]
        [(== t:-NonPosReal type-equal?) #'(flat-named-contract 'Nonpositive-Real (and/c real? (lambda (x) (not (> x 0)))))]
        [(== t:-Real type-equal?) #'(flat-named-contract 'Real real?)]
        [(== t:-ExactNumber type-equal?) #'(flat-named-contract 'Exact-Number (and/c number? exact?))]
        [(== t:-InexactComplex type-equal?)
         #'(flat-named-contract 'Inexact-Complex
                                (and/c number?
                                       (lambda (x)
                                         (and (inexact-real? (imag-part x))
                                              (inexact-real? (real-part x))))))]
        [(== t:-Number type-equal?) #'(flat-named-contract 'Number number?)]

        [(Base: sym cnt _ _) #`(flat-named-contract '#,sym (flat-contract-predicate #,cnt))]
        [(Refinement: par p?)
         #`(and/c #,(t->c par) (flat-contract #,p?))]
        [(Union: elems)
         (let-values ([(vars notvars) (partition F? elems)])
           (unless (>= 1 (length vars))
             (exit (fail #:reason "union type includes multiple distinct type variables")))
           (with-syntax
               ([cnts (append (map t->c vars) (map t->c notvars))])
             #'(or/c . cnts)))]
        [(and t (Function: _)) (t->c/fun t)]
        [(Set: t)
         #`(set/c #,(t->c t #:kind (contract-kind-min kind chaperone-sym)))]
        [(Sequence: ts) #`(sequence/c #,@(map t->c ts))]
        [(Vector: t)
         (set-chaperone!)
         #`(vectorof #,(t->c/both t))]
        [(HeterogeneousVector: ts)
         (set-chaperone!)
         #`(vector/c #,@(map t->c/both ts))]
        [(Box: t)
         (set-chaperone!)
         #`(box/c #,(t->c/both t))]
        [(Pair: t1 t2)
         #`(cons/c #,(t->c t1) #,(t->c t2))]
        [(Promise: t)
         (set-chaperone!)
         #`(promise/c #,(t->c t))]
        [(Opaque: p?)
         #`(flat-named-contract (quote #,(syntax-e p?)) #,p?)]
        [(Continuation-Mark-Keyof: t)
         (set-chaperone!)
         #`(continuation-mark-key/c #,(t->c/both t))]
        ;; TODO: this is not quite right for case->
        [(Prompt-Tagof: s (Function: (list (arr: (list ts ...) _ _ _ _))))
         (set-chaperone!)
         #`(prompt-tag/c #,@(map t->c/both ts) #:call/cc #,(t->c/both s))]
        [(Evt: t)
         (set-chaperone!)
         #`(evt/c #,(t->c/both t))]
        ;; TODO
        [(F: v) (cond [(assoc v (vars)) => second]
                      [else (int-err "unknown var: ~a" v)])]
        ;; FIXME: should unwrap multiple Polys
        [(Poly: vs type) (=> fail)
         (define resolved (resolve type))
         (unless (Class? resolved)
           (fail))
         (define poly-ids (generate-temporaries vs))
         (define/with-syntax (all/c ...) poly-ids)
         #`(let ([all/c (new-∀/c (quote all/c))] ...)
             #,(parameterize ([vars (append (map list vs poly-ids) (vars))])
                 (t->c resolved)))]
        [(Poly: vs b)
         ;; Don't generate poly contracts for non-functions
         (define function-type?
           (let loop ([ty ty])
             (match (resolve ty)
               [(Function: _) #t]
               [(Union: elems) (andmap loop elems)]
               [(Poly: _ body) (loop body)]
               [(PolyDots: _ body) (loop body)]
               [_ #f])))
         (unless function-type?
           (exit (fail #:reason "cannot generate contract for non-function polymorphic type")))
         (if (not (from-untyped? typed-side))
             ;; in typed positions, no checking needed for the variables
             (parameterize ([vars (append (for/list ([v (in-list vs)]) (list v #'any/c)) (vars))])
               (t->c b))
             ;; in untyped positions, use `parameteric/c'
             (match-let ([(Poly-names: vs-nm _) ty])
               (with-syntax ([(v ...) (generate-temporaries vs-nm)])
                 (set-impersonator!)
                 (parameterize ([vars (append (stx-map list vs #'(v ...))
                                              (vars))])
                   #`(parametric->/c (v ...) #,(t->c b))))))]
        [(? PolyRow?) (t->c/poly-row ty)]
        [(Mu: n b)
         (match-let ([(Mu-name: n-nm _) ty])
           (with-syntax ([(n*) (generate-temporaries (list n-nm))])
             (parameterize ([vars (cons (list n #'n*) (vars))]
                            [current-contract-kind
                             (contract-kind-min kind chaperone-sym)])
               (define ctc (t->c/both b))
               #`(letrec ([n* (recursive-contract
                                #,ctc
                                #,(contract-kind->keyword
                                   (current-contract-kind)))])
                   n*))))]
        [(Instance: (? F? t))
         (t->c t)]
        [(Instance: (or (? Mu? t) (? Name? t)))
         #`(instanceof/c #,(t->c t))]
        [(Instance: (Class: _ _ fields methods _))
         (set-impersonator!)
         (match-define (list (list field-names field-types) ...) fields)
         (match-define (list (list public-names public-types) ...) methods)
         (define/with-syntax (field-name ...) field-names)
         (define/with-syntax (field-ctc ...)
           (for/list ([field-type (in-list field-types)])
             (syntax-property (t->c field-type) 'inferred-name (void))))
         (define/with-syntax (public-name ...) public-names)
         (define/with-syntax (public-ctc ...)
           (for/list ([public-type (in-list public-types)])
             (syntax-property (t->c/method public-type) 'inferred-name (void))))
         #'(object/c-strict (public-name public-ctc) ...
                            (field [field-name field-ctc] ...))]
        [(Class: row-var
                 (list (list by-name-inits by-name-init-ty _) ...)
                 (list (list field-names field-types) ...)
                 (list (list name fcn) ...)
                 (list (list augment-names aug-fcn) ...))
         (set-impersonator!)
         ;; Only apply a sealing contract if the type actually has
         ;; a row variable. In that case, look up the contract from
         ;; the vars parameter.
         (define seal/c
           (and (F? row-var) (second (assoc (F-n row-var) (vars)))))
         (define method-contract-map
           (for/hash ([n (in-list name)] [f (in-list fcn)])
             (values n (list (generate-temporary n)
                             (syntax-property (t->c/method f)
                                              'inferred-name
                                              (void))))))
         (define-values (public-names public-gens public-ctcs)
           (for/lists (_1 _2 _3) ([k+v (in-hash-pairs method-contract-map)])
             (values (car k+v) (car (cdr k+v)) (cadr (cdr k+v)))))
         (define pubment-names (set-intersect name augment-names))
         (define override-names (set-subtract name pubment-names))
         (define/with-syntax (augment-ctc ...)
           (for/list ([f (in-list aug-fcn)])
             (t->c/method f)))
         (define/with-syntax (pubment-ctc ...)
          (for/list ([name (in-list pubment-names)])
            (car (hash-ref method-contract-map name))))
         (define/with-syntax (override-ctc ...)
          (for/list ([name (in-list override-names)])
            (car (hash-ref method-contract-map name))))
         (define/with-syntax (pub-name ...) public-names)
         (define/with-syntax (pub-gen ...) public-gens)
         (define/with-syntax (pub-ctc ...) public-ctcs)
         (define/with-syntax (field-name ...) field-names)
         (define/with-syntax (field-gen ...)
           (generate-temporaries field-names))
         (define/with-syntax (field-ctc ...)
           (for/list ([field-type (in-list field-types)])
             (syntax-property (t->c field-type) 'inferred-name (void))))
         (define/with-syntax (augment-name ...) augment-names)
         (define/with-syntax (pubment-name ...) pubment-names)
         (define/with-syntax (override-name ...) override-names)
         (define/with-syntax (by-name-cnt ...)
           (for/list ([t (in-list by-name-init-ty)])
             (t->c/neg t)))
         (define/with-syntax (by-name-init ...) by-name-inits)
         (define class/c-stx
           #'(let ([pub-gen pub-ctc] ...
                   [field-gen field-ctc] ...)
               (class/c
                (init [by-name-init by-name-cnt] ...)
                (pub-name pub-gen) ...
                (inherit [pub-name pub-gen] ...)
                (super [override-name override-ctc] ...)
                (field [field-name field-gen] ...)
                (inherit-field [field-name field-gen] ...)
                (inner [augment-name augment-ctc] ...)
                (override [override-name override-ctc] ...)
                (augment [pubment-name pubment-ctc] ...))))
         (if seal/c
             #`(and/c #,seal/c #,class/c-stx)
             class/c-stx)]
        [(Value: '()) #'null?]
        [(Struct: nm par (list (fld: flds acc-ids mut?) ...) proc poly? pred?)
         (cond
           [(assf (λ (t) (type-equal? t ty)) structs-seen)
            =>
            cdr]
           [proc (exit (fail #:reason "procedural structs are not supported"))]
           [(and (equal? kind flat-sym) (ormap values mut?))
            (exit (fail #:reason
                        (~a "expected a first-order contract, but got"
                            " a struct with at least one mutable field")))]
           [poly?
            (with-syntax* ([struct-ctc (generate-temporary 'struct-ctc)])
              (define field-contracts
                (for/list ([fty (in-list flds)] [mut? (in-list mut?)])
                  (with-syntax* ([rec (generate-temporary 'rec)])
                    (define required-recursive-kind
                       (contract-kind-min kind (if mut? impersonator-sym chaperone-sym)))
                    (define t->c/mut (if mut? t->c/both t->c))
                    ;(printf "kind: ~a mut-k: ~a req-rec-kind: ~a\n" kind (if mut? impersonator-sym chaperone-sym) required-recursive-kind)
                    (parameterize ((current-contract-kind (contract-kind-min kind chaperone-sym)))
                      (let ((fld-ctc (t->c/mut fty #:seen (cons (cons ty #'rec) structs-seen)
                                               #:kind required-recursive-kind)))
                        #`(let ((rec (recursive-contract struct-ctc #,(contract-kind->keyword (current-contract-kind)))))
                            #,fld-ctc))))))
              #`(letrec ((struct-ctc (struct/c #,nm #,@field-contracts))) struct-ctc))]
           [else #`(flat-named-contract '#,(syntax-e pred?) #,pred?)])]
        [(Syntax: (Base: 'Symbol _ _ _)) #'identifier?]
        [(Syntax: t)
         #`(syntax/c #,(t->c t #:kind flat-sym))]
        [(Value: v) #`(flat-named-contract '#,v (lambda (x) (equal? x '#,v)))]
        ;; TODO Is this sound?
        [(Param: in out)
         (set-impersonator!)
         #`(parameter/c #,(t->c in) #,(t->c out))]
        [(Hashtable: k v)
         (when (equal? kind flat-sym) (exit (fail)))
         #`(hash/c #,(t->c k #:kind chaperone-sym) #,(t->c v) #:immutable 'dont-care)]
        [else
         (exit (fail #:reason "contract generation not supported for this type"))]))
    (cond [(and cache
                (not (member (cons ty typed-side) (dont-cache)))
                ;; FIXME: factor this out into a helper function
                (andmap (λ (var-pair)
                          (define var (car var-pair))
                          (not (member var (fv ty))))
                        (vars)))
           (define cache-key (cons (Type-seq ty) typed-side))
           (define id (if (and (Name? ty) (not (Name-struct? ty)))
                          (dict-ref name-mapping
                                    (cons (syntax-e (Name-id ty)) typed-side))
                          (generate-temporary)))
           (cond [(dict-has-key? cache cache-key)
                  (cadr (dict-ref cache cache-key))]
                 [else
                  (dict-set! cache (cons (Type-seq ty) typed-side) (list id ctc))
                  (define types-box (current-contract-types))
                  (set-box! types-box
                            (cons (cons (Type-seq ty) typed-side) (unbox types-box)))
                  id])]
          [else ctc])]))))

