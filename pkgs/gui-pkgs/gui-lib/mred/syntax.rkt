#lang racket/base

(require racket/contract
         (for-syntax racket/base)
         (for-syntax syntax/parse))

(provide provide-with-contract-from)

;; macro to use the contract names defined in contract.rkt
;; in providing mred exports
(define-syntax (provide-with-contract-from stx)
  (syntax-parse stx
    [(_ file:str name:id ...)
     (with-syntax ([(contract-name ...)
                    (map
                     (lambda (id)
                       (datum->syntax
                        id
                        (string->symbol (format "~a/c" (syntax-e id)))))
                     (syntax->list (syntax (name ...))))])
       #'(begin 
          (provide (except-out (all-from-out file)
                               name ...))
          (provide/contract
            [name contract-name] ...)))]))
