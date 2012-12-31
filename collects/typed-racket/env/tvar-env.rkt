#lang racket/base

;; this implements the Delta environment from the TOPLAS paper
;; (as well as every other paper on System F)

;; this environment maps type variables names (symbols)
;; to types representing the type variable
;;
;; The mapped-to type is used to distinguish type variables bound
;; at different scopes

(require "tvar-env-helper.rkt")

(provide initial-tvar-env
         current-tvars
         extend-tvars
         extend-tvars/new
         bound-tvar?
         lookup-tvar)

(define-tvar-ids
  initial-tvar-env
  current-tvars
  extend-tvars
  extend-tvars/new
  bound-tvar?
  lookup-tvar
  extend
  extend/many)

