#lang typed/racket

;; Tests for struct: guard expressions

(require typed/rackunit)

(struct: thing ([name : (U String Symbol)])
  #:transparent
  #:guard (λ: ([name : (U String Symbol)] [type-name : Symbol])
            (cond [(string? name) (string->symbol name)]
                  [(symbol? name)
                   (symbol->string name)]
                  [else (error type-name "bad name: ~e" name)])))

(check-not-exn (λ () (thing "apple")))
(check-exn exn:fail? (λ () (thing 1/2)))
