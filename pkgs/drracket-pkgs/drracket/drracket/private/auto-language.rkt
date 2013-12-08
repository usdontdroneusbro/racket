#lang typed/racket/no-check

(require typed/framework
	 typed/racket/gui
         #;framework
         #;racket/gui
         #;racket/class)

(provide pick-new-language looks-like-module?)

(define-type Language:Language%
  (Class [capability-value (Symbol -> Any)]
         [config-panel
          ((Instance Panel%) ->
           (case-> (-> Any) (Any -> Void)))]
         [create-executable
          (Any (U (Instance Dialog%) (Instance Frame%)) String -> Void)]
         [default-settings (-> Any)]
         [default-settings? (Any -> Boolean)]
         [first-opened (Any -> Void)]
         [front-end/complete-program
          (Input-Port Any -> (-> (U Sexp Syntax EOF)))]
         [front-end/finished-complete-program (Any -> Any)]
         [front-end/interaction
          (Input-Port Any -> (-> (U Sexp Syntax EOF)))]
         [get-comment-character (-> (Values String Char))]
         [get-language-name (-> String)]
         [get-language-numbers (-> (Pairof Number (Listof Number)))]
         [get-language-position (-> (Pairof String (Listof String)))]
         [get-language-url (-> (Option String))]
         [get-metadata (Symbol Any -> String)]
         [get-metadata-lines (-> Natural)]
         [get-one-line-summary (-> String)]
         [get-reader-module (-> (Option Sexp))]
         [get-style-delta
          (-> (U #f (Instance Style-Delta%)
                 (Listof (List (Instance Style-Delta%) Number Number))))]
         [extra-repl-information (Any Output-Port -> Void)]
         [marshall-settings (Any -> Any)]
         [metadata->settings (String -> Any)]
         [on-execute (Any ((-> Any) -> Any) -> Any)]
         [render-value (Any Any Output-Port -> Void)]
         [render-value/format
          (Any Any Output-Port (U 'infinity Number) -> Void)]
         [unmarshall-settings (Any -> Any)]))

(: pick-new-language : (Instance Text:File<%>)
                       (Listof (Instance Language:Language%))
                       (U #f (Instance Language:Language%)) Any
                       -> 
                       (values (U #f (Instance Language:Language%)) Any))
(define (pick-new-language text all-languages module-language module-language-settings)
  (with-handlers ([exn:fail:read? (λ (x) (values #f #f))])
    (let: ([found-language? : (U #f (Instance Language:Language%)) #f]
           [settings : Any #f])
      
      (for-each
       (λ: ([lang : (Instance Language:Language%)])
         (let ([lang-spec (send lang get-reader-module)])
           (when lang-spec
             (let* ([lines (send lang get-metadata-lines)]
                    [str (send text get-text
                               0
                               (send text paragraph-end-position
                                     ;; FIXME: this assumption is bogus and
                                     ;;        so is the documented type
                                     (assert (- lines 1) positive?)))]
                    [sp (open-input-string str)])
               (when (regexp-match #rx"#reader" sp)
                 (let ([spec-in-file (read sp)])
                   (when (equal? lang-spec spec-in-file)
                     (set! found-language? lang)
                     (set! settings (send lang metadata->settings str))
                     (send text while-unlocked
                           (λ () 
                             (send text delete 0 (send text paragraph-start-position lines)))))))))))
       all-languages)
      
      ;; check to see if it looks like the module language.
      (unless found-language?
        (when module-language
          (when (looks-like-module? text)
            (set! found-language? module-language)
            (set! settings module-language-settings))))
      
      (values found-language?
              settings))))

(: looks-like-module? ((Instance Text%) -> Boolean))
(define (looks-like-module? text)
  (or (looks-like-new-module-style? text)
      (looks-like-old-module-style? text)
      (with-handlers ((exn:fail? (λ (x) #f)))
        (procedure?
         (read-language (open-input-text-editor text 0 'end (λ (x) x) text #f) 
                        (λ () #f)))))) 

(: looks-like-old-module-style? ((Instance Text%) -> Boolean))
(define (looks-like-old-module-style? text)
  (with-handlers ((exn:fail:read? (λ (x) #f)))
    (let* ([tp (open-input-text-editor text 0 'end (lambda (s) s) text #t)]
           ;; FIXME: this needs to use call-with-default-reading-parameterization
           [r1 (parameterize ([read-accept-reader #f]) (read tp))]
           [r2 (parameterize ([read-accept-reader #f]) (read tp))])
      (and (eof-object? r2)
           (pair? r1)
           (eq? (car r1) 'module)))))

(: looks-like-new-module-style? ((Instance Text%) -> Boolean))
(define (looks-like-new-module-style? text)
  (let* ([tp (open-input-text-editor text 0 'end (lambda (s) s) text #t)]
         [l1 (with-handlers ([exn:fail? (lambda (exn) eof)])
               ;; If tp contains a snip, read-line fails.
               (read-line tp))])
    (and (string? l1)
         (regexp-match? #rx"#lang .*$" l1))))
