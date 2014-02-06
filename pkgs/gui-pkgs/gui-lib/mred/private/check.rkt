(module check mzscheme
  (require mzlib/class
           (prefix wx: "kernel.rkt")
           "wx.rkt"
           "const.rkt")
  (provide (protect (all-defined)))

  (define (key-code-symbol? x)
    (and (member x '(start cancel clear shift rshift control rcontrol
                           menu pause capital prior next end home left 
                           up right down escape select print execute
                           snapshot insert help numpad0 numpad1 numpad2
                           numpad3 numpad4 numpad5 numpad6 numpad7
                           numpad8 numpad9 numpad-enter multiply add
                           separator subtract decimal divide f1 f2 f3
                           f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15
                           f16 f17 f18 f19 f20 f21 f22 f23 f24 numlock
                           scroll wheel-up wheel-down wheel-left
                           wheel-right release press))
         #t))
  
  (define (who->name who)
    (cond
     [(symbol? who) who]
     [(eq? (car who) 'method) (string->symbol (format "~a in ~a" (caddr who) (cadr who)))]
     [(eq? (car who) 'iconstructor) (iconstructor-name (cadr who))]
     [else (constructor-name (cadr who))]))

  (define (label-string? s)
    (and (string? s) 
	 (let ([l (string-length s)])
	   (and l
		(<= 0 l 200)))))

  (define (constructor-name who)
    (string->symbol (format "initialization for ~a%" who)))

  (define (iconstructor-name who)
    (string->symbol (format "initialization for a class that implements ~a<%>" who)))

  (define (check-orientation cwho l)
    (void))

  (define (check-container-ready cwho p)
    (when p
      (let ([wx (mred->wx p)])
	(unless wx
	  (raise-arguments-error (who->name cwho)
                                 "container is not yet fully initialized" 
                                 "container" p)))))

  (define (check-instance who class class-name false-ok? v)
    (void))

  (define (check-string/false who str)
    (void))

  (define (check-path who str)
    (void))

  (define (check-path/false who str)
    (void))

  (define (check-string who str)
    (void))

  (define (check-label-string who str)
    (void))

  (define (check-label-string/false who str)
    (void))

  (define (check-char/false who c)
    (void))

  (define (check-callback who callback)
    (unless (and (procedure? callback)
		 (procedure-arity-includes? callback 2))
      (raise-argument-error (who->name who) "(procedure-arity-includes/c 2)" callback)))

  (define (check-callback1 who callback)
    (unless (and (procedure? callback)
		 (procedure-arity-includes? callback 1))
      (raise-argument-error (who->name who) "(procedure-arity-includes/c 2)" callback)))

  (define (check-bounded-integer min max false-ok?)
    (lambda (who range)
      (void)))
  
  (define check-slider-integer (check-bounded-integer (- GAUGE-MAX) GAUGE-MAX #f))

  (define check-margin-integer (check-bounded-integer 0 1000 #f))

  (define check-gauge-integer (check-bounded-integer 1 GAUGE-MAX #f))
  (define check-gauge-range-integer (check-bounded-integer 0 GAUGE-MAX #f))

  (define (check-wheel-step cwho wheel-step)
    (void))

  (define (check-fraction who x)
    (void))

  (define (-check-non-negative-integer who i false-ok?)
    (void))

  (define (check-non-negative-integer who i)
    (-check-non-negative-integer who i #f))

  (define (check-non-negative-integer/false who i)
    (-check-non-negative-integer who i #t))

  (define check-init-dimension (check-bounded-integer 0 WIN-SIZE-MAX #t))
  (define check-dimension (check-bounded-integer 0 WIN-SIZE-MAX #f))
  (define check-position (check-bounded-integer (- WIN-SIZE-MAX) WIN-SIZE-MAX #f))
  (define check-init-position (check-bounded-integer (- WIN-SIZE-MAX) WIN-SIZE-MAX #t))

  (define (check-label-string-or-bitmap who label)
    (void))

  (define (check-label-string-or-bitmap-or-both who label)
    (void))

  (define (check-label-string-or-bitmap/false who label)
    (void))

  (define (check-label-string/bitmap/iconsym who label)
    (void))

  (define (check-font who f)
    (void))

  (define (check-style who reqd other-allowed style)
    (unless (and (list? style) (andmap symbol? style))
      (raise-argument-error (who->name who) "(listof symbol?)" style))
    (when reqd
      (unless (ormap (lambda (i) (memq i reqd)) style)
        (letrec ([or-together (lambda (l)
                                (if (= (length l) 2)
                                    (format "~e or ~e" (car l) (cadr l))
                                    (let loop ([l l])
                                      (if (null? (cdr l))
                                          (format "or ~e" (car l))
                                          (format "~e, ~a" (car l) (loop (cdr l)))))))])
          (raise-arguments-error (who->name who)
                                 (string-append
                                  "missing a required option in given style list\n"
                                  "  must include: " (or-together reqd))
                                 "given" style))))
    (if (and (not reqd) (null? other-allowed))
	(unless (null? style)
	  (raise-arguments-error (who->name who) 
                                 "empty style list required" 
                                 "given" style))
	(let* ([l (append (or reqd null) other-allowed)]
	       [bad (ormap (lambda (x) (if (memq x l) #f x)) style)])
	  (when bad
	    (raise-arguments-error (who->name who) 
                                   "invalid symbol in given style list"
                                   "invalid symbol" bad
                                   "given" style))
	  (let loop ([l style])
	    (unless (null? l)
	      (when (memq (car l) (cdr l))
		(raise-arguments-error (who->name who) 
                                       "duplicate style in given style list"
                                       "duplicate" (car l)
                                       "given" style))
	      (loop (cdr l))))))))
