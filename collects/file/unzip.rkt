#lang racket
(module io racket
  (require racket/async-channel
           racket/file
           (except-in racket/contract case->)
           (only-in (lib "contract.ss") ->r case->))
  
  (define optional/c (lambda (contract) (or/c contract false/c)))
  
  ;; ===========================================================================
  ;; UTILITIES
  ;; ===========================================================================
  
  ;; with-output-to-string
  ;; captures all standard output in a string
  #;(define-syntax with-output-to-string
    (syntax-rules ()
      [(_ e1 e2 ...)
       (let ([p (open-output-string)])
         (parameterize ([current-output-port p])
           e1 e2 ...
           (get-output-string p)))]))
  
  ;; with-temporary-file
  ;; creates a temporary file and automatically deletes it when finished
  (define-syntax with-temporary-file
    (syntax-rules ()
      [(_ file (args ...) e1 e2 ...)
       (let ([file (make-temporary-file args ...)])
         (dynamic-wind
          void
          (lambda () e1 e2 ...)
          (lambda ()
            (when (file-exists? file)
              (delete-file file)))))]))
  
  ;; seekable-port? : port -> boolean
  (define (seekable-port? port)
    (and (file-stream-port? port)
         (with-handlers ([exn:fail:filesystem? (lambda (exn) #f)]
                         [exn:fail:contract? (lambda (exn) #f)])
           (and (file-position port (file-position port))
                #t))))
  
  ;; This procedure must create a separate thread because clients may read
  ;; from the input port lazily, but the transform procedure eagerly reads
  ;; all input from the underlying port.
  
  ;; However, if an exception is raised by the transformer, that exception
  ;; should be propagated to the client thread. So we catch the exception,
  ;; send it to a shared channel, and hang onto it to be rereaised in the
  ;; client thread when the client reaches that point in the input.
  
  (define-struct filter-error (value))
  
  (define (make-filter-input-port/debug transform in close-orig? history)
    (let-values ([(pipe-input pipe-output) (make-pipe)])
      (let* ([chan (make-channel)]
             [handler (lambda (exn)
                        (close-output-port pipe-output)
                        (channel-put chan (make-filter-error exn))
                        (when history
                          (async-channel-put history `(exn transformer ,exn))))])
        (thread (lambda ()
                  (with-handlers ([(lambda (exn) #t) handler])
                    (transform in pipe-output)
                    (when history
                      (async-channel-put history 'done-transform))
                    (close-output-port pipe-output))))
        (make-input-port (object-name in)
                         (lambda (buffer)
                           (let ([count (read-bytes-avail!* buffer pipe-input)])
                             (cond
                               [(and (eof-object? count) (channel-try-get chan))
                                => (lambda (err)
                                     (raise (filter-error-value err)))]
                               [else count])))
                         #f
                         (lambda ()
                           (close-input-port pipe-input)
                           (when close-orig? (close-input-port in)))))))
  
  ;; make-filter-input-port : (input-port output-port -> any) [input-port] [boolean] -> input-port
  (define make-filter-input-port
    (lambda (transform [in (current-input-port)] [close-orig? #f])
      (make-filter-input-port/debug transform in close-orig? #f)))
  
  ;; TODO: make-filter-output-port
  
  ;  (define make-filter-output-port
  ;    (lambda (transform [out (current-output-port)])
  ;      ...))
  
  (define exact-integer/c
    (and/c integer? exact?))
  
  (define (log-base-2 n)
    (/ (log n) (log 2)))
  
  (define (next-power-of-2 n)
    (inexact->exact
     (expt 2 (ceiling (log-base-2 n)))))
  
  (define (next-multiple-of-8 n)
    (inexact->exact
     (* 8 (ceiling (/ n 8)))))
  
  ;; ===========================================================================
  ;; BIT TWIDDLING
  ;; ===========================================================================
  
  ;; ones-mask : nat -> exact-integer
  ;; creates an integer of n bytes all set to #xff
  (define (ones-mask n)
    (sub1 (arithmetic-shift 1 (* 8 n))))
  
  ;; bit-set? : nat exact-integer -> boolean
  ;; determines whether the given bit (zero-indexed) is enabled
  (define (bit-set? i n)
    (not (zero? (bitwise-and n (arithmetic-shift 1 i)))))
  
  ;; stretch-bytes : bytes nat [boolean] [byte] -> bytes
  (define stretch-bytes
    (lambda (bytes len [big-endian? (system-big-endian?)] [fill-byte 0])
      (let ([real-len (bytes-length bytes)])
        (cond
          [(= real-len len) bytes]
          [(< real-len len)
           (let ([extra (make-bytes (- len real-len) fill-byte)])
             (if big-endian?
                 (bytes-append extra bytes)
                 (bytes-append bytes extra)))]
          [else (error 'stretch-bytes "too many bytes: ~a" real-len)]))))
  
  ;; negative-bytes? : bytes boolean -> boolean
  ;; tests if a byte string represents a negative two's-complement integer
  (define (negative-bytes? bytes start-k end-k big-endian?)
    (bit-set? 7 (bytes-ref bytes
                           (if big-endian? start-k (sub1 end-k)))))
  
  ;; bytes->integer : bytes boolean [boolean] [nat] [nat] -> exact-integer
  (define bytes->integer
    (lambda (bytes signed? [big-endian? (system-big-endian?)] [start-k 0] [end-k (bytes-length bytes)])
      (let ([unsigned (bytes->unsigned bytes start-k end-k big-endian?)])
        (if (and signed? (negative-bytes? bytes start-k end-k big-endian?))
            (- (add1 (bitwise-xor unsigned (ones-mask (- end-k start-k)))))
            unsigned))))
  
  ;; bytes->unsigned : bytes nat nat boolean -> nat
  ;; interprets a byte string as an unsigned integer
  (define (bytes->unsigned bytes start-k end-k big-endian?)
    (let* ([end (bytes-length bytes)]
           [goal (if big-endian? (sub1 start-k) end-k)]
           [step (if big-endian? sub1 add1)])
      (let loop ([i (if big-endian? (sub1 end-k) start-k)] [n 0] [mult 1])
        (if (= i goal)
            n
            (loop (step i)
                  (+ n (* mult (bytes-ref bytes i)))
                  (* mult 256))))))
  
  ;; fits? : exact-integer nat boolean -> boolean
  ;; determines whether num fits in n-bytes bytes
  (define (fits? num n-bytes signed?)
    (if signed?
        (or (and (negative? num)
                 (bit-set? (sub1 (* n-bytes 8)) num)
                 ;; TODO: is that right?
                 (< (- num) (arithmetic-shift 1 (* 8 n-bytes))))
            (and (not (negative? num))
                 (not (bit-set? (sub1 (* n-bytes 8)) num))
                 (< num (arithmetic-shift 1 (* 8 n-bytes)))))
        (and (not (negative? num))
             (< num (arithmetic-shift 1 (* 8 n-bytes))))))
  
  ;; minimum-bytes : exact-integer -> nat
  ;; the minimum number of bytes needed to encode n in two's-complement
  (define (minimum-bytes n)
    (let ([bit-count (next-multiple-of-8
                      (ceiling (log-base-2 (add1 (abs n)))))])
      (next-power-of-2
       (if (or (and (negative? n) (not (bit-set? (sub1 bit-count) n)))
               (and (not (negative? n)) (bit-set? (sub1 bit-count) n)))
           (add1 (/ bit-count 8))
           (/ bit-count 8)))))
  
  ;; integer->bytes : exact-integer boolean [(optional nat)] [boolean] -> bytes
  (define integer->bytes
    (lambda (n signed? [big-endian? (system-big-endian?)] [size-n #f])
      (when (and size-n (not (fits? n size-n signed?)))
        (raise
         (make-exn:fail:contract
          (format
           "integer-bytes: integer does not fit into ~a signed byte~a: ~a"
           size-n (if (= size-n 1) "" "s") n))))
      (let* ([size-n (or size-n (next-power-of-2 (minimum-bytes n)))]
             [bytes (make-bytes size-n (if (negative? n) 255 0))]
             [start-k (if big-endian? (sub1 size-n) 0)]
             [end-k (if big-endian? -1 size-n)]
             [step (if big-endian? sub1 add1)])
        (let loop ([n n] [i start-k])
          (if (= i end-k)
              bytes
              (begin
                (bytes-set! bytes i (bitwise-and n #xff))
                (loop (arithmetic-shift n -8) (step i))))))))
  
  ;; TODO: integer->bytes!
  
  ;; ===========================================================================
  ;; INPUT
  ;; ===========================================================================
  
  ;; skip-bytes : nat [input-port] -> any
  ;; skips the given number of bytes from an input port
  (define skip-bytes
    (lambda (k [in (current-input-port)])
      (read-bytes k in)
      (void)))
  
  ;; read-c-string : [input-port] -> bytes
  ;; reads a byte string until reaching #\nul or EOF
  (define read-c-string
    (lambda ([in (current-input-port)])
      (let loop ([result null])
        (let ([b (read-byte in)])
          (if (or (eof-object? b) (zero? b))
              (list->bytes (reverse result))
              (loop (cons b result)))))))
  
  ;; read-c-string! : bytes [input-port] [nat] [nat] -> (union eof nat)
  ;; reads a byte string destructively until reaching #\nul or EOF
  (define read-c-string!
    (lambda (b [in (current-input-port)] [s-k 0] [e-k (bytes-length b)])
      (let loop ([read 0] [i s-k])
        (let ([byte (read-byte in)])
          (cond
            [(and (zero? read) (eof-object? byte)) byte]
            [(or (eof-object? byte)
                 (zero? byte)
                 (= i e-k))
             read]
            [else
             (bytes-set! i byte)
             (loop (add1 read) (add1 i))])))))
  
  ;; read-integer : nat boolean [input-port] [boolean] -> exact-integer
  ;; reads a two's-complement integer from an input port
  (define read-integer
    (lambda (k signed? [in (current-input-port)] [big-endian? (system-big-endian?)])
      (bytes->integer (read-bytes k in) signed? big-endian?)))
  
  ;; peek-integer : nat boolean [input-port] [boolean] -> exact-integer
  ;; reads a two's-complement integer from an input port without advancing
  (define peek-integer
    (lambda (k signed? [in (current-input-port)] [big-endian? (system-big-endian?)])
      (bytes->integer (peek-bytes k 0 in) signed? big-endian?)))
  
  ;; read-chars : nat [input-port] -> (listof char)
  ;; reads a fixed number of characters from an input port
  (define read-chars
    (lambda (k [in (current-input-port)])
      (build-list k (lambda (i) (read-char in)))))
  
  ;; peek-chars : nat [input-port] -> (listof char)
  ;; reads a fixed number of characters from an input port without advancing
  (define peek-chars
    (lambda (k [in (current-input-port)])
      (string->list (peek-string k 0 in))))
  
  ;; read-lines : [input-port mode-symbol] -> (listof string)
  (define read-lines
    (lambda ([in (current-input-port)] [mode-symbol 'linefeed])
      (let loop ([result '()])
        (let ([line (read-line in mode-symbol)])
          (if (eof-object? line)
              (reverse result)
              (loop (cons line result)))))))
  
  ;; ===========================================================================
  ;; OUTPUT
  ;; ===========================================================================
  
  ;; write-c-string : bytes [output-port] [nat] [nat] -> any
  ;; writes a C-style (#\nul-terminated) string to an output port
  (define write-c-string
    (lambda (b [out (current-output-port)] [s-k 0] [e-k (bytes-length b)])
      (write-bytes b out s-k e-k)
      (write-byte 0 out)))
  
  ;; write-integer : exact-integer boolean [output-port] [boolean] [(optional nat)] -> any
  ;; writes the binary representation of an integer to an output port
  (define write-integer
    (lambda (n signed? [out (current-output-port)] [big-endian? (system-big-endian?)] [size-n #f])
      (let ([bytes (integer->bytes n signed? big-endian? size-n)])
        (write-bytes bytes out))))
  
  ;; write-chars : (listof char) [output-port] -> any
  ;; writes a sequence of characters to an output port
  (define write-chars
    (lambda (chars [out (current-output-port)])
      (for-each (lambda (c)
                  (write-char c out))
                chars)))
  
  ;; write-lines : (listof string) [output-port] -> any
  ;; writes a sequence of strings to an output port
  (define write-lines
    (lambda (lines [out (current-output-port)])
      (for-each (lambda (line)
                  (display line out)
                  (newline out))
                lines)))
  
  (provide with-output-to-string with-temporary-file)
  
  (define mode-symbol/c
    (symbols 'linefeed 'return 'return-linefeed 'any 'any-one))
  
  (provide/contract
   [make-filter-input-port (((input-port? output-port? . -> . any))
                            (input-port?)
                            . ->* .
                            input-port?)]
   [stretch-bytes (case->
                   (([bytes bytes?]
                     [len (and/c natural-number/c (>=/c (bytes-length bytes)))])
                    . ->r .
                    bytes?)
                   (([bytes bytes?]
                     [len (and/c natural-number/c (>=/c (bytes-length bytes)))]
                     [big-endian? boolean?])
                    . ->r .
                    bytes?)
                   (([bytes bytes?]
                     [len (and/c natural-number/c (>=/c (bytes-length bytes)))]
                     [big-endian? boolean?]
                     [fill-byte byte?])
                    . ->r .
                    bytes?))]
   [bit-set? (natural-number/c exact-integer/c . -> . boolean?)]
   [bytes->integer ((bytes? boolean?)
                    (boolean? natural-number/c natural-number/c)
                    . ->* .
                    exact-integer/c)]
   [integer->bytes ((exact-integer/c boolean?)
                    (boolean? (optional/c natural-number/c))
                    . ->* .
                    bytes?)]
   [seekable-port? (port? . -> . boolean?)]
   [skip-bytes ((natural-number/c)
                (input-port?)
                . ->* .
                any)]
   [read-chars ((natural-number/c)
                (input-port?)
                . ->* .
                (listof char?))]
   [peek-chars ((natural-number/c)
                (input-port?)
                . ->* .
                (listof char?))]
   [read-c-string (()
                   (input-port?)
                   . ->* .
                   bytes?)]
   [read-c-string! ((bytes?)
                    (input-port? natural-number/c natural-number/c)
                    . ->* .
                    (or/c eof-object? natural-number/c))]
   [read-integer ((natural-number/c boolean?)
                  (input-port? boolean?)
                  . ->* .
                  exact-integer/c)]
   [read-lines (() (input-port? mode-symbol/c) . ->* . (listof string?))]
   [peek-integer ((natural-number/c boolean?)
                  (input-port? boolean?)
                  . ->* .
                  exact-integer/c)]
   [write-chars (((listof char?))
                 (input-port?)
                 . ->* .
                 any)]
   [write-integer ((exact-integer/c boolean?)
                   (output-port? boolean? (optional/c natural-number/c))
                   . ->* .
                   any)]
   [write-c-string ((bytes?)
                    (output-port? natural-number/c natural-number/c)
                    . ->* .
                    any)]
   [write-lines (((listof string?)) (output-port?) . ->* . any)]))

(module zip-constants racket
  (provide (all-defined-out))
  ;; PKZIP specification:
  ;; http://www.pkware.com/company/standards/appnote/
  
  (define *local-file-header*                      #x04034b50)
  (define *archive-extra-record*                   #x08064b50)
  (define *central-file-header*                    #x02014b50)
  (define *digital-signature*                      #x05054b50)
  (define *zip64-end-of-central-directory-record*  #x06064b50)
  (define *zip64-end-of-central-directory-locator* #x07064b50)
  (define *end-of-central-directory-record*        #x06054b50)
  
  (define *system*
    (case (system-type)
      [(unix oskit) 3]
      [(windows) 0]
      [(macos) 7]
      [(macosx) 19]))
  (define *os-specific-separator-regexp*
    (case (system-type)
      [(unix macosx oskit) #rx"/"]
      [(windows) #rx"\\\\"]
      [(macos) #rx":"]))
  
  ;; The PKZIP specification includes an entry in the central directory for
  ;; an entry's "external file attributes," which for standard ZIP files is
  ;; the MS-DOS (i.e., FAT) directory attribute byte.
  
  ;; See http://en.wikipedia.org/wiki/FAT32 for a description of the directory
  ;; attribute byte.
  
  (define *msdos-read-only* #b00000001)
  (define *msdos-hidden*    #b00000010)
  (define *msdos-system*    #b00000100)
  (define *msdos-volume*    #b00001000)
  (define *msdos-directory* #b00010000)
  (define *msdos-archive*   #b00100000)
  
  (define *external-attributes:file* 0)
  (define *external-attributes:directory* *msdos-directory*))

(module file racket
  (provide dirname)
  ;; dirname : path -> path
  (define (dirname p)
    (let-values ([(parent name must-be-dir?) (split-path p)])
      (cond
        [(not parent) (build-path p)]
        [(eq? parent 'relative) (build-path 'same)]
        [else parent]))))

(require (submod "." zip-constants)
         (submod "." io)
         (submod "." file)
         racket/file
         racket/port
         file/gunzip)

;; ===========================================================================
;; DATATYPES AND UTILITIES
;; ===========================================================================

;; (alistof bytes zip-entry)
(define-struct zip-directory (contents))

;; nat * boolean
(define-struct zip-entry (offset dir?))

(define-struct (exn:fail:unzip exn:fail) ())
(define-struct (exn:fail:unzip:no-such-entry exn:fail:unzip) (entry))

(define (raise-unzip-error message)
  (raise
   (make-exn:fail:unzip (string->immutable-string (format "unzip: ~a" message))
                        (current-continuation-marks))))

(define (raise-entry-not-found entry)
  (raise
   (make-exn:fail:unzip:no-such-entry
    (string->immutable-string
     (format "unzip: entry not found: \"~a\"" (bytes->string/latin-1 entry)))
    (current-continuation-marks)
    entry)))

;; zip-directory-entries : zip-directory -> (listof bytes)
(define (zip-directory-entries zipdir)
  (map car (zip-directory-contents zipdir)))

;; zip-directory-lookup : bytes zip-directory -> (option zip-entry)
(define (zip-directory-lookup entry zipdir)
  (let loop ([contents (zip-directory-contents zipdir)])
    (cond
      [(null? contents) #f]
      [(or (bytes=? entry (caar contents))
           (bytes=? (bytes-append entry #"/") (caar contents)))
       (cdar contents)]
      [else (loop (cdr contents))])))

;; zip-directory-contains? : (union string path bytes) zip-directory -> boolean
(define (zip-directory-contains? entry zipdir)
  (if (bytes? entry)
      (and (zip-directory-lookup entry zipdir) #t)
      (zip-directory-contains? (path->zip-path entry) zipdir)))

;; matches-directory? : bytes bytes -> boolean
(define (bytes-prefix? dirname entry-name)
  (let ([dirname-len (bytes-length dirname)]
        [entry-name-len (bytes-length entry-name)])
    (and (>= entry-name-len dirname-len)
         (bytes=? (subbytes entry-name 0 dirname-len) dirname))))

;; zip-directory-includes-directory? : (union string path bytes) zip-directory -> boolean
(define (zip-directory-includes-directory? dirname zipdir)
  (if (bytes? dirname)
      (ormap (lambda (pair)
               (bytes-prefix? dirname (car pair)))
             (zip-directory-contents zipdir))
      (zip-directory-includes-directory? (path->zip-path dirname) zipdir)))

;; path->zip-path : (union path string) -> bytes
(define (path->zip-path p)
  (if (path? p)
      (bytes->zip-bytes (path->bytes p))
      (bytes->zip-bytes (string->bytes/latin-1 p))))

(define (bytes->zip-bytes b)
  (regexp-replace* *os-specific-separator-regexp* b #"/"))

;; ===========================================================================
;; UNZIPPING ENGINE
;; ===========================================================================

(define *slash-byte* (char->integer #\/))

(define (directory-entry? name)
  (= (bytes-ref name (sub1 (bytes-length name))) *slash-byte*))

;; unzip-one-entry : input-port (bytes boolean input-port -> a) -> a
(define (unzip-one-entry in read-entry)
  (let ([read-int (lambda (count) (read-integer count #f in #f))])
    (let* ([signature            (read-int 4)]
           [version              (read-bytes 2 in)]
           [bits                 (read-int 2)]
           [compression          (read-int 2)]
           [time                 (read-int 2)]
           [date                 (read-int 2)]
           [crc-32               (read-int 4)]
           [compressed           (read-int 4)]
           [uncompressed         (read-int 4)]
           [filename-length      (read-int 2)]
           [extra-length         (read-int 2)]
           [filename             (read-bytes filename-length in)]
           [extra                (read-bytes extra-length in)])
      (let* ([mark (file-position in)]
             [dir? (directory-entry? filename)]
             ;; TODO: when Matthew fixes inflate.ss, this should go
             ;;       back to the commented code
             [in0 in])
        ;; appnote VI-J : if bit 3 is set, the fields crc-32,
        ;; compressed size, and uncompressed size are set to
        ;; zero in the local header
        ;               [in0 (if (bit-set? 3 bits)
        ;                        in
        ;                        (make-limited-input-port in compressed #f))])
        (dynamic-wind
         void
         (lambda ()
           (read-entry filename
                       dir?
                       (if (zero? compression)
                           in0
                           (make-filter-input-port inflate in0))))
         (lambda ()
           ;; appnote VI-C : if bit 3 is set, then the file data
           ;; is immediately followed by a data descriptor
           (if (bit-set? 3 bits)
               (skip-bytes 12 in)
               (file-position in (+ mark compressed)))))))))

;; find-central-directory : input-port nat -> nat nat nat
(define (find-central-directory in size)
  (let loop ([pos (- size 18)])
    (unless (positive? pos)
      (raise-unzip-error "no central directory"))
    (file-position in pos)
    (let* ([read-int (lambda (count) (read-integer count #f in #f))]
           [signature (read-int 4)])
      (if (= signature *end-of-central-directory-record*)
          (let ([disk-number       (read-int 2)]
                [directory-disk    (read-int 2)]
                [disk-entries      (read-int 2)]
                [entry-count       (read-int 2)]
                [directory-length  (read-int 4)]
                [directory-offset  (read-int 4)]
                [comment-length    (read-int 2)])
            (if (= (- size (file-position in)) comment-length)
                (values directory-offset directory-length entry-count)
                (loop (sub1 pos))))
          (loop (sub1 pos))))))

;; read-central-directory : input-port nat -> (alistof bytes zip-entry)
(define (read-central-directory in size)
  (let-values ([(offset length count) (find-central-directory in size)])
    (file-position in offset)
    (build-list count
                (lambda (i)
                  (let* ([read-int (lambda (count)
                                     (read-integer count #f in #f))]
                         [signature (read-int 4)])
                    (unless (= signature *central-file-header*)
                      (raise-unzip-error
                       (format "bad central file header signature: ~a"
                               signature)))
                    (let ([version             (read-int 2)]
                          [required            (read-int 2)]
                          [bits                (read-int 2)]
                          [compression         (read-int 2)]
                          [time                (read-int 2)]
                          [date                (read-int 2)]
                          [crc-32              (read-int 4)]
                          [compressed          (read-int 4)]
                          [uncompressed        (read-int 4)]
                          [filename-length     (read-int 2)]
                          [extra-length        (read-int 2)]
                          [comment-length      (read-int 2)]
                          [disk-number         (read-int 2)]
                          [internal-attributes (read-int 2)]
                          [external-attributes (read-int 4)]
                          [relative-offset     (read-int 4)])
                      (let* ([filename (read-bytes filename-length)]
                             [dir? (directory-entry? filename)])
                        (skip-bytes (+ extra-length comment-length) in)
                        (cons filename (make-zip-entry relative-offset dir?)))))))))

;; ===========================================================================
;; FRONT END
;; ===========================================================================

;; unzip : [input-port (bytes boolean input-port -> any)] -> any
(define unzip
  (lambda ([in (current-input-port)] [read-entry *default-entry-reader*])
    (when (= (peek-integer 4 #f in #f) *local-file-header*)
      (unzip-one-entry in read-entry)
      (unzip in read-entry))))

;; read-zip-directory : (union string path) -> zip-directory
(define (read-zip-directory path)
  (make-zip-directory
   (with-input-from-file path
     (lambda ()
       (read-central-directory (current-input-port)
                               (file-size path))))))

;; unzip-entry : (union string path) zip-directory bytes [(bytes boolean input-port -> a)] -> a
(define unzip-entry
  (lambda (path dir entry-name [read-entry *default-entry-reader*])
    (cond
      [(zip-directory-lookup entry-name dir)
       => (lambda (entry)
            (with-input-from-file path
              (lambda ()
                (file-position (current-input-port) (zip-entry-offset entry))
                (unzip-one-entry (current-input-port) read-entry))))]
      [else (raise-entry-not-found entry-name)])))

;; ===========================================================================
;; ENTRY PARSERS
;; ===========================================================================

;; make-filesystem-entry-reader : [output-flag] -> (bytes boolean input-port -> any)
(define make-filesystem-entry-reader
  (lambda ([flag 'error])
    (lambda (name dir? in)
      (let ([path (bytes->path name)])
        (if dir?
            (unless (directory-exists? path)
              (make-directory* path))
            (let ([parent (dirname path)])
              (unless (directory-exists? parent)
                (make-directory* parent))
              (with-output-to-file path
                (lambda ()
                  (copy-port in (current-output-port)))
                flag)))))))

(define *default-entry-reader* (make-filesystem-entry-reader))

;; make-piped-entry-reader : output-port -> (bytes boolean input-port -> any)
(define (make-piped-entry-reader out)
  (lambda (name dir? in)
    (unless dir?
      (copy-port in out))))

(define output-flag/c
  (symbols 'error 'replace 'truncate 'truncate/replace 'append 'update))


(provide/contract
 [exn:fail:unzip? (any/c . -> . boolean?)]
 [exn:fail:unzip:no-such-entry? (any/c . -> . boolean?)]
 [make-exn:fail:unzip (string? continuation-mark-set? . -> . exn:fail:unzip?)]
 [make-exn:fail:unzip:no-such-entry (string? continuation-mark-set? bytes? . -> . exn:fail:unzip:no-such-entry?)]
 [exn:fail:unzip:no-such-entry-entry (exn:fail:unzip:no-such-entry? . -> . bytes?)]
 [zip-directory? (any/c . -> . boolean?)]
 [zip-directory-entries (zip-directory? . -> . (listof bytes?))]
 [zip-directory-contains? ((or/c string? path? bytes?) zip-directory? . -> . boolean?)]
 [zip-directory-includes-directory? ((or/c string? path? bytes?) zip-directory? . -> . boolean?)])

(provide/contract
 [output-flag/c contract?])

(provide/contract
 [unzip (() (input-port? (bytes? boolean? input-port? . -> . any)) . ->* . any)]
 [read-zip-directory ((or/c string? path?) . -> . zip-directory?)]
 [unzip-entry (((or/c string? path?) zip-directory? bytes?)
               ((bytes? boolean? input-port? . -> . any))
               . ->* .
               any)]
 [path->zip-path ((or/c string? path?) . -> . bytes?)]
 [make-filesystem-entry-reader (() (output-flag/c) . ->* . (bytes? boolean? input-port? . -> . any))]
 [make-piped-entry-reader (output-port? . -> . (bytes? boolean? input-port? . -> . any))])
