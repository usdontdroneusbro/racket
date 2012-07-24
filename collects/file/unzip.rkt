#lang racket

;; Library for unzipping zip archives

(require racket/async-channel
         racket/file
         racket/port
         file/gunzip)

(provide
 (contract-out
  [exn:fail:unzip? (any/c . -> . boolean?)]
  [exn:fail:unzip:no-such-entry? (any/c . -> . boolean?)]
  [make-exn:fail:unzip
   (string? continuation-mark-set? . -> . exn:fail:unzip?)]
  [make-exn:fail:unzip:no-such-entry
   (string? continuation-mark-set? bytes? . -> . exn:fail:unzip:no-such-entry?)]
  [exn:fail:unzip:no-such-entry-entry
   (exn:fail:unzip:no-such-entry? . -> . bytes?)]
  [zip-directory? (any/c . -> . boolean?)]
  [zip-directory-entries (zip-directory? . -> . (listof bytes?))]
  [zip-directory-contains?
   ((or/c string? path? bytes?) zip-directory? . -> . boolean?)]
  [zip-directory-includes-directory?
   ((or/c string? path? bytes?) zip-directory? . -> . boolean?)]

  [output-flag/c contract?]

  [unzip
   (() (input-port? (bytes? boolean? input-port? . -> . any)) . ->* . any)]
  [read-zip-directory ((or/c string? path?) . -> . zip-directory?)]
  [unzip-entry (((or/c string? path?) zip-directory? bytes?)
                ((bytes? boolean? input-port? . -> . any))
                . ->* .
                any)]
  [path->zip-path ((or/c string? path?) . -> . bytes?)]
  [make-filesystem-entry-reader
   (() (output-flag/c) . ->* . (bytes? boolean? input-port? . -> . any))]
  [make-piped-entry-reader
   (output-port? . -> . (bytes? boolean? input-port? . -> . any))]))

;; ===========================================================================
;; UTILITIES
;; ===========================================================================

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
(define (make-filter-input-port transform
                                [in (current-input-port)]
                                [close-orig? #f])
  (make-filter-input-port/debug transform in close-orig? #f))

;; dirname : path -> path
(define (dirname p)
  (let-values ([(parent name must-be-dir?) (split-path p)])
    (cond
      [(not parent) (build-path p)]
      [(eq? parent 'relative) (build-path 'same)]
      [else parent])))

;; ===========================================================================
;; BIT TWIDDLING
;; ===========================================================================

;; ones-mask : nat -> exact-integer
;; creates an integer of n bytes all set to #xff
(define (ones-mask n)
  (sub1 (arithmetic-shift 1 (* 8 n))))

;; bit-set? : nat exact-integer -> boolean
;; renamed for old references
(define bit-set? bitwise-bit-set?)

;; negative-bytes? : bytes boolean -> boolean
;; tests if a byte string represents a negative two's-complement integer
(define (negative-bytes? bytes start-k end-k big-endian?)
  (bit-set? 7 (bytes-ref bytes
                         (if big-endian? start-k (sub1 end-k)))))

;; bytes->integer : bytes boolean [boolean] [nat] [nat] -> exact-integer
(define (bytes->integer bytes signed?
                        [big-endian? (system-big-endian?)]
                        [start-k 0]
                        [end-k (bytes-length bytes)])
  (let ([unsigned (bytes->unsigned bytes start-k end-k big-endian?)])
    (if (and signed? (negative-bytes? bytes start-k end-k big-endian?))
        (- (add1 (bitwise-xor unsigned (ones-mask (- end-k start-k)))))
        unsigned)))

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

;; ===========================================================================
;; INPUT
;; ===========================================================================

;; skip-bytes : nat [input-port] -> any
;; skips the given number of bytes from an input port
(define (skip-bytes k [in (current-input-port)])
  (read-bytes k in)
  (void))

;; read-integer : nat boolean [input-port] [boolean] -> exact-integer
;; reads a two's-complement integer from an input port
(define (read-integer k signed?
                      [in (current-input-port)]
                      [big-endian? (system-big-endian?)])
  (bytes->integer (read-bytes k in) signed? big-endian?))

;; peek-integer : nat boolean [input-port] [boolean] -> exact-integer
;; reads a two's-complement integer from an input port without advancing
(define (peek-integer k signed?
                      [in (current-input-port)]
                      [big-endian? (system-big-endian?)])
  (bytes->integer (peek-bytes k 0 in) signed? big-endian?))

;; ===========================================================================
;; ZIP CONSTANTS
;; ===========================================================================

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
    [(unix) 3]
    [(windows) 0]
    [(macosx) 19]))
(define *os-specific-separator-regexp*
  (case (system-type)
    [(unix macosx) #rx"/"]
    [(windows) #rx"\\\\"]))

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
(define *external-attributes:directory* *msdos-directory*)

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
(define (unzip [in (current-input-port)] [read-entry *default-entry-reader*])
  (when (= (peek-integer 4 #f in #f) *local-file-header*)
    (unzip-one-entry in read-entry)
    (unzip in read-entry)))

;; read-zip-directory : (union string path) -> zip-directory
(define (read-zip-directory path)
  (make-zip-directory
   (with-input-from-file path
     (lambda ()
       (read-central-directory (current-input-port)
                               (file-size path))))))

;; unzip-entry : (union string path) zip-directory bytes [(bytes boolean input-port -> a)] -> a
(define (unzip-entry path dir entry-name [read-entry *default-entry-reader*])
  (cond
   [(zip-directory-lookup entry-name dir)
    => (lambda (entry)
         (with-input-from-file path
           (lambda ()
             (file-position (current-input-port) (zip-entry-offset entry))
             (unzip-one-entry (current-input-port) read-entry))))]
   [else (raise-entry-not-found entry-name)]))

;; ===========================================================================
;; ENTRY PARSERS
;; ===========================================================================

;; make-filesystem-entry-reader : [output-flag] -> (bytes boolean input-port -> any)
(define (make-filesystem-entry-reader [flag 'error])
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
              flag))))))

(define *default-entry-reader* (make-filesystem-entry-reader))

;; make-piped-entry-reader : output-port -> (bytes boolean input-port -> any)
(define (make-piped-entry-reader out)
  (lambda (name dir? in)
    (unless dir?
      (copy-port in out))))

(define output-flag/c
  (symbols 'error 'replace 'truncate 'truncate/replace 'append 'update))
