#lang scribble/doc
@(require "common.rkt"
          (for-label file/zip file/unzip
                     file/gzip file/gunzip
                     racket/file))

@title[#:tag "unzip"]{@exec{zip} File Decompression}

@defmodule[file/unzip]{The @racketmodname[file/unzip] library provides
utilities to decompress @exec{zip} archive files, which are compatible
with both Windows and Unix (including Mac OS X) unpacking.}

@defproc[(unzip [in input-port?]
                [read-entry (-> bytes? boolean? input-port? any)
                 (make-filesystem-entry-reader)])
         any]{

Unzips an entire zip file from the given input port, which defaults to
the value of the @racket[current-input-port] parameter. For each entry
in the zip file, the @racket[read-entry] procedure is evaluated with
three arguments: the byte string representing the entry name, a
boolean flag indicating whether the entry represents a directory, and
an input port containing the inflated contents of the entry. The
@racket[read-entry] procedure defaults to the value of
@racket[(make-filesystem-entry-reader)].
}

@defproc[(read-zip-directory [path (or/c string? path?)])
         zip-directory?]{

Reads the central directory of a zip file and generates a
@racket[zip-directory?]  representing the zip file's contents. The
@racket[path] parameter must refer to a readable file.

This procedure performs limited I/O: it reads the list of entries from
the zip file but does not inflate any of their contents. This
procedure does not leave any new ports open.
}

@defproc[(unzip-entry [path (or/c string? path?)]
                      [zipdir zip-directory?]
                      [entry bytes?]
                      [read-entry (-> bytes? boolean? input-port? any)
                       (make-filesystem-entry-reader)])
         any]{

Unzips a single entry from a zip file. The @racket[path] parameter
must refer to a readable file, and the @racket[zipdir] argument must
be a zip-directory representing the zip file's contents, as created by
@racket[read-zip-directory]. The @racket[entry] parameter is a byte
string whose name must be found in the zip file's central directory.

The @racket[read-entry] argument is used to read the contents of the
zip entry. Its arguments are the zip entry name, a boolean flag
indicating whether the entry represents a directory, and an input port
from which the procedure can read the contents of the entry. The
result of this procedure is used as the result of
@racket[unzip-entry].

The @racket[read-entry] argument defaults to the value of
@racket[(make-filesystem-entry-reader)].
}

@defproc[(path->zip-path [path (or/c string? path?)])
         bytes?]{

Converts a file name potentially containing path separators in the
current operating system's format to use path separators recognized by
the zip file format, namely the '/' character.
}

@defproc[(make-filesystem-entry-reader [sym output-flag/c])
         (-> bytes? boolean? input-port? any)]{

Creates a zip entry reader that can be used with either @racket[unzip]
or @racket[unzip-entry] and whose behavior is to save entries to the
local filesystem. Intermediate directories are always created if
necessary before creating files. Directory entries are created as
directories in the filesystem, and their entry contents are ignored.

If the input-port argument is provided to the entry reader (i.e., by
@racket[unzip-entry]), it reads the contents of the input port and
saves them to the local file. Otherwise, it returns an output port
that can be used by @racket[unzip] to save the contents of the entry
to the local file.
}

@defproc[(make-piped-entry-reader [out output-port?])
         (-> bytes? boolean? input-port? any)]{

Creates a zip entry reader for either @racket[unzip] or
@racket[unzip-entry] whose behavior is to pipe the contents of a zip
entry, unless it represents a directory, to the specified output
port.
}

@defstruct[(exn:fail:unzip exn:fail) ()]{

Raised when an error occurs in unzipping a file.
}

@defstruct[(exn:fail:unzip:no-such-entry exn:fail:unzip)
           ([entry bytes?])]{

Raised when a requested entry cannot be found in a zip file. The
@racket[entry] field is a byte string representing the entry's path
name.
}

@defthing[output-flag/c contract?]{

The contract for the @racket[output-flag] type. An output flag is one
of @racket['error], @racket['replace], @racket['truncate],
@racket['truncate/replace], @racket['append], @racket['update].

Arguments that can be used as a mode flag for writing to output
files. See @racket[open-output-file] or details.
}

@defproc[(zip-directory? [x any/c])
         boolean?]{

Determines whether @racket[x] is a zip-directory.

A zip-directory is an abstract datatype that encapsulates the
information stored in the central directory of a zip file. The
@racket[read-zip-directory] procedure produces a zip-directory, which
can then be used by @racket[unzip-entry] to extract individual entries
from the file, and by the following library procedures to query the
directory for information about the contents of the zip file.
}

@defproc[(zip-directory-entries [zd zip-directory?])
         (listof bytes?)]{

Extracts the list of zip entries from a zip file.
}

@defproc[(zip-directory-contains? [name (or/c string? path? bytes?)]
                                  [zd zip-directory?])
         boolean?]{

Determines whether the given entry name occurs in the zip
directory. If @racket[name] is provided as a string or path, it is
automatically converted to a byte string with @racket[path->zip-path].

Directory entries match with or without trailing slashes.
}

@defproc[(zip-directory-includes-directory? [name (or/c string? path? bytes?)]
                                            [zd zip-directory?])
         boolean?]{

Determines whether the given name is included anywhere in the zip
directory as a filesystem directory, either as an entry itself or as
the containing directory of other entries. If @racket[name] is
provided as a string or path, it is automatically converted to a byte
string with @racket[path->zip-path].
}
