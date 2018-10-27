;;; Gash -- Guile As SHell
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of Gash.
;;;
;;; Gash is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Gash is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Gash.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; The initial bournish.scm was taken from Guix.

;;; Code:

(define-module (gash bournish-commands)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)

  #:use-module (gash guix-build-utils)
  #:use-module (gash guix-utils)
  #:use-module (gash compress)
  #:use-module (gash config)
  #:use-module (gash io)
  #:use-module (gash ustar)
  #:use-module (gash util)

  #:export (
            %bournish-commands
            cat-command
            display-tabulated
            find-command
            grep-command
            ls-command
            reboot-command
            rm-command
            wc-command
            which-command
            ))

;;; Commentary:

;;; This code is taken from (guix build bournish)

;;;
;;; This is a super minimal Bourne-like shell language for Guile.  It is meant
;;; to be used at the REPL as a rescue shell.  In a way, this is to Guile what
;;; eshell is to Emacs.
;;;
;;; Code:

(define (wrap-command command name)
  (lambda args
    (catch #t
      (cut apply command args)
      (lambda (key . args)
        (format (current-error-port) "~a: ~a ~a\n" name key args)
        1))))

(define* (display-tabulated lst
                            #:key
                            (terminal-width 80)
                            (column-gap 2))
  "Display the list of string LST in as many columns as needed given
TERMINAL-WIDTH.  Use COLUMN-GAP spaces between two subsequent columns."
  (define len (length lst))
  (define column-width
    ;; The width of a column.  Assume all the columns have the same width
    ;; (GNU ls is smarter than that.)
    (+ column-gap (reduce max 0 (map string-length lst))))
  (define columns
    (max 1
         (quotient terminal-width column-width)))
  (define pad
    (if (zero? (modulo len columns))
        0
        columns))
  (define items-per-column
    (quotient (+ len pad) columns))
  (define items (list->vector lst))

  (let loop ((indexes (unfold (cut >= <> columns)
                              (cut * <> items-per-column)
                              1+
                              0)))
    (unless (>= (first indexes) items-per-column)
      (for-each (lambda (index)
                  (let ((item (if (< index len)
                                  (vector-ref items index)
                                  "")))
                    (display (string-pad-right item column-width))))
                indexes)
      (newline)
      (loop (map 1+ indexes)))))

(cond-expand
 (guile
  ;; Support -1, see https://lists.gnu.org/archive/html/bug-guile/2018-07/msg00009.html
  (module-define! (resolve-module '(ice-9 getopt-long)) 'short-opt-rx (make-regexp "^-([a-zA-Z0-9]+)(.*)")))
 (else))

(define (ls-command-implementation . args)
  ;; Run-time support procedure.
  (lambda _
    (let* ((option-spec
            '((all (single-char #\a))
              (help)
              (long (single-char #\l))
              (one-file-per-line (single-char #\1))
              (version)))
           (options (getopt-long (cons "ls" args) option-spec))
           (all? (option-ref options 'all #f))
           (help? (option-ref options 'help #f))
           (long? (option-ref options 'long #f))
           (one-file-per-line? (option-ref options 'one-file-per-line #f))
           (version? (option-ref options 'version #f))
           (files (option-ref options '() '())))
      (cond (help? (display "Usage: ls [OPTION]... [FILE]...

Options:
  -a, --all      do not ignore entries starting with .
      --help     display this help and exit
  -l, --long     use a long listing format
      --version  display version information and exit
  -1             list one file per line
"))
            (version? (format #t "ls (GASH) ~a\n" %version))
            (else
             (let* ((files (if (null? files) (scandir ".")
                               (append-map (lambda (file)
                                             (catch 'system-error
                                               (lambda ()
                                                 (match (stat:type (lstat file))
                                                   ('directory
                                                    ;; Like GNU ls, list the contents of
                                                    ;; FILE rather than FILE itself.
                                                    (match (scandir file
                                                                    (match-lambda
                                                                      ((or "." "..") #f)
                                                                      (_ #t)))
                                                      (#f
                                                       (list file))
                                                      ((files ...)
                                                       (map (cut string-append file "/" <>)
                                                            files))))
                                                   (_
                                                    (list file))))
                                               (lambda args
                                                 (let ((errno (system-error-errno args)))
                                                   (format (current-error-port) "~a: ~a~%"
                                                           file (strerror errno))
                                                   '()))))
                                           files)))
                    (files (if all? files
                               (filter (negate (cut string-prefix? "." <>)) files))))
               (cond (long? (for-each (lambda (f) (display-file f) (newline)) files))
                     (one-file-per-line? (for-each stdout files))
                     (else (display-tabulated files)))))))))

(define ls-command (wrap-command ls-command-implementation "ls"))

(define (which-command program . rest)
  (lambda _
    (stdout (search-path (executable-path) program))))

(define (cat-command-implementation . args)
  (lambda _
    (fold (lambda (file p)
            (if (string=? file "-") (dump-port (current-input-port) (current-output-port))
                (call-with-input-file file
                  (lambda (port)
                    (dump-port port (current-output-port))))))
          0 args)))

(define cat-command (wrap-command cat-command-implementation "cat"))

(define (rm-command-implementation . args)
  (lambda _
    (cond ((member "-r" args)
           (for-each delete-file-recursively
                     (apply delete (cons "-r" args))))
          (else
           (for-each delete-file args)))))

(define rm-command (wrap-command rm-command-implementation "rm"))

(define (lines+chars port)
  "Return the number of lines and number of chars read from PORT."
  (let loop ((lines 0) (chars 0))
    (match (read-char port)
      ((? eof-object?)              ;done!
       (values lines chars))
      (#\newline                    ;recurse
       (loop (1+ lines) (1+ chars)))
      (_                            ;recurse
       (loop lines (1+ chars))))))

(define (file-exists?* file)
  "Like 'file-exists?' but emits a warning if FILE is not accessible."
  (catch 'system-error
    (lambda ()
      (stat file))
    (lambda args
      (let ((errno (system-error-errno args)))
        (format (current-error-port) "~a: ~a~%"
                file (strerror errno))
        #f))))

(define (wc-print file)
  (let-values (((lines chars)
                (call-with-input-file file lines+chars)))
              (format #t "~a ~a ~a~%" lines chars file)))

(define (wc-l-print file)
  (let-values (((lines chars)
                (call-with-input-file file lines+chars)))
              (format #t "~a ~a~%" lines file)))

(define (wc-c-print file)
  (let-values (((lines chars)
                (call-with-input-file file lines+chars)))
              (format #t "~a ~a~%" chars file)))

(define (wc-command-implementation . files)
  (for-each wc-print (filter file-exists?* files)))

(define (wc-l-command-implementation . files)
  (for-each wc-l-print (filter file-exists?* files)))

(define (wc-c-command-implementation . files)
  (for-each wc-c-print (filter file-exists?* files)))

(define (wc-command . args)
  "Emit code for the 'wc' command."
  (lambda _
    (cond ((member "-l" args)
           (apply wc-l-command-implementation (delete "-l" args)))
          ((member "-c" args)
           (apply wc-c-command-implementation (delete "-c" args)))
          (else
           (apply wc-command-implementation args)))))

(define (reboot-command . args)
  "Emit code for 'reboot'."
  ;; Normally Bournish is used in the initrd, where 'reboot' is provided
  ;; directly by (guile-user).  In other cases, just bail out.
  (if (defined? 'reboot)
      (reboot)
      (begin
        (format (current-error-port)
                "I don't know how to reboot, sorry about that!~%")
        1)))

(define %not-colon (char-set-complement (char-set #\:)))
(define (executable-path)
  "Return the search path for programs as a list."
  (match (getenv "PATH")
    (#f  '())
    (str (string-tokenize str %not-colon))))

(define (cp-command-implementation source dest . rest)
  (lambda _ (copy-file source dest)))

(define cp-command (wrap-command cp-command-implementation "cp"))

(define (find-command-implementation . args)
  ;; Run-time support procedure.
  (lambda _
    (let* ((option-spec
            '((help)
              (version)))
           (options (getopt-long (cons "find" args) option-spec))
           (help? (option-ref options 'help #f))
           (version? (option-ref options 'version #f))
           (files (option-ref options '() '()))
           (files (if (null? files) '(".") files))
           (file (car files)))
      (when (> (length files) 1)
        (format (current-error-port) "find: too many FILEs: ~s\n" files)
        (error "find failed"))
      ;; TODO: find [OPTION]... [FILE]... [EXPRESSION]...
      ;; and options: esp: -x, -L
      (cond (help? (display "Usage: find [OPTION]... [FILE]

Options:
  --help     display this help and exit
  --version  display version information and exit
"))
            (version? (format #t "find (GASH) ~a\n" %version))
            (else
             (let* ((files (find-files file #:directories? #t #:fail-on-error? #t)))
               (for-each stdout files)))))))

(define find-command (wrap-command find-command-implementation "find"))

(define (grep-command . args)
  (lambda _
    (let* ((option-spec
            '((help)
              (line-number (single-char #\n))
              (files-with-matches (single-char #\l))
              (files-without-match (single-char #\L))
              (with-file-name (single-char #\H))
              (no-file-name (single-char #\h))
              (only-matching (single-char #\o))
              (version (single-char #\V))))
           (options (getopt-long (cons "grep" args) option-spec))
           (help? (option-ref options 'help #f))
           (version? (option-ref options 'version #f))
           (files (option-ref options '() '())))
      (cond (help? (display "Usage: grep [OPTION]... PATTERN [FILE]...

Options:
  --help                     display this help and exit
  -h, --no-filename          suppress the file name prefix on output
  -H, --with-filename        print file name with output lines
  -l, --files-with-matches   print only names of FILEs with selected lines
  -L, --files-without-match  print only names of FILEs with no selected lines
  -n, --line-number          print line number with output lines
  -o, --only-matching        show only the part of a line matching PATTERN
  -V, --version              display version information and exit
"))
            (version? (format #t "grep (GASH) ~a\n" %version))
            ((null? files) #t)
            (else
             (let* ((pattern (car files))
                    (files (if (pair? (cdr files)) (cdr files)
                               (list "-")))
                    (matches (append-map (cut grep pattern <>) files)))
               (define (display-match o)
                 (let* ((s (grep-match-string o))
                        (s (if (option-ref options 'only-matching #f)
                               (substring s (grep-match-column o) (grep-match-end-column o))
                               s))
                        (s (if (option-ref options 'line-number #f)
                               (string-append (number->string (grep-match-line o)) ":" s)
                               s))
                        (s (if (option-ref options 'with-file-name #f)
                               (string-append (grep-match-file-name o) ":" s)
                               s)))
                   (stdout s)))
               (define (files-with-matches)
                 (delete-duplicates (map grep-match-file-name matches)))
               (cond ((option-ref options 'files-with-matches #f)
                      (let ((result (files-with-matches)))
                        (and (pair? result)
                             (for-each stdout result)
                             0)))
                     ((option-ref options 'files-without-match #f)
                      (let* ((result (files-with-matches))
                             (result (filter (negate (cut member <> result)) files)))
                        (and (pair? result)
                             (for-each stdout result)
                             0)))
                     (else
                      (and (pair? matches)
                           (for-each display-match matches)
                           0)))))))))

(define (multi-opt options name)
  (let ((opt? (lambda (o) (and (eq? (car o) name) (cdr o)))))
    (filter-map opt? options)))

(define (tar-command . args)
  (lambda _
    (let* ((option-spec
	    '((create (single-char #\c))
              (compress (single-char #\Z))
              (gzip (single-char #\z))
              (bzip2 (single-char #\j))
              (xz (single-char #\J))
              (group (value #t))
              (extract (single-char #\x))
              (file (single-char #\f) (value #t))
              (help (single-char #\h))
              (mtime (value #t))
              (list (single-char #\t))
              (numeric-owner?)
              (owner (value #t))
              (sort (value #t))
	      (verbose (single-char #\v))
              (version (single-char #\V))))
           (args (cons "tar" args))
	   (options (getopt-long args option-spec))
           (options (if (or (option-ref options 'create #f)
                            (option-ref options 'extract #f)
                            (option-ref options 'list #f)
                            (null? (cdr args))
                            (string-prefix? "-" (cadr args))) options
                            (let ((args (cons* (car args)
                                               (string-append "-" (cadr args))
                                               (cddr args))))
                              (getopt-long args option-spec))))
           (create? (option-ref options 'create #f))
           (list? (option-ref options 'list #f))
           (extract? (option-ref options 'extract #f))
           (file (option-ref options 'file "-"))
	   (files (option-ref options '() '()))
           (compress? (option-ref options 'compress #f))
           (bzip2? (option-ref options 'bzip2 #f))
           (gzip? (option-ref options 'gzip #f))
           (xz? (option-ref options 'xz #f))
           (compression (cond (bzip2? 'bzip2)
                              (compress? 'compress)
                              (gzip? 'gzip)
                              (xz? 'xz)
                              (else (and (or extract? list? )
                                         (cond ((string-suffix? ".Z" file) 'compress)
                                               ((string-suffix? ".bz2" file) 'bzip2)
                                               ((string-suffix? ".gz" file) 'gzip)
                                               ((string-suffix? ".xz" file) 'xz)
                                               (else #f))))))
	   (help? (option-ref options 'help #f))
	   (usage? (and (not help?) (not (or (and create? (pair? files))
                                             extract? list?))))
	   (verbosity (length (multi-opt options 'verbose)))
           (version? (option-ref options 'version #f)))
      (cond ((or help? usage?) (format (if usage? (current-error-port) #t)
                                       "\
Usage: tar [OPTION]... [FILE]...
  -c, --create               create a new archive
  -f, --file=ARCHIVE         use archive file or device ARCHIVE
      --group=NAME           force NAME as group for added files
  -h, --help                 display this help
      --mtime=DATE-OR-FILE   set mtime for added files from DATE-OR-FILE
      --numeric-owner        always use numbers for user/group names
      --owner=NAME           force NAME as owner for added files
      --sort=ORDER           directory sorting order: none (default), name or
                             inode
  -t, --list                 list the contents of an archive
  -V, --version              display version
  -v, --verbose              verbosely list files processed
  -x, --extract              extract files from an archive
  -z, --gzip                 filter the archive through gzip
  -Z, --compress             filter the archive through compress
")
             (exit (if usage? 2 0)))
            (version? (format #t "tar (GASH) ~a\n" %version) (exit 0))
            (create?
             (let ((files (if (not (option-ref options 'sort #f)) files
                              (sort files string<)))
                   (group (and=> (option-ref options 'group #f) string->number))
                   (mtime (and=> (option-ref options 'mtime #f) string->number))
                   (numeric-owner? (option-ref options 'numeric-owner? #f))
                   (owner (and=> (option-ref options 'owner #f) string->number)))
               (if (or compression (equal? file "-"))
                   (let ((port (if (equal? file "-") (current-output-port)
                                   (open-file file "wb"))))
                     (call-with-compressed-output-port compression port
                       (cut apply write-ustar-port <>
                            `(,files
                              ,@(if group `(#:group ,group) '())
                              ,@(if mtime `(#:mtime ,mtime) '())
                              ,@(if numeric-owner? `(#:numeric-owner? ,numeric-owner?) '())
                              ,@(if owner `(#:owner ,owner) '())
                              ,@(if owner `(#:owner ,owner) '())
                              #:verbosity ,verbosity))))
                   (apply write-ustar-archive
                          `(,file
                            ,files
                            ,@(if group `(#:group ,group) '())
                            ,@(if mtime `(#:mtime ,mtime) '())
                            ,@(if numeric-owner? `(#:numeric-owner? ,numeric-owner?) '())
                            ,@(if owner `(#:owner ,owner) '())
                            ,@(if owner `(#:owner ,owner) '())
                            #:verbosity ,verbosity)))))
            (extract?
             (if (or compression (equal? file "-"))
                 (let ((port (if (equal? file "-") (current-input-port)
                                 (open-file file "rb"))))
                   (call-with-decompressed-port compression port
                     (cut read-ustar-port <> files #:verbosity verbosity)))
                 (read-ustar-archive file files #:verbosity verbosity)))
            (list?
             (if (or compression (equal? file "-"))
                 (let ((port (if (equal? file "-") (current-input-port)
                                 (open-file file "rb"))))
                   (call-with-decompressed-port compression port
                     (cut list-ustar-port <> files #:verbosity (1+ verbosity))))
                 (list-ustar-archive file files #:verbosity (1+ verbosity))))))))

(define (compress-command . args)
  (lambda _
    (let* ((option-spec
	    '((bits (single-char #\b) (value #t))
              (decompress (single-char #\d))
              (help (single-char #\h))
              (stdout (single-char #\c))
	      (verbose (single-char #\v))
              (version (single-char #\V))))
           (args (cons "compress" args))
	   (options (getopt-long args option-spec))
           (bits (string->number (option-ref options 'bits "16")))
           (decompress? (option-ref options 'decompress #f))
           (stdout? (option-ref options 'stdout #f))
	   (files (option-ref options '() '()))
	   (help? (option-ref options 'help #f))
	   (usage? (and (not help?) (or (and (null? files) (isatty? (current-input-port))))))
	   (verbose? (option-ref options 'verbose #f))
           (version? (option-ref options 'version #f)))
      (cond ((or help? usage?) (format (if usage? (current-error-port) #t)
                                       "\
Usage: compress [OPTION]... [FILE]...
  -b, --bits=BITS   use a maximum of BITS bits per code [16]
  -c, --stdout      write on standard output, keep original files unchanged
  -d, --decompress  decompress
  -h, --help        display this help
  -v, --verbose     show compression ratio
  -V, --version     display version
")
             (exit (if usage? 2 0)))
            (version? (format #t "compress (GASH) ~a\n" %version) (exit 0))
            (decompress? (if (pair? files) (uncompress-file (car files) verbose?)
                             (uncompress-port (current-input-port) (current-output-port) verbose?)))
            (else (if (pair? files) (compress-file (car files) bits verbose?)
                      (compress-port (current-input-port) (current-output-port) bits verbose?)))))))

(define %bournish-commands
  `(
    ("cat"      . ,cat-command)
    ("compress" . ,compress-command)
    ("cp"       . ,cp-command)
    ("find"     . ,find-command)
    ("grep"     . ,grep-command)
    ("ls"       . ,ls-command)
    ("reboot"   . ,reboot-command)
    ("tar"      . ,tar-command)
    ("wc"       . ,wc-command)
    ("which"    . ,which-command)
    ))
