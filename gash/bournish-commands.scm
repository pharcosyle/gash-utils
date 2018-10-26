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
              (one-file-per-line (single-char #\1))
              (version)))
           (options (getopt-long (cons "ls" args) option-spec))
           (all? (option-ref options 'all #f))
           (help? (option-ref options 'help #f))
           (one-file-per-line? (option-ref options 'one-file-per-line #f))
           (version? (option-ref options 'version #f))
           (files (option-ref options '() '())))
      (cond (help? (display "Usage: ls [OPTION]... [FILE]...

Options:
  -a, --all      do not ignore entries starting with .
  -1             list one file per line
      --help     display this help and exit
      --version  display version information and exit
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
               (if one-file-per-line? (for-each stdout files)
                   (display-tabulated files))))))))

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

(define (tar-command . args)
  (lambda _
    (let* ((option-spec
	    '((create (single-char #\c))
              (extract (single-char #\x))
              (file (single-char #\f) (value #t))
              (help (single-char #\h))
	      (version (single-char #\V))))
           (args (cons "tar" args))
	   (options (getopt-long args option-spec))
           (create? (option-ref options 'create #f))
           (extract? (option-ref options 'extract #f))
           (file (option-ref options 'file "/dev/stdout"))
	   (files (option-ref options '() '()))
	   (help? (option-ref options 'help #f))
	   (usage? (and (not help?) (not (or (and create? (pair? files)) extract?))))
	   (version? (option-ref options 'version #f)))
      (cond ((or help? usage?) (format (if usage? (current-error-port) #t)
                                       "\
Usage: tar [OPTION]... [FILE]...
  -c, --create           create a new archive
  -f, --file=ARCHIVE     use archive file or device ARCHIVE
  -h, --help             display this help
  -V, --version          display version
  -x, --extract          extract files from an archive
")
             (exit (if usage? 2 0)))
            (version? (format #t "tar (GASH) ~a\n" %version) (exit 0))
            (create?
             (write-ustar-archive file files))
            (extract?
             (extract-ustar-archive file files))))))

(define %bournish-commands
  `(
    ("cat"     . ,cat-command)
    ("cp"      . ,cp-command)
    ("find"    . ,find-command)
    ("grep"    . ,grep-command)
    ("ls"      . ,ls-command)
    ("reboot"  . ,reboot-command)
    ("tar"     . ,tar-command)
    ("wc"      . ,wc-command)
    ("which"   . ,which-command)
    ))
