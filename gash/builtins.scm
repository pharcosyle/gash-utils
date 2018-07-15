;;; Gash -- Guile As SHell
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

(define-module (gash builtins)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 local-eval)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (gash bournish-commands)
  #:use-module (gash config)
  #:use-module (gash environment)
  #:use-module (gash gash)
  #:use-module (gash guix-build-utils)
  #:use-module (gash io)
  #:use-module (gash job)
  #:use-module (gash pipe)
  #:use-module (gash util)

  #:export (
            %builtin-commands
            builtin
            pipeline
            command
            glob
            singlequotes
            doublequotes
            sequence
            splice
            for
            split
            substitution
            script
            if-clause

            bg-command
            cd-command
            echo-command
            exit-command
            fg-command
            find-command
            help-command
            pwd-command
            set-command
            ))

(define (cd-command . args)
  (match args
    (() (cd-command (getenv "HOME")))
    ((dir)
     (assignment "OLDPWD" (getcwd))
     (if (string=? dir "-") (chdir (variable "OLDPWD"))
         (chdir dir)))
    ((args ...)
     (format (current-error-port) "cd: too many arguments: ~a\n" (string-join args)))))

(define (echo-command . args)
  (match args
    (() (newline))
    (("-n" args ...) (display (string-join args)))
    (_ (display (string-join args)) (newline))))

(define (bg-command . args)
  (match args
    (() (bg 1))
    ((job x ...) (bg (string->number (car job))))))

(define (fg-command . args)
  (match args
    (() (fg 1))
    ((job x ...) (fg (string->number (car job))))))

(define pwd-command (lambda _ (stdout (getcwd))))

(define (set-command . args) ;; TODO export; env vs set
  (define (display-var o)
    (format #t "~a=~a\n" (car o) (cdr o)))
  (match args
    (() (for-each display-var global-variables))
    (("-e") (set-shell-opt! "errexit" #t))
    (("+e") (set-shell-opt! "errexit" #f))
    (("-x") (set-shell-opt! "xtrace" #t))
    (("+x") (set-shell-opt! "xtrace" #f))))

(define (exit-command . args)
  (match args
    (() (exit 0))
    ((status)
     (exit (string->number status)))
    ((args ...)
     (format (current-error-port) "exit: too many arguments: ~a\n" (string-join args)))))

(define (help-command . _)
  (display "\
Hello, this is GASH, Guile As SHell.

GASH is work in progress; many language constructs work, globbing
mostly works, pipes work, some redirections work.
")
  (when (or %prefer-builtins? (not (PATH-search-path "ls")))
    (display "\nIt features the following, somewhat naive builtin commands\n")
    (display-tabulated (map car %builtin-commands))))

(define (cp-command-implementation source dest . rest)
  (copy-file source dest))

(define cp-command (wrap-command cp-command-implementation "cp"))

(define find-command-implementation
  ;; Run-time support procedure.
  (case-lambda
    (()
     (find-command-implementation "."))
    (args
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
                (for-each stdout files))))))))

(define find-command (wrap-command find-command-implementation "find"))

(define command-command
  (case-lambda
    (() #t)
    (args
     (let* ((option-spec
             '((describe (single-char #\V))
               (help)
               (show (single-char #\v))
               (version)))
            (options (getopt-long (cons "command" args) option-spec))
            (help? (option-ref options 'help #f))
            (version? (option-ref options 'version #f))
            (files (option-ref options '() '())))
       (cond (help? (display "Usage: command [OPTION]... [COMMAND [ARG]...]

Options:
  --help     display this help and exit
  --version  display version information and exit
  -v         display a description of COMMAND similar to the `type' builtin
  -V         display a more verbose description of COMMAND
"))
             (version? (format #t "command (GASH) ~a\n" %version))
             ((null? files) #t)
             ((option-ref options 'describe #f)
              (let* ((command (car files))
                     (builtin (builtin `(,command) #:prefer-builtin? %prefer-builtins?)))
                (cond (builtin (format #t "~a is a shell builtin\n" command)
                               0)
                      (else (let ((program (PATH-search-path command)))
                              (if (string? program) (begin (format #t "~a hashed (~a)\n" command ) 0)
                                  1))))))
             ((option-ref options 'show #f)
              (let* ((command (car files))
                     (builtin (builtin `(,command) #:prefer-builtin? %prefer-builtins?)))
                (if builtin (begin (stdout command) 0)
                    (let ((program (PATH-search-path command)))
                      (if (string? program) (begin (stdout program) 0)
                          1)))))
             (else (let* ((command (car files))
                          (builtin (builtin command #:prefer-builtin? %prefer-builtins?)))
                     ;; FIXME:
                     `(command ,@args))))))))

(define type-command
  (case-lambda
    (() #t)
    (args
     (let* ((option-spec
             '((help)
               (canonical-file-name (single-char #\p))
               (version)))
            (options (getopt-long (cons "type" args) option-spec))
            (help? (option-ref options 'help #f))
            (version? (option-ref options 'version #f))
            (files (option-ref options '() '())))
       (cond (help? (display "Usage: type [OPTION]... [COMMAND]

Options:
  --help     display this help and exit
  -p         display canonical file name of COMMAND
  --version  display version information and exit
"))
             (version? (format #t "type (GASH) ~a\n" %version))
             ((null? files) #t)
             ((option-ref options 'canonical-file-name #f)
              (let* ((command (car files))
                     (builtin (builtin `(,command) #:prefer-builtin? %prefer-builtins?)))
                (if builtin 0
                    (let ((program (PATH-search-path command)))
                      (and (string? program)
                           (stdout program)
                           0)))))
             (else
              (let* ((command (car files))
                     (builtin (builtin `(,command) #:prefer-builtin? %prefer-builtins?)))
                (cond (builtin (format #t "~a is a shell builtin\n" command)
                               0)
                      (else (let ((program (PATH-search-path command)))
                              (if (string? program) (begin (format #t "~a hashed (~a)\n" command ) 0)
                                  1)))))))))))

(define test-command
  (case-lambda
    (() #f)
    (args
     (let* ((option-spec
             '((is-directory (single-char #\d))
               (exists (single-char #\e))
               (has-size (single-char #\s))
               (help)
               (is-directory (single-char #\d))
               (is-file (single-char #\f))
               (is-symbolic-link (single-char #\L))
               (is-symbolic-link (single-char #\h))
               (is-readable (single-char #\r))
               (is-writable (single-char #\w))
               (is-exeutable (single-char #\x))
               (version)))
            (options (getopt-long (cons "test" args) option-spec))
            (help? (option-ref options 'help #f))
            (version? (option-ref options 'version #f))
            (files (option-ref options '() '()))
            (file (and (pair? files) (car files))))
       (cond (help? (display "Usage: test [EXPRESSION]

Options:
  -d FILE    FILE exists and is a directory
  -e FILE    FILE exists
  -f FILE    FILE exists and is a regular file
  -h FILE    FILE exists and is a symbolic link (same as -L)
  -L FILE    FILE exists and is a symbolic link (same as -h)
  -r FILE    FILE exists and read permission is granted
  -s FILE    FILE exists and has a size greater than zero
  -w FILE    FILE exists and write permission is granted
  -x FILE    FILE exists and execute (or search) permission is granted
  --help     display this help and exit
  --version  display version information and exit
"))
             (version? (format #t "test (GASH) ~a\n" %version))
             ((null? files) #f)
             ((and (= (length files) 3)
                   (member (cadr files) '("=" "==")))
              (match files
                ((or (left "=" right)
                     (left "==" right))
                 (equal? left right))
                (expression
                 (pipeline (command expression)))))
             ((not (= (length files) 1))
              (format (current-error-port) "test: too many files: ~a\n" files)
              1)
             ((option-ref options 'is-file #f)
              (regular-file? file))
             ((option-ref options 'is-directory #f)
              (directory-exists? file))
             ((option-ref options 'exists #f)
              (file-exists? file))
             ((option-ref options 'is-symbolic-link #f)
              (symbolic-link? file))
             ((option-ref options 'is-readable #f)
              (access? file R_OK))
             ((option-ref options 'has-size #f)
              (and (file-exists? file)
                   (not (zero? (stat:size (stat file))))))
             ((option-ref options 'is-writable #f)
              (access? file W_OK))
             ((option-ref options 'is-exeutable #f)
              (access? file X_OK))
             (else
              (error "gash: test: not supported" args)))))))

(define bracket-command
  (case-lambda
    (() #f)
    (args
     (cond ((and (pair? args) (equal? (car args) "--help"))
            (test-command "--help"))
           ((and (pair? args) (equal? (car args) "--version"))
            (test-command "--version"))
           (else
            (if (not (equal? (last args) "]")) (begin
                                                 (format (current-error-port) "gash: [: missing `]'\n")
                                                 #f)
                (apply test-command (drop-right args 1))))))))

(define grep-command
  (case-lambda
    (() #f)
    (args
     (let* ((option-spec
             '((help)
               (line-number (single-char #\n))
               (files-with-matches (single-char #\l))
               (files-without-match (single-char #\L))
               (with-file-name (single-char #\H))
               (no-file-name (single-char #\h))
               (only-matching (single-char #\o))
               (version (single-char #\V))))
            (options (getopt-long (cons "ls" args) option-spec))
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
                     (files (cdr files))
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
                            0))))))))))

(define (PATH-search-path program)
  (search-path (string-split (getenv "PATH") #\:) program))

(define* (builtin ast #:key prefer-builtin?)
  ;; FIXME: distinguish between POSIX compliant builtins and
  ;; `best-effort'/`fallback'?
  "Possibly modify command to use a builtin."
  (when (> %debug-level 0)
    (format (current-error-port) "builtin ast=~s\n" ast))
  (receive (command args)
      (match ast
        (((and (? string?) command) args ...) (values command args))
        (_ (values #f #f)))
    (let ((program (and command
                        (cond ((string-prefix? "/" command)
                               (when (not (file-exists? command))
                                 (format (current-error-port) "gash: ~a: no such file or directory\n" command))
                               command)
                              (else (PATH-search-path command))))))
      ;; FIXME: find some generic strerror/errno way: what about permissions and stuff?
      ;; after calling  system* we're too late for that?
      (when (> %debug-level 0)
        (format (current-error-port) "command ~a => ~s ~s\n" (or program 'builtin) command args))
      (cond ((and program (not prefer-builtin?))
             (when (not program)
               (format (current-error-port) "gash: ~a: command not found\n" command))
             (when (not (access? program X_OK))
               (format (current-error-port) "gash: ~a: permission denied\n" command))
             #f)
            ((and command (assoc-ref %builtin-commands command))
             =>
             (lambda (command)
               (if args
                   (apply command (map (cut local-eval <> (the-environment)) args))
                   (command))))
            (else #f)))))

(define (command . args)
  (define (exec command)
    (cond ((procedure? command) command)
          ((every string? command)
           (let* ((program (car command))
                  (escape-builtin? (and (string? program) (string-prefix? "\\" program)))
                  (program (if escape-builtin? (string-drop program 1) program))
                  (command (cons program (cdr command))))
             (or (builtin command #:prefer-builtin? (and %prefer-builtins?
                                                          (not escape-builtin?)))
                 (cut apply (compose status:exit-val system*) command))))
          (else (lambda () #t))))
  (exec (append-map glob args)))

(define (glob pattern)
  (define (glob? pattern)
    (and (string? pattern) (string-match "\\?|\\*" pattern)))
  (define (glob2regex pattern)
    (let* ((pattern (regexp-substitute/global #f "\\." pattern 'pre "\\." 'post))
           (pattern (regexp-substitute/global #f "\\?" pattern 'pre "." 'post))
           (pattern (regexp-substitute/global #f "\\*" pattern 'pre ".*" 'post)))
      (make-regexp (string-append "^" pattern "$"))))
  (define (glob-match regex path) ;; pattern path -> bool
    (regexp-match? (regexp-exec regex path)))
  (define (glob- pattern file-names)
    (map (lambda (file-name)
           (if (string-prefix? "./" file-name) (string-drop file-name 2) file-name))
         (append-map (lambda (file-name)
                       (map (cut string-append (if (string=? "/" file-name) "" file-name) "/" <>)
                            (filter (conjoin (negate (cut string-prefix? "." <>))
                                             (cute glob-match (glob2regex pattern) <>))
                                    (or (scandir file-name) '()))))
                     file-names)))
  (cond
   ((not pattern) '(""))
   ((glob? pattern) (let ((absolute? (string-prefix? "/" pattern)))
                      (let loop ((patterns (filter (negate string-null?) (string-split pattern #\/)))
                                 (file-names (if absolute? '("/") '("."))))
                        (if (null? patterns)
                            file-names
                            (begin
                              (loop (cdr patterns) (glob- (car patterns) file-names)))))))
   (#t (list pattern))))

(define (singlequotes . o)
  (string-join o ""))

(define (doublequotes . o)
  (string-join (append-map glob  o) ""))

(define (sequence . args)
  (apply append args))

(define (script . o)
  o)

(define (for name sequence body)
  (for-each (lambda (value)
              (assignment name value)
              (body))
            (sequence)))

(define (split o)
  ((compose string-tokenize string-trim-right) o))

(define-syntax-rule (substitution commands)
  (split (with-output-to-string (lambda _ commands))))

(define-syntax if-clause
  (lambda (x)
    (syntax-case x ()
      ((_ expr then)
       (with-syntax ((it (datum->syntax x 'it)))
         #'(let ((it expr))
             (if (zero? it) then))))
      ((_ expr then else)
       (with-syntax ((it (datum->syntax x 'it)))
         #'(let ((it expr))
             (if (zero? it) then else)))))))

(define (pipeline . commands)
  (define (handle job)
    (let* ((stati (cond ((job? job) (map status:exit-val (job-status job)))
                        ((boolean? job) (list (if job 0 1)))
                        ((number? job) (list job))
                        (else (list 0))))
           (status (if (shell-opt? "pipefail") (or (find (negate zero?) stati) 0)
                       (car stati)))
           (pipestatus (string-append
                        "("
                        (string-join
                         (map (lambda (s i)
                                (format #f "[~a]=\"~a\"" s i))
                              stati
                              (iota (length stati))))
                        ")")))
      (assignment "PIPESTATUS" pipestatus)
      (assignment "?" (number->string status))
      (when (and (not (zero? status))
                 (shell-opt? "errexit"))
        (exit status))
      status))
  (when (> %debug-level 1)
    (format (current-error-port) "pijp: commands=~s\n" commands))
  ;; FIXME: after running a builtin, we still end up here with the builtin's result
  ;; that should probably not happen, however, cater for it here for now
  (match commands
    (((and (? boolean?) boolean))
     (handle boolean))
    (((and (? number?) number))
     (handle number))
    (((? unspecified?))
     (handle #t))
    (_ (handle (apply pipeline+ #t commands)))))

(define %builtin-commands
  `(
    ("bg"      . ,bg-command)
    ("cat"     . ,cat-command)
    ("command" . ,command-command)
    ("cd"      . ,cd-command)
    ("cp"      . ,cp-command)
    ("echo"    . ,echo-command)
    ("exit"    . ,exit-command)
    ("fg"      . ,fg-command)
    ("find"    . ,find-command)
    ("grep"    . ,grep-command)
    ("help"    . ,help-command)
    ("jobs"    . ,jobs-command)
    ("ls"      . ,ls-command)
    ("pwd"     . ,pwd-command)
    ("reboot"  . ,reboot-command)
    ("rm"      . ,rm-command)
    ("set"     . ,set-command)
    ("test"    . ,test-command)
    ("type"    . ,type-command)
    ("wc"      . ,wc-command)
    ("which"   . ,which-command)
    ("["       . ,bracket-command)
    ))
