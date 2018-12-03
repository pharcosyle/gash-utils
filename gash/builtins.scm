;;; Gash --- Guile As SHell
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

  #:use-module (gash config)
  #:use-module (gash gash)              ; %prefer-builtins?
  #:use-module (gash bournish-commands)
  #:use-module (gash environment)
  #:use-module (gash shell-utils)
  #:use-module (gash io)
  #:use-module (gash job)
  #:use-module (gash pipe)
  #:use-module (gash script)
  #:use-module (gash util)

  #:export (
            %builtin-commands
            PATH-search-path
            trace

            bg-command
            cd-command
            echo-command
            eval-command
            exit-command
            fg-command
            help-command
            jobs-command
            pwd-command
            set-command
            shift-command
            ))

(define (PATH-search-path program)
  (search-path (string-split (getenv "PATH") #\:) program))

(define (cd-command . args)
  (match args
    (() (cd-command (getenv "HOME")))
    ((dir)
     (let ((old (variable "OLDPWD")))
       (assignment "OLDPWD" (getcwd))
       (catch #t
         (lambda _
           (if (string=? dir "-") (chdir old)
               (chdir dir))
           0)
         (lambda (key command fmt args exit)
           (apply format (current-error-port) "cd: ~a: ~a\n" (cons dir args))
           1))))
    ((args ...)
     (format (current-error-port) "cd: too many arguments: ~a\n" (string-join args)))))

(define (echo-command . args)
  (lambda _
    (match args
      (() (newline))
      (("-n" args ...) (display (string-join args)))
      (_ (display (string-join args)) (newline)))))

(define (bg-command . args)
  (match args
    (() (bg 1))
    ((job x ...) (bg (string->number (car job))))))

(define (fg-command . args)
  (match args
    (() (fg 1))
    ((job x ...) (fg (string->number (car job))))))

(define (jobs-command)
  (format (current-error-port) "jobs: ~s\n" job-table)
  (for-each (lambda (job) (display-job job)) (reverse job-table)))

(define (pwd-command . _)
  (lambda _ (stdout (getcwd))))

(define (set-command . args) ;; TODO export; env vs set
  (define (display-var o)
    (format #t "~a=~a\n" (car o) (cdr o)))
  (match args
    (() (lambda _ (for-each display-var %global-variables)))
    (("-e") (set-shell-opt! "errexit" #t))
    (("+e") (set-shell-opt! "errexit" #f))
    (("-u") (set-shell-opt! "nounset" #t))
    (("+u") (set-shell-opt! "nounset" #f))
    (("-x") (set-shell-opt! "xtrace" #t))
    (("+x") (set-shell-opt! "xtrace" #f))
    (((and (? string?) arg)) (let* ((lst (string->string-list arg))
                                    (set (car lst)))
                               (when (not (member set '("-" "+")))
                                 (error (format #f "set: no such option:~s\n" args)))
                               (apply set-command (map (cut string-append set <>) (cdr lst)))))
    ((h ...) (last (map set-command args)))))

(define (shift-command . args)
  (lambda _
    (match args
      (() (when (pair? (cdr (%command-line)))
            (%command-line (cons (car (%command-line)) (cddr (%command-line)))))))))

(define (eval-command . args)
  (lambda _
    (match args
      (() #t)
      ((args ...)
       (let ((ast (parse-string (string-join args))))
         ;;(ignore-error (run ast))
         (run ast)
         (assignment "?" "0")
         #t)))))

(define (exit-command . args)
  (match args
    (() (exit 0))
    ((status)
     (exit (string->number status)))
    ((args ...)
     (format (current-error-port) "exit: too many arguments: ~a\n" (string-join args)))))

(define (help-command . _)
  (lambda _
    (display "\
Hello, this is GASH, Guile As SHell.

GASH is work in progress; many language constructs work, globbing
mostly works, pipes work, some redirections work.
")
    (display "\nIt has these builtin commands:\n")
    (display-tabulated (map car %builtin-commands))
    (when (or %prefer-builtins? (not (PATH-search-path "ls")))
      (display "\nand features the following, somewhat naive, bournish commands:\n")
      (display-tabulated (map car %bournish-commands)))))

(define command-command
  (case-lambda
    (() #t)
    (args
     (lambda _
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
                                (if (string? program) (begin (format #t "~a hashed (~a)\n" command program) 0)
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
                       `(command ,@args)))))))))

(define type-command
  (case-lambda
    (() #t)
    (args
     (lambda _
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
                                (if (string? program) (begin (format #t "~a hashed (~a)\n" command program) 0)
                                    1))))))))))))

(define test-command
  (case-lambda
    (() #f)
    (args
     (lambda _
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
                 (string-not-null (single-char #\n))
                 (string-null (single-char #\z))
                 (version)))
              (options (getopt-long (cons "test" args) option-spec))
              (help? (option-ref options 'help #f))
              (version? (option-ref options 'version #f))
              (files (option-ref options '() '()))
              (file (and (pair? files) (car files)))
              (no-options? (and file
                                (= (length options) 1))))
         (cond (help? (display "Usage: test [EXPRESSION]

Expression:

  STRING     equivalent to -n STRING

  STRING1 = STRING2
  STRING1 == STRING2
             the strings are equal

  STRING1 != STRING2
             the strings are not equal

Options:
  -d FILE    FILE exists and is a directory
  -e FILE    FILE exists
  -f FILE    FILE exists and is a regular file
  -h FILE    FILE exists and is a symbolic link (same as -L)
  -L FILE    FILE exists and is a symbolic link (same as -h)
  -n STRING  the length of STRING is nonzero
  -r FILE    FILE exists and read permission is granted
  -s FILE    FILE exists and has a size greater than zero
  -w FILE    FILE exists and write permission is granted
  -x FILE    FILE exists and execute (or search) permission is granted
  -z STRING  the length of STRING is zero
  --help     display this help and exit
  --version  display version information and exit
"))
               (version? (format #t "test (GASH) ~a\n" %version))
               ((null? files) #f)
               ((or (option-ref options 'string-not-null #f)
                    (and no-options?
                         (= (length files) 1)))
                (not (string-null? file)))
               ((option-ref options 'string-null #f)
                (string-null? file))
               ((and (= (length files) 3)
                     (member (cadr files) '("=" "==")))
                (match files
                  ((or (left "=" right)
                       (left "==" right))
                   (equal? left right))
                  ((left "!=" right)
                   (not (equal? left right)))
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
                (error "gash: test: not supported" args))))))))

(define bracket-command
  (case-lambda
    (() #f)
    (args
     (lambda _
       (cond ((and (pair? args) (equal? (car args) "--help"))
              (test-command "--help"))
             ((and (pair? args) (equal? (car args) "--version"))
              (test-command "--version"))
             (else
              (if (not (equal? (last args) "]")) (begin
                                                   (format (current-error-port) "gash: [: missing `]'\n")
                                                   #f)
                  (apply test-command (drop-right args 1)))))))))

(define (term->string o)
  (match o
    ((? string?) o)
    (('variable name) (variable name))
    (('variable-or name default) (variable-or name default))
    (('variable-and name default) (variable-and name default))
    (_ (format #f "~s" o))))

(define (trace commands)
  `(xtrace
    ,(lambda _
       (when (shell-opt? "xtrace")
         (for-each
          (lambda (o)
            (match o
              (('command (and command (or (? string?) ('variable _))) ...)
               (format (current-error-port) "+ ~a\n" (string-join (map term->string command))))
              (('command ('assignment name value))
               (format (current-error-port) "+ ~a=~a\n" name (term->string value)))
              (_ (format (current-error-port) "+ ~s <FIXME>\n" o))))
          (reverse commands))))))

(define %builtin-commands
  `(
    ("bg"      . ,bg-command)
    ("command" . ,command-command)
    ("cd"      . ,cd-command)
    ("echo"    . ,echo-command)
    ("eval"    . ,eval-command)
    ("exit"    . ,exit-command)
    ("fg"      . ,fg-command)
    ("help"    . ,help-command)
    ("jobs"    . ,jobs-command)
    ("pwd"     . ,pwd-command)
    ("set"     . ,set-command)
    ("shift"   . ,shift-command)
    ("test"    . ,test-command)
    ("type"    . ,type-command)
    ("["       . ,bracket-command)
    ))
