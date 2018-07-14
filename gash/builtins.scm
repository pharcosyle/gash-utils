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
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 local-eval)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (gash bournish-commands)
  #:use-module (gash config)
  #:use-module (gash environment)
  #:use-module (gash gash)
  #:use-module (gash guix-build-utils)
  #:use-module (gash io)
  #:use-module (gash job)

  #:export (
            %builtin-commands
            builtin
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
                   `(,apply ,command ',(map (cut local-eval <> (the-environment)) args))
                   command)))
            (else #f)))))

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
    (("-n" args ...) (map display args))
    (_ (map display args) (newline))))

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
            (options (getopt-long (cons "ls" args) option-spec))
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
            (options (getopt-long (cons "ls" args) option-spec))
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
            (options (getopt-long (cons "ls" args) option-spec))
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
    ("help"    . ,help-command)
    ("jobs"    . ,jobs-command)
    ("ls"      . ,ls-command)
    ("pwd"     . ,pwd-command)
    ("reboot"  . ,reboot-command)
    ("rm"      . ,rm-command)
    ("set"     . ,set-command)
    ("type"    . ,type-command)
    ("wc"      . ,wc-command)
    ("which"   . ,which-command)
    ))
