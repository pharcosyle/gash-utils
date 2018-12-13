(define-module (gash gash)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (ice-9 buffered-input)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 local-eval)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)

  #:use-module (gash config)
  #:use-module (gash builtins)
  #:use-module (gash bournish-commands)
  #:use-module (gash environment)
  #:use-module (gash job)
  #:use-module (gash pipe)
  #:use-module (gash io)
  #:use-module (gash script)
  #:use-module (gash util)

  #:export (main
            %debug-level
            %prefer-builtins?
            parse
            parse-string))

(catch #t
  (lambda _ (use-modules (ice-9 readline)))
  (lambda (key . args)
    (use-modules (gash readline))))

(define %debug-level 0)       ; 1 informational, 2 verbose, 3 peg tracing
(define %prefer-builtins? #f) ; use builtin, even if COMMAND is available in PATH?
(define %geesh-parser? #f)    ; use Geesh parser [EXPERIMENTAL]

(define (parse-string string)
  (let ((parser (cond (%geesh-parser? (@ (gash geesh) parse-string))
                      (else (@ (gash grammar) parse-string)))))
    (parser string)))

(define (parse port)
  (let ((parser (cond (%geesh-parser? (@ (gash geesh) parse))
                      (else (@ (gash grammar) parse)))))
    (parser port)))

(define (file-to-ast file-name)
  (call-with-input-file file-name parse))

(define (display-help)
  (let ((builtins (sort (map car (append (%bournish-commands) ;;%builtin-commands
                                         )) string<)))
    (display (string-append "\
Usage: gash [OPTION]... [FILE]...
  or gash [OPTION]... -- BUILTIN [ARG]...

Options:
  -c, --command=STRING  Evaluate STRING and exit
  -e, --errexit         Exit upon error
  -d, --debug           Enable PEG tracing
  -g, --geesh           Use Geesh parser [EXPERIMENTAL]
  -h, --help            Display this help
  -p, --parse           Parse the shell script and print the parse tree
  --prefer-builtins     Use builtins, even if command is available in PATH
  -v, --version         Display the version
  -x, --xtrace          Print simple command trace

Builtins:
  " (string-join builtins) "
"))))

(define (display-version)
  (display (string-append "
gash (GASH) " %version "

Copyright (C) 2016,2017,2018 R.E.W. van Beusekom <rutger.van.beusekom@gmail.com>
and others.

This is Gash, Guile As SHell.  Gash is free software and is covered by
the GNU General Public License version 3 or later, see COPYING for the
copyleft.
")))

(define (main args)
  (let ((thunk
         (lambda ()
           (job-control-init)
           (let* ((option-spec '((command (single-char #\c) (value #t))
                                 (debug (single-char #\d))
                                 (errexit (single-char #\e))
                                 (help (single-char #\h))
                                 (parse (single-char #\p))
                                 (prefer-builtins)
                                 (geesh (single-char #\g))
                                 (version (single-char #\v))
                                 (xtrace (single-char #\x))))
                  (builtin-command-line (and=> (member "--" args) cdr))
                  (args (take-while (negate (cut equal? <> "--")) args))
                  (options (getopt-long args option-spec #:stop-at-first-non-option #t))
                  (command? (option-ref options 'command #f))
                  (opt? (lambda (name) (lambda (o) (and (eq? (car o) name) (cdr o)))))
                  (debug (length (filter-map (opt? 'debug) options)))
                  (debug? (option-ref options 'debug #f))
                  (help? (option-ref options 'help #f))
                  (parse? (option-ref options 'parse #f))
                  (version? (option-ref options 'version #f))
                  (files (option-ref options '() '())))
             (set! %prefer-builtins? (option-ref options 'prefer-builtins #f))
             (set! %geesh-parser? (option-ref options 'geesh #f))
             (set-shell-opt! "errexit" (option-ref options 'errexit #f))
             (set-shell-opt! "xtrace" (option-ref options 'xtrace #f))
             (when (option-ref options 'debug #f)
               (set! %debug-level debug))
             (cond
              (help? (display-help))
              (version? (display-version))
              (command? (let ((ast (parse-string command?)))
                          (if parse? (pretty-print ast)
                              (run ast))
                          (exit (script-status))))
              ((pair? files)
               (let* ((script (car files))
                      (ast (file-to-ast script)))
                 (if parse? (pretty-print ast)
                     (parameterize ((%command-line files))
                      (run ast)))
                 (exit (script-status))))
              (builtin-command-line
               (let* ((builtin (car builtin-command-line))
                      (args (cdr builtin-command-line))
                      (command (assoc-ref (%bournish-commands) builtin)))
                 ((apply command args))))
              (#t (let* ((HOME (string-append (getenv "HOME") "/.gash_history"))
                         (thunk (lambda ()
                                  (let loop ((line (readline (prompt))))
                                    (when (not (eof-object? line))
                                      (let* ((ast (parse-string line)))
                                        (when (and ast
                                                   (not (string-null? line)))
                                          (unless parse?
                                            (run ast))
                                          (add-history line))
                                        (loop (let ((previous (if ast "" (string-append line "\n")))
                                                    (next (readline (if ast (prompt) "> "))))
                                                (if (eof-object? next) next
                                                    (string-append previous next))))))))))
                    (clear-history)
                    (read-history HOME)
                    (with-readline-completion-function completion thunk)
                    (write-history HOME)
                    (newline))))))))
    (thunk)))

(define prompt
  (let* ((l (string #\001))
         (r (string #\002))
         (e (string #\033))
         (user (getenv "USER"))
         (host (gethostname))
         (home (getenv "HOME")))
    (lambda ()
      (let* ((cwd (getcwd))
             (cwd (if (string-prefix? home cwd)
                      (string-replace cwd "~" 0 (string-length home))
                      cwd)))
        (report-jobs)
        (string-append
         l e "[01;32m" r user "@" host l e "[00m" r ":"
         l e "[01;34m" r cwd l e "[00m" r (if (zero? (getuid)) "# " "$ "))))))

(define (string-prefix s1 s2)
  (substring/read-only s1 0 (string-prefix-length s1 s2)))

(define next->file-completion (lambda () #f))
(define next->binary-completion (lambda () #f))

(define (isdir? path)
  (and (access? path F_OK) (eq? 'directory (stat:type (stat path)))))

(define (ls dir)
  (map (lambda (path)
         (if (isdir? (string-append dir path))
             (string-append path "/")
             path))
       (sort (filter (negate (cut string-every #\. <>))
                     (scandir (if (string-null? dir) (getcwd) dir))) string<?)))

(define (complete prefix list)
  (if (string-null? prefix) list
      (filter (cut string-prefix? prefix <>) list)))

(define (slash dir)
  (if (string-suffix? "/" dir) dir
      (string-append dir "/")))

(define (after-slash path)
  (let ((at (string-index-right path #\/)))
    (if at (string-drop path (+ 1 at))
        path)))


(define (file-name-completion text continue?)
  (if continue?
      (next->file-completion)
      (let* ((dir (slash (if (isdir? text) text (dirname text))))
             (listing (ls dir))
             (dir (if (string=? "./" dir) "" dir))
             (completions (complete (after-slash text) listing)))
        (set! next->file-completion
          (lambda ()
            (if (null? completions)
                #f
                (let ((completion (car completions)))
                  (set! completions (cdr completions))
                  (string-append dir completion)))))
        (next->file-completion))))

(define (search-binary-in-path-completion text continue?)
  (if (not continue?)
      (let* ((paths (string-split (getenv "PATH") #\:))
             (binaries (apply append (filter identity (map scandir paths))))
             (completions (sort (filter (cut string-prefix? text <>) binaries) string<?)))
        (set! next->binary-completion (lambda ()
                                        (if (null? completions)
                                            #f
                                            (let ((completion (car completions)))
                                              (set! completions (cdr completions))
                                              completion))))
        (next->binary-completion))
      (next->binary-completion)))

(define (completion text continue?)
  (or (file-name-completion text continue?) (search-binary-in-path-completion text continue?)))
