(define-module (gash gash)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (ice-9 buffered-input)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 local-eval)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 readline)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)

  #:use-module (gash config)
  #:use-module (gash environment)
  #:use-module (gash job)
  #:use-module (gash pipe)
  #:use-module (gash peg)
  #:use-module (gash io)
  #:use-module (gash util)

  #:export (main
            %debug-level
            %prefer-builtins?
            shell-opt?))

(define %debug-level 0)       ; 1 informational, 2 verbose, 3 peg tracing
(define %prefer-builtins? #f) ; use builtin, even if COMMAND is available in PATH?

(define (remove-shell-comments s)
  (string-join (map
                (lambda (s)
                  (let* ((n (string-index s #\#)))
                    (if n (string-pad-right s (string-length s) #\space  0 n)
                        s)))
                (string-split s #\newline)) "\n"))

(define (remove-escaped-newlines s)
  (reduce (lambda (next prev)
            (let* ((escaped? (string-suffix? "\\" next))
                   (next (if escaped? (string-drop-right next 1) next))
                   (sep (if escaped? "" "\n")))
              (string-append prev sep next)))
          "" (string-split s #\newline)))

(define (file-to-string filename)
  (stdout "\n\n** " filename ":")
  ((compose read-string open-input-file) filename))

(define (string-to-ast string)
  ((compose parse remove-escaped-newlines remove-shell-comments) string))

(define (file-to-ast filename)
  ((compose string-to-ast file-to-string) filename))

(define (display-help)
  (display "\
gash [options]
  -c, --command=STRING Evaluate STRING and exit
  -e, --errexit        Exit upon error
  -d, --debug          Enable PEG tracing
  -h, --help           Display this help
  -p, --parse          Parse the shell script and print the parse tree
  --prefer-builtins    Use builtins, even if command is available in PATH
  -v, --version        Display the version
  -x, --xtrace         Print simple command trace
"))

(define (display-version)
  (display (string-append "
GASH " %version "

Copryright (C) 2016,2017,2018 R.E.W. van Beusekom <rutger.van.beusekom@gmail.com>

This is gash, Guile As SHell.  Gash is free software and is covered by
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
                                 (version (single-char #\v))
                                 (xtrace (single-char #\x))))
                  (options (getopt-long args option-spec #:stop-at-first-non-option #t ))
                  (command? (option-ref options 'command #f))
                  (opt? (lambda (name) (lambda (o) (and (eq? (car o) name) (cdr o)))))
                  (debug (length (filter-map (opt? 'debug) options)))
                  (debug? (option-ref options 'debug #f))
                  (help? (option-ref options 'help #f))
                  (parse? (option-ref options 'parse #f))
                  (version? (option-ref options 'version #f))
                  (files (option-ref options '() '())))
             (set! %prefer-builtins? (option-ref options 'prefer-builtins #f))
             (set-shell-opt! "errexit" (option-ref options 'errexit #f))
             (set-shell-opt! "xtrace" (option-ref options 'xtrace #f))
             (when (option-ref options 'debug #f)
               (set! %debug-level debug))
             (cond
              (help? (display-help))
              (version? (display-version))
              (command? (let ((ast (string-to-ast command?)))
                          (exit (assoc-ref %global-variables "?"))))
              ((pair? files)
               (let* ((asts (map file-to-ast files))
                      (status (assoc-ref %global-variables "?")))
                 (exit status)))
              (#t (let* ((HOME (string-append (getenv "HOME") "/.gash_history"))
                         (thunk (lambda ()
                                  (let loop ((line (readline (prompt))))
                                    (when (not (eof-object? line))
                                      (let* ((ast (string-to-ast line)))
                                        (when (and ast
                                                   (not (string-null? line)))
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

(define (expand identifier o) ;;identifier-string -> symbol
  (define (expand- o)
    (let ((dollar-identifier (string-append "$" identifier)))
      (match o
        ((? symbol?) o)
        ((? string?) (if (string=? o dollar-identifier) (string->symbol identifier) o))
        ((? list?) (map expand- o))
        (_ o))))
  (map expand- o))

(define (DEAD-background ast)
  (match ast
    (('pipeline fg rest ...) `(pipeline #f ,@rest))
    (_ ast)))

(define (shell-opt? name)
  (member name (string-split (assoc-ref %global-variables "SHELLOPTS") #\:)))

(define (tostring . args)
  (with-output-to-string (cut map display args)))

;; transform ast -> list of expr
;; such that (map eval expr)
(define (DEAD-transform ast)
  (format (current-error-port) "transform=~s\n" ast)
  (match ast
     (('script term "&") (list (background (transform term))))
     (('script term) `(,(transform term)))
     (('script terms ...) (transform terms))
     (('substitution "$(" script ")") (local-eval (cons 'substitute (cddr (car (transform script)))) (the-environment)))
     (('substitution "`" script "`") (local-eval (cons 'substitute (cddr (car (transform script)))) (the-environment)))
     ((('term command)) `(,(transform command)))
     ((('term command) ...) (map transform command))
     ((('term command) (('term commands) ...)) (map transform (cons command commands)))
     (('compound-list terms ...) (transform terms))
     (('if-clause "if" (expression "then" consequent "fi"))
      `(if (equal? 0 (status:exit-val ,@(transform expression)))
           (begin ,@(transform consequent))))
     (('if-clause "if" (expression "then" consequent ('else-part "else" alternative) "fi"))
      `(if (equal? 0 (status:exit-val ,@(transform expression)))
           (begin ,@(transform consequent))
           (begin ,@(transform alternative))))
     (('for-clause ("for" identifier sep do-group)) #t)
     (('for-clause "for" ((identifier "in" lst sep) do-group))
      `(for-each (lambda (,(string->symbol identifier))
                   (begin ,@(expand identifier (transform do-group))))
                 (glob ,(transform lst))))
     (('do-group "do" (command "done")) (transform command))
     (('pipeline command) (pk 1) (let* ((command (transform command))) (or (builtin command) `(pipeline #t ,@command))))
     (('pipeline command piped-commands) (pk 2) `(pipeline #t ,@(transform command) ,@(transform piped-commands)))
     (('simple-command ('word (assignment name value))) `((lambda _ (let ((name ,(tostring (transform name)))
                                                                          (value ,(tostring (transform value))))
                                                                      (stderr "assignment: " name "=" value)
                                                                      (set! global-variables (assoc-set! global-variables name (glob value)))))))
     (('simple-command ('word s)) `((glob ,(transform s))))
     (('simple-command ('word s1) ('io-redirect "<<" ('here-document s2))) `((append (glob "echo") (cons "-n" (glob ,s2))) (glob ,(transform s1))))
     (('simple-command ('word s1) ('word s2)) `((append (glob ,(transform s1)) (glob ,(transform s2)))))
     (('simple-command ('word s1) (('word s2) ...)) `((append (glob ,(transform s1)) (append-map glob (list ,@(map transform s2))))))
     (('variable s) s)
     (('literal s) (transform s))
     (('singlequotes s) (string-concatenate `("'" ,s "'")))
     (('doublequotes s) (string-concatenate `("\"" ,s "\"")))
     (('backticks s) (string-concatenate `("`" ,s "`")))
     (('delim ('singlequotes s ...)) (string-concatenate (map transform s)))
     (('delim ('doublequotes s ...)) (string-concatenate (map transform s)))
     (('delim ('backticks s ...)) (string-concatenate (map transform s)))
     ((('pipe _) command) (transform command))
     (((('pipe _) command) ...) (map (compose car transform) command))
     ((_ o) (transform o)) ;; peel the onion: (symbol (...)) -> (...)
     (_ ast))) ;; done

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


(define (filename-completion text continue?)
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
  (or (filename-completion text continue?) (search-binary-in-path-completion text continue?)))
