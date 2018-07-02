(define-module (gash gash)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (ice-9 ftw)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 local-eval)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 readline)
  #:use-module (ice-9 buffered-input)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)

  #:use-module (gash job)
  #:use-module (gash pipe)
  #:use-module (gash peg)
  #:use-module (gash io)
  #:use-module (gash util)

  #:export (main))

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
  ((compose read-string open-input-file) filename))

(define (string-to-ast string)
  ((compose parse remove-escaped-newlines remove-shell-comments) string))

(define (file-to-ast filename)
  ((compose string-to-ast file-to-string) filename))

(define (display-help)
  (display "\
gash [options]
  -c, --command=STRING Evaluate STRING and exit
  -d, --debug          Enable PEG tracing
  -h, --help           Display this help
  -p, --parse          Parse the shell script and print the parse tree
  --prefer-builtins    Use builtins, even if command is available in PATH
  -v, --version        Display the version
"))

(define (display-version)
  (display "
GASH 0.1

Copryright (C) 2016 R.E.W. van Beusekom, rutger.van.beusekom@gmail.com.

This is gash, Guile As SHell. Gash is free software and is covered by
the GNU Public License, see COPYING for the copyleft.

"))

(define global-variables '())

(define (main args)
  (map (lambda (key-value)
         (let* ((key-value (string-split key-value #\=))
                (key (car key-value))
                (value (cadr key-value)))
           (set! global-variables (assoc-set! global-variables key value))))
       (environ))
  (let ((thunk
         (lambda ()
           (job-control-init)
           (let* ((option-spec '((command (single-char #\c) (value #t))
                                 (debug (single-char #\d) (value #f))
                                 (help (single-char #\h) (value #f))
                                 (parse (single-char #\p) (value #f))
                                 (prefer-builtins)
                                 (version (single-char #\v) (value #f))))
                  (options (getopt-long args option-spec #:stop-at-first-non-option #t ))
                  (command? (option-ref options 'command #f))
                  (help? (option-ref options 'help #f))
                  (parse? (option-ref options 'parse #f))
                  (version? (option-ref options 'version #f))
                  (files (option-ref options '() '()))
                  (run
                   (lambda (ast)
                     (cond (parse?
                            (let ((ast- (transform ast)))
                              (stdout "parsed: " ast)
                              (stdout "prepared: " ast-)
                              #t))
                           (#t
                            (sh-exec ast))))))
             (set! %prefer-builtins? (option-ref options 'prefer-builtins #f))
             (cond
              (help? (display-help))
              (version? (display-version))
              (command? (let ((ast (string-to-ast command?)))
                          (exit (if ast (run ast)
                                    0))))
              ((pair? files)
               (let* ((asts (map file-to-ast files))
                      (status (map run asts)))
                 (quit (every identity status))))
              (#t (let* ((HOME (string-append (getenv "HOME") "/.gash_history"))
                         (thunk (lambda ()
                                  (let loop ((line (readline (prompt))))
                                    (when (not (eof-object? line))
                                      (let ((ast (string-to-ast line)))
                                        (when ast
                                          (if (not (string-null? line))
                                              (add-history line))
                                          (run ast))
                                        (loop (let ((previous (if ast "" (string-append line "\n")))
                                                    (next (readline (if ast (prompt) "> "))))
                                                (if (eof-object? next) next
                                                    (string-append previous next))))))))))
                    (clear-history)
                    (read-history HOME)
                    (with-readline-completion-function completion thunk)
                    (write-history HOME))
                  (newline)))))))
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


;; TODO: add braces
(define (glob pattern) ;; pattern -> list of path

  (define (glob? pattern)
    (string-match "\\?|\\*" pattern))

  (define (glob2regex pattern)
    (let* ((pattern (regexp-substitute/global #f "\\." pattern 'pre "\\." 'post))
           (pattern (regexp-substitute/global #f "\\?" pattern 'pre "." 'post))
           (pattern (regexp-substitute/global #f "\\*" pattern 'pre ".*" 'post)))
      (make-regexp (string-append "^" pattern "$"))))

  (define (glob-match regex path) ;; pattern path -> bool
    (regexp-match? (regexp-exec regex path)))

  (define (glob- pattern paths)
    (map (lambda (path)
           (if (string-prefix? "./" path) (string-drop path 2) path))
         (append-map (lambda (path)
                       (map (cute string-append (if (string=? "/" path) "" path) "/" <>)
                            (filter (conjoin (negate (cut string-prefix? "." <>))
                                             (cute glob-match (glob2regex pattern) <>))
                                    (or (scandir path) '()))))
                     paths)))
  (pk 'pattern: pattern 'glob:
      (cond
       ((not pattern) '(""))
       ((string=? "$?" pattern) (pk 'status: (list (assoc-ref global-variables '?))))
       ((glob? pattern) (let ((absolute? (string-prefix? "/" pattern)))
                          (let loop ((patterns (filter (negate string-null?) (string-split pattern #\/)))
                                     (paths (if absolute? '("/") '("."))))
                            (if (null? patterns)
                                paths
                                (loop (cdr patterns) (glob- (car patterns) paths))))))
       (#t (list pattern)))))

(define (background ast)
  (match ast
    (('pipeline fg rest ...) `(pipeline #f ,@rest))
    (_ ast)))

(define (PATH-search-path program)
  (search-path (string-split (getenv "PATH") #\:) program))

(define (cd-command . args)
  (match args
    (() (chdir (getenv "HOME")))
    ((dir)
     (chdir dir))
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
  (for-each display-var global-variables))

(define %commands
  ;; Built-in commands.
  `(
    ("echo" . ,echo-command)
    ("cd"   . ,cd-command)
    ("pwd"  . ,pwd-command)
    ("jobs" . ,jobs-command)
    ("bg"   . ,bg-command)
    ("fg"   . ,fg-command)
    ("set"  . ,set-command)

    ;; Bournish
    ;; ("echo"   ,(lambda strings `(list ,@strings)))
    ;; ("cd"     ,(lambda (dir) `(chdir ,dir)))
    ;; ("pwd"    ,(lambda () `(getcwd)))
    ;; ("rm"     ,rm-command)
    ;; ("cp"     ,(lambda (source dest) `(copy-file ,source ,dest)))
    ;; ("help"   ,help-command)
    ;; ("ls"     ,ls-command)
    ;; ("which"  ,which-command)
    ;; ("cat"    ,cat-command)
    ;; ("wc"     ,wc-command)
    ;; ("reboot" ,reboot-command)

    ))

(define %prefer-builtins? #t) ; use builtin, even if COMMAND is available in PATH?
(define (builtin ast)
  (receive (command args)
      (match ast
        ((('append ('glob command) args ...)) (values command args))
        ((('glob command)) (values command #f))
        (_ (values #f #f)))
    (let ((program (and command (PATH-search-path command))))
      (format (current-error-port) "command ~a => ~s ~s\n" program command args)
      (cond ((and program (not %prefer-builtins?))
             #f)
            ((and command (assoc-ref %commands command))
             =>
             (lambda (command)
               (if args
                   `(,apply ,command ,@args)
                   `(,command))))
            (else
             (match ast
               (('for-each rest ...) ast)
               (('if rest ...) ast)
               (#t #t)
               (_ #f)))))))

;; transform ast -> list of expr
;; such that (map eval expr)

(define (transform ast)
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
    (('pipeline command) (let* ((command (transform command))) (or (builtin command) `(pipeline #t ,@command))))
    (('pipeline command piped-commands) `(pipeline #t ,@(transform command) ,@(transform piped-commands)))
    (('simple-command ('word (assignment name value))) (set! global-variables (assoc-set! global-variables (transform name) (transform value))) #t)
    (('simple-command ('word s)) `((glob ,(transform s))))
    (('simple-command ('word s1) ('io-redirect "<<" ('here-document s2))) `((append (glob "echo") (cons "-n" (glob ,s2))) (glob ,(transform s1))))
    (('simple-command ('word s1) ('word s2)) `((append (glob ,(transform s1)) (glob ,(transform s2)))))
    (('simple-command ('word s1) (('word s2) ...)) `((append (glob ,(transform s1)) (append-map glob (list ,@(map transform s2))))))
    (('variable s) (assoc-ref global-variables (string-drop s 1)))
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

(define (sh-exec ast)
  (define (exec cmd)
    (format (current-error-port) "sh-exec:exec cmd=~s\n" cmd)
    (local-eval cmd (the-environment)))
  (let ((ast (transform ast)))
    (match ast
      ('script #t) ;; skip
      (_ (let* ((job (map exec ast))
                (stati (append-map (lambda (o)
                                     (cond ((job? o) (job-status o))
                                           ((boolean? o) (list (if o 0 1)))
                                           (else (list 0)))) ; some commands return a string?
                                   job))
                (status (or (find (negate zero?) (map status:exit-val stati)) 0)))
           (set! global-variables (assoc-set! global-variables '$pipe? stati))
           (set! global-variables (assoc-set! global-variables '? status))
           (set! global-variables (assoc-set! global-variables 'fubar status))
           status)))))

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
         l e "[01;34m" r cwd l e "[00m" r "$ ")))))

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
