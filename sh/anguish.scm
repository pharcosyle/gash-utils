(define-module (sh anguish)
  :use-module (statprof)

  :use-module (srfi srfi-1)
  :use-module (srfi srfi-26)
  :use-module (ice-9 ftw)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 local-eval)
  :use-module (ice-9 match)
  :use-module (ice-9 pretty-print)
  :use-module (ice-9 rdelim)
  :use-module (ice-9 readline)
  :use-module (ice-9 regex)

  :use-module (sh pipe)
  :use-module (sh peg)

  :export (main))

(define (stdout . o)
  (map (lambda (o) (display o (current-output-port))) o)
  (newline (current-output-port))
  (force-output (current-output-port))
  o)

(define (stderr . o)
  (map (lambda (o) (display o (current-error-port))) o)
  (newline)
  o)

(define (file-to-string filename)
  ((compose read-string open-input-file)  filename))

(define (string-to-ast string)
  ((compose parse remove-shell-comments) string))

(define (file-to-ast filename)
  ((compose string-to-ast file-to-string) filename))

(define (main args)
  (let ((thunk (lambda ()
                 (job-control-init)
                 (let* ((option-spec '((debug (single-char #\d) (value #f))
                                       (help (single-char #\h) (value #f))
                                       (parse (single-char #\p) (value #f))
                                       (version (single-char #\v) (value #f))))
                        (options (getopt-long args option-spec
                                              #:stop-at-first-non-option #t ))
                        (help? (option-ref options 'help #f))
                        (parse? (option-ref options 'parse (null? #f)))
                        (version? (option-ref options 'version #f))
                        (files (option-ref options '() '()))
                        (run (lambda (ast) (and ast
                                                (cond (parse?
                                                       (let ((ast- (transform ast)))
                                                         (format (current-output-port) "parsed  : ~s\n\n" ast)
                                                         (format (current-output-port) "prepared  : ~s\n\n" ast-)
                                                         #t))
                                                      (#t
                                                       (sh-exec ast)))))))
                   (cond
                    (help?
                     (display "\
anguish [options]
  -h, --help       Display this help
  -p, --parse      Parse the shell script and print the parse tree
  -v, --version    Display the version
"))
                    (version?
                     (display "
Anguish 0.1
Copryright (C) 2016 R.E.W. van Beusekom, rutger.van.beusekom@gmail.com.

This is anguish, ANother GUIle SHell, or the feeling you might have
when your shell lacks a real programming language. Anguish is free
software and is covered by the GNU Public License, see COPYING for the
copyleft.
"))
                    ((pair? files)
                     (let* ((asts (map file-to-ast files))
                            (status (map run asts)))
                       (quit (every identity status))))
                    (#t (let* ((HOME (string-append (getenv "HOME") "/.anguishistory"))
                               (thunk (lambda ()
                                        (let loop ((line (readline (prompt))))
                                          (if (not (eof-object? line))
                                              (begin
                                                (let ((ast (string-to-ast line)))
                                                  (if (not (string-null? line))
                                                      (add-history line))
                                                  (run ast))
                                                (loop (readline (prompt)))))))))
                          (clear-history)
                          (read-history HOME)
                          (with-readline-completion-function completion thunk)
                          (write-history HOME))
                        (newline)))))))
    ;;(statprof thunk #:hz 100 #:count-calls? #t)
    (thunk)))

(define (remove-shell-comments s)
  (string-join (map
                (lambda (s)
                  (let* ((n (string-index s #\#)))
                    (if n (string-pad-right s (string-length s) #\space  0 n)
                        s)))
                (string-split s #\newline)) "\n"))

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
    (let* ((regex (regexp-substitute/global #f "\\." pattern 'pre "\\." 'post))
           (regex (regexp-substitute/global #f "\\?" pattern 'pre "." 'post))
           (regex (regexp-substitute/global #f "\\*" pattern 'pre ".*" 'post)))
      (make-regexp (string-append "^" regex "$"))))

  (define (glob-match regex path) ;; pattern path -> bool
    (regexp-match? (regexp-exec regex path)))

  (define (glob- pattern paths)
    (append-map (lambda (path)
                  (let ((empty? (string=? "" path)))
                    (map (lambda (extension) (if empty? extension (string-join (list path "/" extension) "")))
                         (filter (cute glob-match (glob2regex pattern) <>)
                                 (filter (negate (cut string-any #\. <> 0 1)) (scandir (if empty? (getcwd) path)))))))
                paths))

  (if (glob? pattern)
      (let ((absolute? (char=? #\/ (string-ref pattern 0))))
        (let loop ((patterns (string-split pattern #\/))
                   (paths (if absolute? '("/") `(""))))
          (if (null? patterns) paths
              (loop (cdr patterns) (glob- (car patterns) paths)))))
      (list pattern)))


(define (builtin ast)
  (match ast
    (('append ('glob "cd") arg) `(apply chdir ,arg))
    (('append ('glob "fg") ('glob arg)) `(fg ,(string->number arg)))
    (('append ('glob "bg") ('glob arg)) `(bg ,(string->number arg)))
    (('append ('glob "echo") args ...) `(stdout (string-join ,@args " ")))
    (('glob "echo") `(stdout))
    (('glob "fg") `(fg 1))
    (('glob "bg") `(bg 1))
    (('glob "jobs") `(jobs))
    (('for-each rest ...) ast)
    (('if rest ...) ast)
    (_ #f)))

(define (background ast)
  (match ast
    (('pipeline fg rest ...) `(pipeline #f ,@rest))
    (_ ast)))

;; transform ast -> list of expr
;; such that (map eval expr)

(define (transform ast)
  (match ast
    (('script term "&") (list (background (transform term))))
    (('script term) (list (transform term)))
    (('script terms ...) (transform terms))
    ((('term command)) (list (transform command)))
    ((('term command) ...) (map transform command))
    ((('term command) (('term commands) ...)) (map transform (cons command commands)))
    (('compound-list terms ...) (transform terms))
    (('if-clause "if" (expression "then" consequent "fi")) `(if (equal? 0 (status:exit-val (begin ,@(transform expression)))) (begin ,@(transform consequent))))
    (('if-clause "if" (expression "then" consequent ('else-part "else" alternative) "fi")) `(if (equal? 0 (status:exit-val ,@(transform expression))) (begin ,@(transform consequent)) (begin ,@(transform alternative))))
    (('for-clause "for" ((identifier "in" lst sep) do-group)) `(for-each (lambda (,(string->symbol identifier)) (begin ,@(expand identifier (transform do-group)))) (glob ,(transform lst))))
    (('do-group "do" (command "done")) (transform command))
    (('pipeline command) (let* ((command (transform command))) (or (builtin command) `(pipeline #t ,command))))
    (('pipeline command piped-commands) `(pipeline #t ,(transform command) ,@(transform piped-commands)))
    (('simple-command ('word s)) `(glob ,(transform s)))
    (('simple-command ('word s1) ('word s2)) `(append (glob ,(transform s1)) (glob ,(transform s2))))
    (('simple-command ('word s1) (('word s2) ...)) `(append (glob ,(transform s1)) (append-map glob (list ,@(map transform s2)))))
    (('literal s) (transform s))
    (('singlequotes s) (string-concatenate (list "'" s "'")))
    (('doublequotes s) (string-concatenate (list "\"" s "\"")))
    (('backticks s) (string-concatenate (list "`" s "`")))
    (('delim ('singlequotes s ...)) (string-concatenate (map transform s)))
    (('delim ('doublequotes s ...)) (string-concatenate (map transform s)))
    (('delim ('backticks s ...)) (string-concatenate (map transform s)))
    ((('pipe _) command ...) (map transform command))
    (((('pipe _) command) ...) (map transform command))
    ((_ o) (transform o)) ;; peel the onion: (symbol (...)) -> (...)
    (_ ast))) ;; done

(define (sh-exec ast)
  (define (exec cmd)
    (local-eval cmd (the-environment)))
  (let* (;;(print (format (current-error-port) "parsed: ~s\n" ast))
         (ast (transform ast))
         ;;(print (format (current-error-port) "transformed: ~s\n" ast))
         )
    (match ast
      ('script #t) ;; skip
      (_ (begin (map exec ast) #t)))))


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

(define (redraw-current-line)
  (dynamic-call (dynamic-func "rl_refresh_line"
                              (dynamic-link "libreadline.so"))
                #f))

(define (filename-completion text state)
  (if (not state)
      (let ((completions (filter (cut string-prefix? text <>)
                                 (scandir (getcwd)))))
        (cond ((< 1 (length completions)) (begin (newline)
                                                 (display (string-join completions " ")) (newline)
                                                 (redraw-current-line)
                                                 #f))
              ((= 1 (length completions)) (car completions))
              (#t #f)))
      #f))

(define (search-binary-in-path-completion text state)
  (if (not state)
      (let ((completions (filter (cut string-prefix? text <>)
                                 (scandir "/bin"))))
        (cond ((< 1 (length completions)) (begin (newline)
                                                 (display (string-join completions " ")) (newline)
                                                 (redraw-current-line)
                                                 #f))
              ((= 1 (length completions)) (car completions))
              (#t #f)))
      #f))

(define (completion text state)
  (or (filename-completion text state)
      (search-binary-in-path-completion text state)))
