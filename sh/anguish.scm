(define-module (sh anguish)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-26)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 match)
  :use-module (ice-9 pretty-print)
  :use-module (ice-9 rdelim)
  :use-module (ice-9 readline)
  :use-module (ice-9 ftw)
  :use-module (ice-9 local-eval)

  :use-module (sh pipe)
  :use-module (sh peg)

  :export (main))

(define (stdout . o)
  (map (lambda (o) (display o (current-output-port))) o)
  (newline)
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
                                          (format (current-output-port) "prepared: ~s\n\n" ast-)
                                          #t))
                                       (#t
                                        (sh-exec ast)
                                        #t))))))
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
                                   (add-history line)
                                   (run ast))
                                 (loop (readline (prompt)))))))))
           (activate-readline)
           (clear-history)
           (read-history HOME)
           (with-readline-completion-function completion thunk)
           ;;(thunk)
           (write-history HOME))
         (newline)))))


(define (remove-shell-comments s)
  (string-join (map
                (lambda (s)
                  (let* ((n (string-index s #\#)))
                    (if n (string-pad-right s (string-length s) #\space  0 n)
                        s)))
                (string-split s #\newline)) "\n"))

(define (expand identifier o) ;;identifier-string -> symbol
  (define (foo o)
    (let ((dollar-identifier (string-append "$" identifier)))
      (match o
        ((? symbol?) o)
        ((? string?) (if (string=? o dollar-identifier) (string->symbol identifier) o))
        ((? list?) (map foo o)))))
  (map foo o))

(define (glob o) ;;HAX0R, more todo
  (define (foo o)
    (match o
      ("*" (map car (cddr (file-system-tree (getcwd)))))
      ((? symbol?) o)
      ((? string?) o)
      ((? list?) (map foo o))
      (_ o)))
  (map foo o))

(define (transform ast)
  (match ast
    (('script command) (transform command))
    (('script command separator) (transform command))
    (('if-clause "if" (expression "then" consequent "fi")) `(if (equal? 0 (status:exit-val ,(transform expression))) ,(transform consequent)))
    (('if-clause "if" (expression "then" consequent ('else-part "else" alternative) "fi")) `(if (equal? 0 (status:exit-val ,(transform expression))) ,(transform consequent) ,(transform alternative)))
    (('for-clause "for" ((identifier "in" lst sep) do-group)) (list 'for-each (list 'lambda (list (string->symbol identifier)) (expand identifier (transform do-group))) (cons 'list (transform (glob lst)))))
    (('do-group "do" (command "done")) (transform command))
    (('pipeline command) (let ((command (transform command))) (if (eq? 'list (car command)) `(apply system* ,command) command)))
    (('pipeline command piped-commands) (cons 'pipeline (cons (transform command) (transform piped-commands))))
    (('simple-command ('word s)) `(list ,(transform s)))
    (('simple-command ('word s1) ('word s2)) `(list ,(transform s1) ,(transform s2)))
    (('simple-command ('word s1) (('word s2) ...)) (append `(list ,(transform s1)) (map transform s2)))
    (('literal s) (transform s))
    (('singlequotes s) (string-concatenate (list "'" s "'")))
    (('doublequotes s) (string-concatenate (list "\"" s "\"")))
    (('backticks s) (string-concatenate (list "`" s "`")))
    (('delim ('singlequotes s ...)) (string-concatenate (map transform s)))
    (('delim ('doublequotes s ...)) (string-concatenate (map transform s)))
    (('delim ('backticks s ...)) (string-concatenate (map transform s)))
    ((('pipe _) command ...) (map transform command))
    (((('pipe _) command) ...) (map transform command))
    ((_ o) (transform o)) ;;peel the onion: (symbol (...)) -> (...)
    (_ ast)))

(define (sh-exec ast) ;; (local-eval (transform ast) (the-environment))
  ;;(stdout "parsed: " ast)
  (let ((cmd (transform ast)))
    ;;(stdout "executing: " cmd)
    (match cmd
      (('list "cd" argument ...) (apply chdir argument))
      ('script '())
      (_ (local-eval cmd (the-environment))))))

(define (prompt)
  (let* ((esc (string #\033))
         (CWD (getcwd))
         (HOME (getenv "HOME"))
         (cwd (if (string-prefix? HOME CWD)
                  (string-replace CWD "~" 0 (string-length HOME))
                  CWD)))
    (string-append esc "[01;34m" cwd esc "[00m$ ")))

(define (redraw-current-line)
  (dynamic-call (dynamic-func "rl_refresh_line"
                              (dynamic-link "libreadline.so"))
                #f))

(define (filename-completion text state)
  (if (not state)
      (let ((completions (map car
                              (filter (cut string-prefix? text <>)
                                      (map car (cddr (file-system-tree (getcwd))))))))
        (cond ((< 1 (length completions)) (begin (newline)
                                                 (display (string-join completions " ")) (newline)
                                                 (redraw-current-line)
                                                 #f))
              ((= 1 (length completions)) (car completions))
              (#t #f)))
      #f))

(define (search-binary-in-path-completion text state)
  (if (not state)
      (let ((completions (map car
                              (filter (cut string-prefix? text <>)
                                      (map car (cddr (file-system-tree "/bin")))))))
        (cond ((< 1 (length completions)) (begin (newline)
                                                 (display (string-join completions " ")) (newline)
                                                 (redraw-current-line)
                                                 #f))
              ((= 1 (length completions)) (car completions))
              (#t #f)))
      #f))

(define (completion text state)
  (or (filename-completion text state)
      ;;(search-binary-in-path-completion text state)
      ))
