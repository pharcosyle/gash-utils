(define-module (sh anguish)
  :use-module (srfi srfi-1)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 match)
  :use-module (ice-9 pretty-print)
  :use-module (ice-9 rdelim)
  :use-module (ice-9 readline)
  :use-module (ice-9 ftw)

  :export (main))

(use-modules ((sh pipe) :renamer (symbol-prefix-proc 'sh:)))
(use-modules ((sh peg) :renamer (symbol-prefix-proc 'sh:)))

(define (file-to-string filename)
  ((compose read-string open-input-file)  filename))

(define (string-to-ast string)
  ((compose sh:parse remove-shell-comments) string))

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
                                          (display "parsed  : ") (display ast) (newline)(newline)
                                          (display "prepared: ") (display ast-) (newline)(newline)
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

(define (builtin cmd)
  (if (and (pair? cmd) (string? (car cmd)) (string=? (car cmd) "cd"))
      (lambda () (chdir (cadr cmd)))
      #f))

(define (transform ast)
  (match ast
    (('script command 'separator) (transform command))
    (('pipeline command) (transform command))
    (('pipeline command piped-commands) (cons 'pipeline (cons (transform command) (transform piped-commands))))
    (('simple-command ('word s)) (list (transform s)))
    (('simple-command ('word s1) ('word s2)) (list (transform s1) (transform s2)))
    (('simple-command ('word s1) (('word s2) ...)) (cons (transform s1) (map transform s2)))
    (('literal s) (transform s))
    (('singlequotes s) (string-concatenate (list "'" s "'")))
    (('doublequotes s) (string-concatenate (list "\"" s "\"")))
    (('backticks s) (string-concatenate (list "`" s "`")))
    (('delim ('singlequotes s ...)) (string-concatenate (map transform s)))
    (('delim ('doublequotes s ...)) (string-concatenate (map transform s)))
    (('delim ('backticks s ...)) (string-concatenate (map transform s)))
    ((('pipe _) command ...) (map transform command))
    (((('pipe _) command) ...) (map transform command))
    ((_ o) (transform o))
    (_ ast)))

(define (sh-exec ast)
  (let ((cmd (transform ast)))
    ;(display "executing: ")(display cmd) (newline)
    (if (builtin cmd)
        ((builtin cmd))
        (if (and (pair? cmd) (eq? 'pipeline (car cmd)))
            (sh:pipeline (cdr cmd))
            (apply system* cmd)))))

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
                              (filter (lambda (entry) (string-prefix? text (car entry)))
                                      (cddr (file-system-tree (getcwd)))))))
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
                              (filter (lambda (entry) (string-prefix? text (car entry)))
                                      (cddr (file-system-tree "/bin"))))))
        (cond ((< 1 (length completions)) (begin (newline)
                                                 (display (string-join completions " ")) (newline)
                                                 (redraw-current-line)
                                                 #f))
              ((= 1 (length completions)) (car completions))
              (#t #f)))
      #f))

(define (completion text state)
  (or (filename-completion text state)
      ;(search-binary-in-path-completion text state)
      ))
