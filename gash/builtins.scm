(define-module (gash builtins)
  #:use-module (ice-9 match)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (gash bournish-commands)
  #:use-module (gash gash)
  #:use-module (gash job)
  #:use-module (gash peg)

  #:export (
            %builtin-commands
            PATH-search-path
            bg-command
            cd-command
            echo-command
            exit-command
            fg-command
            help-command
            pwd-command
            set-command
            ))
 
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
Hello, this is gash, Guile As SHell.

Gash is work in progress; many language constructs work, globbing
mostly works, pipes work, some redirections work.
")
  (when (or %prefer-builtins? (not (PATH-search-path "ls")))
    (display "\nIt features the following, somewhat naive builtin commands\n")
    (display-tabulated (map car %commands))))

(define (cp-command-implementation source dest . rest)
  (copy-file source dest))

(define cp-command (wrap-command cp-command-implementation "cp"))

(define %builtin-commands
  `(
    ("bg"     . ,bg-command)
    ("cat"    . ,cat-command)
    ("cd"     . ,cd-command)
    ("cp"     . ,cp-command)
    ("echo"   . ,echo-command) BROKEN wrt variables for now
    ("exit"   . ,exit-command)
    ("fg"     . ,fg-command)
    ("help"   . ,help-command)
    ("jobs"   . ,jobs-command)
    ("ls"     . ,ls-command)
    ("pwd"    . ,pwd-command)
    ("reboot" . ,reboot-command)
    ("rm"     . ,rm-command)
    ("set"    . ,set-command)
    ("wc"     . ,wc-command)
    ("which"  . ,which-command)
    ))
