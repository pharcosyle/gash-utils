(define-module (geesh shell)
  #:use-module (geesh built-ins)
  #:use-module (geesh environment)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (sh:exec-let
            sh:exec
            sh:subshell
            sh:substitute-command
            sh:with-redirects))

;;; Commentary:
;;;
;;; This module provides functions for executing Shell language
;;; constructs.
;;;
;;; Code:

(define *fd-count* 3)

(define (fd->current-port fd)
  "Return the current port (e.g. @code{current-input-port})
corresponding to the the Shell file descriptor @var{fd}."
  (match fd
    (0 current-input-port)
    (1 current-output-port)
    (2 current-error-port)))

(define (install-current-ports!)
  "Install all current ports into their usual file descriptors.  For
example, if @code{current-input-port} is a @code{file-port?}, make the
process file descriptor 0 refer to the file open for
@code{current-input-port}.  If any current port is a @code{port?} but
not a @code{file-port?}, its corresponding file descriptor will refer
to @file{/dev/null}."
  ;; XXX: Input/output ports?  Closing other FDs?
  (for-each (lambda (i)
              (match ((fd->current-port i))
                ((? file-port? port)
                 (dup port i))
                ((? input-port? port)
                 (dup (open-file "/dev/null" "r") i))
                ((? output-port? port)
                 (dup (open-file "/dev/null" "w") i))
                (_ #t)))
            (iota *fd-count*)))

(define (exec-utility env bindings path name args)
  "Execute @var{path} as a subprocess with environment @var{env} and
extra environment variables @var{bindings}.  The first argument given
to the new process will be @var{name}, and the rest of the arguments
will be @var{args}."
  (let ((utility-env (environment->environ env bindings)))
    (match (primitive-fork)
      (0 (install-current-ports!)
         (apply execle path utility-env name args))
      (pid (match-let (((pid . status) (waitpid pid)))
             (set-var! env "?" (number->string (status:exit-val status))))))))

(define (slashless? s)
  "Test if the string @var{s} does not contain any slashes ('/')."
  (not (string-index s #\/)))

(define (split-search-path s)
  "Split the search path string @var{s}."
  (if (string-null? s) '() (string-split s #\:)))

(define (find-utility env name)
  "Search for the path of the utility @var{name} using @var{env}.  If
it cannot be found, return @code{#f}."
  (let loop ((prefixes (split-search-path (var-ref* env "PATH"))))
    (and (pair? prefixes)
         (let* ((prefix (car prefixes))
                (path (if (string-suffix? "/" prefix)
                          (string-append prefix name)
                          (string-append prefix "/" name))))
           (if (access? path X_OK)
               path
               (loop (cdr prefixes)))))))

(define (sh:exec-let env bindings name . args)
  "Find and execute @var{name} with arguments @var{args}, environment
@var{env}, and extra environment variable bindings @var{bindings}."
  (if (slashless? name)
      (or (and=> (search-special-built-ins name)
                 (lambda (proc)
                   (for-each (match-lambda
                               ((name . value)
                                (set-var! env name value)))
                             bindings)
                   (apply proc env args)))
          ;; TODO: Functions.
          (and=> (search-built-ins name)
                 (lambda (proc)
                   ;; TODO: Use 'bindings' here.
                   (apply proc env args)))
          (and=> (find-utility env name)
                 (lambda (path)
                   (exec-utility env bindings path name args)))
          (error "Command not found."))
      (exec-utility env bindings name name args)))

(define (sh:exec env name . args)
  "Find and execute @var{name} with arguments @var{args} and
environment @var{env}."
  (apply sh:exec-let env '() name args))


;;; Redirects.

(define (save-and-set-redirect env redir)
  "Update the current port parameters according to @code{redir}, and
return a pair consisting of the Shell file descriptor that has been
changed and a copy of its old value."

  (define* (save-and-set fd target #:optional (open-flags 0))
    (let ((saved-port ((fd->current-port fd))))
      (match target
        ((? port?) ((fd->current-port fd) target))
        ((? string?) ((fd->current-port fd) (open target open-flags)))
        ;; TODO: Verify open-flags.
        ((? integer?) ((fd->current-port fd) ((fd->current-port target))))
        (#f (close-port (fd->current-port fd))))
      `(,fd . ,saved-port)))

  (match redir
    (('< (? integer? fd) (? string? filename))
     (save-and-set fd filename O_RDONLY))
    (('> (? integer? fd) (? string? filename))
     ;; TODO: Observe noclobber.
     (save-and-set fd filename (logior O_WRONLY O_CREAT O_TRUNC)))
    (('>! (? integer? fd) (? string? filename))
     (save-and-set fd filename (logior O_WRONLY O_CREAT O_TRUNC)))
    (('>> fd filename)
     (save-and-set fd filename (logior O_WRONLY O_CREAT O_APPEND)))
    (('<> fd filename)
     (save-and-set fd filename (logior O_RDWR O_CREAT)))
    (('<& (? integer? fd1) (? integer? fd2))
     (save-and-set fd1 fd2))
    (('<& (? integer? fd) '-)
     (save-and-set fd #f))
    (('>& (? integer? fd1) (? integer? fd2))
     (save-and-set fd1 fd2))
    (('>& (? integer? fd) '-)
     (save-and-set fd #f))
    (('<< (? integer? fd) text)
     (let ((port (tmpfile)))
       (display text port)
       (seek port 0 SEEK_SET)
       (save-and-set fd port)))))

(define (restore-saved-port saved-port)
  "Restore a Shell file descriptor to its previous state as described
by @var{saved-port}, where @var{saved-port} is a return value of
@code{save-and-set-redirect}."
  (match saved-port
    ((fd . saved-fd)
     (let ((port ((fd->current-port fd))))
       ((fd->current-port fd) saved-fd)
       (unless (any (cut eq? port <>)
                    (map (lambda (fd)
                           ((fd->current-port fd)))
                         (iota *fd-count*)))
         (close port))))))

(define (sh:with-redirects env redirs thunk)
  "Call @var{thunk} with the redirects @var{redirs} in effect."
  (let ((saved-ports #f))
    (dynamic-wind
      (lambda ()
        (set! saved-ports
          (map (cut save-and-set-redirect env <>) redirs)))
      thunk
      (lambda ()
        (for-each restore-saved-port (reverse saved-ports))))))


;;; Subshells and command substitution.

(define* (%subshell thunk)
  "Run @var{thunk} in a new process and return the ID of the new
process."
  (match (primitive-fork)
    (0 (thunk)
       (primitive-exit))
    (pid pid)))

(define (sh:subshell env thunk)
  "Run @var{thunk} in a subshell environment."
  (match-let* ((pid (%subshell thunk))
               ((pid . status) (waitpid pid)))
    (set-var! env "?" (number->string (status:exit-val status)))))

(define (sh:substitute-command env thunk)
  "Run @var{thunk} in a subshell environment and return its output as
a string."
  (match-let* (((sink . source) (pipe))
               (thunk* (lambda ()
                         (close-port sink)
                         (let ((in (open-file "/dev/null" "r"))
                               (err (open-file "/dev/null" "w")))
                           (parameterize ((current-input-port in)
                                          (current-output-port source)
                                          (current-error-port err))
                             (thunk)))))
               (pid (%subshell thunk*)))
    (close-port source)
    (match-let ((result (string-trim-right (get-string-all sink) #\newline))
                ((pid . status) (waitpid pid)))
      (set-var! env "?" (number->string (status:exit-val status)))
      result)))
