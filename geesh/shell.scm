(define-module (geesh shell)
  #:use-module (geesh built-ins)
  #:use-module (geesh environment)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (sh:exec-let
            sh:exec
            sh:subshell
            sh:with-redirects))

;;; Commentary:
;;;
;;; This module provides functions for executing Shell language
;;; constructs.
;;;
;;; Code:

(define (exec-utility env bindings path name args)
  "Execute @var{path} as a subprocess with environment @var{env} and
extra environment variables @var{bindings}.  The first argument given
to the new process will be @var{name}, and the rest of the arguments
will be @var{args}."
  (let ((utility-env (environment->environ env bindings)))
    (match (primitive-fork)
      (0 (apply execle path utility-env name args))
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

(define (save-and-install-redirect! env redir)
  "Install the redirect @var{redir} into the current process and
return a pair consisting of the file descriptor that has been changed
and a dup'ed copy of its old value.  If @var{redir} is a here-document
redirect, the return value is a pair where the first element is the
pair previously described and the second element is the temporary
filename used for the here-document contents."

  (define* (save-and-dup2! fd target #:optional (open-flags 0))
    (let ((saved-fd (catch 'system-error
                      (lambda () (dup fd))
                      (lambda data
                        (unless (= EBADF (system-error-errno data))
                          (apply throw data))
                        #f))))
      (match target
        ((? string?) (dup2 (open-fdes target open-flags) fd))
        ;; TODO: Verify open-flags.
        ((? integer?) (dup2 target fd))
        (#f (close-fdes fd)))
      `(,fd . ,saved-fd)))

  (match redir
    (('< (? integer? fd) (? string? filename))
     (save-and-dup2! fd filename O_RDONLY))
    (('> (? integer? fd) (? string? filename))
     ;; TODO: Observe noclobber.
     (save-and-dup2! fd filename (logior O_WRONLY O_CREAT O_TRUNC)))
    (('>! (? integer? fd) (? string? filename))
     (save-and-dup2! fd filename (logior O_WRONLY O_CREAT O_TRUNC)))
    (('>> fd filename)
     (save-and-dup2! fd filename (logior O_WRONLY O_CREAT O_APPEND)))
    (('<> fd filename)
     (save-and-dup2! fd filename (logior O_RDWR O_CREAT)))
    (('<& (? integer? fd1) (? integer? fd2))
     (save-and-dup2! fd1 fd2))
    (('<& (? integer? fd) '-)
     (save-and-dup2! fd #f))
    (('>& (? integer? fd1) (? integer? fd2))
     (save-and-dup2! fd1 fd2))
    (('>& (? integer? fd) '-)
     (save-and-dup2! fd #f))
    (('<< (? integer? fd) text)
     (let ((port (mkstemp! (string-copy "/tmp/geesh-here-doc-XXXXXX"))))
       (display text port)
       (seek port 0 SEEK_SET)
       `(,(save-and-dup2! fd (port->fdes port)) . ,(port-filename port))))))

(define (restore-saved-fdes! fd-pair)
  "Restore a file-descriptor to its previous state as described by
@var{fd-pair}, where @var{fd-pair} is a return value of
@code{save-and-install-redirect!}."
  (match fd-pair
    (((fd . saved-fd) . filename)
     (restore-saved-fdes! `(,fd . ,saved-fd))
     (delete-file filename))
    ((fd . #f)
     (close-fdes fd))
    ((fd . saved-fd)
     (dup2 saved-fd fd))))

(define (sh:with-redirects env redirs thunk)
  "Call @var{thunk} with the redirects @var{redirs} in effect."
  (let ((saved-fds #f))
    (dynamic-wind
      (lambda ()
        (flush-all-ports)
        (set! saved-fds
          (map (cut save-and-install-redirect! env <>) redirs)))
      thunk
      (lambda ()
        (flush-all-ports)
        (for-each restore-saved-fdes! (reverse saved-fds))))))


;;; Subshells.

(define (sh:subshell env thunk)
  "Run @var{thunk} in a subshell environment."
  (match (primitive-fork)
    (0 (thunk)
       (primitive-exit))
    (pid (match-let (((pid . status) (waitpid pid)))
           (set-var! env "?" (number->string (status:exit-val status)))))))
