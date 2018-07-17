(define-module (geesh shell)
  #:use-module (geesh built-ins)
  #:use-module (geesh environment)
  #:use-module (ice-9 match)
  #:export (sh:exec-let
            sh:exec))

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
