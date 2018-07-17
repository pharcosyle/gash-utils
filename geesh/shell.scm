(define-module (geesh shell)
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

(define (sh:exec-let env bindings name . args)
  "Execute @var{name} with arguments @var{args}, environment
@var{env}, and extra environment variable bindings @var{bindings}."
  (exec-utility env bindings name name args))

(define (sh:exec env name . args)
  "Execute @var{name} with arguments @var{args} and environment
@var{env}."
  (apply sh:exec-let env '() name args))
