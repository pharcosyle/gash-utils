(define-module (geesh shell)
  #:use-module (geesh built-ins)
  #:use-module (geesh environment)
  #:use-module (geesh pattern)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (sh:and
            sh:case
            sh:cond
            sh:exec-let
            sh:exec
            sh:for
            sh:not
            sh:or
            sh:pipeline
            sh:set-redirects
            sh:subshell
            sh:substitute-command
            sh:while
            sh:with-redirects
            sh:until))

;;; Commentary:
;;;
;;; This module provides functions for executing Shell language
;;; constructs.
;;;
;;; Code:

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

(define (exec-utility bindings path name args)
  "Execute @var{path} as a subprocess with extra environment variables
@var{bindings}.  The first argument given to the new process will be
@var{name}, and the rest of the arguments will be @var{args}."
  (let ((utility-env (get-environ bindings)))
    ;; We need to flush all ports here to ensure the proper sequence
    ;; of output.  Without flushing, output that we have written could
    ;; stay in a buffer while the utility (which does not know about
    ;; the buffer) produces its output.
    (flush-all-ports)
    (match (primitive-fork)
      (0 (install-current-ports!)
         (apply execle path utility-env name args))
      (pid (match-let (((pid . status) (waitpid pid)))
             (set-status! (status:exit-val status)))))))

(define (slashless? s)
  "Test if the string @var{s} does not contain any slashes ('/')."
  (not (string-index s #\/)))

(define (split-search-path s)
  "Split the search path string @var{s}."
  (if (string-null? s) '() (string-split s #\:)))

(define (find-utility name)
  "Search for the path of the utility @var{name} using the current
search path as specified by the environment variable @code{$PATH}.  If
it cannot be found, return @code{#f}."
  (let loop ((prefixes (split-search-path (getvar "PATH" ""))))
    (and (pair? prefixes)
         (let* ((prefix (car prefixes))
                (path (if (string-suffix? "/" prefix)
                          (string-append prefix name)
                          (string-append prefix "/" name))))
           (if (access? path X_OK)
               path
               (loop (cdr prefixes)))))))

(define (sh:exec-let bindings name . args)
  "Find and execute @var{name} with arguments @var{args} and extra
environment variable bindings @var{bindings}."
  (if (slashless? name)
      (or (and=> (search-special-built-ins name)
                 (lambda (proc)
                   (for-each (match-lambda
                               ((name . value)
                                (setvar! name value)))
                             bindings)
                   (let ((exit-val (apply proc args)))
                     (set-status! exit-val))))
          (and=> (getfun name)
                 (lambda (proc)
                   (with-arguments (cons (car (program-arguments)) args)
                     (lambda ()
                       (apply proc args)))))
          (and=> (search-built-ins name)
                 (lambda (proc)
                   ;; TODO: Use 'bindings' here.
                   (let ((exit-val (apply proc args)))
                     (set-status! exit-val))))
          (and=> (find-utility name)
                 (lambda (path)
                   (exec-utility bindings path name args)))
          (begin (format (current-error-port)
                         "~a: ~a: Command not found.~%"
                         (car (program-arguments)) name)
                 (set-status! 127)))
      (exec-utility bindings name name args)))

(define (sh:exec name . args)
  "Find and execute @var{name} with arguments @var{args}."
  (apply sh:exec-let '() name args))


;;; Redirects.

(define (redir->parameter+port redir)
  "Convert @var{redir} into a pair consisting of the current-port
parameter to be updated and the port that should be its new value (or
@code{#f} if it should be considered closed)."

  (define* (make-parameter+port fd target #:optional (open-flags 0))
    (let ((port (match target
                  ((? port?) target)
                  ((? string?) (open target open-flags))
                  ;; TODO: Verify open-flags.
                  ((? integer?) ((fd->current-port target)))
                  (#f #f))))
      `(,(fd->current-port fd) . ,port)))

  (match redir
    (('< (? integer? fd) (? string? filename))
     (make-parameter+port fd filename O_RDONLY))
    (('> (? integer? fd) (? string? filename))
     ;; TODO: Observe noclobber.
     (make-parameter+port fd filename (logior O_WRONLY O_CREAT O_TRUNC)))
    (('>! (? integer? fd) (? string? filename))
     (make-parameter+port fd filename (logior O_WRONLY O_CREAT O_TRUNC)))
    (('>> fd filename)
     (make-parameter+port fd filename (logior O_WRONLY O_CREAT O_APPEND)))
    (('<> fd filename)
     (make-parameter+port fd filename (logior O_RDWR O_CREAT)))
    (('<& (? integer? fd1) (? integer? fd2))
     (make-parameter+port fd1 fd2))
    (('<& (? integer? fd) '-)
     (make-parameter+port fd #f))
    (('>& (? integer? fd1) (? integer? fd2))
     (make-parameter+port fd1 fd2))
    (('>& (? integer? fd) '-)
     (make-parameter+port fd #f))
    (('<< (? integer? fd) text)
     (let ((port (tmpfile)))
       (display text port)
       (seek port 0 SEEK_SET)
       (make-parameter+port fd port)))))

(define (sh:set-redirects redirs)
  "Put the redirects @var{redirs} into effect."
  (let loop ((redirs redirs))
    (match redirs
      (() #t)
      ((redir . rest)
       (match (false-if-exception
               (redir->parameter+port redir))
         (#f (set-status! 1))
         ((parameter . port)
          (parameter port)
          (loop rest)))))))

(define (sh:with-redirects redirs thunk)
  "Call @var{thunk} with the redirects @var{redirs} in effect."
  ;; This may be too clever!  We need to parameterize a variable
  ;; number of things in a particular order, and this seems to be the
  ;; only way.
  ((fold-right (lambda (redir thunk)
                 (lambda ()
                   (match (false-if-exception
                           (redir->parameter+port redir))
                     (#f (set-status! 1))
                     ((parameter . port)
                      (parameterize ((parameter port))
                        (thunk))
                      (when (output-port? port)
                        (force-output port))))))
               thunk
               redirs)))


;;; Subshells and command substitution.

(define* (%subshell thunk)
  "Run @var{thunk} in a new process and return the ID of the new
process."
  ;; We need to flush all ports before forking to avoid copying the
  ;; port buffers into the child process, which could lead to
  ;; duplicate output.
  (flush-all-ports)
  (match (primitive-fork)
    (0 (with-continuation-barrier
        (lambda ()
          (thunk)
          (primitive-exit (get-status))))
       (primitive-exit 1))
    (pid pid)))

(define (sh:subshell thunk)
  "Run @var{thunk} in a subshell environment."
  (match-let* ((pid (%subshell thunk))
               ((pid . status) (waitpid pid)))
    (set-status! (status:exit-val status))))

(define (sh:substitute-command thunk)
  "Run @var{thunk} in a subshell environment and return its output as
a string."
  (match-let* (((sink . source) (pipe))
               (thunk* (lambda ()
                         (close-port sink)
                         (with-output-to-port source thunk)))
               (pid (%subshell thunk*)))
    (close-port source)
    (match-let ((result (string-trim-right (get-string-all sink) #\newline))
                ((pid . status) (waitpid pid)))
      (set-status! (status:exit-val status))
      result)))


;;; Pipelines.

(define (swap-and-shift-pairs pairs)
  "Swap and shift @var{pairs} over by one.  For example, the list
@code{((a . b) (c . d))} becomes @code{((#f . b) (a . d) (c . #f))}"
  (let ((kons (lambda (pair acc)
                (match-let (((a . b) pair))
                  (match acc
                    ((head . rest) `(,b (,a . ,head) ,@rest))
                    (() `(,b (,a . #f))))))))
    (match (fold-right kons '() pairs)
      ((head . rest) `((#f . ,head) ,@rest))
      (() '()))))

(define (make-pipes xs)
  "Cons each element of @var{xs} to a pair of ports such that the first
port is an input port connected to the second port of the previous
element's pair, and the second port is an output port connected to the
first port of next element's pair.  The first pair will have @code{#f}
for an input port and the last will have @code{#f} as an output port."
  (match xs
    (() '())
    ((x) `((,x . (#f . #f))))
    (_ (let ((pipes (map (lambda (x) (pipe)) (cdr xs))))
         (map cons xs (swap-and-shift-pairs pipes))))))

(define (plumb in out thunk)
  "Run @var{thunk} in a new process with @code{current-input-port} set
to @var{in} and @code{current-output-port} set to @var{out}.  If
@var{in} or @var{out} is @code{#f}, the corresponding ``current'' port
is left unchanged."
  (let* ((thunk* (lambda ()
                   (let ((in (or in (current-input-port)))
                         (out (or out (current-output-port))))
                     (parameterize ((current-input-port in)
                                    (current-output-port out))
                       (thunk)))))
         (pid (%subshell thunk*)))
    (when in (close-port in))
    (when out (close-port out))
    pid))

(define (sh:pipeline . thunks)
  "Run each thunk in @var{thunks} in its own process with the output
of each thunk sent to the input of the next thunk."
  (let ((pids (map (match-lambda
                     ((thunk . (source . sink))
                      (plumb source sink thunk)))
                   (make-pipes thunks))))
    (unless (null? pids)
      (match-let* ((pid (last pids))
                   ((pid . status) (waitpid pid)))
        (set-status! (status:exit-val status))))))


;;; Boolean expressions.

(define (sh:and thunk1 thunk2)
  "Run @var{thunk1} and if it exits with status zero, run
@var{thunk2}."
  (thunk1)
  (when (= (get-status) 0)
    (thunk2)))

(define (sh:or thunk1 thunk2)
  "Run @var{thunk1} and if it exits with a nonzero status, run
@var{thunk2}."
  (thunk1)
  (unless (= (get-status) 0)
    (thunk2)))

(define (sh:not thunk)
  "Run @var{thunk}, inverting its exit status."
  (thunk)
  (let ((inverted-status (if (= (get-status) 0) 1 0)))
    (set-status! inverted-status)))


;;; Loops.

(define (sh:for bindings thunk)
  "Run @var{thunk} for each binding in @var{bindings}.  The value of
@var{bindings} have the form @code{(@var{name} (@var{value} ...))}."
  (set-status! 0)
  (match-let (((name (values ...)) bindings))
    (call-with-break
      (lambda ()
        (for-each (lambda (value)
                    (setvar! name value)
                    (call-with-continue thunk))
                  values)))))

(define (sh:while test-thunk thunk)
  (call-with-break
    (lambda ()
      (let loop ((last-status 0))
        (test-thunk)
        (cond
         ((= (get-status) 0)
          (thunk)
          (loop (get-status)))
         (else
          (set-status! last-status)))))))

(define (sh:until test-thunk thunk)
  (sh:while (lambda () (sh:not test-thunk)) thunk))


;;; Conditionals.

(define (sh:case value . cases)
  (set-status! 0)
  (let loop ((cases cases))
    (match cases
      (() #t)
      (((patterns thunk) . tail)
       (if (any (cut pattern-match? <> value) patterns)
           (thunk)
           (loop tail))))))

(define (sh:cond . cases)
  (set-status! 0)
  (let loop ((cases cases))
    (match cases
      (() (set-status! 0))
      (((#t thunk))
       (thunk))
      (((test-thunk thunk) . tail)
       (test-thunk)
       (if (= (get-status) 0)
           (thunk)
           (loop tail))))))
