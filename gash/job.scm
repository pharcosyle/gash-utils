(define-module (gash job)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)

  #:use-module (gash io)
  #:use-module (gash util)

  #:export (bg
            fg
            job?
            job-add-process
            job-control-init
            job-debug-id
            job-setup-process
            job-status
            jobs-command
            new-job
            report-jobs
            wait))

(define-record-type <process>
  (make-process pid command status)
  process?
  (pid process-pid)
  (command process-command)
  (status process-status set-process-status!))

(define-record-type <job>
  (make-job id pgid processes debug-id)
  job?
  (id job-id)
  (pgid job-pgid set-job-pgid!)
  (processes job-processes set-job-processes!)
  (debug-id job-debug-id))

(define debug-id
  (let ((id -1))
    (lambda ()
      (set! id (1+ id))
      (number->string id))))

(define (new-job)
  (let ((job (make-job (+ 1 (length job-table)) #f '() (debug-id))))
    (set! job-table (cons job job-table))
    job))

(define job-table '()) ;; list of <job>

(define (job-index index)
  (let ((index (- (length job-table) index)))
    (if (<= 0 index)
        (list-ref job-table index)
        #f)))

(define (status->state status)
  (cond ((not status) 'Running)
        ((status:exit-val status) 'Done)
        ((status:term-sig status) 'Terminated)
        ((status:stop-sig status) 'Stopped)))

(define (job-command job)
  (string-join (map (compose string-join process-command) (reverse (job-processes job))) " | "))

(define (display-job job)
  (stdout "[" (job-id job) "] " (map status->state (job-status job)) "\t\t"
          (job-command job)))

(define (jobs-command)
  (for-each (lambda (job) (display-job job)) (reverse job-table)))

(define (job-status job)
  (map process-status (job-processes job)))

(define (job-update job pid status)
  (unless (= 0 pid)
    (let ((proc (find (compose (cut eqv? pid <>) process-pid) (job-processes job))))
      (when proc
        (set-process-status! proc status)))))

(define (job-running? job)
  (find (compose not process-status) (job-processes job)))

(define (job-stopped? job)
  (find status:stop-sig (filter-map process-status (job-processes job))))

(define (job-completed? job)
  (let ((state (map (compose status->state process-status) (job-processes job))))
    (every (cut member <> '(Done Terminated)) state)))

(define (add-to-process-group job pid)
  (let* ((interactive? (isatty? (current-error-port)))
         (pgid (if interactive?
                   (or (job-pgid job) pid)
                   (getpgrp))))
    (set-job-pgid! job pgid)
    (when interactive? (setpgid pid pgid))
    pgid))

(define (job-add-process fg? job pid command)
  (let ((pgid (add-to-process-group job pid)))
    (set-job-pgid! job pgid)
    (stderr "job-add-process fg?=~a\n" fg?)
    (when (and (isatty? (current-error-port))
               fg?)
      (tcsetpgrp (current-error-port) pgid))
    (set-job-processes! job (cons (make-process pid command #f) (job-processes job)))))

(define (job-setup-process fg? job)
  (when (isatty? (current-error-port))
    (when (and (isatty? (current-error-port))
               fg?)
      (tcsetpgrp (current-error-port) (add-to-process-group job (getpid))))
    (map (cut sigaction <> SIG_DFL)
         (list SIGINT SIGQUIT SIGTSTP SIGTTIN SIGTTOU SIGCHLD))))

(define (job-control-init)
  (when (isatty? (current-error-port))
    (let ((pgid (getpgrp)))
      (while (and #f  ;; FIXME: make check backgrouds our tests
                  (isatty? (current-error-port))
                  (not (eqv? (tcgetpgrp (current-error-port)) pgid)))
        (kill (- pgid) SIGTTIN))) ;; oops we are not in the foreground
    (map (cut sigaction <> SIG_IGN) (list SIGINT SIGQUIT SIGTSTP SIGTTIN SIGTTOU))
    (sigaction SIGCHLD SIG_DFL)
    (let ((pid (getpid)))
      (setpgid pid pid) ;; create new process group for ourself
      (tcsetpgrp (current-error-port) pid))))

(define (reap-jobs)
  (set! job-table (filter (disjoin job-running? job-stopped?) job-table)))

(define (report-jobs)
  (when (not (null? job-table))
    (let* ((pid-status (waitpid WAIT_ANY (logior WUNTRACED WNOHANG)))
           (pid (car pid-status))
           (status (cdr pid-status)))
      (unless (= 0 pid)
        (map (cut job-update <> pid status) job-table)
        (map display-job (filter job-completed? job-table))
        (reap-jobs)))))

(define (wait job)
  (when (job-running? job)
    (let loop ()
      (let* ((pid-status (waitpid (- (job-pgid job)) WUNTRACED))
             (pid (car pid-status))
             (status (cdr pid-status)))
        (job-update job pid status)
        (if (job-running? job) (loop)))))
  (unless (job-completed? job)
    (newline) (display-job job))
  (reap-jobs)
  (or (find (negate zero?) (job-status job))
      0))

(define (fg index)
  (let ((job (job-index index)))
    (cond (job
           (let ((pgid (job-pgid job)))
             (tcsetpgrp (current-error-port) pgid)
             (kill (- (job-pgid job)) SIGCONT))
           (stdout (job-command job))
           (wait job))
          (#t
           (stderr "fg: no such job " index)))))

(define (bg index)
  (let ((job (job-index index)))
    (cond (job
           (map (cut set-process-status! <>  #f) (job-processes job))
           (kill (- (job-pgid job)) SIGCONT))
          (#t
           (stderr "fg: no such job " index)))))
