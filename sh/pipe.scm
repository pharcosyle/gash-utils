(define-module (sh pipe)

  :use-module (ice-9 popen)
  :use-module (ice-9 pretty-print)

  :use-module (srfi srfi-1)
  :use-module (srfi srfi-8)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-26)

  :export (pipeline job-control-init jobs))

(define (stdout . o)
  (map display o)
  (newline))

(define (stderr . o)
  (map (cut display <> (current-error-port)) o)
  (newline))

(define-record-type <process>
  (make-process pid command status)
  process?
  (pid process-pid)
  (command process-command)
  (status process-status set-process-status!))

(define-record-type <job>
  (make-job id pgid processes)
  job?
  (id job-id)
  (pgid job-pgid set-job-pgid!)
  (processes job-processes set-job-processes!))

(define job-table '()) ;; list of <job>

(define (status->state status)
  (cond ((status:exit-val status) 'Completed)
        ((status:term-sig status) 'Terminated)
        ((status:stop-sig status) 'Stopped)
        (#t 'Running)))

(define (jobs)
  (map (lambda (job number)
         (stdout "[" number "]?  " (status->state (job-status job)) "\t\t"
                 (process-command (car (job-processes job)))))
       (reverse job-table)
       (iota (length job-table) 1 1)))

(define (job-status job)
  (process-status (car (job-processes job))))

(define (job-update job pid status)
  (unless (= 0 pid)
    (let ((proc (find (compose (cute eqv? pid <>) process-pid) (job-processes job))))
      (set-process-status! proc status))))

(define (job-running? job)
  (find (compose not process-status) (job-processes job)))

(define (job-stopped? job)
  (find (compose status:stop-sig process-status) (job-processes job)))

(define (add-to-process-group job pid)
  (let* ((pgid (job-pgid job))
         (pgid (or pgid pid)))
    (setpgid pid pgid)
    pgid))

(define (job-add-process job pid command)
  (let ((pgid (add-to-process-group job pid)))
    (set-job-pgid! job pgid)
    (tcsetpgrp (current-error-port) pgid)
    (set-job-processes! job (cons (make-process pid command #f) (job-processes job)))))

(define (job-control-init)
  (let* ((interactive? (isatty? (current-error-port)))
         (pgid (getpgrp))
         (pid (getpid)))
    (when interactive?
      (while (not (eqv? (tcgetpgrp (current-error-port)) pgid))
        (kill (- pgid) SIGTTIN)) ;; oops we are not in the foreground
      (map (cut sigaction <> SIG_IGN)
           (list SIGINT SIGQUIT SIGTSTP SIGTTIN SIGTTOU))
      (sigaction SIGCHLD SIG_DFL)
      (setpgid pid pid) ;; create new process group for ourself
      (tcsetpgrp (current-error-port) pid))))

(define (pipe*)
  (let ((p (pipe)))
    (values (car p) (cdr p))))

;;              lhs        rhs
;; [source] w -> r [filter] w -> r [sink]

(define (exec* command) ;; list of strings
  (apply execlp (cons (car command) command)))

(define (setup-process job)
  (tcsetpgrp (current-error-port) (add-to-process-group job (getpid)))
  (map (cut sigaction <> SIG_DFL)
       (list SIGINT SIGQUIT SIGTSTP SIGTTIN SIGTTOU SIGCHLD))
  (fdes->inport 0) (map fdes->outport '(1 2))) ;; reset stdin/stdout/stderr

(define (spawn-source job interactive? command)
  (receive (r w) (pipe*)
    (let ((pid (primitive-fork)))
      (cond ((= 0 pid) (close r)
             (setup-process job)
             (move->fdes w 1)
             (exec* command))
            (#t
             (job-add-process job pid command)
             (close w)
             r)))))

(define (spawn-filter job interactive? src command)
  (receive (r w) (pipe*)
    (let ((pid (primitive-fork)))
      (cond ((= 0 pid)
             (setup-process job)
             (move->fdes src 0)
             (close r)
             (move->fdes w 1)
             (exec* command))
            (#t
             (job-add-process job pid command)
             (close w)
             r)))))

(define (spawn-sink job interactive? src command)
  (let ((pid (primitive-fork)))
    (cond ((= 0 pid)
           (setup-process job)
           (and src (move->fdes src 0))
           (exec* command))
          (#t
           (job-add-process job pid command)
           (and src (close src))))))

;; TODO:
;;   report job status: before prompt or by calling jobs
;;   remove reported terminated or completed jobs

(define (pipeline . commands)
  (let ((interactive? (isatty? (current-error-port)))
        (job (make-job (length job-table) #f '())))
    (set! job-table (cons job job-table))
    (if (> (length commands) 1)
        (let loop ((src (spawn-source job interactive? (car commands)))
                   (commands (cdr commands)))
          (if (null? (cdr commands))
              (spawn-sink job interactive? src (car commands))
              (loop (spawn-filter job interactive? src (car commands))
                    (cdr commands))))
        (spawn-sink job interactive? #f (car commands)))
    (let loop ()
      (let* ((pid-status (waitpid WAIT_ANY WUNTRACED))
             (pid (car pid-status))
             (status (cdr pid-status)))
        (job-update job pid status)
        (if (job-running? job) (loop))))
    (tcsetpgrp (current-error-port) (getpid))
    ;;(pretty-print job-table)
    (job-status job)
    (reap-jobs)))

(define (disjoin . predicates)
  (lambda (. arguments)
   (any (cut apply <> arguments) predicates)))

(define (reap-jobs)
  (set! job-table (filter (disjoin job-running? job-stopped?) job-table)))

;;(pipeline (list "ls" "/")
;;(pipeline (list "ls" "/") (list "grep" "o") (list "tr" "o" "e"))
