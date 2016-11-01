(define-module (sh pipe)

  :use-module (ice-9 popen)
  :use-module (ice-9 pretty-print)

  :use-module (srfi srfi-1)
  :use-module (srfi srfi-8)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-26)

  :export (pipeline job-control-init jobs report-jobs fg bg))

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
  (stdout "[" (job-id job) "] " (status->state (job-status job)) "\t\t"
          (job-command job)))

(define (jobs)
  (map (lambda (job)
         (display-job job))
       (reverse job-table)))

(define (job-status job)
  (process-status (last (job-processes job))))

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
  (let* ((pgid (job-pgid job))
         (pgid (or pgid pid)))
    (setpgid pid pgid)
    pgid))

(define (job-add-process fg? job pid command)
  (let ((pgid (add-to-process-group job pid)))
    (set-job-pgid! job pgid)
    (if fg? (tcsetpgrp (current-error-port) pgid))
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

(define (setup-process fg? job)
  (when (isatty? (current-error-port))
    (when fg? (tcsetpgrp (current-error-port) (add-to-process-group job (getpid))))
    (map (cut sigaction <> SIG_DFL)
         (list SIGINT SIGQUIT SIGTSTP SIGTTIN SIGTTOU SIGCHLD)))
  (fdes->inport 0) (map fdes->outport '(1 2))) ;; reset stdin/stdout/stderr

(define (spawn-source fg? job command)
  (receive (r w) (pipe*)
    (let ((pid (primitive-fork)))
      (cond ((= 0 pid) (close r)
             (setup-process fg? job)
             (move->fdes w 1)
             (exec* command))
            (#t
             (job-add-process fg? job pid command)
             (close w)
             r)))))

(define (spawn-filter fg? job src command)
  (receive (r w) (pipe*)
    (let ((pid (primitive-fork)))
      (cond ((= 0 pid)
             (setup-process fg? job)
             (move->fdes src 0)
             (close r)
             (move->fdes w 1)
             (exec* command))
            (#t
             (job-add-process fg? job pid command)
             (close w)
             r)))))

(define (spawn-sink fg? job src command)
  (let ((pid (primitive-fork)))
    (cond ((= 0 pid)
           (setup-process fg? job)
           (if src (move->fdes src 0))
           (exec* command))
          (#t
           (job-add-process fg? job pid command)
           (and src (close src))))))


(define (pipeline fg? . commands)
  (let* ((index (+ 1 (length job-table)))
         (job (make-job index #f '())))
    (if (> (length commands) 1)
        (let loop ((src (spawn-source fg? job (car commands)))
                   (commands (cdr commands)))
          (if (null? (cdr commands))
              (spawn-sink fg? job src (car commands))
              (loop (spawn-filter fg? job src (car commands))
                    (cdr commands))))
        (spawn-sink fg? job #f (car commands)))
    (set! job-table (cons job job-table))
    (if fg? (wait job))))

(define (disjoin . predicates)
  (lambda (. arguments)
    (any (cut apply <> arguments) predicates)))

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
  (let loop ()
    (let* ((pid-status (waitpid (- (job-pgid job)) WUNTRACED))
           (pid (car pid-status))
           (status (cdr pid-status)))
      (job-update job pid status)
      (if (job-running? job) (loop))))
  (tcsetpgrp (current-error-port) (getpid))
  (unless (job-completed? job)
    (newline) (display-job job))
  (reap-jobs)
  (job-status job))

(define (fg index)
  (let ((job (job-index index)))
    (cond (job
           (tcsetpgrp (current-error-port) (job-pgid job))
           (kill (- (job-pgid job)) SIGCONT)
           (stdout (job-command job))
           (wait job))
          (#t
           (stderr "fg: no such job " index)))))

(define (bg index)
  (let ((job (job-index index)))
    (map (cut set-process-status! <>  #f) (job-processes job))
    (kill (- (job-pgid job)) SIGCONT)))

;;(pipeline (list "ls" "/")
;;(pipeline (list "ls" "/") (list "grep" "o") (list "tr" "o" "e"))
