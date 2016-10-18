(define-module (sh pipe)
  :use-module (ice-9 popen)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-8)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-26)
  :export (pipeline job-control-init))

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
  (status process-status set-process-status!)) ;; '(running stopped completed terminated)

(define-record-type <job>
  (make-job id pgid processes)
  job?
  (id job-id)
  (pgid job-pgid set-job-pgid!)
  (processes job-processes set-job-processes!))

(define job-table '()) ;; list of <job>

;; (define (job-at index)
;;   (let ((len (length job-table)))
;;     (if (or (> index len) (< index 0)) #f
;;         (list-ref job-table (- len index)))))

(define (add-to-process-group job pid)
  (let* ((pgid (job-pgid job))
         (pgid (or pgid pid)))
    (setpgid pid pgid)
    pgid))

(define (job-add-process job pid command)
  (let ((pgid (add-to-process-group job pid)))
    (set-job-pgid! job pgid)
    (tcsetpgrp (current-error-port) pgid)
    (set-job-processes! job (cons (make-process pid command 'running) (job-processes job)))))

(define (job-control-init)
  (let* ((interactive? (isatty? (current-error-port)))
         (pgid (getpgrp))
         (pid (getpid)))
    (when interactive?
      (while (not (eqv? (tcgetpgrp (current-error-port)) pgid))
        (kill (- pgid) SIGTTIN)) ;; we are not in the foreground
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

(define (spawn-source job interactive? command)
  (receive (r w) (pipe*)
    (let ((pid (primitive-fork)))
      (cond ((= 0 pid) (close r)
             (tcsetpgrp (current-error-port) (add-to-process-group job (getpid)))
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
             (tcsetpgrp (current-error-port) (add-to-process-group job (getpid)))
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
           (tcsetpgrp (current-error-port) (add-to-process-group job (getpid)))

           (map (cut sigaction <> SIG_DFL)
               (list SIGINT SIGQUIT SIGTSTP SIGTTIN SIGTTOU SIGCHLD))

           (and src (move->fdes src 0))
           (exec* command))
          (#t
           (job-add-process job pid command)
           (and src (close src))))))

(define (job-launch command fg?) ;; todo: integrate into pipeline
  (let* ((interactive? (isatty? (current-error-port)))
         (foo (stdout "ISATTY: " interactive?))
         (pgid (getpid))
         (pid (primitive-fork)))
    (if (= 0 pid)
        (when interactive? ;; the child i.e. command
          (setpgid (getpid) (getpid)) ;; put job in own process group
          (if fg? (tcsetpgrp (current-error-port) (getpid))) ;; put job in foreground

          (map (cut sigaction <> SIG_DFL)
               (list SIGINT SIGQUIT SIGTSTP SIGTTIN SIGTTOU SIGCHLD))
          (fdes->inport 0)
          (map fdes->outport '(1 2))

          (exec* command)
          (exit 1))
        (when interactive? ;; the parent i.e. shell
          (setpgid pid pid) ;; put job in own process group
          (tcsetpgrp (current-error-port) pid) ;; put job in foreground
          (when fg?
            (waitpid pid WUNTRACED)
            (tcsetpgrp (current-error-port) (getpid))))))) ;; put shell in foreground

(define (pipelinex . commands)
  (stdout "pipeline: " commands)
  (job-launch (car commands) #t))

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
        (spawn-sink job interactive? #f (car commands))))
  (waitpid WAIT_ANY WUNTRACED)
  (tcsetpgrp (current-error-port) (getpgrp))
  (stdout "job-table: " job-table))

;;(pipeline (list "ls" "/")
;;(pipeline (list "ls" "/") (list "grep" "o") (list "tr" "o" "e"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define (mark-job-status pid status)
;;   (if (not (= 0 pid))
;;       (cond ((status:stop-sig status)
;;              (set-job-stopped! pid job-table))
;;             ((status:term-sig status)
;;              (set-job-terminated! pid job-table))
;;             ((status:exit-val status)
;;              (set-job-completed! pid job-table)))))

;; (define (job-wait job)
;;   (let loop ()
;;     (let* ((status (job-status job))
;;            (pid-status (waitpid WAIT_ANY WUNTRACED))
;;            (pid (car pid-status))
;;            (status (cdr pid-status)))
;;       (mark-job-status pid status)
;;       (if (eq? status (job-status job)) (loop)))))

;; (define (set-job-stopped! job-table pid)
;;   (set-job-status! (assoc-ref job-table pid) 'stopped))

;; (define (set-job-terminated! job-table pid) ;; signal
;;   (set-job-status! (assoc-ref job-table pid) 'terminated))

;; (define (set-job-completed! job-table pid) ;; exit value
;;   (set-job-status! (assoc-ref job-table pid) 'completed))

;; (define (notify-job-status job-table) ;; call when prompting, from SIGCHLD handler or
;;   (let* ((pid-status (waitpid WAIT_ANY (logior WUNTRACED WNOHANG)))
;;          (pid (car pid-status))
;;          (status (cdr pid-status)))
;;     (mark-job-status pid status)))

;; (define (job-by-id job-table id)
;;   (if (< job-id (length job-table))
;;       (list-ref job-table job-id)
;;       #f))

;; (define (background job-id)
;;   (let ((job (job-by-id job-table job-id)))
;;     (if (and job (eq? 'stopped (job-status job)))
;;         (kill (- (job-pgid job)) SIGCONT))))

;; (define (foreground job-id)
;;   (let ((job (job-by-id job-table job-id)))
;;     (tcsetpgrp (current-error-port) (job-pgid job))
;;     (if (and job (eq? 'stopped (job-status job)))
;;         (kill (- (job-pgid job)) SIGCONT))
;;     (job-wait job)
;;     (tcsetpgrp (current-error-port) (getpid))))
