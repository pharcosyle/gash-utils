(define-module (sh pipe)
  :use-module (ice-9 popen)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-8)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-26)
  :export (pipeline))

(define (pipe*)
  (let ((p (pipe)))
    (values (car p) (cdr p))))

;;              lhs        rhs
;; [source] w -> r [filter] w -> r [sink]

(define (exec* command) ;; list of strings
  (apply execlp (cons (car command) command)))

(define (spawn-source command)
  (receive (r w) (pipe*)
    (let ((pid (primitive-fork)))
      (cond ((= 0 pid) (close r)
             (move->fdes w 1)
             (exec* command))
            (#t
             (close w)
             r)))))

(define (spawn-filter src command)
  (receive (r w) (pipe*)
    (let ((pid (primitive-fork)))
      (cond ((= 0 pid)
             (move->fdes src 0)
             (close r)
             (move->fdes w 1)
             (exec* command))
            (#t
             (close w)
             r)))))

(define (spawn-sink src command)
  (let ((pid (primitive-fork)))
    (cond ((= 0 pid)
           (move->fdes src 0)
           (exec* command))
          (#t
           (close src)
           (waitpid pid)))))

(define (pipeline . commands)
  (if (< 1 (length commands))
      (let loop ((src (spawn-source (car commands)))
                 (commands (cdr commands)))
        (if (null? (cdr commands)) (spawn-sink src (car commands))
            (loop (spawn-filter src (car commands))
                  (cdr commands))))
      (apply system* (car commands))))

;;(pipeline (list "ls" "/")
;;(pipeline (list "ls" "/") (list "grep" "o") (list "tr" "o" "e"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stdout . o)
  (map display o)
  (newline))

(define (stderr . o)
  (map (cut display <> (current-error-port)) o)
  (newline))

(define-record-type <job>
  (make-job id pid pgid command status )
  job?
  (id job-id)
  (pid job-pid)
  (pgid job-pgid)
  (command job-command)
  (status job-status set-job-status!)) ;; '(running stopped completed terminated)

(define job-table '())

(define (job-control-init)
  (let* ((interactive? (isatty? (current-input-port)))
         (pid (getpid))
         (pgid pid))
    (when interactive?
        (map (cut sigaction <> SIG_IGN)
             (list SIGINT SIGQUIT SIGTSTP SIGTTIN SIGTTOU SIGCHLD))
        (setpgid pid pgid)
        (tcsetpgrp (current-input-port) pid))))

(define (job-launch command fg?)
  (let* ((interactive? (isatty? (current-input-port)))
         (pgid (getpid))
         (pid (primitive-fork)))
    (if (= 0 pid)
        (when interactive?
          (setpgid pid pgid)
          (if fg? (tcsetpgrp (current-input-port) pgid))
          (map (cut sigaction <> SIG_DFL)
               (list SIGINT SIGQUIT SIGTSTP SIGTTIN SIGTTOU SIGCHLD))
          (map move->fdes
               (list (current-input-port) (current-output-port) (current-error-port))
               (iota 4))
          (exec* command)
          (exit 1))
        (when interactive?
          (setpgid pid pgid)
          (set! job-table
            (acons pid
                   (make-job (length job-table) pid pgid command 'running)
                   job-table))
          (when fg?
            (waitpid pid WUNTRACED)
            (tcsetpgrp (current-input-port) (getpid)))))))

(define (set-job-stopped! job-table pid)
  (set-job-status! (assoc-ref job-table pid) 'stopped))

(define (update-job-status job-table)
  (let* ((pid-status (waitpid WAIT_ANY (logior WUNTRACED WNOHANG)))
         (pid (car pid-status))
         (status (cdr pid-status)))
    (if (and (not (= 0 pid))
             (status:stop-sig status))
        (set-job-stopped! pid job-table))))

;(define (handle-children sig))

(define (background job-id)
  (let ((job (if (< job-id (length job-table)) (list-ref job-table job-id) #f)))
    (if (and job (eq? 'stopped job-status))
        (kill (- (job-pgid job)) SIGCONT))))

;; (define (foreground job)
;;   ())

;; (init)

;; (launch (list "sleep" "10") #t)
