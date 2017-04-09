(define-module (gash pipe)

  :use-module (ice-9 popen)
  :use-module (ice-9 rdelim)

  :use-module (srfi srfi-1)
  :use-module (srfi srfi-8)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-26)

  :use-module (gash job)

  :export (pipeline substitute))

(define (pipe*)
  (let ((p (pipe)))
    (values (car p) (cdr p))))

;;              lhs        rhs
;; [source] w -> r [filter] w -> r [sink]

(define (exec* command) ;; list of strings
  (catch #t (lambda () (apply execlp (cons (car command) command)))
    (lambda (key . args) (display (string-append (caaddr args) "\n"))
            (exit #f))))

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
             (if src (move->fdes src 0))
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
  (let ((job (new-job)))
    (if (> (length commands) 1)
        (let loop ((src (spawn-source fg? job (car commands)))
                   (commands (cdr commands)))
          (if (null? (cdr commands))
              (spawn-sink fg? job src (car commands))
              (loop (spawn-filter fg? job src (car commands))
                    (cdr commands))))
        (spawn-sink fg? job #f (car commands)))
    (if fg? (wait job))))

;;(pipeline #f (list "ls" "/"))
;;(pipeline #f (list "ls" "/") (list "grep" "o") (list "tr" "o" "e"))

(define (read-n-format r)
  (string-trim (string-map (lambda (c)
                             (if (eq? #\newline c) #\space c))
                           (read-string r))
               #\space))

(define (substitute . commands)
  (let* ((fg? #f)
         (job (new-job))
         (output (read-n-format
                  (if (> (length commands) 1)
                      (let loop ((src (spawn-source fg? job (car commands)))
                                 (commands (cdr commands)))
                        (if (null? (cdr commands))
                            (spawn-filter fg? job src (car commands))
                            (loop (spawn-filter fg? job src (car commands))
                                  (cdr commands))))
                      (spawn-filter fg? job #f (car commands))))))
    (wait job)
    output))

;;(display (substitute '("ls") '("cat"))) (newline)
