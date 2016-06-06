(define-module (sh pipe)
  :use-module (ice-9 popen)
  :use-module (srfi srfi-8)
  :export (pipeline))

(define (pipe*)
  (let ((p (pipe)))
    (values (car p) (cdr p))))

;;              lhs         rhs
;; [source] w -> r [filter] w -> r [sink]

(define (exec* command)
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

(define (pipeline commands)
  (if (< 1 (length commands))
      (let loop ((src (spawn-source (car commands)))
                 (commands (cdr commands)))
        (if (null? (cdr commands)) (spawn-sink src (car commands))
            (loop (spawn-filter src (car commands))
                  (cdr commands))))))

;;(pipeline (list (list "ls" "/") (list "grep" "o") (list "tr" "o" "e")))
