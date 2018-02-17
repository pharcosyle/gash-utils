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
    (lambda (key . args) (format (current-error-port) "~a\n" (caaddr args))
            (exit #f))))

(define (spawn-source fg? job command)
  (receive (r w) (pipe*)
    (let ((pid (primitive-fork)))
      (cond ((= 0 pid)
	     (close r)
	     (setup-process fg? job)
	     (move->fdes w 1)
             (if (procedure? command)
		 (begin
		   (close-port (current-output-port))
		   (set-current-output-port w)
		   (command)
		   (exit 0))
		 (exec* command)))
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
	     (if (procedure? command)
		 (begin
		   (close-port (current-input-port))
		   (close-port (current-output-port))
		   (set-current-input-port src)
		   (set-current-output-port w)
		   (command)
		   (exit 0))
		 (exec* command)))
            (#t
             (job-add-process fg? job pid command)
             (close w)
             r)))))

(define (spawn-sink fg? job src command)
  (let ((pid (primitive-fork)))
    (cond ((= 0 pid)
           (setup-process fg? job)
           (if src (move->fdes src 0))
	   (if (procedure? command)
	       (begin
		   (close-port (current-input-port))
		   (set-current-input-port src)
		   (command)
		   (exit 0))
	       (exec* command)))
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

;;(pipeline #t (list "sleep" "10"))
;;(pipeline #f (lambda () (display 'foo)) '("grep" "o") '("tr" "o" "e"))
(pipeline #f
	  (lambda () (display "bin\nboot\nroot\ntoot\nusr\nvar"))
	  ;;'("tr" "o" "e")
	  (lambda () (display (string-map (lambda (c) (if (eq? c #\o) #\e c)) (read-string))))
	  (lambda () (display (read-string)) (newline)))

(define (pipeline->string . commands)
  (let* ((fg? #f)
         (job (new-job))
         (output (read-string
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

(define (substitute . commands)
  (string-trim-right
   (string-map (lambda (c)
                 (if (eq? #\newline c) #\space c))
               (apply pipeline->string commands))
   #\space))

;; (display (pipeline->string '("ls") '("cat"))) (newline)
;; (display (substitute '("ls") '("cat"))) (newline)
