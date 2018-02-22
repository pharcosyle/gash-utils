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


(define* (spawn fg? job command #:optional (input '()) (output 0))
  ;;(format #t "spawn: ~a ~a\n" (length input) output)
  (let* ((ofd (iota output 1)) ;; output file descriptors 1, ...
	 (count (length input))
	 (start (1+ output))
	 (ifd (cond
	       ((= count 0) '())
	       ((= count 1) '(0))
	       ((#t (cons 0 (iota (1- count) start))))))
	 (ifd (if (pair? input) (cons 0 ifd) ifd))
	 ;;(foo (format #t "ifd: ~a\n" ifd))
	 ;;(foo (format #t "ofd: ~a\n" ofd))
	 (pipes (map (lambda (. _) (pipe)) ofd))
	 (r (map car pipes))
	 (w (map cdr pipes))
	 (pid (primitive-fork)))
      (cond ((= 0 pid)
	     (setup-process fg? job)
	     (map close r)
	     (map move->fdes w ofd)
	     (map move->fdes input ifd)
             (if (procedure? command)
		 (begin
		   (when (pair? input)
		     (close-port (current-input-port))
		     (set-current-input-port (car input)))
		   (when (pair? w)
		     (close-port (current-output-port))
		     (set-current-output-port (car w)))
		   (command)
		   (exit 0))
		 (exec* command)))
            (#t
             (job-add-process fg? job pid command)
             (map close w)
             r))))

(define (pipeline+ fg? open? . commands)
  (let* ((job (new-job))
	 (ports (if (> (length commands) 1)
		   (let loop ((input (spawn fg? job (car commands) '() 1)) ;; spawn-source
			      (commands (cdr commands)))
		     (if (null? (cdr commands))
			 (spawn fg? job (car commands) input (if open? 1 0)) ;; spawn-sink
			 (loop (spawn fg? job (car commands) input 1) ;; spawn-filter
			       (cdr commands))))
		   (spawn fg? job (car commands) `((current-input-port))))))
    (if fg? (wait job) (values job ports))))

(define (pipeline fg? . commands)
  (apply pipeline+ (cons* fg? #f commands)))

;;(pipeline #f '("head" "-c128" "/dev/urandom") '("tr" "-dc" "A-Z0-9") (lambda () (display (read-string))))
;;(pipeline #f '("head" "-c128" "/dev/urandom") '("tr" "-dc" "A-Z0-9") '("cat"))
;;(pipeline #f (lambda () (display 'foo)) '("grep" "o") '("tr" "o" "e"))

;; (pipeline #f
;; 	  (lambda () (display "\nbin\nboot\nroot\nusr\nvar"))
;; 	  '("tr" "u" "a")
;; 	  (lambda () (display (string-map (lambda (c) (if (eq? c #\o) #\e c)) (read-string))))
;; 	  '("cat")
;; 	  (lambda () (display (read-string))))

;; (receive (job ports)
;;     (pipeline+ #f #t
;; 	      (lambda () (display "\nbin\nboot\nroot\nusr\nvar"))
;; 	      '("tr" "u" "a")
;; 	      (lambda () (display (string-map (lambda (c) (if (eq? c #\o) #\e c)) (read-string))))
;; 	      '("cat"))
;;   (display (read-string (car ports))))


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

;; _
;;  \
;;   -
;; _/

;; (display (pipeline->string
;;   (lambda () (display "\nbin\nboot\nroot\nusr\nvar"))
;;   '("tr" "u" "a")
;;   (lambda () (display (string-map (lambda (c) (if (eq? c #\o) #\e c)) (read-string))))
;;   '("cat")
;;   (lambda () (display (read-string)) (newline))))

(define (substitute . commands)
  (string-trim-right
   (string-map (lambda (c)
                 (if (eq? #\newline c) #\space c))
               (apply pipeline->string commands))
   #\space))

;; (display (pipeline->string '("ls") '("cat"))) (newline)
;; (display (substitute '("ls") '("cat"))) (newline)
