;;; Gash-Utils
;;; Copyright © 2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018, 2020 Timothy Sample <samplet@ngyro.com>
;;;
;;; This file is part of Gash-Utils.
;;;
;;; Gash-Utils is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Gash-Utils is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Gash-Utils.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(define-module (gash commands sed)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (gash commands sed reader)
  #:use-module (gash commands config)
  #:use-module (gash shell-utils)
  #:use-module (gash util)
  #:export (sed))

(define-immutable-record-type <env>
  (make-env in out line target hold queue sub? cycle? read? quiet? labels)
  env?
  (in env-in)                         ; port - Input port.
  (out env-out)                       ; port - Output port.
  (line env-line set-env-line)        ; integer - Current line number.
  (target env-target set-env-target)  ; string - Pattern space.
  (hold env-hold set-env-hold)        ; string - Hold space.
  (queue env-queue set-env-queue)     ; list - Output queue.
  (sub? env-sub? set-env-sub?)        ; bool - Made a substitution?
  (cycle? env-cycle? set-env-cycle?)  ; bool - Start next cycle?
  (read? env-read? set-env-read?)     ; bool - Read next line?
  (quiet? env-quiet?)                 ; bool - Suppress default output?
  (labels env-labels))                ; alist - Labels for branching

(define (interpolate-match s m)
  (let loop ((chrs (string->list s)) (acc '()))
    (match chrs
      (() (reverse-list->string acc))
      ((#\& . rest)
       (loop rest (append-reverse (string->list (match:substring m)) acc)))
      ((#\\ (? char-numeric? chr) . rest)
       (let* ((i (- (char->integer chr) (char->integer #\0)))
              (ref (string->list (match:substring m i))))
         (loop rest (append-reverse ref acc))))
      ((#\\ #\\ . rest) (loop rest (cons #\\ acc)))
      ((#\\ #\& . rest) (loop rest (cons #\& acc)))
      ((#\\ #\n . rest) (loop rest (cons #\newline acc)))
      ((#\\ #\r . rest) (loop rest (cons #\return acc)))
      ((#\\ #\t . rest) (loop rest (cons #\tab acc)))
      ((#\\ 2nd . rest) (loop rest (cons 2nd acc)))
      ((chr . rest) (loop rest (cons chr acc))))))

(define (replace->lambda string global?)
  (lambda (l m+)
    ;; Iterate over matches M+ and
    ;; return the modified line
    ;; based on L.
    (let loop ((m* m+)                  ; matches
               (o  0)                   ; offset in L
               (r  '()))                ; result
      (match m*
        (()
         (let ((r (cons (substring l o) r)))
           (string-concatenate-reverse r)))
        ((m . rest)
         (let* ((refs (- (vector-length m) 2))
                (replace (interpolate-match string m))
                (replace (cons* replace (substring l o (match:start m)) r)))
           (if global? (loop rest (match:end m) replace)
               (loop '() (match:end m) replace))))))))

(define extended? (make-parameter #f))

(define end-of-script-tag (make-prompt-tag))

(define (make-regexp-factory)
  (let* ((previous-pattern #f)
         (ht (make-hash-table))
         (make-regexp/memoized
          (lambda args
            (or (hash-ref ht args #f)
                (let ((regexp (apply make-regexp args)))
                  (hash-set! ht args regexp)
                  regexp)))))
    (lambda (pattern . flags)
      (if (string-null? pattern)
          (if previous-pattern
              (apply make-regexp/memoized previous-pattern flags)
              (error "SED: no previous regular expression"))
          (begin
            (set! previous-pattern pattern)
            (apply make-regexp/memoized pattern flags))))))

(define regexp-factory
  (make-parameter
   (lambda _
     (error "SED: no regexp-factory available"))))

(define (substitute str pattern replacement flags)
  (let* ((global? (memq 'g flags))
         (flags (cons (if (extended?) regexp/extended regexp/basic)
                      (if (memq 'i flags) `(,regexp/icase) '())))
         (regexp (apply (regexp-factory) pattern flags))
         (proc (replace->lambda replacement global?)))
    (match (list-matches regexp str)
      ((and m+ (_ _ ...)) (proc str m+))
      (_ str))))

(define (translate str source dest)
  (unless (= (string-length source) (string-length dest))
    (error "SED: different length strings given to 'y' function"))
  (string-map (lambda (chr)
                (match (string-index source chr)
                  (#f chr)
                  (k (string-ref dest k))))
              str))

(define (address->pred address)
  (match address
    ((? string?)
     (let* ((flags `(,(if (extended?) regexp/extended regexp/basic)))
            (pattern address)
            (regexp (apply (regexp-factory) pattern flags)))
       (lambda (env)
         (regexp-exec regexp (env-target env)))))
    ((? number?)
     (lambda (env)
       (= (env-line env) address)))
    ('$
     (lambda (env)
       (eof-object? (lookahead-char (env-in env)))))
    ((a1 . a2)
     (let ((pred1 (address->pred a1))
           (pred2 (address->pred a2)))
       (let ((inside? #f))
         (lambda (env)
           (cond
            (inside? (when (pred2 env) (set! inside? #f)) #t)
            ((pred1 env) (set! inside? #t) #t)
            (else #f))))))
    (_ (error "SED: unsupported address type" address))))

(define (address-pred->pred apred)
  (match apred
    (('not apred*) (negate (address-pred->pred apred*)))
    ('always (const #t))
    (('at address) (address->pred address))
    (('in from to) (address->pred (cons from to)))
    (_ (error "SED: unsupported address predicate" apred))))

(define (find-labels commands)
  (let loop ((commands commands) (acc '()))
    (match commands
      (() acc)
      (((_ . (': label)) . rest)
       (loop rest (acons label rest acc)))
      (((_ . ('begin . sub-commands)) . rest)
       (loop (append sub-commands rest) acc))
      ((_ . rest)
       (loop rest acc)))))

(define (execute-function function env)
  (match function
    (('begin . commands)
     (execute-commands commands env))
    (('a text)
     (set-env-queue env (cons (string-append text "\n") (env-queue env))))
    (('comment text)
     env)
    (('d)
     (abort-to-prompt end-of-script-tag (set-env-target env #f)))
    (('D)
     (match (string-index (env-target env) #\newline)
       (#f (execute-function '(d) env))
       (k (abort-to-prompt end-of-script-tag
                           (set-fields env
                             ((env-target) (substring (env-target env)
                                                      (1+ k)))
                             ((env-read?) #f))))))
    (('g)
     (set-env-target env (env-hold env)))
    (('G)
     (set-env-target env (string-append (env-target env) "\n" (env-hold env))))
    (('h)
     (set-env-hold env (env-target env)))
    (('i text)
     (display text (env-out env))
     (newline (env-out env))
     env)
    (('H)
     (set-env-hold env (string-append (env-hold env) "\n" (env-target env))))
    (('N)
     (match (read-line (env-in env))
       ((? eof-object?)
        (abort-to-prompt end-of-script-tag (set-env-cycle? env #f)))
       (x (set-env-target env (string-append (env-target env) "\n" x)))))
    (('n)
     (unless (env-quiet? env)
       (display (env-target env) (env-out env))
       (newline (env-out env)))
     (match (read-line (env-in env))
       ((? eof-object?) (abort-to-prompt end-of-script-tag
                                         (set-fields env
                                           ((env-target) #f)
                                           ((env-cycle?) #f))))
       (line (set-env-target env line))))
    (('p)
     (display (env-target env) (env-out env))
     (newline (env-out env))
     env)
    (('P)
     (let ((end (or (string-index (env-target env) #\newline)
                    (string-length (env-target env)))))
       (display (substring (env-target env) 0 end) (env-out env))
       (newline (env-out env))
       env))
    (('q)
     (abort-to-prompt end-of-script-tag (set-env-cycle? env #f)))
    (('r path)
     (set-env-queue env (cons `(read ,path) (env-queue env))))
    (('s pattern replacement flags)
     (let* ((target (env-target env))
            (target* (substitute target pattern replacement flags))
            (sub? (not (eq? target target*))))
       (when (and sub? (member 'p flags))
         (display target* (env-out env))
         (newline (env-out env)))
       (set-fields env
         ((env-target) target*)
         ((env-sub?) sub?))))
    (('w path)
     (set-env-queue env (cons `(write ,path) (env-queue env))))
    (('x)
     (set-fields env
       ((env-target) (env-hold env))
       ((env-hold) (env-target env))))
    (('y source dest)
     (set-env-target env (translate (env-target env) source dest)))
    (_ (error "SED: unsupported function" function))))

(define get-address-pred
  (let ((address-pred-alist '()))
    (lambda (apred)
      (or (assoc-ref address-pred-alist apred)
          (let ((pred (address-pred->pred apred)))
            (set! address-pred-alist
                  (acons apred pred address-pred-alist))
            pred)))))

(define (fast-forward env apred end)
  (define in-pred (get-address-pred apred))
  (define end-pred (address->pred end))
  (let loop ((env (set-fields env
                    ((env-target) (read-line (env-in env)))
                    ((env-line) (1+ (env-line env))))))
    (cond
     ((eof-object? (env-target env)) env)
     ((and (in-pred env) (end-pred env)) env)
     (else (loop (set-fields env
                   ((env-target) (read-line (env-in env)))
                   ((env-line) (1+ (env-line env)))))))))

(define* (execute-commands commands env)
  (match commands
    (() env)
    (((apred . function) . rest)
     ;; XXX: This should be "compiled" ahead of time so that it only
     ;; runs once intead of once per line.
     (if ((get-address-pred apred) env)
         ;; Handle branching functions here, since they are the only
         ;; ones that need to change what commands get executed next.
         ;; Also, the 'c' function needs access to the address
         ;; predicate, so handle it here as well.
         (match function
           ((': label)
            (execute-commands rest env))
           (('b "")
            (abort-to-prompt end-of-script-tag env))
           (('b label)
            (match (assoc-ref (env-labels env) label)
              (#f (error "SED: no such label" label))
              (commands* (execute-commands commands* env))))
           (('c text)
            (let ((env* (match apred
                          ((or ('in _ end) ('not ('in _ end)))
                           (fast-forward env apred end))
                          (_ env))))
              (unless (eof-object? (env-target env*))
                (display text (env-out env*))
                (newline (env-out env*)))
              (abort-to-prompt end-of-script-tag (set-env-target env* #f))))
           (('t "")
            (if (env-sub? env)
                (abort-to-prompt end-of-script-tag env)
                (execute-commands rest env)))
           (('t label)
            (match (assoc-ref (env-labels env) label)
              (#f (error "SED: no such label" label))
              (commands*
               (if (env-sub? env)
                   (execute-commands commands* (set-env-sub? env #f))
                   (execute-commands rest env)))))
           (_
            (let ((env* (execute-function function env)))
              (execute-commands rest env*))))
         (execute-commands rest env)))
    ((cmd . rest) (error "SED: could not process command" cmd))))

(define* (edit-stream commands #:optional
                      (in (current-input-port))
                      (out (current-output-port))
                      #:key quiet?)

  (define (flush-then-loop loop env)
    (when (and (env-target env) (not quiet?))
      (display (env-target env) out)
      (newline out))
    (for-each (match-lambda
                (('read path) (call-with-input-file path
                                (cut dump-port <> out)))
                (('write path) (call-with-output-file path
                                 (cut put-string <> (env-target env))))
                (str (display str out)))
              (reverse (env-queue env)))
    (if (env-cycle? env)
        (let ((env* (if (env-read? env)
                        (set-fields env
                          ((env-target) (read-line (env-in env)))
                          ((env-line) (1+ (env-line env))))
                        env)))
          (loop (set-fields env*
                  ((env-queue) '())
                  ((env-sub?) #f)
                  ((env-read?) #t))))
        #f))

  (parameterize ((regexp-factory (make-regexp-factory)))
    (let loop ((env (make-env in out 1 (read-line in) ""
                              '() #f #t #t quiet?
                              (find-labels commands))))
      (if (eof-object? (env-target env))
          #t
          (let ((env* (call-with-prompt end-of-script-tag
                        (lambda () (execute-commands commands env))
                        (lambda (cont env*) env*))))
            (flush-then-loop loop env*))))))

;; TODO: Should this be part of 'cat'?
(define (file-concatenate files)
  (define port #f)
  (define %files files)
  (define (next-file!)
    (match %files
      (() (set! port #f))
      ((file . rest)
       (when port (close-port port))
       (set! port (open-file file "r"))
       (set! %files rest))))
  (next-file!)
  (make-soft-port (vector #f #f #f
                          (lambda ()
                            (let loop ()
                              (if port
                                  (match (get-char port)
                                    ((? eof-object?) (begin (next-file!)
                                                            (loop)))
                                    (chr chr))
                                  (eof-object))))
                          (lambda ()
                            (when port (close-port port))))
                  "r"))

(define (sed . args)
  (let* ((option-spec
	  '((expression (single-char #\e) (value #t))
            (extended (single-char #\r))
            (posix-extended (single-char #\E))
            (file (single-char #\f) (value #t))
            (help (single-char #\h))
            (in-place (single-char #\i))
            (separate (single-char #\s))
            (quiet (single-char #\n))
            (version (single-char #\V))))
	 (options (getopt-long args option-spec))
	 (files (option-ref options '() '()))
	 (help? (option-ref options 'help #f))
         (in-place? (option-ref options 'in-place #f))
         (separate? (option-ref options 'separate #f))
         (quiet? (option-ref options 'quiet #f))
	 (usage? (and (not help?) (or (and (null? files) (isatty? (current-input-port))))))
         (version? (option-ref options 'version #f)))
    (when (or (option-ref options 'extended #f)
              (option-ref options 'posix-extended #f))
      (extended? #t))
    (cond (version? (format #t "sed (GASH) ~a\n" %version) (exit 0))
          ((or help? usage?) (format (if usage? (current-error-port) #t)
                                     "\
Usage: sed [OPTION]... [SCRIPT] [FILE]...
  -e, --expression=SCRIPT    add SCRIPT to the commands to be executed
  -E, -r, --regexp-extended  use extended regular expressions in the script
  -f, --file=SCRIPT          add contents of SCRIPT to the commands to be executed
  -h, --help                 display this help
  -i, --in-place             edit files in place
  -s, --separate             process each file independently
  -n, --quiet                only write explicitly selected output
  -V, --version              display version
")
           (exit (if usage? 2 0)))
          (else
           (let ((mixed (mixed-multi-opt options '(expression file))))
             (define (option->script o)
               (match o
                 (('expression . expression) expression)
                 (('file . file) (with-input-from-file file read-string))))
             (receive (scripts files)
                 (if (pair? mixed) (values (map option->script mixed) files)
                     (values (list-head files 1) (cdr files)))
               (let* ((script (string-join scripts "\n"))
                      (commands
                       (call-with-input-string script
                         (cut read-sed-all <> #:extended? (extended?)))))
                 (cond ((and in-place? (pair? files))
                        (any (lambda (file)
                               (with-atomic-file-replacement file
                                 (negate (cut edit-stream commands <> <>
                                              #:quiet? quiet?))))
                             files))
                       ((and separate? (pair? files))
                        (any (lambda (file)
                               (call-with-input-file file
                                 (negate (cut edit-stream commands <>
                                              #:quiet? quiet?))))
                             files))
                       ((pair? files)
                        (call-with-port (file-concatenate files)
                          (cut edit-stream commands <> #:quiet? quiet?)))
                       (else (edit-stream commands #:quiet? quiet?))))))))))

(define main sed)

;;; Local Variables:
;;; eval: (put 'call-with-port 'scheme-indent-function 1)
;;; eval: (put 'set-fields 'scheme-indent-function 1)
;;; eval: (put 'with-atomic-file-replacement 'scheme-indent-function 1)
;;; End:
