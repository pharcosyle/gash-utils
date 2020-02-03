;;; Gash-Utils
;;; Copyright © 2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018 Timothy Sample <samplet@ngyro.com>
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

  #:export (
            sed
            ))

(define-immutable-record-type <env>
  (make-env in out line target hold queue sub? labels)
  env?
  (in env-in)                         ; port - Input port.
  (out env-out)                       ; port - Output port.
  (line env-line set-env-line)        ; integer - Current line number.
  (target env-target set-env-target)  ; string - Pattern space.
  (hold env-hold set-env-hold)        ; string - Hold space.
  (queue env-queue set-env-queue)     ; list - Output queue.
  (sub? env-sub? set-env-sub?)        ; bool - Made a substitution?
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

(define (replace-escapes str)
  (let* ((str (string-replace-string str "\\n" "\n"))
         (str (string-replace-string str "\\r" "\r"))
         (str (string-replace-string str "\\t" "\t")))
    str))

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
         (regexp (apply (regexp-factory) (replace-escapes pattern) flags))
         (proc (replace->lambda (replace-escapes replacement) global?)))
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
            (pattern (replace-escapes address))
            (regexp (apply (regexp-factory) pattern flags)))
       (lambda (env)
         (regexp-exec regexp (env-target env)))))
    ((? number?)
     (lambda (env)
       (= (env-line env) address)))
    (((and (? number?) from) . (and (? number?) to))
     (lambda (env)
       (and (>= (env-line env) from)
            (<= (env-line env) to))))
    (((and (? number?) from) . (and (? string?) to))
     (let ((to-seen? #f))
       (lambda (env)
         (let ((result (and (>= (env-line env) from)
                            (not to-seen?))))
           (set! to-seen? (or to-seen? ((address->pred to) env)))
           result))))
    (((and (? string?) from) . (and (? number?) to))
     (let ((from-seen? #f))
       (lambda (env)
         (set! from-seen? (or from-seen? ((address->pred from) env)))
         (and from-seen?
              (<= (env-line env) to)))))
    (((and (? string?) from) . (and (? string?) to))
     (let ((from-seen? #f)
           (to-seen? #f))
       (lambda (env)
         (set! from-seen? (or from-seen? ((address->pred from) env)))
         (let ((result (and from-seen?
                            (not to-seen?))))
           (set! to-seen? (or to-seen? ((address->pred to) env)))
           result))))
    (((and (? number?) from) . '$)
     (lambda (env)
       (>= (env-line env) from)))
    (((and (? string?) string) . '$)
     (let ((from-seen? #f))
       (lambda (env)
         (set! from-seen? (or from-seen? ((address->pred string) env)))
         from-seen?)))
    ('$
     (lambda (env)
       (eof-object? (lookahead-char (env-in env)))))
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
    (('d)
     (abort-to-prompt end-of-script-tag (set-env-target env #f) #t))
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
       ((? eof-object?) (abort-to-prompt end-of-script-tag env #f))
       (x (set-env-target env (string-append (env-target env) "\n" x)))))
    (('n)
     (display (env-target env) (env-out env))
     (newline (env-out env))
     (set-env-target env (read-line (env-in env))))
    (('p)
     (display (env-target env) (env-out env))
     (newline (env-out env))
     env)
    (('q)
     (abort-to-prompt end-of-script-tag env #f))
    (('r path)
     (set-env-queue env (cons `(file ,path) (env-queue env))))
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

(define* (execute-commands commands env)
  (match commands
    (() env)
    (((apred . function) . rest)
     ;; XXX: This should be "compiled" ahead of time so that it only
     ;; runs once intead of once per line.
     (if ((get-address-pred apred) env)
         ;; Handle branching functions here, since they are the only
         ;; ones that need to change what commands get executed next.
         (match function
           ((': label)
            (execute-commands rest env))
           (('b "")
            (abort-to-prompt end-of-script-tag env #t))
           (('b label)
            (match (assoc-ref (env-labels env) label)
              (#f (error "SED: no such label" label))
              (commands* (execute-commands commands* env))))
           (('t "")
            (if (env-sub? env)
                (abort-to-prompt end-of-script-tag env #t)
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
                (('file path) (call-with-input-file path
                                (cut dump-port <> out)))
                (str (display str out)))
              (reverse (env-queue env)))
    (loop (set-fields env
            ((env-target) (read-line (env-in env)))
            ((env-line) (1+ (env-line env)))
            ((env-queue) '())
            ((env-sub?) #f))))

  (parameterize ((regexp-factory (make-regexp-factory)))
    (let loop ((env (make-env in out 1 (read-line in) ""
                              '() #f (find-labels commands))))
      (unless (eof-object? (env-target env))
        (call-with-prompt end-of-script-tag
          (lambda ()
            (let ((env* (execute-commands commands env)))
              (flush-then-loop loop env*)))
          (lambda (cont env* cycle?)
            (flush-then-loop (if cycle? loop noop) env*))))
      #t)))

(define (sed . args)
  (let* ((option-spec
	  '((expression (single-char #\e) (value #t))
            (extended (single-char #\r))
            (posix-extended (single-char #\E))
            (file (single-char #\f) (value #t))
            (help (single-char #\h))
            (in-place (single-char #\i))
            (quiet (single-char #\n))
            (version (single-char #\V))))
	 (options (getopt-long args option-spec))
	 (files (option-ref options '() '()))
	 (help? (option-ref options 'help #f))
         (in-place? (option-ref options 'in-place #f))
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
                        (for-each (lambda (file)
                                    (with-atomic-file-replacement file
                                      (cut edit-stream commands <> <>
                                           #:quiet? quiet?)))
                                  files))
                       ((pair? files)
                        (for-each (lambda (file)
                                    (call-with-input-file file
                                      (cut edit-stream commands <>
                                           #:quiet? quiet?)))
                                  files))
                       (else (edit-stream commands #:quiet? quiet?))))))))))

(define main sed)
