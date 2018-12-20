;;; Gash --- Guile As SHell
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018 Timothy Sample <samplet@ngyro.com>
;;;
;;; This file is part of Gash.
;;;
;;; Gash is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Gash is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Gash.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(define-module (gash commands sed)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)

  #:use-module (gash commands sed reader)
  #:use-module (gash config)
  #:use-module (gash guix-utils)
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

(define (replace->lambda string global?)
  (define (replace->string m s)
    (list->string
     (let loop ((lst (string->list string)))
       (cond ((null? lst) '())
             ((null? (cdr lst)) lst)
             ((and (eq? (car lst) #\\)
                   (char-numeric? (cadr lst)))
              (let ((i (- (char->integer (cadr lst)) (char->integer #\0))))
                (append (string->list (match:substring m i)) (loop (cddr lst)))))
             ((and (eq? (car lst) #\\)
                   (eq? (cadr lst) #\n))
              (append '(#\newline) (cddr lst)))
             ((and (eq? (car lst) #\\)
                   (eq? (cadr lst) #\t))
              (append '(#\tab) (cddr lst)))
             ((and (eq? (car lst) #\\)
                   (eq? (cadr lst) #\r))
              (append '(#\return) (cddr lst)))
             ((and (eq? (car lst) #\\)
                   (eq? (cadr lst) #\\))
              (append '(#\\ #\\) (cddr lst)))
             (else (cons (car lst) (loop (cdr lst))))))))
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
                (replace (replace->string m string))
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
       (lambda (str _)
         (regexp-exec regexp str))))
    ((? number?)
     (lambda (_ n)
       (= n address)))
    (_ (error "SED: unsupported address type" address))))

(define (address-pred->pred apred)
  (match apred
    (('not apred*) (negate (address-pred->pred apred*)))
    ('always (const #t))
    (('at address) (address->pred address))
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
    (('g)
     (set-env-target env (env-hold env)))
    (('G)
     (set-env-target env (string-append (env-target env) "\n" (env-hold env))))
    (('h)
     (set-env-hold env (env-target env)))
    (('H)
     (set-env-hold env (string-append (env-hold env) "\n" (env-target env))))
    (('p)
     (display (env-target env) (env-out env))
     (newline (env-out env))
     env)
    (('q)
     (abort-to-prompt end-of-script-tag env #:cycle? #f))
    (('s pattern replacement flags)
     (let* ((target (env-target env))
            (target* (substitute target pattern replacement flags)))
       (set-fields env
         ((env-target) target*)
         ((env-sub?) (not (eq? target target*))))))
    (('x)
     (set-fields env
       ((env-target) (env-hold env))
       ((env-hold) (env-target env))))
    (('y source dest)
     (set-env-target env (translate (env-target env) source dest)))
    (_ (error "SED: unsupported function" function))))

(define* (execute-commands commands env)
  (match commands
    (() env)
    (((apred . function) . rest)
     ;; XXX: This should be "compiled" ahead of time so that it only
     ;; runs once intead of once per line.
     (if ((address-pred->pred apred) (env-target env) (env-line env))
         ;; Handle branching functions here, since they are the only
         ;; ones that need to change what commands get executed next.
         (match function
           ((': label)
            (execute-commands rest env))
           (('b "")
            (abort-to-prompt end-of-script-tag env))
           (('b label)
            (match (assoc-ref (env-labels env) label)
              (#f (error "SED: no such label" label))
              (commands* (execute-commands commands* env))))
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
    (unless quiet?
      (display (env-target env) out)
      (newline out))
    (for-each (cut display <> out) (reverse (env-queue env)))
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
          (lambda* (cont env* #:key (cycle? #t))
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
           (let* ((script-files (multi-opt options 'file))
                  (scripts (multi-opt options 'expression)))
             (receive (scripts files)
                 (cond
                  ((and (pair? script-files) (pair? scripts))
                   ;; XXX: Until we respect the order in which scripts
                   ;; are specified, we cannot do this properly.
                   (error "SED: cannot mix argument and file scripts"))
                  ((pair? script-files)
                   (values (map (cut call-with-input-file <> get-string-all)
                                script-files)
                           files))
                  ((pair? scripts) (values scripts files))
                  (else (values (list-head files 1) (cdr files))))
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

(use-modules (ice-9 rdelim))
(define main sed)
