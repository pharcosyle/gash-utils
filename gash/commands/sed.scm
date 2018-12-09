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
  #:use-module (srfi srfi-26)

  #:use-module (gash commands sed reader)
  #:use-module (gash config)
  #:use-module (gash guix-utils)
  #:use-module (gash shell-utils)
  #:use-module (gash util)

  #:export (
            sed
            ))

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

(define quit-tag (make-prompt-tag))

(define (substitute str pattern replacement flags)
  (let* ((global? (memq 'g flags))
         (flags (cons (if (extended?) regexp/extended regexp/basic)
                      (if (memq 'i flags) `(,regexp/icase) '())))
         (regexp (apply make-regexp (replace-escapes pattern) flags))
         (proc (replace->lambda (replace-escapes replacement) global?)))
    (match (list-matches regexp str)
      ((and m+ (_ _ ...)) (proc str m+))
      (_ str))))

(define (address->pred address)
  (if (string? address)
      (let* ((flags `(,(if (extended?) regexp/extended regexp/basic)))
             (pattern (replace-escapes address))
             (regexp (apply make-regexp pattern flags)))
        (cut regexp-exec regexp <>))
      (error "SED: unsupported address type" address)))

(define (execute-function function str)
  (match function
    (('begin . commands)
     (execute-commands commands str))
    (('q) (abort-to-prompt quit-tag str))
    (('s pattern replacement flags)
     (substitute str pattern replacement flags))
    (_ (error "SED: unsupported function" function))))

(define (execute-commands commands str)
  (match commands
    (() str)
    ((('always function) . rest)
     (execute-commands rest (execute-function function str)))
    ((('at address function) . rest)
     ;; XXX: This should be "compiled" ahead of time so that it only
     ;; runs once intead of once per line.
     (if ((address->pred address) str)
         (execute-commands rest (execute-function function str))
         (execute-commands rest str)))
    ((cmd . rest) (error "SED: could not process command" cmd))))

(define* (edit-stream commands #:optional
                      (in (current-input-port))
                      (out (current-output-port)))
  (let loop ((pattern-space (read-line in)))
    (unless (eof-object? pattern-space)
      (call-with-prompt quit-tag
        (lambda ()
          (let ((result (execute-commands commands pattern-space)))
            (display result out)
            (newline out)
            (loop (read-line in))))
        (lambda (cont result)
          (display result out)
          (newline out))))
    #t))

(define (sed . args)
  (let* ((option-spec
	  '((expression (single-char #\e) (value #t))
            (extended (single-char #\r))
            (posix-extended (single-char #\E))
            (file (single-char #\f) (value #t))
            (help (single-char #\h))
            (in-place (single-char #\i))
            (version (single-char #\V))))
	 (options (getopt-long args option-spec))
	 (files (option-ref options '() '()))
	 (help? (option-ref options 'help #f))
         (in-place? (option-ref options 'in-place #f))
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
  -V, --version              display version
")
           (exit (if usage? 2 0)))
          (else
           (let* ((script-files (multi-opt options 'file))
                  (scripts (multi-opt options 'expression)))
             (receive (scripts files)
                 (if (pair? (append script-files scripts)) (values scripts files)
                     (values (list-head files 1) (cdr files)))
              (when (pair? script-files)
                (error "SED: script files not supported"))
              (let* ((script (string-join scripts "\n"))
                     (commands
                      (call-with-input-string script
                        (cut read-sed-all <> #:extended? (extended?)))))
                (cond ((and in-place? (pair? files))
                       (with-atomic-file-replacement
                        (cut edit-stream commands <> <>)))
                      ((pair? files)
                       (for-each (lambda (file)
                                   (call-with-input-file file
                                     (cut edit-stream commands <>)))
                                 files))
                      (else (edit-stream commands))))))))))

(use-modules (ice-9 rdelim))
(define main sed)
