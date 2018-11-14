;;; Gash --- Guile As SHell
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

  #:use-module (gash config)
  #:use-module (gash guix-utils)
  #:use-module (gash shell-utils)

  #:export (
            sed
            ))

(define (replace->lambda string modifiers)
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
           (if (memq #\g modifiers) (loop rest (match:end m) replace)
               (loop '() (match:end m) replace))))))))

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
         (extended? (or (option-ref options 'extended #f)
                        (option-ref options 'posix-extended #f)))
	 (help? (option-ref options 'help #f))
         (in-place? (option-ref options 'in-place #f))
	 (usage? (and (not help?) (or (and (null? files) (isatty? (current-input-port))))))
         (version? (option-ref options 'version #f)))
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
              (define (script->command o)
                (cond ((string-prefix? "s" o)
                       (let* ((command (substring o 1))
                              (string (substring command 1))
                              (separator (string-ref command 0)))
                         (receive (search replace modifier-string)
                             (apply values (string-split string separator))
                           (let* ((modifiers (string->list modifier-string))
                                  (flags (if extended? (list regexp/extended) (list regexp/basic)))
                                  (flags (if (memq #\i modifiers) (cons regexp/icase flags)
                                             flags)))
                             `((,search . ,flags) . ,(replace->lambda replace modifiers))))))
                      (else (error (format #f "SED: command not supported: ~s\n" o)))))
              (when (pair? script-files)
                (error "SED: script files not supported"))
              (let ((commands (map script->command scripts)))
                (cond ((and in-place? (pair? files))
                       (for-each (lambda (file) (substitute* file commands)) files))
                      ((pair? files)
                       (for-each (lambda (file)
                                   (with-input-from-file file
                                     (lambda _ (substitute-port commands))))
                                 files))
                      (else (substitute-port commands))))))))))

(use-modules (ice-9 rdelim))
(define main sed)
