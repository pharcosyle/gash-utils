;;; Gash -- Guile As SHell
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

(define-module (gash commands chmod)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)

  #:use-module (gash config)
  #:use-module (gash guix-utils)
  #:use-module (gash shell-utils)

  #:export (
            chmod
            ))

(define-immutable-record-type <chmodifier>
  (make-chmodifier users operation permissions)
  chmodifier?
  (users chmodifier-users)
  (operation chmodifier-operation)
  (permissions chmodifier-permissions))

(define (parse-modifier o)
  (let* ((c (string->symbol (substring o 0 1)))
         (o (if (memq c '(- + =)) (string-append "a" o) o))
         (users (string->symbol (substring o 0 1))))
    (when (not (memq users '(u g o a)))
      (error (format #f "chmod: no such user: ~a" users)))
    (let ((operation (string->symbol (substring o 1 2))))
      (when (not (memq operation '(- + =)))
        (error (format #f "chmod: no such operation: ~a" operation)))
      (let* ((perm-string (substring o 2))
             (perm (string->number perm-string 8)))
        (if perm (make-numeric-chmodifier perm)
            (let ((perms (map char->symbol (string->list perm-string))))
              (make-chmodifier users operation perms)))))))

(define (char->symbol c)
  (string->symbol (make-string 1 c)))

(define (parse-modifiers o)
  (or (and=> (string->number o 8) (compose list (cut make-numeric-chmodifier <>)))
      (map parse-modifier (string-split o #\,))))

(define (make-numeric-chmodifier o)
  (make-chmodifier 'o '= (list o)))

(define (apply-chmodifiers file modifiers)
  (let* ((mode (stat:mode (lstat file)))
         (executable? (if (zero? (logand mode #o111)) 0 1)))
    (let loop ((modifiers modifiers) (mode mode))
      (if (null? modifiers) ((@ (guile) chmod) file mode)
          (loop (cdr modifiers)
                (let* ((m (car modifiers))
                       (n (chmodifier-numeric-mode m executable?))
                       (o (chmodifier-operation m)))
                  (case o
                    ((=) n)
                    ((+) (logior mode n))
                    ((-) (logand mode n))
                    (else (error (format #f
                                         "chmod: operation not supported: ~s\n" o))))))))))

(define (chmodifier-numeric-mode o executable?)
  (let* ((permissions (chmodifier-permissions o))
         (users (chmodifier-users o)))
    (let loop ((permissions permissions))
      (if (null? permissions) 0
          (+ (let* ((p (car permissions))
                    (base (cond ((number? p) p)
                                ((symbol? p)
                                 (case p
                                   ((r) 4)
                                   ((w) 2)
                                   ((x) 1)
                                   ((X) executable?))))))
               (case users
                 ((a) (+ base (ash base 3) (ash base 6)))
                 ((o) base)
                 ((g) (ash base 3))
                 ((u) (ash base 6))))
             (loop (cdr permissions)))))))

(define (chmod . args)
  (let* ((option-spec
	  '((reference (value #t))
            (recursive (single-char #\R))
            (help (single-char #\h))
            (version (single-char #\V))
            (writable (single-char #\w))
            (readable (single-char #\r))
            (executable (single-char #\x))
            (xecutable (single-char #\X))))
	 (options (getopt-long args option-spec))
	 (files (option-ref options '() '()))
	 (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
         (files (option-ref options '() '()))
         (reference (option-ref options 'reference #f))
         (readable? (option-ref options 'readable #f))
         (writable? (option-ref options 'writable #f))
         (executable? (option-ref options 'executable? #f))
         (xecutable? (option-ref options 'xecutable? #f))
         (usage? (and (not help?)
                      (< (length files) (if (or reference
                                                readable?
                                                writable?
                                                executable?
                                                xecutable?) 1 2)))))
    (cond (version? (format #t "chmod (GASH) ~a\n" %version) (exit 0))
          ((or help? usage?) (format (if usage? (current-error-port) #t)
                                     "\
Usage: chmod [OPTION]... {MODE | --reference=REF_FILE} FILE...
Change the mode of each FILE to MODE.
With --reference, change the mode of each FILE to that of RFILE.

Options:
      --help              display this help and exit
  -R, --recursive         change files and directories recursively
      --reference=RFILE   use RFILE's mode instead of MODE values
      --version           output version information and exit

Each MODE is of the form '[ugoa]*([-+=]([rwxXst]*|[ugo]))+|[-+=][0-7]+'.
")
           (exit (if usage? 2 0)))
          (else
           (receive (modifiers files)
               (cond
                (reference (values (list (make-numeric-chmodifier
                                          (stat:mode (stat reference)))) files))
                ((or readable? writable? executable? xecutable?)
                 (let* ((m '())
                        (m (if readable?   (cons (make-chmodifier 'o '- '(r)) m) m))
                        (m (if writable?   (cons (make-chmodifier 'o '- '(w)) m) m))
                        (m (if executable? (cons (make-chmodifier 'o '- '(x)) m) m))
                        (m (if xecutable?  (cons (make-chmodifier 'o '- '(X)) m) m)))
                   (values m files)))
                (else (values (parse-modifiers (car files)) (cdr files))))
             (let ((files (if (option-ref options 'recursive #f) (append-map find-files files)
                              files)))
               (for-each (cut apply-chmodifiers <> modifiers) files)))))))

(define main chmod)
