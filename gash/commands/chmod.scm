;;; Gash -- Guile As SHell
;;; Copyright © 2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
  #:use-module (ice-9 receive)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (gash commands config)
  #:use-module (gash shell-utils)

  #:export (
            chmod
            ))

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
	 (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
         (files (option-ref options '() '()))
         (reference (option-ref options 'reference #f))
         (readable? (option-ref options 'readable #f))
         (writable? (option-ref options 'writable #f))
         (executable? (option-ref options 'executable #f))
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
      --reference=FILE    use FILE's mode instead of MODE values
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
                (else (values (parse-chmodifiers (car files)) (cdr files))))
             (let ((files (if (not (option-ref options 'recursive #f)) files
                              (append-map (cut find-files <> #:directories? #t) files))))
               (for-each (cut apply-chmodifiers <> modifiers) (reverse files))))))))

(define main chmod)
