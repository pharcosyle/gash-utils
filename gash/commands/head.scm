;;; Gash-Utils
;;; Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gash commands head)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (gash commands config)
  #:use-module (gash shell-utils)

  #:export (head))

(define (head-lines inport outport lines)
  (let loop ((line (read-line inport)) (lines lines))
    (unless (or (zero? lines)
                (eof-object? line))
      (display line outport)
      (newline outport)
      (loop (read-line inport) (1- lines)))))

(define (number-opt o)
  (and (string? o)
       (string-prefix? "-" o)
       (string->number (substring o 1))))

(define (head . args)
  (let* ((option-spec
	  '((lines (single-char #\n) (value #t))
            (quiet (single-char #\q))
            (silent)
            (verbose)
            (help (single-char #\h))
            (version (single-char #\V))))
	 (options (getopt-long args option-spec))
         (lines (option-ref options 'lines #f))
         (quiet? (option-ref options 'quiet #f))
         (silent? (option-ref options 'silent #f))
         (verbose? (option-ref options 'verbose #f))

	 (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
	 (files (option-ref options '() '()))
         (files (filter (negate number-opt) files))
         (usage? (and #f (not help?))))
    (cond (version? (format #t "head (GASH) ~a\n" %version) (exit 0))
          ((or help? usage?) (format (if usage? (current-error-port) #t)
                                     "\
Usage: head [OPTION]... [FILE]...
Print the first 10 lines of each FILE to standard output.
With more than one FILE, precede each with a header giving the file name.

With no FILE, or when FILE is -, read standard input.

Options:
  -n, --lines=NUM         print the first NUM lines instead of the first 10
  -q, --quiet, --silent   never print headers giving file names
  -v, --verbose           always print headers giving file names
      --help              display this help and exit
      --version           output version information and exit
")
           (exit (if usage? 2 0)))
          (else
           (let ((files (if (pair? files) files
                            (list "-")))
                 (lines (or (and=> lines string->number)
                            (and=> (find number-opt args) number-opt)
                            10))
                 (header? (and (or (> (length files) 1)
                                   verbose?)
                               (not quiet?)
                               (not silent?)))
                 (outport (current-output-port)))
             (when (< lines 0)
               (error (format #f "head: negative line count not supported: ~s\n" lines)))
             (for-each
              (lambda (file)
                (when header?
                  (format outport "==> ~a <==\n" file))
                (let ((inport (if (equal? file "-") (current-input-port)
                                  (and (or (file-exists?* file) (exit 2))
                                       (open-input-file file)))))
                  (head-lines inport outport lines)))
              files))))))

(define main head)
