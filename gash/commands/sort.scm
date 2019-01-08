;;; Gash --- Guile As SHell
;;; Copyright © 2018 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gash commands sort)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (gash config)
  #:use-module (gash io)
  #:use-module (gash shell-utils))

;;; Commentary:

;;; Code:

(define (sort . args)
  (let* ((option-spec
	  '((numeric-sort (single-char #\n))
            (reverse (single-char #\r))
            (unique (single-char #\u))

            (help (single-char #\h))
            (version (single-char #\V))))
	 (options (getopt-long args option-spec))
         (numeric-sort? (option-ref options 'numeric-sort #f))
         (reverse? (option-ref options 'reverse #f))
         (unique? (option-ref options 'unique #f))

	 (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
	 (files (option-ref options '() '()))
         (usage? #f))
    (cond (version? (format #t "sort (GASH) ~a\n" %version) (exit 0))
          ((or help? usage?) (format (if usage? (current-error-port) #t)
                                     "\
Usage: sort OPTION... [FILE]...
Write sorted concatenation of all FILEs to standard output.

With no FILE, or when FILE is -, read standard input.

Options:
  -n, --numeric-sort          compare according to string numerical value
  -r, --reverse           reverse the result of comparisons
  -u, --unique            delete duplicate lines
      --help              display this help and exit
      --version           output version information and exit
")
           (exit (if usage? 2 0)))
          (else
           (let* ((files (if (pair? files) files
                             (list "-")))
                  (lines (append-map (compose (cut read-lines <>) (lambda (file) (if (equal? file "-") (current-input-port) (open-input-file file)))) files))
                  (sorted (sort! lines (if numeric-sort? string-numeric<? string-locale<?)))
                  (sorted (if unique? (delete-duplicates! sorted string=?) sorted))
                  (sorted (if reverse? (reverse! sorted) sorted)))
             (for-each stdout sorted))))))

(define main sort)
