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

(define-module (gash commands grep)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)

  #:use-module (gash guix-utils)
  #:use-module (gash compress)
  #:use-module (gash config)
  #:use-module (gash io)
  #:use-module (gash ustar)
  #:use-module (gash util)
  #:use-module (gash shell-utils)

  #:export (
            grep
            ))

(define (grep . args)
  (let* ((option-spec
          '((help)
            (line-number (single-char #\n))
            (files-with-matches (single-char #\l))
            (files-without-match (single-char #\L))
            (with-file-name (single-char #\H))
            (no-file-name (single-char #\h))
            (only-matching (single-char #\o))
            (version (single-char #\V))))
         (options (getopt-long args option-spec))
         (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
         (files (option-ref options '() '())))
    (cond (version? (format #t "grep (GASH) ~a\n" %version))
          (help? (display "Usage: grep [OPTION]... PATTERN [FILE]...

Options:
  --help                     display this help and exit
  -h, --no-filename          suppress the file name prefix on output
  -H, --with-filename        print file name with output lines
  -l, --files-with-matches   print only names of FILEs with selected lines
  -L, --files-without-match  print only names of FILEs with no selected lines
  -n, --line-number          print line number with output lines
  -o, --only-matching        show only the part of a line matching PATTERN
  -V, --version              display version information and exit
"))
          ((null? files) #t)
          (else
           (let* ((pattern (car files))
                  (files (if (pair? (cdr files)) (cdr files)
                             (list "-")))
                  (matches (append-map (cut grep+ pattern <>) files)))
             (define (display-match o)
               (let* ((s (grep-match-string o))
                      (s (if (option-ref options 'only-matching #f)
                             (substring s (grep-match-column o) (grep-match-end-column o))
                             s))
                      (s (if (option-ref options 'line-number #f)
                             (string-append (number->string (grep-match-line o)) ":" s)
                             s))
                      (s (if (option-ref options 'with-file-name #f)
                             (string-append (grep-match-file-name o) ":" s)
                             s)))
                 (stdout s)))
             (define (files-with-matches)
               (delete-duplicates (map grep-match-file-name matches)))
             (cond ((option-ref options 'files-with-matches #f)
                    (let ((result (files-with-matches)))
                      (and (pair? result)
                           (for-each stdout result)
                           0)))
                   ((option-ref options 'files-without-match #f)
                    (let* ((result (files-with-matches))
                           (result (filter (negate (cut member <> result)) files)))
                      (and (pair? result)
                           (for-each stdout result)
                           0)))
                   (else
                    (and (pair? matches)
                         (for-each display-match matches)
                         0))))))))

(define main grep)
