;;; Gash-Utils
;;; Copyright © 2018, 2021 Timothy Sample <samplet@ngyro.com>
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

(define-module (gash commands sort)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (gash commands config)
  #:use-module (gash io)
  #:use-module (gash shell-utils)
  #:use-module (gash-utils options))

;;; Commentary:

;;; Code:

(define *help-message* "\
Usage: sort OPTION... [FILE]...
Write sorted concatenation of all FILEs to standard output.

With no FILE, or when FILE is -, read standard input.

Options:
  -n, --numeric-sort      compare according to string numerical value
  -r, --reverse           reverse the result of comparisons
  -c, --check             check that input is already sorted
  -u, --unique            delete duplicate lines
      --help              display this help and exit
      --version           output version information and exit
")

(define *version-message*
  (format #f "sort (~a) ~a~%" %package-name %version))

(define *options-grammar*
  (make-options-grammar
   `((flag numeric-sort #\n)
     (flag reverse #\r)
     (flag check #\c)
     (flag unique #\u)
     (message ("help" #\h) ,*help-message*)
     (message ("version" #\V) ,*version-message*))
   #:default 'files))

(define guile-sort (@ (guile) sort))

(define (sort . args)
  (let* ((options (parse-options args *options-grammar*))
         (numeric-sort? (assoc-ref options 'numeric-sort))
         (reverse? (assoc-ref options 'reverse))
         (check? (assoc-ref options 'check))
         (unique? (assoc-ref options 'unique))
         (files (or (assoc-ref options 'files) '())))
    (let* ((files (if (pair? files) files
                      (list "-")))
           (lines (append-map (compose (cut read-lines <>) (lambda (file) (if (equal? file "-") (current-input-port) (open-input-file file)))) files))
           (sorted (guile-sort lines (if numeric-sort? string-numeric<? string-locale<?)))
           (sorted (if unique? (delete-duplicates! sorted string=?) sorted))
           (sorted (if reverse? (reverse! sorted) sorted)))
      (when (and check? (not (equal? lines sorted)))
        (exit 1))
      (unless check?
        (for-each stdout sorted)))))

(define main sort)
