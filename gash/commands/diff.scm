;;; Gash -- Guile As SHell
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

;;; Commentary:

;;; Code:

(define-module (gash commands diff)
  #:use-module (ice-9 getopt-long)

  #:use-module (srfi srfi-1)

  #:use-module (gash config)
  #:use-module (gash diff)
  #:export (
            diff
            ))

(define (diff . args)
  (let* ((option-spec
	  '((help (single-char #\h))
            (version (single-char #\V))
            (unified (single-char #\u))))
	 (options (getopt-long args option-spec))
	 (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
	 (files (option-ref options '() '()))
         (usage? (and (not help?) (not (= (length files) 2)))))
    (cond (version? (format #t "diff (GASH) ~a\n" %version) (exit 0))
          ((or help? usage?) (format (if usage? (current-error-port) #t)
                                     "\
Usage: diff [OPTION]... FILES
Compare FILES line by line.

Options:
      --help              display this help and exit
  -u, --unified           output three lines of unified context
      --version           output version information and exit
")
           (exit (if usage? 2 0)))
          (else
           
           (let ((hunks (apply diff-files (list-head files 2))))
             (when (pair? hunks)
               (display (string-join (append-map hunk->lines hunks) "\n" 'suffix))
               (exit 1)))))))

(define main diff)
