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

(define-module (gash commands uniq)
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 rdelim)

  #:use-module (gash config)
  #:use-module (gash shell-utils)

  #:export (
            uniq
            ))

(define* (uniq-lines inport outport #:key count?)
  (let loop ((line (read-line inport)) (count 1))
    (unless (eof-object? line)
      (let ((next (read-line inport)))
        (cond ((equal? next line) (loop next (1+ count)))
              (else
               (when count?
                 (format outport "~7@a " count))
               (display line outport)
               (newline outport)
               (loop next 1)))))))

(define (uniq . args)
  (let* ((option-spec
	  '((help (single-char #\h))
            (version (single-char #\V))
            (count (single-char #\c))))
	 (options (getopt-long args option-spec))
	 (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
         (count? (option-ref options 'count #f))
	 (files (option-ref options '() '()))
         (usage? (and (not help?) (not (<= (length files) 2)))))
    (cond (version? (format #t "uniq (GASH) ~a\n" %version) (exit 0))
          ((or help? usage?) (format (if usage? (current-error-port) #t)
                                     "\
Usage: uniq [OPTION]... [INPUT [OUTPUT]]
Filter adjacent matching lines from INPUT (or standard input),
writing to OUTPUT (or standard output).

With no options, matching lines are merged to the first occurrence.

Options:
      --help              display this help and exit
  -c, --count             prefix lines by the number of occurrences
      --version           output version information and exit
")
           (exit (if usage? 2 0)))
          (else
           (let ((inport (if (pair? files) (and (or (file-exists?* (car files)) (exit 2))
                                                (open-input-file (car files)))
                             (current-input-port)))
                 (outport (if (= (length files) 2) (and (or (file-exists?* (cadr files)) (exit 2))
                                                        (open-output-file (cadr files)))
                              (current-output-port))))
             (uniq-lines inport outport #:count? count?))))))

(define main uniq)
