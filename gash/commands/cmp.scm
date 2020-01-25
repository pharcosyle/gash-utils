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

(define-module (gash commands cmp)
  #:use-module (ice-9 getopt-long)

  #:use-module (gash commands config)
  #:use-module (gash shell-utils)

  #:export (
            cmp
            ))

(define (cmp . args)
  (let* ((option-spec
	  '((help (single-char #\h))
            (version (single-char #\V))
            (quiet)
            (silent (single-char #\s))))
	 (options (getopt-long args option-spec))
	 (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
         (quiet? (or (option-ref options 'quiet #f)
                     (option-ref options 'silent #f)))
	 (files (option-ref options '() '()))
         (usage? (and (not help?) (not (= (length files) 2)))))
    (cond (version? (format #t "cmp (GASH) ~a\n" %version) (exit 0))
          ((or help? usage?) (format (if usage? (current-error-port) #t)
                                     "\
Usage: cmp [OPTION]... FILES
Compare FILES byte by byte.

Options:
      --help              display this help and exit
  -s, --quiet, --silent   suppress all normal output
      --version           output version information and exit
")
           (exit (if usage? 2 0)))
          (else
           (let* ((file (car files))
                  (file2 (cadr files)))
             (or (file-exists?* file) (exit 2))
             (or (file-exists?* file2) (exit 2))
             (let ((port (open-input-file file))
                   (port2 (open-input-file file2)))
               (let loop ((count 0) (line 1))
                 (let ((c (read-char port))
                       (c2 (read-char port2)))
                   (cond ((and (eof-object? c) (eof-object? c2))
                          (exit 0))
                         ((eof-object? c)
                          (unless quiet?
                            (format (current-error-port) "cmp: EOF on ~a after byte ~a, in line ~a\n"
                                    file count line))
                          (exit 1))
                         ((eof-object? c2)
                          (unless quiet?
                            (format (current-error-port) "cmp: EOF on ~a after byte ~a, in line ~a\n"
                                    file2 count line))
                          (exit 1))
                         ((eq? c c2) (loop (1+ count) (if (eq? c #\newline) (1+ line) line)))
                         (else
                          (unless quiet?
                            (format (current-error-port) "~a ~a differ: char ~a, in line ~a\n"
                                    file file2 count line))
                          (exit 1)))))))))))

(define main cmp)
