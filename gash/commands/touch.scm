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

(define-module (gash commands touch)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 receive)

  #:use-module (srfi srfi-26)

  #:use-module (gash config)
  #:use-module (gash shell-utils)

  #:export (
            touch
            ))

(define (create-or-touch file time)
  (let ((exists? (file-exists? file)))
    (when (not exists?) (with-output-to-file file (cut display "")))
    (cond (time (utime file time time))
          (exists? (let ((time (current-time)))
                     (utime file time time))))))

(define (parse-date string)
  (if (string-prefix? "@" string)
      (string->number (substring string 1))
      (error (format #f "touch: cannot parse date:~a\n" string))))

(define (touch . args)
  (let* ((option-spec
	  '((date (single-char #\d) (value #t))
            (help (single-char #\h))
            (reference (single-char #\r) (value #t))
            (version (single-char #\V))))
	 (options (getopt-long args option-spec))
	 (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
         (date (option-ref options 'date #f))
         (reference (option-ref options 'reference #f))
	 (files (option-ref options '() '()))
         (usage? (and (not help?) (null? files))))
    (cond (version? (format #t "touch (GASH) ~a\n" %version) (exit 0))
          ((or help? usage?) (format (if usage? (current-error-port) #t)
                                     "\
Usage: touch [OPTION]... FILE...
Update the access and modification times of each FILE to the current time.

Options:
  -d, --date=DATE         parse DATE and use it instead of current time
      --help              display this help and exit
  -r, --reference=FILE    use FILE's times instead of current time
      --version           output version information and exit

Each MODE is of the form '[ugoa]*([-+=]([rwxXst]*|[ugo]))+|[-+=][0-7]+'.
")
           (exit (if usage? 2 0)))
          (else
           (let ((time (and=> date parse-date)))
             (for-each (cut create-or-touch <> time) files))))))

(define main touch)
