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

(define-module (gash commands dirname)
  #:use-module (ice-9 getopt-long)

  #:use-module (gash commands config)

  #:export (
            dirname
            ))

(define (dirname . args)
  (let* ((option-spec
	  '((help (single-char #\h))
            (version (single-char #\V))
            (zero (single-char #\z))))
	 (options (getopt-long args option-spec))
	 (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
         (files (option-ref options '() '()))
         (zero? (option-ref options 'zero #f))
         (usage? (and (not help?) (null? files))))
    (cond (version? (format #t "dirname (GASH) ~a\n" %version) (exit 0))
          ((or help? usage?) (format (if usage? (current-error-port) #t)
                                     "\
Usage: dirname [OPTION] NAME...
Output each NAME with its last non-slash component and trailing slashes
removed; if NAME contains no /'s, output '.' (meaning the current directory).

Options:
      --help              display this help and exit
      --version           output version information and exit
  -z, --zero              end each output line with NUL, not newline
")
           (exit (if usage? 2 0)))
          (else
           (for-each (lambda (file)
                       (display ((@ (guile) dirname) file))
                       (if zero? (display #\nul) (newline)))
                     files)))))

(define main dirname)
