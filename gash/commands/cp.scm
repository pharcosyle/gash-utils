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

(define-module (gash commands cp)
  #:use-module (ice-9 getopt-long)

  #:use-module (gash commands config)
  #:use-module (gash shell-utils)

  #:export (
            cp
            ))

(define (cp . args)
  (let* ((option-spec
	  '((force (single-char #\f))
            (verbose (single-char #\v))

            (help (single-char #\h))
            (version (single-char #\V))))
	 (options (getopt-long args option-spec))
         (force? (option-ref options 'force #f))
         (verbose? (option-ref options 'verbose #f))

	 (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
	 (files (option-ref options '() '()))
         (usage? (and (not help?) (< (length files) 2))))
    (cond (version? (format #t "cp (GASH) ~a\n" %version) (exit 0))
          ((or help? usage?) (format (if usage? (current-error-port) #t)
                                     "\
Usage: cp [OPTION]... SOURCE... DEST

Options:
  -f, --force     remove existing destination files
  -v, --verbose   display name of each copied file
  -h, --help      display this help and exit
  -V, --version   display version information and exit
")
           (exit (if usage? 2 0)))
          (else
           (copy-files files #:force? force? #:verbose? verbose?)))))

(define main cp)
