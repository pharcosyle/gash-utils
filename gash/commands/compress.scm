;;; Gash-Utils
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gash commands compress)
  #:use-module (ice-9 getopt-long)
  #:use-module (srfi srfi-1)
  #:use-module (gash commands config)
  #:use-module (gash compat)
  #:use-module (gash compress)
  #:use-module (gash guix-utils)
  #:export (compress))

(define (compress . args)
  (let* ((option-spec
	  '((bits (single-char #\b) (value #t))
            (decompress (single-char #\d))
            (help (single-char #\h))
            (stdout (single-char #\c))
	    (verbose (single-char #\v))
            (version (single-char #\V))))
	 (options (getopt-long args option-spec))
         (bits (string->number (option-ref options 'bits "16")))
         (decompress? (option-ref options 'decompress #f))
         (stdout? (option-ref options 'stdout #f))
	 (files (option-ref options '() '()))
	 (help? (option-ref options 'help #f))
	 (usage? (and (not help?) (or (and (null? files) (isatty? (current-input-port))))))
	 (verbose? (option-ref options 'verbose #f))
         (version? (option-ref options 'version #f)))
    (cond (version? (format #t "compress (GASH) ~a\n" %version) (exit 0))
          ((or help? usage?) (format (if usage? (current-error-port) #t)
                                     "\
Usage: compress [OPTION]... [FILE]...
  -b, --bits=BITS   use a maximum of BITS bits per code [16]
  -c, --stdout      write on standard output, keep original files unchanged
  -d, --decompress  decompress
  -h, --help        display this help
  -v, --verbose     show compression ratio
  -V, --version     display version
")
           (exit (if usage? 2 0)))
          (decompress? (if (pair? files) (uncompress-file (car files) verbose?)
                           (uncompress-port (current-input-port) (current-output-port) verbose?)))
          (else (if (pair? files) (compress-file (car files) bits verbose?)
                    (compress-port (current-input-port) (current-output-port) bits verbose?))))))

(define main compress)
