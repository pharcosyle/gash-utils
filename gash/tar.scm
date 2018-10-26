;;; Gash --- Guile As SHell
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of Gash.
;;;
;;; Gash is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Gash is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Gash.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gash tar)
  #:use-module (ice-9 getopt-long)
  #:use-module (gash config)
  #:use-module (gash ustar)
  #:export (main))

(define (parse-opts args)
  (let* ((option-spec
	  '((create (single-char #\c))
            (extract (single-char #\x))
            (file (single-char #\f) (value #t))
            (help (single-char #\h))
	    (version (single-char #\V))))
	 (options (getopt-long args option-spec))
         (create? (option-ref options 'create #f))
         (extract? (option-ref options 'extract #f))
	 (help? (option-ref options 'help #f))
	 (files (option-ref options '() '()))
	 (usage? (and (not help?) (not (or (and create? (pair? files)) extract?))))
	 (version? (option-ref options 'version #f)))

    (or
     (and version?
	  (format #t "tar (GASH) ~a\n" %version)
          (exit 0))
     (and (or help? usage?)
          (format (or (and usage? (current-error-port)) (current-output-port))
           (string-append "\
Usage: tar [OPTION]... [FILE]...
  -c, --create           create a new archive
  -e, --extract          extract files from an archive
  -f, --file=ARCHIVE     use archive file or device ARCHIVE
  -h, --help             display this help
  -V, --version          display version
"))
	   (exit (or (and usage? 2) 0)))
     options)))

(define (main args)
  (let* ((options (parse-opts args))
         (create? (option-ref options 'create #f))
         (extract? (option-ref options 'extract #f))
         (file (option-ref options 'file "/dev/stdout"))
         (files (option-ref options '() '())))
    (cond (create?
           (write-ustar-archive file files))
          (extract?
           (read-ustar-archive file files)))))
