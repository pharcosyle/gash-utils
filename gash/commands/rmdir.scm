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

(define-module (gash commands rmdir)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)

  #:use-module (gash config)
  #:use-module (gash guix-utils)
  #:use-module (gash shell-utils)

  #:export (
            rmdir
            ))

(define (rmdir . args)
  (let* ((option-spec
	  '((help (single-char #\h))
            (parents (single-char #\p))
            (version (single-char #\V))))
	 (options (getopt-long args option-spec))
	 (files (option-ref options '() '()))
         (parents? (option-ref options 'parents #f))
	 (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
         (files (option-ref options '() '()))
         (usage? (and (not help?) (null? files))))
    (cond (version? (format #t "rmdir (GASH) ~a\n" %version) (exit 0))
          ((or help? usage?) (format (if usage? (current-error-port) #t)
                                     "\
Usage: rmdir [OPTION]... DIRECTORY...
Remove the DIRECTORY(ies), if they are empty.

Options:
      --help              display this help and exit
  -p, --parents           remove DIRECTORY and its ancestors; e.g., 'rmdir -p a/b/c' is
                          similar to 'rmdir a/b/c a/b a'
      --version           output version information and exit

")
           (exit (if usage? 2 0)))
          (else
           (if parents? (for-each rmdir-p files)
               (for-each rmdir files))))))

(define main rmdir)
