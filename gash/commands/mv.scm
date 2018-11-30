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
;;; Gash is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Gash.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(define-module (gash commands mv)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (gash config)
  #:use-module (gash shell-utils)
  #:export (
            mv
            ))

(define (mv name . args)
  (match args
    ((or "-h" "--help")
     (format #t "mv SOURCE... DEST\n"))
    ((or "-V" "--version")
     (format #t "mv (GASH) ~a\n" %version) (exit 0))
    ((source dest)
     (rename-file source dest))
    ((sources ... dest)
     (unless (directory-exists? dest)
       (error (format #f "mv: target `~a' is not a directory\n" dest)))
     (for-each rename-file
               sources
               (map (cut string-append dest "/" <>) sources)))))

(define main mv)
