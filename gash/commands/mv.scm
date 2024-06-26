;;; Gash-Utils
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Timothy Sample <samplet@ngyro.com>
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

(define-module (gash commands mv)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (gash commands config)
  #:use-module (gash shell-utils)
  #:use-module (gash-utils options)
  #:export (mv))

(define *help-message* "\
Usage: mv [OPTION]... SOURCE... DEST
Options:
  -f, --force     ignored for compatibility
  -h, --help      display this help and exit
  -V, --version   display version information and exit
")

(define *version-message*
  (format #f "mv (~a) ~a~%" %package-name %version))

(define *options-grammar*
  (make-options-grammar
   `((flag force #\f)
     (message ("help" #\h) ,*help-message*)
     (message ("version" #\V) ,*version-message*))
   #:default 'files))

(define (mv . args)
  (let* ((options (parse-options args *options-grammar*))
         (force? (assoc-ref options 'force))
         (files (or (assoc-ref options 'files) '())))
    (match files
      ((source (and (? directory-exists?) dir))
       (rename-file source (string-append dir "/" (basename source))))
      ((source dest)
       (rename-file source dest))
      ((sources ..1 dir)
       (unless (directory-exists? dir)
         (error (format #f "mv: target `~a' is not a directory\n" dir)))
       (for-each
        rename-file
        sources
        (map (compose (cute string-append dir "/" <>) basename)
             sources)))
      (x (format (current-error-port) "mv: invalid options: ~s~%" x)
         (display *help-message*)
         (exit 2)))))

(define main mv)
