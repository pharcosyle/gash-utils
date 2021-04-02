;;; Gash-Utils
;;; Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 Timothy Sample <samplet@ngyro.com>
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
  #:use-module (gash-utils options)
  #:export (cp))

(define *help-message* "\
Usage: cp [OPTION]... SOURCE... DEST

Options:
  -f, --force     remove existing destination files
  -p, --preserve  preserve timestamps and permissions
  -v, --verbose   display name of each copied file
  -h, --help      display this help and exit
  -V, --version   display version information and exit
")

(define *version-message*
  (format #f "cp (~a) ~a~%" %package-name %version))

(define *options-grammar*
  (make-options-grammar
   `((flag force #\f)
     (flag preserve #\p)
     (flag verbose #\v)
     (message ("help" #\h) ,*help-message*)
     (message ("version" #\V) ,*version-message*))
   #:default 'files))

(define (cp . args)
  (let* ((options (parse-options args *options-grammar*))
         (force? (assoc-ref options 'force))
         (preserve? (assoc-ref options 'preserve))
         (verbose? (assoc-ref options 'verbose))
	 (files (or (assoc-ref options 'files) '())))
    (copy-files files
                #:force? force?
                #:preserve? preserve?
                #:verbose? verbose?)))

(define main cp)
