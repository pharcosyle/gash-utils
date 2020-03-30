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

(define-module (gash commands mkdir)
  #:use-module (gash commands config)
  #:use-module (gash shell-utils)
  #:use-module (gash-utils options)
  #:export (mkdir'))

(define *help-message* "\
Usage: mkdir [OPTION]... DIRECTORY...
Create the DIRECTORY(ies), if they do not already exist.

Options:
  -m, --mode=MODE   set file mode (as in chmod), not a=rwx - umask
  -p, --parents     no error if existing, make parent directories as needed
  -h, --help        display this help and exit
  -V, --version     output version information and exit
")

(define *version-message*
  (format #f "mkdir (~a) ~a~%" %package-name %version))

(define *options-grammar*
  (make-options-grammar
   `((value mode #\m)
     (flag parents #\p)
     (message ("help" #\h) ,*help-message*)
     (message ("version" #\V) ,*version-message*))
   #:default 'files))

(define (call-with-umask mask thunk)
  (let ((outer-mask #f)
        (inner-mask mask))
    (dynamic-wind
      (lambda () (set! outer-mask (umask inner-mask)))
      thunk
      (lambda () (set! inner-mask (umask outer-mask))))))

(define (mkdir' . args)
  (let* ((options (parse-options args *options-grammar*))
         (mode (assoc-ref options 'mode))
         (parents? (assoc-ref options 'parents))
	 (files (or (assoc-ref options 'files) '())))
    (when (null? files)
      (format (current-error-port) "mkdir: missing operand~%")
      (exit 2))
    (let* ((chomodifiers (parse-chmodifiers (or mode "755")))
           (mode (chmodifiers->mode chomodifiers)))
      ;; XXX: This seems like a fragile way to invert the mode.
      (call-with-umask (bit-extract (lognot mode) 0 9)
        (lambda ()
          (for-each (if parents? mkdir-p (@ (guile) mkdir)) files))))))

(define main mkdir')
