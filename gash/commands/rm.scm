;;; Gash-Utils
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

;;; The initial bournish.scm was taken from Guix.

;;; Code:

(define-module (gash commands rm)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (gash commands config)
  #:use-module (gash compat)
  #:use-module (gash shell-utils)
  #:use-module (gash-utils options)
  #:export (rm))

(define *help-message* "\
Usage: rm [OPTION]... [FILE]...
  -f, --force           ignore nonexistent files
  -i, --interactive     prompt before removing a file
  -r, -R, --recursive   remove directories recursively
  -h, --help            display this help
  -V, --version         display version
")

(define *version-message*
  (format #f "rm (~a) ~a~%" %package-name %version))

(define *options-grammar*
  (make-options-grammar
   `((toggle communication
             (("force" #\f) none)
             (("interactive" #\i) ask))
     (flag recursive #\r #\R)
     (message ("help" #\h) ,*help-message*)
     (message ("version" #\V) ,*version-message*))
   #:default 'files))

(define (rm . args)
  (let* ((options (parse-options args *options-grammar*))
         (communication (or (assoc-ref options 'communication) 'warn))
         (recursive? (assoc-ref options 'recursive))
         (files (or (assoc-ref options 'files) '())))
    (when (and (null? files) (not (eq? communication 'none)))
      (format (current-error-port) "rm: missing operand~%")
      (exit 2))
    (when (eq? communication 'ask)
      (format (current-error-port) "rm: interactive mode not supported~%")
      (exit EXIT_FAILURE))
    (fold (lambda (file status)
            (catch 'system-error
              (lambda ()
                (if recursive?
                    (delete-file-recursively file)
                    (delete-file file))
                status)
              (lambda args
                (let ((errno (system-error-errno args)))
                  (if (eq? communication 'none)
                      status
                      (begin
                        (format (current-error-port)
                                "rm: cannot remove ~s: ~a~%"
                                file (strerror errno))
                        EXIT_FAILURE))))))
          EXIT_SUCCESS
          files)))

(define (main . args)
  (exit (apply rm args)))
