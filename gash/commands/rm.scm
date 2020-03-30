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
  #:use-module (gash compat)
  #:use-module (gash shell-utils)
  #:use-module (gash-utils options)
  #:export (rm))

(define *options-grammar*
  (make-options-grammar
   `((flag force #\f)
     (flag recursive #\r #\R))
   #:default 'files))

(define (rm . args)
  (let* ((options (parse-options args *options-grammar*))
         (force? (assoc-ref options 'force))
         (recursive? (assoc-ref options 'recursive))
         (files (assoc-ref options 'files)))
    (fold (lambda (file status)
            (catch 'system-error
              (lambda ()
                (if recursive?
                    (delete-file-recursively file)
                    (delete-file file))
                status)
              (lambda args
                (let ((errno (system-error-errno args)))
                  (if force?
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
