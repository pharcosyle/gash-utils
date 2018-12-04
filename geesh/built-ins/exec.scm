;;; The Geesh Shell Interpreter
;;; Copyright 2018 Timothy Sample <samplet@ngyro.com>
;;;
;;; This file is part of Geesh.
;;;
;;; Geesh is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Geesh is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Geesh.  If not, see <http://www.gnu.org/licenses/>.

(define-module (geesh built-ins exec)
  #:use-module (geesh environment)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

;;; Commentary:
;;;
;;; The 'exec' utility.
;;;
;;; Code:

(define (main . args)
  (match args
    ((name . args)
     (flush-all-ports)
     (with-environ (get-environ)
       (lambda ()
         (catch 'system-error
           (lambda ()
             (apply execlp name name args)
             EXIT_SUCCESS)
           (lambda args
             (format (current-error-port)
                     "~a: exec: ~a: ~a.~%"
                     (car (program-arguments)) name
                     (strerror (system-error-errno args)))
             EXIT_FAILURE)))))
    (_ (format (current-error-port)
               "~a: exec: Invalid options ~s.~%"
               (car (program-arguments)) args)
       EXIT_FAILURE)))
