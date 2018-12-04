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

(define-module (geesh built-ins dot)
  #:use-module (geesh built-ins utils)
  #:use-module (geesh environment)
  #:use-module (geesh parser)
  #:use-module (ice-9 match))

;;; Commentary:
;;;
;;; The 'dot' utility.
;;;
;;; Code:

(define (main . args)
  (match args
    ((file)
     (catch 'system-error
       (lambda ()
         (call-with-input-file file
           (lambda (port)
             (set-status! 0)
             (call-with-return
              (lambda ()
                (let loop ()
                  (match (read-sh port)
                    ((? eof-object?) (get-status))
                    (exp ((get-evaluator) exp)
                         (loop)))))))))
       (lambda args
         (format (current-error-port)
                 "~a: .: ~a: ~a.~%"
                 (car (program-arguments)) file
                 (strerror (system-error-errno args)))
         EXIT_FAILURE)))
    (_ (format (current-error-port)
               "~a: .: Invalid options ~s.~%"
               (car (program-arguments)) args)
       EXIT_FAILURE)))
