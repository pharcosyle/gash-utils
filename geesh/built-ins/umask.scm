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

(define-module (geesh built-ins umask)
  #:use-module (ice-9 match))

;;; Commentary:
;;;
;;; The 'umask' utility.
;;;
;;; Code:

(define (main . args)
  (match args
    ((mask)
     (let ((n (string->number mask 8)))
       (cond
        ((and n (integer? n) (>= n 0) (< n 512))
         (umask n)
         EXIT_SUCCESS)
        (else
         (format (current-error-port)
                 "~a: umask: Invalid option ~s.~%"
                 (car (program-arguments)) mask)
         EXIT_FAILURE))))
    (_ (format (current-error-port)
               "~a: umask: Invalid options ~s.~%"
               (car (program-arguments)) args)
       EXIT_FAILURE)))
