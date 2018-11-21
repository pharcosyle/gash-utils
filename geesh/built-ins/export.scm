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

(define-module (geesh built-ins export)
  #:use-module (geesh environment)
  #:use-module (ice-9 match))

;;; Commentary:
;;;
;;; The 'export' utility.
;;;
;;; Code:

(define (split-assignment assignment)
  (match (string-index assignment #\=)
    (#f (values assignment #f))
    (index (let ((name (substring assignment 0 index)))
             (match (substring assignment (1+ index))
               ((? string-null?) (values name #f))
               (value (values name value)))))))

(define (main env . args)
  (match args
    (("-p") (throw 'not-implemented "export -p"))
    (_ (for-each (lambda (assignment)
                   (call-with-values (lambda () (split-assignment assignment))
                     (lambda (name value)
                       (set-var-export! env name value))))
                 args)
       0)))
