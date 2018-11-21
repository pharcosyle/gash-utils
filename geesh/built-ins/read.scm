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

(define-module (geesh built-ins read)
  #:use-module (geesh environment)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim))

;;; Commentary:
;;;
;;; The 'read' utility.
;;;
;;; Code:

(define (main env . args)
  (match (read-line (current-input-port))
    ((? eof-object?) 1)
    (str (set-var! env (car args) str)
         0)))
