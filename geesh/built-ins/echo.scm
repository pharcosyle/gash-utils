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

(define-module (geesh built-ins echo)
  #:export (echo))

;;; Commentary:
;;;
;;; The 'echo' utility.
;;;
;;; Code:

(define (echo env . args)
  (let* ((n? (and (pair? args) (string=? (car args) "-n")))
         (args (if n? (cdr args) args)))
    (display (string-join args " "))
    (unless n?
      (newline))
    0))
