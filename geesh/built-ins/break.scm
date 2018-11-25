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

(define-module (geesh built-ins break)
  #:use-module (geesh environment))

;;; Commentary:
;;;
;;; The 'break' utility.
;;;
;;; Code:

(define (main . args)
  (let* ((arg (and (pair? args) (car args)))
         (n (string->number (or arg "1"))))
    (if (and arg (or (not n) (not (exact-integer? n)) (< n 1)))
        1
        (begin
          ;; Since we do not return, we have to set the status here.
          (set-status! 0)
          (break (1- n))))))
