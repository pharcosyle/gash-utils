;;; Gash -- Guile As SHell
;;; Copyright © 2018 Timothy Sample <samplet@ngyro.com>
;;;
;;; This file is part of Gash.
;;;
;;; Gash is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Gash is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Gash.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gash commands pwd)
  #:use-module (gash environment)
  #:use-module (ice-9 match)
  #:export (pwd))

;;; Commentary:
;;;
;;; The 'pwd' utility.
;;;
;;; Code:

(define (pwd . args)
  (let loop ((args args) (logical? #t))
    (match args
      (("-P" . tail) (loop tail #f))
      (("-L" . tail) (loop tail #t))
      (()
       (display (if logical?
                    (getvar "PWD")
                    (getcwd)))
       (newline)
       0))))

(define (main . args)
  (exit (apply pwd (cdr args))))
