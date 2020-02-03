;;; Gash-Utils
;;; Copyright © 2016,2017,2018 R.E.W. van Beusekom <rutger.van.beusekom@gmail.com>
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gash io)

  #:use-module (srfi srfi-1)
  #:export (pke stdout stderr))

(define (output port o)
  (map (lambda (o) (display o port)) o)
  (newline port)
  (force-output port))

(define (stdout . o)
  (output (current-output-port) o)
  (last o))

(define (stderr . o)
  (output (current-error-port) o)
  (last o))

(define (pke . stuff)
  (newline (current-error-port))
  (display ";;; " (current-error-port))
  (write stuff (current-error-port))
  (newline (current-error-port))
  (car (last-pair stuff)))
