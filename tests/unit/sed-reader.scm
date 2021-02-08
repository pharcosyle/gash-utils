;;; Gash-Utils
;;; Copyright © 2020, 2021 Timothy Sample <samplet@ngyro.com>
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
;;; along with Gash-Utils.  If not, see <http://www.gnu.org/licenses/>.

(define-module (test-sed-reader)
  #:use-module (gash commands sed reader)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64)
  #:use-module (tests unit automake))

;;; Commentary:
;;;
;;; Tests for the sed reader module.
;;;
;;; Code:

(define (parse str)
  (call-with-input-string str read-sed))

(test-begin "sed-reader")

(test-equal "Parses regexes with named character classes"
  '(always s "[[:space:]]" "" (g))
  (parse "s/[[:space:]]//g"))

(test-equal "Parses addresses starting with backslash"
  '((at "^foo") p)
  (parse "\\$^foo$ p"))

(test-equal "Allows escaping address delimiter"
  '((at "abc\\xdef") p)
  (parse "\\xabc\\xdefx p"))

(test-end "sed-reader")
