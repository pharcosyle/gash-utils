;;; Gash-Utils
;;; Copyright © 2020 Timothy Sample <samplet@ngyro.com>
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

(define-module (test-awk-parser)
  #:use-module (gash commands awk parser)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64)
  #:use-module (tests unit automake))

;;; Commentary:
;;;
;;; Tests for the awk parser module.
;;;
;;; Code:

(define (parse str)
  ;; XXX: Currently the parser complains when there is no trailing
  ;; newline.  Remove this hack when that is fixed.
  (let ((str* (string-append str "\n")))
    (call-with-input-string str* read-awk)))

(define (parse* str)
  (match (parse (string-append "{ " str " }"))
    ((('<awk-action> exp)) exp)))

(test-begin "awk-parser")


;;; Statements

(test-equal "Parses a for loop"
  '(<awk-for> (<awk-name> "foo") 1 2 3)
  (parse* "for ( foo ; 1 ; 2 ) 3"))

(test-equal "Parses a for-in loop"
  '(<awk-for-in> "foo" "bar" 1)
  (parse* "for ( foo in bar ) 1"))


;;; Expressions

(test-equal "Parses a regex expression"
  '(<awk-regex> "abc")
  (parse* "/abc/"))

(test-equal "Parses division instead of regex"
  '(/ (/ 1 2) 3)
  (parse* "1 /2/ 3"))

(test-equal "Parses division-assignment instead of regex"
  '(/= (<awk-name> "a") (/= (<awk-name> "b") 1))
  (parse* "a /=b/= 1"))

(test-end)
