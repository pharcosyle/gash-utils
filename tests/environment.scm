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

(define-module (test-environment)
  #:use-module (geesh environment)
  #:use-module (srfi srfi-64)
  #:use-module (tests automake))

;;; Commentary:
;;;
;;; Tests for the environment module.
;;;
;;; Code:

(test-begin "environment")

;;;
;;; Variables.
;;;

(test-equal "Stores existing variables"
  "bar"
  (let ((env (make-environment '(("FOO" . "bar")))))
    (var-ref env "FOO")))

(test-equal "Stores new variables"
  "bar"
  (let ((env (make-environment '())))
    (set-var! env "FOO" "bar")
    (var-ref env "FOO")))

(test-equal "Updates variables"
  "baz"
  (let ((env (make-environment '(("FOO" . "bar")))))
    (set-var! env "FOO" "baz")
    (var-ref env "FOO")))

(test-equal "Returns '#f' for unset variables"
  #f
  (let ((env (make-environment '())))
    (var-ref env "FOO")))

(test-end)
