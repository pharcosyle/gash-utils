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
  #:use-module (srfi srfi-1)
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

;;;
;;; Making and reading 'environs'.
;;;

(define (subset? lst1 lst2)
  "Test if @var{lst1} is a subset of @var{lst2}."
  (every (lambda (x) (member x lst2)) lst1))

(define (set=? lst1 lst2)
  "Test if @var{lst1} is @code{equal?} to @var{lst2} without respect
to order."
  (and (subset? lst1 lst2)
       (subset? lst2 lst1)))

(test-equal "Creates environ from empty environment"
  '()
  (let ((env (make-environment '())))
    (environment->environ env)))

(test-assert "Creates environ from environment"
  (let* ((env (make-environment '(("FOO" . "abc")
                                  ("BAR" . "def"))))
         (environ (environment->environ env)))
    (set=? environ '("FOO=abc" "BAR=def"))))

(test-assert "Creates environ from empty environment and bindings"
  (let* ((env (make-environment '()))
         (bindings '(("FOO" . "abc")
                     ("BAR" . "def")))
         (environ (environment->environ env bindings)))
    (set=? environ '("FOO=abc" "BAR=def"))))

(test-assert "Creates environ from environment and bindings"
  (let* ((env (make-environment '(("FOO" . "abc")
                                  ("BAZ" . "ghi"))))
         (bindings '(("BAR" . "def")
                     ("QUUX" . "jkl")))
         (environ (environment->environ env bindings)))
    (set=? environ '("FOO=abc" "BAR=def" "BAZ=ghi" "QUUX=jkl"))))

(test-assert "Bindings override environment when creating an environ"
  (let* ((env (make-environment '(("FOO" . "abc")
                                  ("BAR" . "def"))))
         (bindings '(("FOO" . "ghi")))
         (environ (environment->environ env bindings)))
    (set=? environ '("FOO=ghi" "BAR=def"))))

(test-assert "Creates an alist from an environ"
  (let* ((environ '("FOO=abc" "BAR=def"))
         (alist (environ->alist environ)))
    (set=? alist '(("FOO" . "abc") ("BAR" . "def")))))

(test-end)
