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

(define-module (test-word)
  #:use-module (geesh environment)
  #:use-module (geesh word)
  #:use-module (srfi srfi-64)
  #:use-module (tests automake))

;;; Commentary:
;;;
;;; Tests for the word module.
;;;
;;; Code:

;; This function exists to add a layer of slippage between the
;; "environment" module and our tests.  The "environment" module is
;; still under development, and it would be annoying to have to
;; rewrite all the tests.
(define* (make-test-env vars #:key (noglob? #f) (nounset? #f))
  "Create a testing environment with the alist @var{vars} as the
current variables.  If @var{noglob?} is set, enable the `noglob'
option.  If @var{nounset?} is set, enable the `nounset' option.  (See
the `set' built-in for details on these options.)"
  (make-environment vars))

(test-begin "word")


;;; Basic string handling.

(test-equal "Converts a simple word (string) to a single field"
  '("foo")
  (expand-word #f "foo"))

(test-equal "Converts a simple word (list) to a single field"
  '("foo")
  (expand-word #f '("foo")))

(test-equal "Concatenates contiguous parts into a single field"
  '("foobar")
  (expand-word #f '("foo" "bar")))

(test-equal "Splits a word along unquoted spaces"
  '("foo" "bar")
  (expand-word #f '("foo bar")))

(test-equal "Splits a word on leading space"
  '("foo" "bar")
  (expand-word #f '("foo" " bar")))

(test-equal "Splits a word on trailing space"
  '("foo" "bar")
  (expand-word #f '("foo " "bar")))

(test-equal "Ignores leading spaces"
  '("foo")
  (expand-word #f '(" foo")))

(test-equal "Ignores trailing spaces"
  '("foo")
  (expand-word #f '("foo ")))

(test-equal "Treats multiple spaces as a single space"
  '("foo" "bar")
  (expand-word #f '("foo  bar")))

(test-equal "Handles multiple joins and splits"
  '("hi_how" "are_you")
  (expand-word #f '("hi_" "how are" "_you")))


;;; Quotes.

(test-equal "Ignores spaces in quotes"
  '("foo bar")
  (expand-word #f '(<sh-quote> "foo bar")))

(test-equal "Concatenates strings and quotes"
  '("foo bar")
  (expand-word #f '("foo" (<sh-quote> " bar"))))

(test-equal "Concatenates quotes"
  '("foo bar")
  (expand-word #f '((<sh-quote> "foo") (<sh-quote> " bar"))))

(test-equal "Handles nested quotes"
  '("foo bar")
  (expand-word #f '(<sh-quote> (<sh-quote> "foo bar"))))

(test-equal "Splits and concatenates words and quotes"
  '("foo" "bar")
  (expand-word #f '((<sh-quote> "foo") " " (<sh-quote> "bar"))))


;;; Tildes.
;;;
;;; Not yet implemented.


;;; Basic parameter references.
;;;
;;; FIXME: Test "nounset" ("set -u").

(test-equal "Resolves parameters"
  '("foo")
  (expand-word (make-test-env '(("x" . "foo")))
               '(<sh-ref> "x")))

(test-equal "Splits parameter results"
  '("foo" "bar")
  (expand-word (make-test-env '(("x" . "foo bar")))
               '(<sh-ref> "x")))

(test-equal "Resolves quoted parameters"
  '("foo")
  (expand-word (make-test-env '(("x" . "foo")))
               '(<sh-quote> (<sh-ref> "x"))))

(test-equal "Ignores spaces in quoted parameters"
  '("foo bar")
  (expand-word (make-test-env '(("x" . "foo bar")))
               '(<sh-quote> (<sh-ref> "x"))))

(test-equal "Treats unset variables as blank"
  '("")
  (expand-word (make-test-env '())
               '(<sh-ref> "x")))


;;; Parameter operations.

;;; or

(test-equal "Handles 'or' when parameter is set"
  '("foo")
  (expand-word (make-test-env '(("x" . "foo")))
               '(<sh-ref-or> "x" "bar")))

(test-equal "Handles 'or' when parameter is set and empty"
  '("")
  (expand-word (make-test-env '(("x" . "")))
               '(<sh-ref-or> "x" "bar")))

(test-equal "Handles 'or' when parameter is unset"
  '("bar")
  (expand-word (make-test-env '())
               '(<sh-ref-or> "x" "bar")))

(test-equal "Handles 'or' fall-through without default"
  '("")
  (expand-word (make-test-env '())
               '(<sh-ref-or> "x" #f)))

;;; or*

(test-equal "Handles 'or*' when parameter is set"
  '("foo")
  (expand-word (make-test-env '(("x" . "foo")))
               '(<sh-ref-or*> "x" "bar")))

(test-equal "Handles 'or*' when parameter is set and empty"
  '("bar")
  (expand-word (make-test-env '(("x" . "")))
               '(<sh-ref-or*> "x" "bar")))

(test-equal "Handles 'or*' when parameter is unset"
  '("bar")
  (expand-word (make-test-env '())
               '(<sh-ref-or*> "x" "bar")))

(test-equal "Handles 'or*' fall-through without default"
  '("")
  (expand-word (make-test-env '())
               '(<sh-ref-or*> "x" #f)))

;;; or!

(test-equal "Handles 'or!' when parameter is set"
  '(("foo") "foo")
  (let ((env (make-test-env '(("x" . "foo")))))
    (list (expand-word env '(<sh-ref-or!> "x" "bar"))
          (var-ref env "x"))))

(test-equal "Handles 'or!' when parameter is set and empty"
  '(("") "")
  (let ((env (make-test-env '(("x" . "")))))
    (list (expand-word env '(<sh-ref-or!> "x" "bar"))
          (var-ref env "x"))))

(test-equal "Handles 'or!' when parameter is unset"
  '(("bar") "bar")
  (let ((env (make-test-env '())))
    (list (expand-word env '(<sh-ref-or!> "x" "bar"))
          (var-ref env "x"))))

(test-equal "Handles 'or!' fall-through without default"
  '(("") "")
  (let ((env (make-test-env '())))
    (list (expand-word env '(<sh-ref-or!> "x" #f))
          (var-ref env "x"))))

;;; or!*

(test-equal "Handles 'or!*' when parameter is set"
  '(("foo") "foo")
  (let ((env (make-test-env '(("x" . "foo")))))
    (list (expand-word env '(<sh-ref-or!*> "x" "bar"))
          (var-ref env "x"))))

(test-equal "Handles 'or!*' when parameter is set and empty"
  '(("bar") "bar")
  (let ((env (make-test-env '(("x" . "")))))
    (list (expand-word env '(<sh-ref-or!*> "x" "bar"))
          (var-ref env "x"))))

(test-equal "Handles 'or!*' when parameter is unset"
  '(("bar") "bar")
  (let ((env (make-test-env '())))
    (list (expand-word env '(<sh-ref-or!*> "x" "bar"))
          (var-ref env "x"))))

(test-equal "Handles 'or!*' fall-through without default"
  '(("") "")
  (let ((env (make-test-env '())))
    (list (expand-word env '(<sh-ref-or!*> "x" #f))
          (var-ref env "x"))))

(test-equal "Does not split fields on assignment"
  '(("foo" "bar") "foo bar")
  (let ((env (make-test-env '(("y" . "foo bar")))))
    (list (expand-word env '(<sh-ref-or!*> "x" (<sh-ref> "y")))
          (var-ref env "x"))))

;;; FIXME: Test 'assert'.

;;; and

(test-equal "Handles 'and' when parameter is set"
  '("bar")
  (expand-word (make-test-env '(("x" . "foo")))
               '(<sh-ref-and> "x" "bar")))

(test-equal "Handles 'and' when parameter is set and empty"
  '("")
  (expand-word (make-test-env '(("x" . "")))
               '(<sh-ref-and> "x" "bar")))

(test-equal "Handles 'and' when parameter is unset"
  '("")
  (expand-word (make-test-env '())
               '(<sh-ref-and> "x" "bar")))

(test-equal "Handles 'and' fall-through without default"
  '("")
  (expand-word (make-test-env '(("x" . "foo")))
               '(<sh-ref-and> "x" #f)))

;;; and*

(test-equal "Handles 'and*' when parameter is set"
  '("bar")
  (expand-word (make-test-env '(("x" . "foo")))
               '(<sh-ref-and*> "x" "bar")))

(test-equal "Handles 'and*' when parameter is set and empty"
  '("bar")
  (expand-word (make-test-env '(("x" . "")))
               '(<sh-ref-and*> "x" "bar")))

(test-equal "Handles 'and*' when parameter is unset"
  '("")
  (expand-word (make-test-env '())
               '(<sh-ref-and*> "x" "bar")))

(test-equal "Handles 'and*' fall-through without default"
  '("")
  (expand-word (make-test-env '(("x" . "foo")))
               '(<sh-ref-and*> "x" #f)))

;;; length

(test-equal "Handles 'length' when parameter is set"
  '("3")
  (expand-word (make-test-env '(("x" . "foo")))
               '(<sh-ref-length> "x")))

(test-equal "Handles 'length' when parameter is unset"
  '("0")
  (expand-word (make-test-env '())
               '(<sh-ref-length> "x")))


;;; Command substition.

(test-equal "Resolves commands"
  '("foo")
  (parameterize ((eval-cmd-sub identity))
    (expand-word #f '(<sh-cmd-sub> "foo"))))

(test-equal "Splits command results"
  '("foo" "bar")
  (parameterize ((eval-cmd-sub identity))
    (expand-word #f '(<sh-cmd-sub> "foo bar"))))

(test-equal "Resolves quoted commands"
  '("foo")
  (parameterize ((eval-cmd-sub identity))
    (expand-word #f '(<sh-quote> (<sh-cmd-sub> "foo")))))

(test-equal "Ignores spaces in quoted commands"
  '("foo bar")
  (parameterize ((eval-cmd-sub identity))
    (expand-word #f '(<sh-quote> (<sh-cmd-sub> "foo bar")))))


;;; Arithmetic expansion.
;;;
;;; Not yet implemented.


;;; Pattern expansion.
;;;
;;; Not yet implemented.


;;; Field splitting (IFS)
;;;
;;; FIXME: Test that field splitting respects the IFS variable.

(test-end)
