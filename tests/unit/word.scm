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
  #:use-module (tests unit automake))

;;; Commentary:
;;;
;;; Tests for the word module.
;;;
;;; Code:

(test-begin "word")


;;; Basic string handling.

(test-equal "Converts a simple word (string) to a single field"
  '("foo")
  (expand-word "foo"))

(test-equal "Converts a simple word (list) to a single field"
  '("foo")
  (expand-word '("foo")))

(test-equal "Concatenates contiguous parts into a single field"
  '("foobar")
  (expand-word '("foo" "bar")))

(test-equal "Splits a word along unquoted spaces"
  '("foo" "bar")
  (expand-word '("foo bar")))

(test-equal "Splits a word on leading space"
  '("foo" "bar")
  (expand-word '("foo" " bar")))

(test-equal "Splits a word on trailing space"
  '("foo" "bar")
  (expand-word '("foo " "bar")))

(test-equal "Ignores leading spaces"
  '("foo")
  (expand-word '(" foo")))

(test-equal "Ignores trailing spaces"
  '("foo")
  (expand-word '("foo ")))

(test-equal "Treats multiple spaces as a single space"
  '("foo" "bar")
  (expand-word '("foo  bar")))

(test-equal "Handles multiple joins and splits"
  '("hi_how" "are_you")
  (expand-word '("hi_" "how are" "_you")))

(test-equal "Handles nested lists"
  '("foo")
  (expand-word '("f" ("oo"))))


;;; Quotes.

(test-equal "Ignores spaces in quotes"
  '("foo bar")
  (expand-word '(<sh-quote> "foo bar")))

(test-equal "Concatenates strings and quotes"
  '("foo bar")
  (expand-word '("foo" (<sh-quote> " bar"))))

(test-equal "Concatenates quotes"
  '("foo bar")
  (expand-word '((<sh-quote> "foo") (<sh-quote> " bar"))))

(test-equal "Handles nested quotes"
  '("foo bar")
  (expand-word '(<sh-quote> (<sh-quote> "foo bar"))))

(test-equal "Splits and concatenates words and quotes"
  '("foo" "bar")
  (expand-word '((<sh-quote> "foo") " " (<sh-quote> "bar"))))


;;; Tildes.
;;;
;;; Not yet implemented.


;;; Basic parameter references.
;;;
;;; FIXME: Test "nounset" ("set -u").

(test-equal "Resolves parameters"
  '("foo")
  (with-variables '(("x" . "foo"))
    (lambda ()
      (expand-word '(<sh-ref> "x")))))

(test-equal "Splits parameter results"
  '("foo" "bar")
  (with-variables '(("x" . "foo bar"))
    (lambda ()
      (expand-word '(<sh-ref> "x")))))

(test-equal "Resolves quoted parameters"
  '("foo")
  (with-variables '(("x" . "foo"))
    (lambda ()
      (expand-word '(<sh-quote> (<sh-ref> "x"))))))

(test-equal "Ignores spaces in quoted parameters"
  '("foo bar")
  (with-variables '(("x" . "foo bar"))
    (lambda ()
      (expand-word '(<sh-quote> (<sh-ref> "x"))))))

(test-equal "Treats empty variables as nothing"
  '()
  (with-variables '(("x" . ""))
    (lambda ()
      (expand-word '(<sh-ref> "x")))))

(test-equal "Treats unset variables as nothing"
  '()
  (with-variables '()
    (lambda ()
      (expand-word '(<sh-ref> "x")))))

(test-equal "Preserves empty variables when quoted"
  '("")
  (with-variables '(("x" . ""))
    (lambda ()
      (expand-word '(<sh-quote> (<sh-ref> "x"))))))

(test-equal "Preserves unset variables when quoted"
  '("")
  (with-variables '()
    (lambda ()
      (expand-word '(<sh-quote> (<sh-ref> "x"))))))


;;; Parameter operations.

;;; or

(test-equal "Handles 'or' when parameter is set"
  '("foo")
  (with-variables '(("x" . "foo"))
    (lambda ()
      (expand-word '(<sh-ref-or> "x" "bar")))))

(test-equal "Handles 'or' when parameter is set and empty"
  '()
  (with-variables '(("x" . ""))
    (lambda ()
      (expand-word '(<sh-ref-or> "x" "bar")))))

(test-equal "Handles 'or' when parameter is unset"
  '("bar")
  (with-variables '()
    (lambda ()
      (expand-word '(<sh-ref-or> "x" "bar")))))

(test-equal "Handles 'or' fall-through without default"
  '()
  (with-variables '()
    (lambda ()
      (expand-word '(<sh-ref-or> "x" #f)))))

;;; or*

(test-equal "Handles 'or*' when parameter is set"
  '("foo")
  (with-variables '(("x" . "foo"))
    (lambda ()
      (expand-word '(<sh-ref-or*> "x" "bar")))))

(test-equal "Handles 'or*' when parameter is set and empty"
  '("bar")
  (with-variables '(("x" . ""))
    (lambda ()
      (expand-word '(<sh-ref-or*> "x" "bar")))))

(test-equal "Handles 'or*' when parameter is unset"
  '("bar")
  (with-variables '()
    (lambda ()
      (expand-word '(<sh-ref-or*> "x" "bar")))))

(test-equal "Handles 'or*' fall-through without default"
  '()
  (with-variables '()
    (lambda ()
      (expand-word '(<sh-ref-or*> "x" #f)))))

;;; or!

(test-equal "Handles 'or!' when parameter is set"
  '(("foo") "foo")
  (with-variables '(("x" . "foo"))
    (lambda ()
      (list (expand-word '(<sh-ref-or!> "x" "bar"))
            (getvar "x")))))

(test-equal "Handles 'or!' when parameter is set and empty"
  '(() "")
  (with-variables '(("x" . ""))
    (lambda ()
      (list (expand-word '(<sh-ref-or!> "x" "bar"))
            (getvar "x")))))

(test-equal "Handles 'or!' when parameter is unset"
  '(("bar") "bar")
  (with-variables '()
    (lambda ()
      (list (expand-word '(<sh-ref-or!> "x" "bar"))
            (getvar "x")))))

(test-equal "Handles 'or!' fall-through without default"
  '(() "")
  (with-variables '()
    (lambda ()
      (list (expand-word '(<sh-ref-or!> "x" #f))
            (getvar "x")))))

;;; or!*

(test-equal "Handles 'or!*' when parameter is set"
  '(("foo") "foo")
  (with-variables '(("x" . "foo"))
    (lambda ()
      (list (expand-word '(<sh-ref-or!*> "x" "bar"))
            (getvar "x")))))

(test-equal "Handles 'or!*' when parameter is set and empty"
  '(("bar") "bar")
  (with-variables '(("x" . ""))
    (lambda ()
      (list (expand-word '(<sh-ref-or!*> "x" "bar"))
            (getvar "x")))))

(test-equal "Handles 'or!*' when parameter is unset"
  '(("bar") "bar")
  (with-variables '()
    (lambda ()
      (list (expand-word '(<sh-ref-or!*> "x" "bar"))
            (getvar "x")))))

(test-equal "Handles 'or!*' fall-through without default"
  '(() "")
  (with-variables '()
    (lambda ()
      (list (expand-word '(<sh-ref-or!*> "x" #f))
            (getvar "x")))))

(test-equal "Does not split fields on assignment"
  '(("foo" "bar") "foo bar")
  (with-variables '(("y" . "foo bar"))
    (lambda ()
      (list (expand-word '(<sh-ref-or!*> "x" (<sh-ref> "y")))
            (getvar "x")))))

;;; FIXME: Test 'assert'.

;;; and

(test-equal "Handles 'and' when parameter is set"
  '("bar")
  (with-variables '(("x" . "foo"))
    (lambda ()
      (expand-word '(<sh-ref-and> "x" "bar")))))

(test-equal "Handles 'and' when parameter is set and empty"
  '()
  (with-variables '(("x" . ""))
    (lambda ()
      (expand-word '(<sh-ref-and> "x" "bar")))))

(test-equal "Handles 'and' when parameter is unset"
  '()
  (with-variables '()
    (lambda ()
      (expand-word '(<sh-ref-and> "x" "bar")))))

(test-equal "Handles 'and' fall-through without default"
  '()
  (with-variables '(("x" . "foo"))
    (lambda ()
      (expand-word '(<sh-ref-and> "x" #f)))))

;;; and*

(test-equal "Handles 'and*' when parameter is set"
  '("bar")
  (with-variables '(("x" . "foo"))
    (lambda ()
      (expand-word '(<sh-ref-and*> "x" "bar")))))

(test-equal "Handles 'and*' when parameter is set and empty"
  '("bar")
  (with-variables '(("x" . ""))
    (lambda ()
      (expand-word '(<sh-ref-and*> "x" "bar")))))

(test-equal "Handles 'and*' when parameter is unset"
  '()
  (with-variables '()
    (lambda ()
      (expand-word '(<sh-ref-and*> "x" "bar")))))

(test-equal "Handles 'and*' fall-through without default"
  '()
  (with-variables '(("x" . "foo"))
    (lambda ()
      (expand-word '(<sh-ref-and*> "x" #f)))))

;;; length

(test-equal "Handles 'length' when parameter is set"
  '("3")
  (with-variables '(("x" . "foo"))
    (lambda ()
      (expand-word '(<sh-ref-length> "x")))))

(test-equal "Handles 'length' when parameter is unset"
  '("0")
  (with-variables '()
    (lambda ()
      (expand-word '(<sh-ref-length> "x")))))


;;; Command substition.

(test-equal "Resolves commands"
  '("foo")
  (parameterize ((eval-cmd-sub identity))
    (expand-word '(<sh-cmd-sub> "foo"))))

(test-equal "Splits command results"
  '("foo" "bar")
  (parameterize ((eval-cmd-sub identity))
    (expand-word '(<sh-cmd-sub> "foo bar"))))

(test-equal "Resolves quoted commands"
  '("foo")
  (parameterize ((eval-cmd-sub identity))
    (expand-word '(<sh-quote> (<sh-cmd-sub> "foo")))))

(test-equal "Ignores spaces in quoted commands"
  '("foo bar")
  (parameterize ((eval-cmd-sub identity))
    (expand-word '(<sh-quote> (<sh-cmd-sub> "foo bar")))))


;;; Arithmetic expansion.
;;;
;;; Not yet implemented.


;;; Pattern expansion.
;;;
;;; Not yet implemented.


;;; Field splitting (IFS)

(test-equal "Respects IFS value"
  '("foo" "bar")
  (with-variables '(("IFS" . "-"))
    (lambda ()
      (expand-word '("foo-bar")))))

(test-equal "Combines multiple whitespace separators"
  '("foo" "bar")
  (with-variables '(("IFS" . " "))
    (lambda ()
      (expand-word '("foo  bar")))))

(test-equal "Keeps multiple non-whitespace separators"
  '("foo" "" "bar")
  (with-variables '(("IFS" . "-"))
    (lambda ()
      (expand-word '("foo--bar")))))

(test-equal "Combines whitespace separators with a non-whitespace separator"
  '("foo" "bar")
  (with-variables '(("IFS" . "- "))
    (lambda ()
      (expand-word '("foo - bar")))))

(test-equal "Keeps multiple non-whitespace separators with whitespace"
  '("foo" "" "bar")
  (with-variables '(("IFS" . "- "))
    (lambda ()
      (expand-word '("foo - - bar")))))

(test-equal "Splits on leading non-whitespace separator"
  '("" "foo")
  (with-variables '(("IFS" . "-"))
    (lambda ()
      (expand-word '("-foo")))))

(test-equal "Does not split on trailing non-whitespace separator"
  '("foo")
  (with-variables '(("IFS" . "-"))
    (lambda ()
      (expand-word '("foo-")))))

(test-equal "Makes one field for single non-whitespace separator"
  '("")
  (with-variables '(("IFS" . "-"))
    (lambda ()
      (expand-word '("-")))))

(test-end)
