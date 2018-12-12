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

(define-module (test-pattern)
  #:use-module (geesh pattern)
  #:use-module (srfi srfi-64)
  #:use-module (tests unit automake))

;;; Commentary:
;;;
;;; Tests for the pattern module.
;;;
;;; Code:

(test-begin "pattern")


;;; Basic matching

(test-assert "Matches single characters"
  (pattern-match? (parse-pattern "a") "a"))

(test-assert "Matches a sequence of characters"
  (pattern-match? (parse-pattern "abc") "abc"))

(test-assert "Matches an empty string"
  (pattern-match? (parse-pattern "") ""))

(test-assert "Fails on pattern too long"
  (not (pattern-match? (parse-pattern "ab") "a")))

(test-assert "Fails on pattern too short"
  (not (pattern-match? (parse-pattern "a") "ab")))


;;; Question marks

(test-assert "Matches a question mark with anything"
  (pattern-match? (parse-pattern "???") "abc"))

(test-assert "Does not match question mark with empty string"
  (not (pattern-match? (parse-pattern "?") "")))


;;; Bracket expressions

(test-assert "Matches with bracket expressions"
  (pattern-match? (parse-pattern "[abc][def][ghi]") "aei"))

(test-assert "Fails on non-matching bracket expressions"
  (not (pattern-match? (parse-pattern "[abc][def][ghi]") "aex")))

(test-assert "Matches unterminated bracket expression normally"
  (pattern-match? (parse-pattern "a[bc") "a[bc"))

(test-assert "Matches with bracket expressions with left brackets"
  (pattern-match? (parse-pattern "a[[]c") "a[c"))

(test-assert "Matches with bracket expressions with right brackets"
  (pattern-match? (parse-pattern "a[]]c") "a]c"))

(test-assert "Matches with bracket expressions starting with hyphen"
  (pattern-match? (parse-pattern "a[-b]c") "a-c"))

(test-assert "Matches with bracket expressions ending with hyphen"
  (pattern-match? (parse-pattern "a[b-]c") "a-c"))

(test-assert "Matches with bracket expressions containing bang"
  (pattern-match? (parse-pattern "a[b!]c") "a!c"))

(test-assert "Matches with negated bracket expressions"
  (pattern-match? (parse-pattern "a[!x]c") "abc"))

(test-assert "Fails on non-matching negated bracket expressions"
  (not (pattern-match? (parse-pattern "a[!x]c") "axc")))

;; We do not fully support the following features.  However, rather
;; than do something strange, we raise explicit errors when they are
;; encountered.

(test-error "Does not allow collating symbols"
  (parse-pattern "[[.ch.]]"))

(test-assert "Allows unterminated collating symbols"
  (parse-pattern "[[.ch]"))

(test-error "Does not allow equivalence classes"
  (parse-pattern "[[=a=]]"))

(test-assert "Allows unterminated equivalence classes"
  (parse-pattern "[[=a]"))

(test-error "Does not allow character classes"
  (parse-pattern "[[:space:]]"))

(test-assert "Allows unterminated character classes"
  (parse-pattern "[[:space]"))

(test-error "Does not allow general character ranges"
  (parse-pattern "[<->]"))

(test-assert "Allows [a-z]"
  (parse-pattern "[a-z]"))

(test-assert "Allows [A-Z]"
  (parse-pattern "[A-Z]"))

(test-assert "Allows [0-9]"
  (parse-pattern "[0-9]"))

(test-assert "Matches with allowed character ranges"
  (pattern-match? (parse-pattern "[a-z][A-Z][0-9]") "mJ2"))


;;; Asterisks

(test-assert "Matches with asterisk"
  (pattern-match? (parse-pattern "*") "abc"))

(test-assert "Matches empty string with asterisk"
  (pattern-match? (parse-pattern "*") ""))

(test-assert "Matches with trailing asterisk"
  (pattern-match? (parse-pattern "foo*") "foobar"))

(test-assert "Fails on non-matching trailing asterisk"
  (not (pattern-match? (parse-pattern "foo*") "goobar")))

(test-assert "Matches with leading asterisk"
  (pattern-match? (parse-pattern "*bar") "foobar"))

(test-assert "Fails on non-matching leading asterisk"
  (not (pattern-match? (parse-pattern "*bar") "foobaz")))

(test-assert "Fails on non-matching leading asterisk (internal match)"
  (not (pattern-match? (parse-pattern "*bar") "foobarbaz")))

(test-assert "Matches with internal asterisk"
  (pattern-match? (parse-pattern "foo*bar") "foo boo! bar"))

(test-assert "Fails on non-matching internal asterisk (start)"
  (not (pattern-match? (parse-pattern "foo*bar") "goo boo! bar")))

(test-assert "Fails on non-matching internal asterisk (end)"
  (not (pattern-match? (parse-pattern "foo*bar") "foo boo! baz")))


;;; Quoting

;; TODO: Test quoting.

(test-end)

;; Local Variables:
;; eval: (put 'test-error 'scheme-indent-function 1)
;; End:
