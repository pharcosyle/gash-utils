;;; The Geesh Shell Interpreter
;;; Copyright 2017, 2018 Timothy Sample <samplet@ngyro.com>
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

(define-module (test-lexer)
  #:use-module (geesh lexer)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64)
  #:use-module (system base lalr)
  #:use-module (tests automake))

;;; Commentary:
;;;
;;; Tests for the lexer module.
;;;
;;; Code:

(define (tokenize str)
  "Covert the string @var{str} into a list of tokens."
  (define (tokenize-port port)
    (let loop ((token (get-token port)) (acc '()))
      (match token
        ('*eoi* (reverse! acc))
        ((? lexical-token?)
         (let* ((category  (lexical-token-category token))
                (source    (lexical-token-source token))
                (value     (lexical-token-value token))
                (offset    (source-location-offset source))
                (length    (source-location-length source)))
           (loop (get-token port)
                 (cons `(,category (,offset . ,length) ,value) acc)))))))
  (call-with-input-string str tokenize-port))

(test-begin "lexer")

;;;
;;; Basic words and operators.
;;;

(test-equal "Lexes one word"
  '((NAME (0 . 3) "foo"))
  (tokenize "foo"))

(test-equal "Ignores escaped newlines (line-joining)"
  '((NAME (0 . 8) "foobar"))
  (tokenize "foo\\\nbar"))

(test-equal "Handles spaces around escaped newlines"
  '((NAME (0 . 3) "foo")
    (NAME (7 . 3) "bar"))
  (tokenize "foo \\\n bar"))

(test-equal "Splits tokens on a space"
  '((NAME (0 . 3) "foo")
    (NAME (4 . 3) "bar"))
  (tokenize "foo bar"))

(test-equal "Splits tokens on a newline"
  '((NAME (0 . 3) "foo")
    (NEWLINE (3 . 1) #\newline)
    (NAME (4 . 3) "bar"))
  (tokenize "foo\nbar"))

(test-equal "Splits tokens on an operator"
  '((NAME (0 . 3) "foo")
    (PIPE (3 . 1) "|")
    (NAME (4 . 3) "bar"))
  (tokenize "foo|bar"))

(test-equal "Recognizes a reserved word"
  '((While (0 . 5) "while"))
  (tokenize "while"))

(test-equal "Recognizes a simple assignment"
  '((ASSIGNMENT-WORD (0 . 7) "foo=bar"))
  (tokenize "foo=bar"))

(test-equal "Recognizes a complex assignment"
  '((ASSIGNMENT-WORD (0 . 11) ("foo=bar" (<sh-ref> "baz"))))
  (tokenize "foo=bar$baz"))

;;;
;;; Comments.
;;;

(test-equal "Ignores comments followed by a newline"
  '((NAME (0 . 3) "foo")
    (NEWLINE (8 . 1) #\newline)
    (NAME (9 . 3) "bar"))
  (tokenize "foo #baz\nbar"))

(test-equal "Ignores comments followed by end-of-input"
  '((NAME (0 . 3) "foo"))
  (tokenize "foo #baz"))

;;;
;;; IO numbers.
;;;

(test-equal "Recogizes an output IO number"
  '((NAME (0 . 3) "foo")
    (IO-NUMBER (4 . 1) "2")
    (GREAT (5 . 1) ">")
    (WORD (6 . 9) "/dev/null"))
  (tokenize "foo 2>/dev/null"))

(test-equal "Recogizes an input IO number"
  '((NAME (0 . 3) "foo")
    (IO-NUMBER (4 . 1) "0")
    (LESS (5 . 1) "<")
    (WORD (6 . 12) "/dev/urandom"))
  (tokenize "foo 0</dev/urandom"))

;;;
;;; Parameter expansions
;;;

(test-equal "Lexes a simple parameter expansion"
  '((WORD (0 . 4) (<sh-ref> "foo")))
  (tokenize "$foo"))

(test-equal "Delimits unbraced, interspersed parameter names"
  '((WORD (0 . 12) ("foo-" (<sh-ref> "baz") "-bar")))
  (tokenize "foo-$baz-bar"))

(test-equal "Delimits braced, interspersed parameter names"
  '((WORD (0 . 12) ("foo" (<sh-ref> "baz") "bar")))
  (tokenize "foo${baz}bar"))

(test-equal "Recognizes the \"length\" parameter operator"
  '((WORD (0 . 7) (<sh-ref-length> "foo")))
  (tokenize "${#foo}"))

(for-each
 (match-lambda
   ((operator . symbol)
    (test-equal (string-append "Recognizes a parameter expansion "
                               "with the \"" operator "\" operator")

      `((WORD (0 . ,(+ (string-length operator) 9)) (,symbol "foo" "bar")))
      (tokenize (string-append "${foo" operator "bar}")))))
 '(("-" . <sh-ref-or>)
   (":-" . <sh-ref-or*>)
   ("=" . <sh-ref-or!>)
   (":=" . <sh-ref-or!*>)
   ("?" . <sh-ref-assert>)
   (":?" . <sh-ref-assert*>)
   ("+" . <sh-ref-and>)
   (":+" . <sh-ref-and*>)
   ("%" . <sh-ref-except-min>)
   ("%%" . <sh-ref-except-max>)
   ("#" . <sh-ref-skip-min>)
   ("##" . <sh-ref-skip-max>)))

(test-equal "Recognizes a parameter expansion operator without default"
  '((WORD (0 . 7) (<sh-ref-or> "foo" #f)))
  (tokenize "${foo-}"))

(test-equal "Splits multidigit parameter name without braces"
  '((WORD (0 . 3) ((<sh-ref> "1") "2")))
  (tokenize "$12"))

(test-equal "Preserves multidigit parameter name with braces"
  '((WORD (0 . 5) (<sh-ref> "12")))
  (tokenize "${12}"))

(for-each
 (lambda (special)
   (test-equal (string-append "Recognizes the \"" special "\" parameter")
     `((WORD (0 . 2) (<sh-ref> ,special)))
     (tokenize (string-append "$" special))))
 '("@" "*" "#" "?" "-" "$" "!" "0"))

(test-equal "Allows brace-nesting in parameter expansions"
  '((WORD (0 . 11) (<sh-ref-or> "foo" "b{}r")))
  (tokenize "${foo-b{}r}"))

(test-equal "Respects escapes in parameter expansions"
  '((WORD (0 . 11) (<sh-ref-or> "foo" ("b" (<sh-quote> "}") "r"))))
  (tokenize "${foo-b\\}r}"))

(test-equal "Respects single quotations in parameter expressions"
  '((WORD (0 . 12) (<sh-ref-or> "foo" ("b" (<sh-quote> "}") "r"))))
  (tokenize "${foo-b'}'r}"))

(test-equal "Respects double quotations in parameter expressions"
  '((WORD (0 . 12) (<sh-ref-or> "foo" ("b" (<sh-quote> "}") "r"))))
  (tokenize "${foo-b\"}\"r}"))

(test-equal "Recognizes nested parameter expansions"
  '((WORD (0 . 13) (<sh-ref-or> "foo" (<sh-ref> "bar"))))
  (tokenize "${foo-${bar}}"))

;;;
;;; Single quotations.
;;;

(test-equal "Lexes a single quotation"
  '((WORD (0 . 5) (<sh-quote> "foo")))
  (tokenize "'foo'"))

(test-equal "Lexes a single quotation in a word"
  '((WORD (0 . 5) ("f" (<sh-quote> "o") "o")))
  (tokenize "f'o'o"))

(test-equal "Ignores special characters in a single quotation"
  '((WORD (0 . 12) (<sh-quote> "foo\n#\\`$<\"")))
  (tokenize "'foo\n#\\`$<\"'"))

;;;
;;; Double quotations.
;;;

(test-equal "Recognizes a double quotation"
  '((WORD (0 . 5) (<sh-quote> "foo")))
  (tokenize "\"foo\""))

(test-equal "Recognizes a double quotation in a word"
  '((WORD (0 . 5) ("f" (<sh-quote> "o") "o")))
  (tokenize "f\"o\"o"))

(test-equal "Ignores special characters in double quotations"
  '((WORD (0 . 9) (<sh-quote> "foo\n#<'")))
  (tokenize "\"foo\n#<'\""))

(test-equal "Respects escapes for special characters in double quotations"
  '((WORD (0 . 10) (<sh-quote> "foo" (<sh-quote> "\"") "bar")))
  (tokenize "\"foo\\\"bar\""))

(test-equal "Ignores escapes for normal characters in double quotations"
  '((WORD (0 . 9) (<sh-quote> "foo\\bar")))
  (tokenize "\"foo\\bar\""))

(test-equal "Ignores escaped newlines (line-joining) in double quotations"
  '((WORD (0 . 10) (<sh-quote> "foobar")))
  (tokenize "\"foo\\\nbar\""))

(test-equal "Recognizes expansions in double quotations"
  '((WORD (0 . 6) (<sh-quote> (<sh-ref> "foo"))))
  (tokenize "\"$foo\""))

;;;
;;; Bracketed commands.
;;;

(test-equal "Recognizes a bracketed command substition"
  '((WORD (0 . 6) (<sh-cmd-sub> (<sh-cmd> "foo"))))
  (parameterize ((read-bracketed-command
                  (lambda (port)
                    (string-for-each (lambda _ (read-char port)) "foo")
                    '(<sh-cmd> "foo"))))
    (tokenize "$(foo)")))

;;;
;;; Backquoted-commands.
;;;

(test-equal "Recognizes a backquoted command substition"
  '((WORD (0 . 5) (<sh-cmd-sub> (<sh-cmd> "foo"))))
  (parameterize ((read-backquoted-command
                  (lambda (port)
                    (string-for-each (lambda _ (read-char port)) "foo")
                    '(<sh-cmd> "foo"))))
    (tokenize "`foo`")))

(test-end)
