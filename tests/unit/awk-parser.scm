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
    (((#t exp)) exp)))

(test-begin "awk-parser")


;;; Constants and lvalues

(test-equal "Parses a number"
  42
  (parse* "42"))

(test-equal "Parses a string"
  "foobar"
  (parse* "\"foobar\""))

(test-equal "Parses a regex"
  '(re "abc")
  (parse* "/abc/"))

(test-equal "Parses a name"
  'x
  (parse* "x"))

(test-equal "Parses an array reference"
  '(array-ref 1 array)
  (parse* "array[1]"))

(test-equal "Parses a multi-dimensional array reference"
  '(array-ref (index x y) array)
  (parse* "array[x, y]"))

(test-equal "Parses a field reference"
  '($ 1)
  (parse* "$1"))


;;; Expressions

(test-equal "Parses post-increment"
  '(post-incr! x)
  (parse* "x++"))

(test-equal "Parses pre-increment"
  '(pre-incr! x)
  (parse* "++x"))

(test-equal "Parses post-decrement"
  '(post-decr! x)
  (parse* "x--"))

(test-equal "Parses pre-decrement"
  '(pre-decr! x)
  (parse* "--x"))

(test-error "Rejects post-incrementing constant"
  #t
  (parse* "1++"))

(test-equal "Parses unary plus"
  '(+ x)
  (parse* "+x"))

(test-equal "Parses addition"
  '(+ x 1)
  (parse* "x + 1"))

(test-equal "Parses unary minus"
  '(- x)
  (parse* "-x"))

(test-equal "Parses substraction"
  '(- x 1)
  (parse* "x - 1"))

(test-equal "Parses multiplication"
  '(* x 2)
  (parse* "x * 2"))

(test-equal "Parses division"
  '(/ x 2)
  (parse* "x / 2"))

(test-equal "Parses division instead of regex"
  '(/ (/ 1 2) 3)
  (parse* "1 /2/ 3"))

(test-equal "Parses modulus"
  '(modulo x 2)
  (parse* "x % 2"))

(test-equal "Parses exponentiation"
  '(expt x 2)
  (parse* "x ^ 2"))

(test-equal "Parses concatenation"
  '(string-append x "/bin")
  (parse* "x \"/bin\""))

(test-equal "Parses concatenation without whitespace"
  '(string-append ($ i) "/")
  (parse* "$i\"/\""))

(test-equal "Parses less-than"
  '(< x 5)
  (parse* "x < 5"))

(test-equal "Parses less-than-or-equal-to"
  '(<= 5 x)
  (parse* "5 <= x"))

(test-equal "Parses equality"
  '(equal? x y)
  (parse* "x == y"))

(test-equal "Parses inequality"
  '(not-equal? x y)
  (parse* "x != y"))

(test-equal "Parses greater-than"
  '(> x 5)
  (parse* "x > 5"))

(test-equal "Parses greater-than-or-equal-to"
  '(>= 5 x)
  (parse* "5 >= x"))

(test-equal "Parses regex match"
  '(string-match x (re "[ab]+"))
  (parse* "x ~ /[ab]+/"))

(test-equal "Parses regex non-match"
  '(not-string-match x (re "[ab]+"))
  (parse* "x !~ /[ab]+/"))

(test-equal "Parses array membership"
  '(array-member? "key" array)
  (parse* "\"key\" in array"))

(test-equal "Parses multi-dimensional array membership"
  '(array-member? (index x y) array)
  (parse* "(x, y) in array"))

(test-equal "Parses logical conjunction"
  '(and x y)
  (parse* "x && y"))

(test-equal "Allows newline after logical conjunction"
  '(and x y)
  (parse* "x &&\ny"))

(test-equal "Parses logical disjunction"
  '(or x y)
  (parse* "x || y"))

(test-equal "Allows newline after logical disjunction"
  '(or x y)
  (parse* "x ||\ny"))

(test-equal "Parses negation disjunction"
  '(not x)
  (parse* "!x"))

(test-equal "Parses a conditional expression"
  '(if x 1 "foo")
  (parse* "x ? 1 : \"foo\""))

(test-equal "Parses assignment"
  '(set! x "foo")
  (parse* "x = \"foo\""))

(test-equal "Parses addition assignment"
  '(set-op! + x 1)
  (parse* "x += 1"))

(test-equal "Parses subtraction assignment"
  '(set-op! - x 1)
  (parse* "x -= 1"))

(test-equal "Parses multiplication assignment"
  '(set-op! * x 2)
  (parse* "x *= 2"))

(test-equal "Parses division assignment"
  '(set-op! / x 2)
  (parse* "x /= 2"))

(test-equal "Parses division-assignment instead of regex"
  '(set-op! / a (set-op! / b 1))
  (parse* "a /=b/= 1"))

(test-equal "Parses regex instead of division assignment"
  '(re "=abc")
  (parse* "/=abc/"))

(test-equal "Parses modulus assignment"
  '(set-op! modulo x 2)
  (parse* "x %= 2"))

(test-equal "Parses exponentiation assignment"
  '(set-op! expt x 2)
  (parse* "x ^= 2"))

(test-equal "Parses user function application with no arguments"
  '(apply foo)
  (parse* "foo()"))

(test-equal "Parses user function application with one argument"
  '(apply foo bar)
  (parse* "foo(bar)"))

(test-equal "Parses user function application with two arguments"
  '(apply foo bar baz)
  (parse* "foo(bar, baz)"))

(test-equal "Does not parse user function application with a space"
  '(string-append foo bar)
  (parse* "foo (bar)"))

(test-equal "Parses built-in function application with no parentheses"
  '(apply length)
  (parse* "length"))

(test-equal "Parses built-in function application with no arguments"
  '(apply length)
  (parse* "length()"))

(test-equal "Parses built-in function application with one argument"
  '(apply length x)
  (parse* "length(x)"))

(test-equal "Parses built-in function application with two arguments"
  '(apply index x "$")
  (parse* "index(x, \"$\")"))

(test-equal "Parses built-in function application with a space"
  '(apply length bar)
  (parse* "length (bar)"))


;;; Input

(test-equal "Parses getline with no arguments"
  '(getline)
  (parse* "getline"))

(test-equal "Parses getline with one argument"
  '(getline x)
  (parse* "getline x"))

(test-equal "Can only getline into an lvalue"
  '(string-append (getline) "foo")
  (parse* "getline \"foo\""))

(test-equal "Parses getline with a file redirect"
  '(with-redirect (read "foo") (getline))
  (parse* "getline < \"foo\""))

(test-equal "Parses getline with an lvalue and a file redirect"
  '(with-redirect (read "foo") (getline ($ 1)))
  (parse* "getline $1 < \"foo\""))

(test-equal "Parses getline with a pipe redirect"
  '(with-redirect (pipe-from "ps") (getline))
  (parse* "\"ps\" | getline"))

(test-equal "Parses getline with an lvalue and a pipe redirect"
  '(with-redirect (pipe-from "ps") (getline (array-ref k array)))
  (parse* "\"ps\" | getline array[k]"))

;; This is explicitly mentioned in POSIX.
(test-equal "Parses a field reference in a getline pipe"
  '(with-redirect (pipe-from ($ x)) (getline))
  (parse* "$x | getline"))

;; The following tests are not checking POSIX-specified behavior.
;; They serve to document some decisions made about ambiguities left
;; unspecified by POSIX.

(test-equal "Parses concatenation in a getline redirect"
  '(with-redirect (read (string-append "foo-" k)) (getline))
  (parse* "getline < \"foo-\" k"))

(test-equal "Parses concatenation in a getline pipe"
  '(with-redirect (pipe-from (string-append "cat " file)) (getline))
  (parse* "\"cat \" file | getline"))

(test-equal "Parses a getline redirect in a getline pipe"
  '(with-redirect (pipe-from (with-redirect (read "foo") (getline)))
     (getline))
  (parse* "getline < \"foo\" | getline"))

(test-equal "Parses a getline pipe with less-than"
  '(< (with-redirect (pipe-from "ps") (getline)) 1)
  (parse* "\"ps\" | getline < 1"))

(test-equal "Redirects for getline are right-associative"
  (let ((inner '(with-redirect (read (getline z)) (getline y))))
    `(with-redirect (read ,inner) (getline x)))
  (parse* "getline x < getline y < getline z"))

(test-equal "Pipes for getline are left-associative"
  (let ((inner '(with-redirect (pipe-from (getline x)) (getline y))))
    `(with-redirect (pipe-from ,inner) (getline z)))
  (parse* "getline x | getline y | getline z"))


;;; Output

(test-equal "Parses print with no arguments"
  '(print)
  (parse* "print"))

(test-equal "Parses print with one arguments"
  '(print "hello")
  (parse* "print \"hello\""))

(test-equal "Parses print with many arguments"
  '(print "foo" x 42)
  (parse* "print \"foo\", x, 42"))

(test-error "Rejects printf with no arguments"
  #t
  (parse* "printf"))

(test-equal "Parses printf with one arguments"
  '(printf "hello")
  (parse* "printf \"hello\""))

(test-equal "Parses printf with many arguments"
  '(printf "foo %s %d" x 42)
  (parse* "printf \"foo %s %d\", x, 42"))

(test-equal "Parses truncating output redirect"
  '(with-redirect (truncate "log.txt") (print "hello"))
  (parse* "print \"hello\" > \"log.txt\""))

(test-equal "Parses appending output redirect"
  '(with-redirect (append "log.txt") (print "hello"))
  (parse* "print \"hello\" >> \"log.txt\""))

(test-equal "Parses piping output redirect"
  '(with-redirect (pipe-to "cat >&2") (print "hello"))
  (parse* "print \"hello\" | \"cat >&2\""))


;;; Simple statements

(test-equal "Parses break"
  '(break)
  (parse* "break"))

(test-equal "Parses continue"
  '(continue)
  (parse* "continue"))

(test-equal "Parses next"
  '(next)
  (parse* "next"))

(test-equal "Parses exit without argument"
  '(exit)
  (parse* "exit"))

(test-equal "Parses exit with argument"
  '(exit 1)
  (parse* "exit 1"))

(test-equal "Parses return without argument"
  '(return)
  (parse* "return"))

(test-equal "Parses return with argument"
  '(return result)
  (parse* "return result"))

(test-equal "Parses array deletion"
  '(array-delete! key array)
  (parse* "delete array[key]"))

(test-equal "Parses multi-dimensional array deletion"
  '(array-delete! (index "x" "y") array)
  (parse* "delete array[\"x\", \"y\"]"))


;;; Statements

(test-equal "Parses an empty statement"
  '(progn)
  (parse* ";"))

(test-equal "Parses empty braces"
  '(progn)
  (parse* "{}"))

(test-equal "Parses an expression in braces"
  '(set! x 2)
  (parse* "{ x = 2 }"))

(test-equal "Parses multiple expressions in braces"
  '(progn (set! x 2) (set! y 2))
  (parse* "{ x = 2; y = 2 }"))

(test-equal "Parses a one-armed if statement"
  '(if x (set! y 2))
  (parse* "if (x) y = 2"))

(test-equal "Parses a two-armed if statement"
  '(if x (set! y 2) (set! y 1))
  (parse* "if (x) y = 2; else y = 1"))

(test-equal "Parses a dangling else with the nearest if"
  '(if 1 (if 2 3 4))
  (parse* "if (1) if (2) 3; else 4"))

(test-equal "Parses a while loop"
  '(while (< x 5) (post-incr! x))
  (parse* "while (x < 5) x++"))

(test-equal "Parses a while loop with multiple statements"
  '(while (< x 5) (set-op! * y 2) (post-incr! x))
  (parse* "while (x < 5) { y *= 2; x++ }"))

(test-equal "Parses a do loop"
  '(do (< x 5) (post-incr! x))
  (parse* "do x++; while (x < 5)"))

(test-equal "Parses a do loop with multiple statements"
  '(do (< x 5) (set-op! * y 2) (post-incr! x))
  (parse* "do { y *= 2; x++ } while (x < 5)"))

(test-equal "Parses a for loop"
  '(for ((set! x 1) (< x 5) (post-incr! x)) (set-op! + y x))
  (parse* "for (x = 1; x < 5 ; x++) y += x"))

(test-equal "Parses a for loop with multiple statements"
  '(for ((set! x 1) (< x 5) (post-incr! x))
     (set-op! + y x)
     (print y))
  (parse* "for (x = 1; x < 5 ; x++) { y += x; print y }"))

(test-equal "Parses a for loop with empty expressions"
  '(for ((progn) #t (progn))
     (set-op! + y x))
  (parse* "for (;;) { y += x }"))

(test-equal "Parses a for-in loop"
  '(for-each (x array) (print x))
  (parse* "for (x in array) print x"))

(test-equal "Parses a for-in loop with multiple statements"
  '(for-each (x array) (post-decr! x) (print x))
  (parse* "for (x in array) { x--; print x }"))


;;; Function definitions

(test-equal "Parses a zero-argument function definition"
  '((defun f () (print "hello")))
  (parse "function f() { print \"hello\" }"))

(test-equal "Parses a one-argument function definition"
  '((defun f (x) (return (* x x))))
  (parse "function f(x) { return x * x }"))

(test-equal "Parses a two-argument function definition"
  '((defun f (x y) (return (+ x y))))
  (parse "function f(x, y) { return x + y }"))


;;; Patterns

(test-equal "Parses an empty pattern"
  '((#t))
  (parse "{}"))

(test-equal "Parses the BEGIN pattern"
  '((begin))
  (parse "BEGIN {}"))

(test-equal "Parses the END pattern"
  '((end))
  (parse "END {}"))

(test-equal "Parses a single pattern"
  '(((re "^a") (print)))
  (parse "/^a/"))

(test-equal "Parses a single pattern"
  '(((re "^a") (print)))
  (parse "/^a/"))

(test-equal "Parses a pattern range"
  '((((re "^a") (re "^m")) (print)))
  (parse "/^a/, /^m/"))

(test-end)

;;; Local Variables:
;;; eval: (put 'test-error 'scheme-indent-function 1)
;;; End:
