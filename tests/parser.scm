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

(define-module (test-parser)
  #:use-module (geesh parser)
  #:use-module (srfi srfi-64)
  #:use-module (tests automake))

;;; Commentary:
;;;
;;; Tests for the parser module.
;;;
;;; Code:

(define (parse str)
  (call-with-input-string str read-sh))

(test-begin "reader")

;; Commands and lists

(test-equal "Parses simple command"
  '(<sh-exec> "echo" "foo")
  (parse "echo foo"))

(test-equal "Parses command with keywords as arguments"
  '(<sh-exec> "echo" "{" "!" "for" "}")
  (parse "echo { ! for }"))

(test-equal "Parses command lists"
  '(<sh-begin> (<sh-exec> "echo" "foo")
               (<sh-exec> "echo" "bar"))
  (parse "echo foo; echo bar"))

(test-equal "Parses asynchronous command lists"
  '(<sh-begin> (<sh-async> (<sh-exec> "echo" "foo"))
               (<sh-async> (<sh-exec> "echo" "bar")))
  (parse "echo foo& echo bar&"))

(test-equal "Parses mixed command lists"
  '(<sh-begin> (<sh-async> (<sh-exec> "echo" "foo"))
               (<sh-exec> "echo" "bar"))
  (parse "echo foo& echo bar"))

(test-equal "Parses commands with assignments"
  '(<sh-exec-let> (("FOO" "bar"))
     "echo" (<sh-ref> "FOO"))
  (parse "FOO=bar echo $FOO"))

(test-equal "Parses commands with default redirects"
  '(<sh-with-redirects> ((> 1 "bar"))
     (<sh-exec> "echo" "foo"))
  (parse "echo foo > bar"))

(test-equal "Parses commands with specific redirects"
  '(<sh-with-redirects> ((< 5 "bar"))
     (<sh-exec> "echo" "foo"))
  (parse "echo foo 5< bar"))

(test-equal "Parses commands with dup redirects"
  '(<sh-with-redirects> ((>& 1 "3"))
     (<sh-exec> "exec"))
  (parse "exec >&3"))

(test-equal "Parses commands with close redirects"
  '(<sh-with-redirects> ((<& 3 "-"))
     (<sh-exec> "exec"))
  (parse "exec 3<&-"))

(test-equal "Parses redirects without a command"
  '(<sh-with-redirects> ((>& 2 "1")) #f)
  (parse "2>&1"))

(test-equal "Parses assignments"
  '(<sh-set!> ("FOO" "bar"))
  (parse "FOO=bar"))

(test-equal "Parses multiple assignments"
  '(<sh-set!> ("FOO" "bar") ("BAZ" "quux"))
  (parse "FOO=bar BAZ=quux"))

;; Boolean expressions

(test-equal "Parses disjunctions"
  '(<sh-or> (<sh-exec> "echo" "foo")
            (<sh-exec> "echo" "bar"))
  (parse "echo foo || echo bar"))

(test-equal "Parses conjunctions"
  '(<sh-and> (<sh-exec> "echo" "foo")
             (<sh-exec> "echo" "bar"))
  (parse "echo foo && echo bar"))

(test-equal "Parses conjunction than disjunction"
  '(<sh-or> (<sh-and> (<sh-exec> "echo" "foo")
                      (<sh-exec> "echo" "bar"))
            (<sh-exec> "echo" "baz"))
  (parse "echo foo && echo bar || echo baz"))

(test-equal "Parses disjunction than conjunction"
  '(<sh-and> (<sh-or> (<sh-exec> "echo" "foo")
                      (<sh-exec> "echo" "bar"))
             (<sh-exec> "echo" "baz"))
  (parse "echo foo || echo bar && echo baz"))

;; Pipelines

(test-equal "Parses pipelines"
  '(<sh-pipe> ((<sh-exec> "cat" "foo.txt")
               (<sh-exec> "grep" "bar")))
  (parse "cat foo.txt | grep bar"))

;; Brace groups and subshells

(test-equal "Parses brace groups"
  '(<sh-begin> (<sh-exec> "echo" "foo")
               (<sh-exec> "echo" "bar"))
  (parse "{ echo foo
            echo bar; }"))

(test-equal "Parses subshells"
  '(<sh-subshell> (<sh-begin> (<sh-exec> "echo" "foo")
                              (<sh-exec> "echo" "bar")))
  (parse "(echo foo; echo bar)"))

;; Here documents

(test-equal "Parses one here-document in a complete command"
  '(<sh-with-redirects> ((<< 0 (<sh-quote> "foo\n")))
     (<sh-exec> "cat"))
  (parse "cat <<eof\nfoo\neof"))

(test-equal "Parses multiple here-documents in a complete command"
  '(<sh-begin> (<sh-with-redirects> ((<< 0 (<sh-quote> "foo\n")))
                 (<sh-exec> "cat"))
               (<sh-with-redirects> ((<< 0 (<sh-quote> "bar\n")))
                 (<sh-exec> "cat")))
  (parse "cat <<eof1; cat <<eof2\nfoo\neof1\nbar\neof2"))

(test-equal "Parses one here-document in a compound list"
  '(<sh-subshell>
    (<sh-with-redirects> ((<< 0 (<sh-quote> "foo\n")))
      (<sh-exec> "cat")))
  (parse "(cat <<eof\nfoo\neof\n)"))

(test-equal "Parses multiple here-documents in a compound list"
  '(<sh-subshell>
    (<sh-begin> (<sh-with-redirects> ((<< 0 (<sh-quote> "foo\n")))
                  (<sh-exec> "cat"))
                (<sh-with-redirects> ((<< 0 (<sh-quote> "bar\n")))
                  (<sh-exec> "cat"))))
  (parse "(cat <<eof1; cat <<eof2\nfoo\neof1\nbar\neof2\n)"))

(test-equal "Parses here-documents in both simultaneously"
  '(<sh-begin> (<sh-subshell>
                (<sh-with-redirects> ((<< 0 (<sh-quote> "foo\n")))
                  (<sh-exec> "cat")))
               (<sh-with-redirects> ((<< 0 (<sh-quote> "bar\n")))
                 (<sh-exec> "cat")))
  (parse "(cat <<eof1); cat <<eof2\nfoo\neof1\nbar\neof2"))

(test-equal "Parses two here-documents split by two newlines"
  '(<sh-subshell>
    (<sh-begin> (<sh-with-redirects> ((<< 0 (<sh-quote> "foo\n")))
                  (<sh-exec> "cat"))
                (<sh-with-redirects> ((<< 0 (<sh-quote> "bar\n")))
                  (<sh-exec> "cat"))))
  (parse "(\ncat <<eof\nfoo\neof\n\ncat <<eof\nbar\neof\n)"))

(test-equal "Parses tab-trimming here-document"
  '(<sh-with-redirects> ((<< 0 (<sh-quote> "foo\n")))
     (<sh-exec> "cat"))
  (parse "cat <<-eof\n\tfoo\n\teof"))

;; For loops

(test-equal "Parses for loops over parameters without seperator"
  '(<sh-for> ("x" (<sh-ref> "@"))
     (<sh-exec> "echo" (<sh-ref> "x")))
  (parse "for x do echo $x; done"))

(test-equal "Parses for loops over parameters with seperator"
  '(<sh-for> ("x" (<sh-ref> "@"))
     (<sh-exec> "echo" (<sh-ref> "x")))
  (parse "for x; do echo $x; done"))

(test-equal "Parses for loops over parameters with \"in\""
  '(<sh-for> ("x" (<sh-ref> "@"))
     (<sh-exec> "echo" (<sh-ref> "x")))
  (parse "for x in; do echo $x; done"))

(test-equal "Parses for loops over word lists"
  '(<sh-for> ("x" ("foo" "bar" "baz"))
     (<sh-exec> "echo" (<sh-ref> "x")))
  (parse "for x in foo bar baz; do echo $x; done"))

;; Case statements

(test-equal "Parses case statements with final seperator"
  '(<sh-case> (<sh-ref> "foo")
     (("bar") (<sh-exec> "echo" "bar")))
  (parse "case $foo in bar) echo bar ;; esac"))

(test-equal "Parses case statements without final seperator"
  '(<sh-case> (<sh-ref> "foo")
     (("bar") (<sh-exec> "echo" "bar")))
  (parse "case $foo in bar) echo bar; esac"))

(test-equal "Parses empty case statements"
  '(<sh-case> (<sh-ref> "foo"))
  (parse "case $foo in esac"))

(test-equal "Parses case statements with empty case item"
  '(<sh-case> (<sh-ref> "foo")
     (("bar") #f))
  (parse "case $foo in bar) esac"))

(test-equal "Parses case statements with multiple case items"
  '(<sh-case> (<sh-ref> "foo")
     (("bar") (<sh-exec> "echo" "bar"))
     (("baz") (<sh-exec> "echo" "baz")))
  (parse "case $foo in bar) echo bar ;; baz) echo baz; esac"))

(test-equal "Parses case statements with compound patterns"
  '(<sh-case> (<sh-ref> "foo")
     (("bar" "baz") (<sh-exec> "echo" (<sh-quote> "bar or baz"))))
  (parse "case $foo in bar | baz) echo 'bar or baz' ;; esac"))

;; If statements

(test-equal "Parses one-branch if statements"
  '(<sh-cond>
    ((<sh-exec> "[" (<sh-ref> "foo") "=" "bar" "]")
     (<sh-exec> "echo" "bar")))
  (parse "if [ $foo = bar ]; then echo bar; fi"))

(test-equal "Parses two-branch if statements"
  '(<sh-cond>
    ((<sh-exec> "[" (<sh-ref> "foo") "=" "bar" "]")
     (<sh-exec> "echo" "bar"))
    (<sh-else>
     (<sh-exec> "echo" "baz")))
  (parse "if [ $foo = bar ]; then echo bar; else echo baz; fi"))

(test-equal "Parses multi-branch if statements"
  '(<sh-cond>
    ((<sh-exec> "[" (<sh-ref> "foo") "=" "bar" "]")
     (<sh-exec> "echo" "bar"))
    ((<sh-exec> "[" (<sh-ref> "foo") "=" "baz" "]")
     (<sh-exec> "echo" "baz"))
    (<sh-else>
     (<sh-exec> "echo" "quux")))
  (parse "if [ $foo = bar ]
          then
              echo bar
          elif [ $foo = baz ]
          then
              echo baz
          else
              echo quux
          fi"))

;; While and until loops

(test-equal "Parses while loops"
  '(<sh-while> (<sh-exec> "is-foo-time")
     (<sh-exec> "foo"))
  (parse "while is-foo-time; do foo; done"))

(test-equal "Parses until loops"
  '(<sh-until> (<sh-exec> "is-no-longer-foo-time")
     (<sh-exec> "foo"))
  (parse "until is-no-longer-foo-time; do foo; done"))

;; Functions

(test-equal "Parses functions"
  '(<sh-define> ("foo")
     (<sh-exec> "echo" "foo"))
  (parse "foo() { echo foo; }"))

;; Nested commands

(test-equal "Parses bracketed command substitions"
  '(<sh-exec> "echo"
              (<sh-cmd-sub> (<sh-exec> "foo"))
              (<sh-cmd-sub> (<sh-exec> "bar")))
  (parse "echo $(foo) $(bar)"))

(test-equal "Parses nested bracketed command substitions"
  '(<sh-exec> "echo"
              (<sh-cmd-sub> (<sh-exec> "foo"
                                       (<sh-cmd-sub> (<sh-exec> "bar")))))
  (parse "echo $(foo $(bar))"))

(test-equal "Parses empty bracketed command substitions"
  '(<sh-exec> "echo" (<sh-cmd-sub> #f))
  (parse "echo $()"))

(test-equal "Parses multiline bracketed command substitions"
  '(<sh-exec> "echo" (<sh-cmd-sub> ((<sh-exec> "foo")
                                    (<sh-exec> "bar"))))
  (parse "echo $(foo
                 bar)"))

(test-equal "Parses backquoted command substitions"
  '(<sh-exec> "echo"
              (<sh-cmd-sub> (<sh-exec> "foo"))
              (<sh-cmd-sub> (<sh-exec> "bar")))
  (parse "echo `foo` `bar`"))

(test-equal "Parses nested backquoted command substitions"
  '(<sh-exec> "echo"
              (<sh-cmd-sub> (<sh-exec> "foo"
                                       (<sh-cmd-sub> (<sh-exec> "bar")))))
  (parse "echo `foo \\`bar\\``"))

(test-equal "Parses empty backquoted command substitions"
  '(<sh-exec> "echo" (<sh-cmd-sub> #f))
  (parse "echo ``"))

(test-equal "Parses multiline backquoted command substitions"
  '(<sh-exec> "echo" (<sh-cmd-sub> ((<sh-exec> "foo")
                                    (<sh-exec> "bar"))))
  (parse "echo `foo
                bar`"))

;; Other tests

(test-assert "Returns EOF on EOF"
  (eof-object? (parse "")))

(test-equal "Parses one statement at a time"
  '((<sh-exec> "echo" "foo")
    (<sh-exec> "echo" "bar"))
  (call-with-input-string "echo foo
                           echo bar"
    (lambda (port)
      (list (read-sh port)
            (read-sh port)))))

(test-end)
