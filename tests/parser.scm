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

(test-equal "Parses command lists"
  '((<sh-exec> "echo" "foo")
    (<sh-exec> "echo" "bar"))
  (parse "echo foo; echo bar"))

(test-equal "Parses asynchronous command lists"
  '((<sh-async> (<sh-exec> "echo" "foo"))
    (<sh-async> (<sh-exec> "echo" "bar")))
  (parse "echo foo& echo bar&"))

(test-equal "Parses mixed command lists"
  '((<sh-async> (<sh-exec> "echo" "foo"))
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
  '(<sh-with-redirects> ((>& 1 3))
     (<sh-exec> "exec"))
  (parse "exec >&3"))

(test-equal "Parses commands with close redirects"
  '(<sh-with-redirects> ((<& 3 -))
     (<sh-exec> "exec"))
  (parse "exec 3<&-"))

(test-equal "Parses redirects without a command"
  '(<sh-with-redirects> ((>& 2 1)) #f)
  (parse "2>&1"))

(test-equal "Parses assignments"
  '(<sh-set!> (("FOO" "bar")))
  (parse "FOO=bar"))

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
            echo bar }"))

(test-equal "Parses subshells"
  '(<sh-subshell> (<sh-begin> (<sh-exec> "echo" "foo")
                              (<sh-exec> "echo" "bar")))
  (parse "(echo foo; echo bar)"))

;; For loops

(test-equal "Parses for loops over parameters without seperator"
  '(<sh-for> ("x" (<sh-ref> "@"))
     (<sh-exec> "echo" (<sh-ref> "x")))
  (parse "for x do echo $x done"))

(test-equal "Parses for loops over parameters with seperator"
  '(<sh-for> ("x" (<sh-ref> "@"))
     (<sh-exec> "echo" (<sh-ref> "x")))
  (parse "for x; do echo $x done"))

(test-equal "Parses for loops over parameters with \"in\""
  '(<sh-for> ("x" (<sh-ref> "@"))
     (<sh-exec> "echo" (<sh-ref> "x")))
  (parse "for x in; do echo $x done"))

(test-equal "Parses for loops over word lists"
  '(<sh-for> ("x" ("foo" "bar" "baz"))
     (<sh-exec> "echo" (<sh-ref> "x")))
  (parse "for x in foo bar baz; do echo $x done"))

;; Case statements

(test-equal "Parses case statements with final seperator"
  '(<sh-case> (<sh-ref> "foo")
     (("bar") (<sh-exec> "echo" "bar")))
  (parse "case $foo in bar) echo bar ;; esac"))

(test-equal "Parses case statements without final seperator"
  '(<sh-case> (<sh-ref> "foo")
     (("bar") (<sh-exec> "echo" "bar")))
  (parse "case $foo in bar) echo bar esac"))

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
  (parse "case $foo in bar) echo bar ;; baz) echo baz esac"))

(test-equal "Parses case statements with compound patterns"
  '(<sh-case> (<sh-ref> "foo")
     (("bar" "baz") (<sh-exec> "echo" (<sh-quote> "bar or baz"))))
  (parse "case $foo in bar | baz) echo 'bar or baz' ;; esac"))

;; If statements

(test-equal "Parses one-branch if statements"
  '(<sh-cond>
    ((<sh-exec> "[" (<sh-ref> "foo") "=" "bar" "]")
     (<sh-exec> "echo" "bar")))
  (parse "if [ $foo = bar ] then echo bar fi"))

(test-equal "Parses two-branch if statements"
  '(<sh-cond>
    ((<sh-exec> "[" (<sh-ref> "foo") "=" "bar" "]")
     (<sh-exec> "echo" "bar"))
    (<sh-else>
     (<sh-exec> "echo" "baz")))
  (parse "if [ $foo = bar ] then echo bar else echo baz fi"))

(test-equal "Parses multi-branch if statements"
  '(<sh-cond>
    ((<sh-exec> "[" (<sh-ref> "foo") "=" "bar" "]")
     (<sh-exec> "echo" "bar"))
    ((<sh-exec> "[" (<sh-ref> "foo") "=" "baz" "]")
     (<sh-exec> "echo" "baz"))
    (<sh-else>
     (<sh-exec> "echo" "quux")))
  (parse "if [ $foo = bar ] then
              echo bar
          elif [ $foo = baz ] then
              echo baz
          else
              echo quux
          fi"))

;; While and until loops

(test-equal "Parses while loops"
  '(<sh-while> (<sh-exec> "is-foo-time")
     (<sh-exec> "foo"))
  (parse "while is-foo-time do foo done"))

(test-equal "Parses until loops"
  '(<sh-until> (<sh-exec> "is-no-longer-foo-time")
     (<sh-exec> "foo"))
  (parse "until is-no-longer-foo-time do foo done"))

;; Functions

(test-equal "Parses functions"
  '(<sh-define> ("foo")
     (<sh-exec> "echo" "foo"))
  (parse "foo() { echo foo }"))

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
