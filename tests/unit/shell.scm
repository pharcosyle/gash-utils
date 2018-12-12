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

(define-module (test-shell)
  #:use-module (geesh environment)
  #:use-module (geesh shell)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-64)
  #:use-module (tests unit automake)
  #:use-module (tests unit config))

;;; Commentary:
;;;
;;; Tests for the shell module.
;;;
;;; Code:

(define (make-temporary-directory)
  (let loop ((name (tmpnam)))
    (catch 'system-error
      (lambda ()
        (mkdir name #o700)
        name)
      (lambda args
        (unless (= (system-error-errno args) EEXIST)
          (apply throw args))
        (loop (tmpnam))))))

(define (delete-recursively path)
  (define enter? (const #t))
  (define (leaf path stat acc) (delete-file path) #f)
  (define down (const #f))
  (define (up path stat acc) (rmdir path) #f)
  (define skip (const #f))
  (define (error path stat errno result)
    (scm-error 'system-error
               "delete-recursively"
               "~A" `(,strerror errno)
               `(,errno)))
  (file-system-fold enter? leaf down up skip error #f path))

(define (call-with-temporary-directory proc)
  (let* ((directory (make-temporary-directory))
         (result (with-continuation-barrier
                  (lambda ()
                    (proc directory)))))
    (delete-recursively directory)
    result))

(define (%make-script object . forms)
  (define (write-script port)
    (chmod port #o755)
    (format port "#!~a --no-auto-compile~%!#~%~%" *guile-path*)
    (for-each (lambda (form)
                (write form port)
                (newline port))
              forms))
  (match object
    ((? port?) (write-script object))
    ((? string?) (call-with-output-file object write-script))))

(define-syntax-rule (make-script path form form1 ...)
  (%make-script path `form `form1 ...))

(test-begin "shell")

(test-assert "Executes a utility by absolute path"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((utility (string-append directory "/utility"))
           (sentinal (string-append directory "/sentinal.txt")))
       (make-script utility
         (with-output-to-file ,sentinal
           (lambda ()
             (display "x"))))
       (sh:exec utility)
       (file-exists? sentinal)))))

(test-assert "Executes a utility by searching PATH"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((utility (string-append directory "/utility"))
           (sentinal (string-append directory "/sentinal.txt")))
       (make-script utility
         (with-output-to-file ,sentinal
           (lambda ()
             (display "x"))))
       (with-variables `(("PATH" . ,directory))
         (lambda () (sh:exec "utility")))
       (file-exists? sentinal)))))

(test-equal "Sets status to 127 if a utility cannot be found"
  127
  (call-with-temporary-directory
   (lambda (directory)
     (with-variables `(("PATH" . ,directory))
       (lambda ()
         (sh:exec "utility")
         (get-status))))))

(test-equal "Executes regular built-ins"
  "foo bar\n"
  (with-output-to-string
    (lambda ()
      (sh:exec "echo" "foo" "bar"))))


;;; Redirects.

;; TODO: Tame this mess with some syntax.

(test-equal "Redirects built-in standard output to file"
  "foo\n"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((foo (string-append directory "/foo.txt")))
       (sh:with-redirects `((> 1 ,foo))
         (lambda ()
           (display "foo")
           (newline)))
       (call-with-input-file foo get-string-all)))))

(test-equal "Redirects built-in standard error to file"
  "foo\n"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((foo (string-append directory "/foo.txt")))
       (sh:with-redirects `((> 2 ,foo))
         (lambda ()
           (display "foo" (current-error-port))
           (newline (current-error-port))))
       (call-with-input-file foo get-string-all)))))

(test-equal "Redirects external standard output to file"
  "foo\n"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((utility (string-append directory "/utility"))
           (foo (string-append directory "/foo.txt")))
       (make-script utility
         (display "foo")
         (newline))
       (sh:with-redirects `((> 1 ,foo))
         (lambda ()
           (sh:exec utility)))
       (call-with-input-file foo get-string-all)))))

(test-equal "Redirects external standard error to file"
  "foo\n"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((utility (string-append directory "/utility"))
           (foo (string-append directory "/foo.txt")))
       (make-script utility
         (display "foo" (current-error-port))
         (newline (current-error-port)))
       (sh:with-redirects `((> 2 ,foo))
         (lambda ()
           (sh:exec utility)))
       (call-with-input-file foo get-string-all)))))

(test-equal "Redirects built-in standard input from file"
  "foo\n"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((foo (string-append directory "/foo.txt"))
           (output (string-append directory "/output.txt")))
       (with-output-to-file foo
         (lambda ()
           (display "foo")
           (newline)))
       (sh:with-redirects `((< 0 ,foo))
         (lambda ()
           (with-output-to-file output
             (lambda ()
               (display (get-string-all (current-input-port)))))))
       (call-with-input-file output get-string-all)))))

(test-equal "Redirects external standard input from file"
  "foo\n"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((utility (string-append directory "/utility"))
           (foo (string-append directory "/foo.txt"))
           (output (string-append directory "/output.txt")))
       (with-output-to-file foo
         (lambda ()
           (display "foo")
           (newline)))
       (make-script utility
         (use-modules (ice-9 textual-ports))
         (with-output-to-file ,output
           (lambda ()
             (display (get-string-all (current-input-port))))))
       (sh:with-redirects `((< 0 ,foo))
         (lambda ()
           (sh:exec utility)))
       (call-with-input-file output get-string-all)))))

;; These next two tests are non-deterministic, so we need to allow
;; multiple right answers.  (This is preferred to using 'force-output'
;; because we want to be sure that 'sh:with-redirects' handles
;; left-over buffered output.)

(test-assert "Redirects built-in standard error to standard output"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((foo (string-append directory "/foo.txt")))
       (sh:with-redirects `((> 1 ,foo) (>& 2 1))
         (lambda ()
           (display "foo")
           (newline)
           (display "bar" (current-error-port))
           (newline (current-error-port))))
       (let ((result (call-with-input-file foo get-string-all)))
         (or (string=? result "foo\nbar\n")
             (string=? result "bar\nfoo\n")))))))

(test-assert "Redirects external standard error to standard output"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((utility (string-append directory "/utility"))
           (foo (string-append directory "/foo.txt")))
       (make-script utility
         (display "foo")
         (newline)
         (display "bar" (current-error-port))
         (newline (current-error-port)))
       (sh:with-redirects `((> 1 ,foo) (>& 2 1))
         (lambda ()
           (sh:exec utility)))
       (let ((result (call-with-input-file foo get-string-all)))
         (or (string=? result "foo\nbar\n")
             (string=? result "bar\nfoo\n")))))))

(test-equal "Appends standard output to file"
  "foo\nbar\n"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((foo (string-append directory "/foo.txt")))
       (with-output-to-file foo
         (lambda ()
           (display "foo")
           (newline)))
       (sh:with-redirects `((>> 1 ,foo))
         (lambda ()
           (display "bar")
           (newline)))
       (call-with-input-file foo get-string-all)))))

(test-equal "Redirects here-document to standard input"
  "foo\n"
  (with-output-to-string
    (lambda ()
      (sh:with-redirects '((<< 0 "foo\n"))
        (lambda ()
          (display (get-string-all (current-input-port))))))))

(test-equal "Redirects work with string ports"
  "foo\n"
  (with-input-from-string "bar\n"
    (lambda ()
      (setvbuf (current-input-port) 'none)
      (with-output-to-string
        (lambda ()
          (sh:with-redirects '((<< 0 "foo\n"))
            (lambda ()
              (display (get-string-all (current-input-port))))))))))

(test-equal "Does not use buffered input from current-input-port"
  "foo\n"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((bar-baz (string-append directory "/bar-baz.txt")))
       (with-output-to-file bar-baz
         (lambda ()
           (display "bar\nbaz\n")))
       (with-input-from-file bar-baz
         (lambda ()
           (setvbuf (current-input-port) 'block 8)
           (get-line (current-input-port))
           (with-output-to-string
             (lambda ()
               (sh:with-redirects '((<< 0 "foo\n"))
                 (lambda ()
                   (display (get-string-all (current-input-port)))))))))))))

(test-equal "Allows here-document and file redirect"
  "foo\n"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((foo (string-append directory "/foo.txt")))
       (sh:with-redirects `((> 1 ,foo) (<< 0 "foo\n"))
         (lambda ()
           (display (get-string-all (current-input-port)))))
       (call-with-input-file foo get-string-all)))))

(test-equal "Uses last here-document specified"
  "foo\n"
  (with-output-to-string
    (lambda ()
      (sh:with-redirects '((<< 0 "bar\n") (<< 0 "foo\n"))
        (lambda ()
          (display (get-string-all (current-input-port))))))))

;; TODO: Read-write tests, closing tests, clobbering tests.


;;; Subshells.

(test-equal "Subshells cannot change variables"
  "foo"
  (with-variables '(("x" . "foo"))
    (lambda ()
      (sh:subshell
        (lambda ()
          (setvar! "x" "bar")))
      (getvar "x"))))

;; TODO: Test other means of manipulating the environment and exit
;; statuses.


;;; Command substitutions.

(test-equal "Substitutes output from built-in"
  "foo"
  (sh:substitute-command
    (lambda ()
      (display "foo"))))

(test-equal "Substitutes output from external utilities"
  "foo"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((utility (string-append directory "/utility")))
       (make-script utility
         (display "foo"))
       (sh:substitute-command
         (lambda ()
           (sh:exec utility)))))))

(test-equal "Trailing newlines are trimmed from substitutions"
  "foo"
  (sh:substitute-command
    (lambda ()
      (display "foo")
      (newline))))

(test-equal "Non-trailing newlines are preserved in substitutions"
  "\nfoo\nbar"
  (sh:substitute-command
    (lambda ()
      (newline)
      (display "foo")
      (newline)
      (display "bar"))))

(test-equal "Empty substitutions produce empty strings"
  ""
  (sh:substitute-command noop))


;; Pipelines.

(test-equal "Built-ins are connected by pipelines"
  "foo"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((foo (string-append directory "/foo.txt")))
       (sh:pipeline (lambda ()
                      (display "foo\n"))
                    (lambda ()
                      (with-output-to-file foo
                        (lambda ()
                          (display (get-line (current-input-port)))))))
       (call-with-input-file foo get-string-all)))))

(test-equal "External utilities are connected by pipelines"
  "foo"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((utility1 (string-append directory "utility1"))
           (utility2 (string-append directory "utility2"))
           (foo (string-append directory "/foo.txt")))
       (make-script utility1
         (display "foo\n"))
       (make-script utility2
         (use-modules (ice-9 textual-ports))
         (with-output-to-file ,foo
           (lambda ()
             (display (get-line (current-input-port))))))
       (sh:pipeline (lambda ()
                      (sh:exec utility1))
                    (lambda ()
                      (sh:exec utility2)))
       (call-with-input-file foo get-string-all)))))

(test-equal "Externals and built-ins are connected by pipelines"
  "foo"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((utility (string-append directory "/utility"))
           (foo (string-append directory "/foo.txt")))
       (make-script utility
         (display "foo\n"))
       (sh:pipeline (lambda ()
                      (sh:exec utility))
                    (lambda ()
                      (with-output-to-file foo
                        (lambda ()
                          (display (get-line (current-input-port)))))))
       (call-with-input-file foo get-string-all)))))

(test-equal "Built-ins and externals are connected by pipelines"
  "foo"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((utility (string-append directory "/utility"))
           (foo (string-append directory "/foo.txt")))
       (make-script utility
         (use-modules (ice-9 textual-ports))
         (with-output-to-file ,foo
           (lambda ()
             (display (get-line (current-input-port))))))
       (sh:pipeline (lambda ()
                      (display "foo\n"))
                    (lambda ()
                      (sh:exec utility)))
       (call-with-input-file foo get-string-all)))))

(test-end)
