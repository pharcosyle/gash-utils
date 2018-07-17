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
  #:use-module (tests automake)
  #:use-module (tests config))

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
           (sentinal (string-append directory "/sentinal.txt"))
           (env (make-environment '())))
       (make-script utility
         (with-output-to-file ,sentinal
           (lambda ()
             (display "x"))))
       (sh:exec env utility)
       (file-exists? sentinal)))))

(test-end)
