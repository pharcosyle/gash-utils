;;; Gash-Utils
;;; Copyright © 2018 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (test-shell-utils)
  #:use-module (gash shell-utils)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-64)
  #:use-module (tests unit automake))

;;; Commentary:
;;;
;;; Tests for the shell-utils module.
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
  (let* ((directory (make-temporary-directory)))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (proc directory))
      (lambda ()
        (delete-recursively directory)))))

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

(test-begin "shell-utils")

(test-assert "mkdir-p"
  (call-with-temporary-directory
   (lambda (directory)
     (mkdir-p "foo/bar")
     (file-exists? "foo/bar"))))

(test-end)
