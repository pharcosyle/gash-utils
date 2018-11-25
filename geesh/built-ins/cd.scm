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

(define-module (geesh built-ins cd)
  #:use-module (geesh environment)
  #:use-module (ice-9 match))

;;; Commentary:
;;;
;;; The 'cd' utility.
;;;
;;; Code:

(define pwd (@@ (geesh built-ins pwd) main))

(define (directory? path)
  "Check if @var{path} refers to a directory."
  (eq? (stat:type (stat path)) 'directory))

(define (canonicalize-path-logically path)
  "Canonicalize @var{path} by removing dot components, processing
dot-dot components, and removing duplicate slashes (with the exception
that if there are exactly two leading slashes, those two are
preserved).  This differs from @code{canonicalize-path} in that it
does not process symbolic links before processing dot-dot components."

  (define char-set:not-slash
    (char-set-complement (char-set #\/)))

  (define (acc->path acc)
    (if (null? acc)
        "/"
        (string-join (reverse acc) "/" 'prefix)))

  ;; XXX: Following POSIX, we should preserve both leading slashes
  ;; when there are exactly two.
  (let loop ((parts (string-tokenize path char-set:not-slash)) (acc '()))
    (match parts
      (() (acc->path acc))
      (("." . tail) (loop tail acc))
      ((".." . tail) (match acc
                       (() (loop tail acc))
                       ((prev . acc-tail)
                        (and (directory? (acc->path acc))
                             (loop tail acc-tail)))))
      ((part . tail) (loop tail (cons part acc))))))

(define (main . args)
  (match args
    (("-")
     (match (main (getvar "OLDPWD" ""))
       (0 (pwd))
       (x x)))
    (_
     (let loop ((args args) (logical? #t))
       (match args
         (()
          (match (getvar "HOME")
            (#f (format (current-error-port)
                        "~a: cd: Could not find home directory.~%"
                        (car (program-arguments)))
                EXIT_FAILURE)
            (directory (main directory))))
         (("-P" . tail) (loop tail #f))
         (("-L" . tail) (loop tail #t))
         ((or (directory) ("--" directory))
          (let ((curpath (cond
                          (logical? (canonicalize-path-logically
                                     (if (string-prefix? "/" directory)
                                         directory
                                         (string-append (getvar "PWD")
                                                        "/" directory))))
                          (else (if (string-prefix? "/" directory)
                                    directory
                                    (string-append (getcwd)
                                                   "/" directory))))))
            (if (catch 'system-error
                  (lambda ()
                    (chdir curpath)
                    #t)
                  (lambda args
                    (format (current-error-port)
                            "~a: cd: ~a: ~a~%"
                            (car (program-arguments)) curpath
                            (strerror (system-error-errno args)))
                    #f))
                (begin
                  (setvar! "OLDPWD" (getvar "PWD"))
                  (setvar! "PWD" (if logical? curpath (getcwd)))
                  EXIT_SUCCESS)
                EXIT_FAILURE)))
         (_ (format (current-error-port)
                    "~a: cd: Invalid arguments."
                    (car (program-arguments)))))))))
