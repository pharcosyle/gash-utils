;;; Gash --- Guile As SHell
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018 Timothy Sample <samplet@ngyro.com>
;;;
;;; This file is part of Gash.
;;;
;;; Gash is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Gash is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Gash.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gash commands test)
  #:use-module (gash shell-utils)
  #:use-module ((ice-9 i18n) #:select (locale-string->integer))
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-syntax-rule (invalid-options proc msg args ...)
  (scm-error 'invalid-options proc msg (list args ...) '()))

(define (string->integer s)
  (call-with-values (lambda () (locale-string->integer s))
    (lambda (n count)
      (unless (and n (= count (string-length s)))
        (invalid-options "string->integer" "Expected an integer: ~s" s))
      n)))

(define (test-integer-strings proc s1 s2)
  (let* ((n1 (string->integer s1))
         (n2 (string->integer s2)))
    (proc n1 n2)))

(define (test . args)
  (match args
    (() #f)
    ((x) (not (string-null? x)))
    ((op x)
     (match op
       ("!" (not (test x)))
       ("-d" (directory-exists? x))
       ("-e" (file-exists? x))
       ("-f" (regular-file? x))
       ((or "-h" "-L") (symbolic-link? x))
       ("-n" (not (string-null? x)))
       ("-r" (access? x R_OK))
       ("-s" (and (file-exists? x)
                  (not (zero? (stat:size (stat x))))))
       ("-w" (access? x W_OK))
       ("-x" (access? x X_OK))
       ("-z" (string-null? x))
       (_ (invalid-options "test" "Unknown operator: ~s" op))))
    ((x op y)
     (match op
       ("=" (string=? x y))
       ("!=" (not (string=? x y)))
       ("-eq" (test-integer-strings = x y))
       ("-ne" (test-integer-strings (compose not =) x y))
       ("-gt" (test-integer-strings > x y))
       ("-ge" (test-integer-strings >= x y))
       ("-lt" (test-integer-strings < x y))
       ("-le" (test-integer-strings <= x y))
       (_ (if (string=? x "!")
              (not (test op y))
              (invalid-options "test" "Unknown operator: ~s" op)))))
    (("!" x op y) (not (test x op y)))
    (_ (invalid-options "test" "Invalid options: ~s" args))))

(define (test/bracket . args)
  (unless (and (pair? args) (string=? (last args) "]"))
    (invalid-options "test/bracket" "Missing final \"]\""))
  (apply test (drop-right args 1)))

(define (main . args)
  (let ((name (match args ((name . _) name) (_ "???")))
        (ags (if (null? args) args (cdr args))))
    (catch #t
      (lambda ()
        (primitive-exit (if (apply test (cdr args))
                            EXIT_SUCCESS
                            EXIT_FAILURE)))
      (lambda args
        (match args
          (('invalid-options proc msg args data)
           (format (current-error-port) "~a: " name)
           (apply format (current-error-port) msg args))
          (('system-error proc msg args data)
           (format (current-error-port) "~a: " name)
           (display (strerror (car args)) (current-error-port)))
          (_ (format (current-error-port)
                     "~a: Caught exception: ~s" name args)))
        (newline (current-error-port))
        (exit EXIT_FAILURE)))))
