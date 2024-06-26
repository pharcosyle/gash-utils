;;; Gash-Utils
;;; Copyright © 2018 Timothy Sample <samplet@ngyro.com>
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
;;; along with Gash-Utils.  If not, see <https://www.gnu.org/licenses/>.

(define-module (gash commands printf)
  #:use-module (gash compat)
  #:use-module (gash-utils file-formats)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26))

(define-syntax-rule (invalid-options proc msg args ...)
  (scm-error 'invalid-options proc msg (list args ...) '()))

(define printf-conversion-adapter
  (make-conversion-adapter
   (lambda (s seed)
     (values (or s "") seed))
   (lambda (n seed)
     (cond
      ;; TODO: Use C-style numbers instead of Scheme-style numbers.
      ((string->number n) => (cut values <> seed))
      (else (format (current-error-port) "printf: invalid number: ~a~%" n)
            (values 0 #f))))
   (lambda (c seed)
     (if (string-null? (or c ""))
         (values #\nul seed)
         (values (string-ref c 0) seed)))))

(define (printf format-string . args)
  (let ((format (parse-file-format format-string)))
    (receive (result seed)
        (apply fold-file-format printf-conversion-adapter #t format args)
      (display result)
      seed)))

(define (main . args)
  (let ((name (match args ((name . _) name) (_ "???")))
        (args (if (null? args) args (cdr args))))
    (catch #t
      (lambda ()
        (match args
          (() (invalid-options "printf" "Invalid options: ~s" args))
          ((format . args)
           (exit (apply printf format args)))))
      (lambda args
        (match args
          (('quit status)
           (exit status))
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
