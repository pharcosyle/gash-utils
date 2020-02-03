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
  #:use-module (ice-9 match))

(define-syntax-rule (invalid-options proc msg args ...)
  (scm-error 'invalid-options proc msg (list args ...) '()))

(define (printf . args)
  (match args
    (("%s" arg) (format #t "~a" arg))
    (("%s\\n" arg) (format #t "~a~%" arg))
    (_ (invalid-options "printf" "Invalid options: ~s" args))))

(define (main . args)
  (let ((name (match args ((name . _) name) (_ "???")))
        (ags (if (null? args) args (cdr args))))
    (catch #t
      (lambda ()
        (apply printf (cdr args))
        (primitive-exit 0))
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
