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

(define-module (geesh built-ins trap)
  #:use-module (geesh environment)
  #:use-module (ice-9 match))

;;; Commentary:
;;;
;;; The 'trap' utility.
;;;
;;; Code:

(define sh:eval (@@ (geesh built-ins eval) main))

(define (action->handler action)
  (let ((n (string->number action)))
    (cond
     ((and n (integer? n) (>= n 0)) SIG_DFL)
     ((string=? action "-") SIG_DFL)
     ((string-null? action) SIG_IGN)
     (else (lambda () (sh:eval action))))))

(define (condition->signum condition)
  (let ((n (string->number condition)))
    (cond
     ((and n (integer? n)) n)
     ((string-ci=? condition "EXIT") 0)
     (else (let* ((name (if (string-prefix-ci? "SIG" condition)
                            (string-upcase condition)
                            (string-append "SIG" (string-upcase condition))))
                  (symb (string->symbol name)))
             (or (and=> (module-variable (current-module) symb)
                        variable-ref)
                 -1))))))

(define (main . args)
  (match args
    (() "print")
    (("--" . args) (main args))
    ((action conditions ..1)
     (let ((handler (action->handler action)))
       (for-each (lambda (condition)
                   (match (condition->signum condition)
                     (0 (set-atexit! handler))
                     (n (sigaction n handler))))
                 conditions))
     EXIT_SUCCESS)
    (_ (format (current-error-port)
               "~a: trap: Invalid options ~s.~%"
               (car (program-arguments)) args)
       EXIT_FAILURE)))
