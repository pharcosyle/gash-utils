;;; The Geesh Shell Interpreter
;;; Copyright 2017, 2018 Timothy Sample <samplet@ngyro.com>
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

(define-module (geesh repl)
  #:use-module (geesh environment)
  #:use-module (geesh eval)
  #:use-module (geesh parser)
  #:use-module (ice-9 rdelim)
  #:export (run-repl))

;;; Commentary:
;;;
;;; The read-eval-print loop (REPL) of the shell.
;;;
;;; Code:

(define (run-repl)
  (let loop ((env (make-environment (environ->alist (environ))))
             (exp (read-sh (current-input-port))))
    (if (eof-object? exp)
        (or (and=> (var-ref env "?") string->number) 0)
        (begin
          (eval-sh env exp)
          (loop env (read-sh (current-input-port)))))))
