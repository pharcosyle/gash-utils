;;; Gash --- Guile As SHell
;;; Copyright © 2018 R.E.W. van Beusekom <rutger.van.beusekom@gmail.com>
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gash environment)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (gash io)

  #:export (
            %command-line
            %functions
            %global-variables
            assignment
            function
            set-shell-opt!
            shell-opt?
            variable
            variable-and
            variable-or
            ))

(define %command-line (make-parameter (command-line)))

;; FIXME: export/env vs set
(define %global-variables
  (map identity ;; FIXME: make mutable
       `(,(cons "SHELL" (car (command-line)))
         ,(cons "SHELLOPTS" "")
         ,(cons "PIPESTATUS" "([0]=\"0\"")
         ,(cons "?" "0")
         ,@(map (lambda (key-value)
                  (let* ((key-value (string-split key-value #\=))
                         (key (car key-value))
                         (value (cadr key-value)))
                    (cons key value)))
                (environ)))))

(define %functions '())

(define* (assignment name #:optional value)
  (if value
      (set! %global-variables
        (assoc-set! %global-variables name value))
      (set! %global-variables
        (assoc-set! %global-variables name "")))
  #t)

(define* (variable name #:optional (default ""))
  (cond ((string->number name)
         =>
         (lambda (n)
           (if (< n (length (%command-line))) (list-ref (%command-line) n)
               "")))
        ((equal? name "@")
         (string-join (cdr (%command-line))))
        ((equal? name "#")
         (number->string (length (cdr (%command-line)))))
        (else
         (or (assoc-ref %global-variables name)
             (if (shell-opt? "nounset") (begin
                                          ;; TODO: throw/error
                                          (format (current-error-port) "gash: ~a: unbound variable\n" name)
                                          #f)
                 default)))))

(define (variable-or name . default)
  (variable name (apply string-append default)))

(define (variable-and name . default)
  (let ((value (variable name #f)))
    (if value (apply string-append default) "")))

(define (set-shell-opt! name set?)
  (let* ((shell-opts (variable "SHELLOPTS"))
         (options (if (string-null? shell-opts) '()
                      (string-split shell-opts #\:)))
         (new-options (if set? (delete-duplicates (sort (cons name options) string<))
                          (filter (negate (cut equal? <> name)) options)))
         (new-shell-opts (string-join new-options ":")))
    (assignment "SHELLOPTS" new-shell-opts)))

(define (shell-opt? name)
  (member name (string-split (assoc-ref %global-variables "SHELLOPTS") #\:)))

(define (function name body)
  (set! %functions
        (assoc-set! %functions name body)))
