;;; Gash-Utils
;;; Copyright © 2020 Timothy Sample <samplet@ngyro.com>
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

(define-module (gash commands env)
  #:use-module (gash commands config)
  #:use-module (gash compat)
  #:use-module (gash-utils options)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module ((srfi srfi-37) #:select (option-names))
  #:export (env))

(define *help-message* "\
Usage: env [OPTION]... [NAME=VALUE]... [UTILITY [ARGUMENT...]]
  -i, --ignore-environment  ignore the existing environment
  -h, --help            display this help
  -V, --version         display version
")

(define *version-message*
  (format #f "env (~a) ~a~%" %package-name %version))

(define *options-grammar*
  (make-options-grammar
   `((flag ignore-environment #\i)
     (message ("help" #\h) ,*help-message*)
     (message ("version" #\V) ,*version-message*))))

(define *option-strings*
  (map (match-lambda
         ((? char? chr) (string #\- chr))
         (str (string-append "--" str)))
       (append-map option-names
                   (options-grammar-options *options-grammar*))))

;; It would be nice if the regular 'parse-options' had something like
;; 'stop-at-first-non-option' from 'getopt-long'.  This is a hack to get
;; that behavior.  It mishandles '--', but so does GNU env.
(define (parse-options* args)
  (define (option? str)
    (and (string-prefix? "-" str)
         (not (string=? str "--"))))
  (match-let (((name . args) args))
    (receive (option-strings operands) (span option? args)
      (let ((options (parse-options (cons name option-strings)
                                    *options-grammar*)))
        (alist-cons '() operands options)))))

(define initial-name-chars
  (char-set-intersection
   char-set:ascii
   (char-set-union char-set:letter (char-set #\_))))

(define name-chars
  (char-set-intersection
   char-set:ascii
   (char-set-union char-set:letter+digit (char-set #\_))))

(define* (name? s #:optional (start 0) (end (string-length s)))
  (and (> end start)
       (char-set-contains? initial-name-chars (string-ref s start))
       (string-every name-chars s (1+ start) end)))

(define (binding? str)
  (and=> (string-index str #\=) (lambda (k) (name? str 0 k))))

(define (update-environ binding initial-environ)
  (let ((name= (substring binding 0 (1+ (string-index binding #\=)))))
    (append (remove (lambda (x) (string-prefix? name= x))
                    initial-environ)
            (list binding))))

(define (env . args)
  (let* ((options (parse-options* args))
         (ignore-environment? (assoc-ref options 'ignore-environment))
         (initial-environ (if ignore-environment? '() (environ))))
    (receive (bindings command)
        (span binding? (or (assoc-ref options '()) '()))
      (pk command)
      (let ((result-environ (fold update-environ initial-environ bindings)))
        (cond
         ((null? command)
          (for-each (lambda (binding) (format #t "~a~%" binding))
                    result-environ)
          EXIT_SUCCESS)
         (else
          (match-let (((utility . utility-args) command))
            (environ result-environ)
            (catch 'system-error
              (lambda ()
                (apply execlp utility utility utility-args))
              (lambda args
                (match (system-error-errno args)
                  (ENOENT (format (current-error-port) "env: ~a: ~a~%"
                                  utility (strerror ENOENT))
                          127)
                  (errno (format (current-error-port) "env: ~a: ~a~%"
                                 utility (strerror errno))
                         126)))))))))))

(define (main . args)
  (exit (apply env args)))
