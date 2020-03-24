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
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-9))

(define-syntax-rule (invalid-options proc msg args ...)
  (scm-error 'invalid-options proc msg (list args ...) '()))

(define* (parse-escaped-string s #:optional (start 0)
                               (end (string-length s)))
  (let loop ((k start) (acc '()))
    (match (and (< k end) (string-ref s k))
      ((or #f #\%) (values (reverse-list->string acc) (- k start)))
      (#\\ (let ((j (1+ k)))
             (match (and (< j end) (string-ref s j))
               (#\\ (loop (1+ j) (cons #\\ acc)))
               (#\a (loop (1+ j) (cons #\alarm acc)))
               (#\b (loop (1+ j) (cons #\backspace acc)))
               (#\f (loop (1+ j) (cons #\page acc)))
               (#\n (loop (1+ j) (cons #\newline acc)))
               (#\r (loop (1+ j) (cons #\return acc)))
               (#\t (loop (1+ j) (cons #\tab acc)))
               (#\v (loop (1+ j) (cons #\vtab acc)))
               (_ (loop j (cons #\\ acc))))))
      (chr (loop (1+ k) (cons chr acc))))))

(define* (parse-conversion-specifier s #:optional (start 0)
                                     (end (string-length s)))
  (let loop ((k start) (acc '()))
    (match (and (< k end) (string-ref s k))
      (#f (error "missing format character"))
      (#\% (values "%" 1))
      (#\s (values (lambda (x) (or x "")) 1))
      (_ (error "unknown conversion specifier")))))

(define* (parse-format-string s #:optional (start 0)
                              (end (string-length s)))
  (let loop ((k start) (acc '()))
    (match (and (< k end) (string-ref s k))
      (#f (reverse acc))
      (#\% (receive (part length)
               (parse-conversion-specifier s (1+ k) end)
             (loop (+ k length 1) (cons part acc))))
      (chr (receive (part length) (parse-escaped-string s k end)
             (loop (+ k length) (cons part acc)))))))

(define (eval-format-string parts . args)
  (let loop ((parts parts) (args args) (acc '()))
    (match parts
      (() (string-concatenate-reverse acc))
      (((? string? str) . parts*)
       (loop parts* args (cons str acc)))
      (((? procedure? proc) . parts*)
       (match args
         (() (loop parts* args (cons (proc #f) acc)))
         ((arg . args*) (loop parts* args* (cons (proc arg) acc))))))))

(define (printf format-string . args)
  (let* ((format (parse-format-string format-string))
         (result (apply eval-format-string format args)))
    (display result)))

(define (main . args)
  (let ((name (match args ((name . _) name) (_ "???")))
        (args (if (null? args) args (cdr args))))
    (catch #t
      (lambda ()
        (match args
          (() (invalid-options "printf" "Invalid options: ~s" args))
          ((format . args)
           (apply printf format args)
           (primitive-exit 0))))
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
