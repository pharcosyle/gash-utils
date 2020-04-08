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

(define-module (gash-utils file-formats)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:export (parse-file-format
            eval-file-format))

(define* (parse-escape s #:optional (start 0) (end (string-length s)))
  (match (and (< start end) (string-ref s start))
    (#\\ (values (string #\\) 1))
    (#\a (values (string #\alarm) 1))
    (#\b (values (string #\backspace) 1))
    (#\f (values (string #\page) 1))
    (#\n (values (string #\newline) 1))
    (#\r (values (string #\return) 1))
    (#\t (values (string #\tab) 1))
    (#\v (values (string #\vtab) 1))
    (_ (values (string #\\) 0))))

(define* (parse-conversion s #:optional (start 0) (end (string-length s)))
  (let loop ((k start) (acc '()))
    (match (and (< k end) (string-ref s k))
      (#f (error "missing format character"))
      (#\% (values "%" 1))
      (#\s (values (lambda (x) (or x "")) 1))
      (_ (error "unknown conversion specifier")))))

(define* (parse-file-format s #:optional (start 0) (end (string-length s)))
  (define stop-chars (char-set #\\ #\%))
  (let loop ((k start) (acc '()))
    (match (string-index s stop-chars k end)
      (#f (reverse (if (< k end)
                       (cons (substring s k end) acc)
                       acc)))
      (j (let ((acc (if (> j k) (cons (substring s k j) acc) acc)))
           (match (string-ref s j)
             (#\% (receive (part length) (parse-conversion s (1+ j) end)
                    (loop (+ j length 1) (cons part acc))))
             (#\\ (receive (part length) (parse-escape s (1+ j) end)
                    (loop (+ j length 1) (cons part acc))))))))))

(define (eval-file-format parts . args)
  (let loop ((parts parts) (args args) (acc '()))
    (match parts
      (() (string-concatenate-reverse acc))
      (((? string? str) . parts*)
       (loop parts* args (cons str acc)))
      (((? procedure? proc) . parts*)
       (match args
         (() (loop parts* args (cons (proc #f) acc)))
         ((arg . args*) (loop parts* args* (cons (proc arg) acc))))))))