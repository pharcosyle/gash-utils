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
  #:use-module (srfi srfi-9)
  #:export (<conversion-adapter>
            make-conversion-adapter
            conversion-adapter?
            parse-file-format
            fold-file-format))

;;; Commentary:
;;;
;;; This module provides an implementation of the POSIX File Format
;;; Notation.
;;;
;;; Code:

(define-record-type <conversion>
  (%make-conversion type proc width?)
  conversion?
  (type conversion-type)
  (proc conversion-proc)
  (width? conversion-width?))

(define* (make-conversion type proc #:optional width?)
  (%make-conversion type proc width?))

(define-record-type <conversion-adapter>
  (make-conversion-adapter to-string to-number to-character)
  conversion-adapter?
  (to-string conversion-to-string)
  (to-number conversion-to-number)
  (to-character conversion-to-character))

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
      (#\c (values (make-conversion 'character string) 1))
      (#\d (values (make-conversion 'number number->string) 1))
      (#\s (values (make-conversion 'string values) 1))
      (#\u (values (make-conversion 'number number->string) 1))
      (_ (error "unknown conversion specifier")))))

(define* (parse-file-format s #:optional (start 0) (end (string-length s))
                            #:key (escaped? #t))
  (define stop-chars (if escaped? (char-set #\\ #\%) #\%))
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

(define (fold-file-format adapter seed parts . args)
  (match-let ((($ <conversion-adapter>
                  to-string to-number to-character) adapter))
    (let loop ((parts parts) (args args) (seed seed) (acc '()))
      (match parts
        (() (values (string-concatenate-reverse acc) seed))
        (((? string? str) . parts*)
         (loop parts* args seed (cons str acc)))
        ((($ <conversion> type proc width?) . parts*)
         (match (cons type args)
           (('string)
            (loop parts* args seed (cons (proc "") acc)))
           (('number)
            (loop parts* args seed (cons (proc 0) acc)))
           (('character)
            (loop parts* args seed (cons (proc #\nul) acc)))
           (('string arg . args*)
            (receive (value seed) (to-string arg seed)
              (loop parts* args* seed (cons (proc value) acc))))
           (('number arg . args*)
            (receive (value seed) (to-number arg seed)
              (loop parts* args* seed (cons (proc value) acc))))
           (('character arg . args*)
            (receive (value seed) (to-character arg seed)
              (loop parts* args* seed (cons (proc value) acc))))))))))
