;;; Gash-Utils
;;; Copyright © 2020, 2022 Timothy Sample <samplet@ngyro.com>
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
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module ((system foreign) #:select (sizeof unsigned-long))
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
  (%make-conversion type proc)
  conversion?
  (type conversion-type)
  (proc conversion-proc))

(define-record-type <conversion-adapter>
  (make-conversion-adapter to-string to-number to-character)
  conversion-adapter?
  (to-string conversion-to-string)
  (to-number conversion-to-number)
  (to-character conversion-to-character))

(define *unsigned-modulus* (expt 256 (sizeof unsigned-long)))

(define (flags-set? flag flags)
  "Check if @var{flag} is in @var{flags}."
  (any (lambda (f) (char=? f flag)) flags))

(define (flags-delete flag flags)
  "Remove @var{flag} from @var{flags}."
  (remove (lambda (f) (char=? f flag)) flags))

(define (ensure-width s prefix width flags)
  "Pad the string @var{s} such that the total length of the padding,
@var{prefix}, and @var{s} is at least @var{width}.  The value of
@var{flags} controls whether the padding will be zero or space as well
as whether the padding comes before or after the prefix."
  (let ((delta (- width (+ (string-length s) (string-length prefix)))))
    (if (positive? delta)
        (cond
         ((flags-set? #\- flags)
          (string-append prefix s (make-string delta #\space)))
         ((flags-set? #\0 flags)
          (string-append prefix (make-string delta #\0) s))
         (else
          (string-append (make-string delta #\space) prefix s)))
        (string-append prefix s))))

(define (integer-conversion string-proc prefix-proc flags width precision)
  "Create a conversion record for formatting integers.  The conversion
will use @var{string-proc} to convert the integer to a string, and
@var{prefix-proc} to compute a prefix from the integer.  The string will
have at least @var{precision} digits and will be at least @var{width}
long, with the padding style determined by @var{flags}."
  (define (ensure-integer-precision n s)
    (let* ((precision (or precision 1))
           (delta (- precision (string-length s))))
      (cond
       ((and (zero? n) (zero? precision)) "")
       ((positive? delta) (string-append (make-string delta #\0) s))
       (else s))))

  (%make-conversion
   'number
   (lambda (n)
     (let* ((s (string-proc n))
            (s (ensure-integer-precision n s))
            (p (prefix-proc n))
            (flags (if precision (flags-delete #\0 flags) flags)))
       (ensure-width s p width flags)))))

(define (make-conversion specifier flags width precision)
  "Create a conversion record for the specifier character
@var{specifier}, with the options @var{flags}, @var{width}, and
@var{precision}."
  (match specifier
    (#\c
     (%make-conversion 'character string))
    ((or #\d #\i)
     (integer-conversion (compose number->string abs)
                         (lambda (n)
                           (cond
                            ((negative? n) "-")
                            ((flags-set? #\+ flags) "+")
                            ((flags-set? #\space flags) " ")
                            (else "")))
                         flags width precision))
    (#\s
     (%make-conversion
      'string
      (lambda (s)
        (let ((s (if precision (string-take s precision) s)))
          (ensure-width s "" width (flags-delete #\0 flags))))))
    (#\u
     (integer-conversion (compose number->string
                                  (cut modulo <> *unsigned-modulus*))
                         (const "") flags width precision))
    ((or #\x #\X)
     (integer-conversion (compose (if (char=? specifier #\X)
                                      string-upcase
                                      identity)
                                  (cut number->string <> 16)
                                  (cut modulo <> *unsigned-modulus*))
                         (lambda _
                           (if (flags-set? #\# flags)
                               (if (char=? specifier #\X) "0X" "0x")
                               ""))
                         flags width precision))
    (_ (error "unsupported format conversion" specifier))))

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

(define* (parse-conversion-flags s #:optional
                                 (start 0) (end (string-length s)))
  (let loop ((k start) (acc '()))
    (match (and (< k end) (string-ref s k))
      ((and flag (or #\- #\+ #\space #\# #\0)) (loop (1+ k) (cons flag acc)))
      (_ (values (reverse acc) (- k start))))))

(define* (parse-conversion-number s #:optional
                                  (start 0) (end (string-length s)))
  (let*-values (((s*) (substring s start end))
                ((result size) (locale-string->integer s*)))
    (values (or result 0) size)))

(define parse-conversion-width parse-conversion-number)

(define* (parse-conversion-precision s #:optional
                                     (start 0) (end (string-length s)))
  (match (and (< start end) (string-ref s start))
    (#\. (let-values (((n size) (parse-conversion-number s (1+ start) end)))
           (values n (1+ size))))
    (_ (values #f 0))))

(define conversion-specifier?
  (let ((conversion-specifiers "aAdiouxXfFeEgGcs%"))
    (lambda (chr)
      (string-index conversion-specifiers chr))))

(define* (parse-conversion-specifier s #:optional
                                     (start 0) (end (string-length s)))
  (match (and (< start end) (string-ref s start))
    (#f (error "missing conversion specifier character"))
    ((and (? conversion-specifier?) chr) (values chr 1))
    (chr (error "invalid conversion specifier character" chr))))

(define* (parse-conversion s #:optional (start 0) (end (string-length s)))
  (match (and (< start end) (string-ref s start))
    (#\% (values "%" 1))
    (_ (let*-values (((flags n)     (parse-conversion-flags s start end))
                     ((k)           (+ start n))
                     ((width n)     (parse-conversion-width s k end))
                     ((k)           (+ k n))
                     ((precision n) (parse-conversion-precision s k end))
                     ((k)           (+ k n))
                     ((specifier n) (parse-conversion-specifier s k end))
                     ((k)           (+ k n)))
         (values (make-conversion specifier flags width precision)
                 (- k start))))))

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
        ((($ <conversion> type proc) . parts*)
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
