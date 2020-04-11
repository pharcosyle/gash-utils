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

(define-module (gash-utils options)
  #:use-module (gash compat)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-37)
  #:export (<options-grammar>
            options-grammar?
            make-options-grammar
            options-grammar-options
            options-grammar-default
            parse-options))

;;; Commentary:
;;;
;;; This module provides mostly declarative command line option
;;; processing.  Guile already has SRFI 37 and 'getopt-long', but
;;; neither of these quite do what we need.  SRFI 37 is too low-level,
;;; and 'getopt-long' does not support some of the POSIX option styles
;;; (such as mutually overriding options).
;;;
;;; Code:

(define-record-type <options-grammar>
  (%make-options-grammar options default)
  options-grammar?
  (options options-grammar-options)
  (default options-grammar-default))

(define (option-description->option option-description)
  "Convert @var{option-description} to a SRFI 37 option.  See
@code{make-options-grammar} for a detailed description of an option
description."
  (match option-description
    (('option . args)
     (apply option args))
    (('flag key . aliases)
     (option (cons (symbol->string key) aliases) #f #f
             (lambda (opt name arg result)
               (alist-cons key #t result))))
    (('value key . aliases)
     (option (cons (symbol->string key) aliases) #t #f
             (lambda (opt name arg result)
               (alist-cons key arg result))))
    (('toggle key ((alias-lists ...) values) ...)
     (option (concatenate alias-lists) #f #f
             (lambda (opt name arg result)
               (let loop ((alias-lists alias-lists) (values values))
                 (match-let (((aliases . next-alias-lists) alias-lists)
                             ((value . next-values) values))
                   (if (member name aliases)
                       (alist-cons key value result)
                       (loop next-alias-lists next-values)))))))
    (('list key . aliases)
     (option (cons (symbol->string key) aliases) #t #f
             (lambda (opt name arg result)
               (let ((tail (or (assoc-ref result key) '())))
                 (alist-cons key (cons arg tail) result)))))
    (('message names message)
     (option names #f #f (lambda _ (display message) (exit #t))))))

(define* (make-options-grammar option-descriptions #:key (default '()))
  "Create an options grammar according to @var{option-descriptions}.

Each option description should be one of the following:

@itemize

@item @code{(flag key . aliases)} --- Accept a flag named @var{key} or
any of @var{aliases}, and store the result under @var{key}.

@item @code{(value key . aliases)} --- Accept a single value named
@var{key} or any of @var{aliases} and store the result under
@var{key}.

@item @code{(list key . aliases)} --- Accept a list of values named
@var{key} or any of @var{aliases} and store the result under
@var{key}.

@item @code{(toggle key ((names ...) value) ...)} --- Accept a set of
mutually overriding flags.  For each pair of @var{names} and
@var{value}, a flag matching one of @var{names} will update the result
under @var{key} to @var{value}.

@item @code{(option . args)} --- Construct an option by passing
@var{args} to @code{option} from SRFI 37.

@end itemize

If the keyword @var{default} is a procedure, it will be used to handle
anonymous (positional) options.  If it is anything else, it will be
used as the key under which to store anonymous options.  By default,
it is the value @code{'()}."
  (%make-options-grammar
   (map option-description->option option-descriptions)
   (match default
     ((? procedure?) default)
     (_ (lambda (arg result)
          (let ((operands (or (assoc-ref result default) '())))
            (alist-cons default (append operands (list arg)) result)))))))

(define (%parse-options args grammar)
  "Parse @var{args} according to the options grammar @var{grammar}."
  (match-let (((program . args) args)
              (($ <options-grammar> options default) grammar))
    (args-fold args options
               (lambda (opt name arg result)
                 (format (current-error-port)
                         "~a: no such option: -~a~%"
                         program (if (string? name)
                                     (string-append "-" name)
                                     name))
                 (exit 2))
               default
               '())))


;;; This next section is just routing around a bug in older versions
;;; of Guile.  Before version 2.2.2, SRFI 37 could not handle null
;;; arguments (cf. <https://bugs.gnu.org/26013>).

(define (make-sentinel args)
  (match-let (((_ . args) args))
    (let loop ((k 0))
      (let ((sentinel (format #f "GASH-UTILS-NULL-~a" k)))
        (if (member sentinel args)
            (loop (1+ k))
            sentinel)))))

(define (fix-grammar-for-null-args grammar sentinel)
  (match-let ((($ <options-grammar> options default) grammar))
    (%make-options-grammar
     (map (lambda (opt)
            (let ((names (option-names opt))
                  (required-arg? (option-required-arg? opt))
                  (optional-arg? (option-optional-arg? opt))
                  (processor (option-processor opt)))
              (option names required-arg? optional-arg?
                      (lambda (fixed-opt name arg . args)
                        (if (equal? arg sentinel)
                            (apply processor opt name "" args)
                            (apply processor opt name arg args))))))
          options)
     (lambda (arg . args)
       (if (string=? arg sentinel)
           (apply default "" args)
           (apply default arg args))))))

(if-guile-version-below (2 2 2)
  (define (parse-options args grammar)
    (let* ((sentinel (make-sentinel args))
           (grammar* (fix-grammar-for-null-args grammar sentinel)))
      (%parse-options (map (match-lambda ("" sentinel) (arg arg))
                           args)
                      grammar*)))
  (define parse-options %parse-options))
