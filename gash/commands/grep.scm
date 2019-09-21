;;; Gash --- Guile As SHell
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018 Timothy Sample <samplet@gnu.org>
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

;;; Commentary:

;;; Code:

(define-module (gash commands grep)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)

  #:use-module (gash commands config)
  #:use-module (gash compat)
  #:use-module (gash io)
  #:use-module (gash util)
  #:use-module (gash shell-utils)

  #:export (
            grep
            ))

(define* (flag name #:optional single-char)
  (option (if single-char
              (list (symbol->string name) single-char)
              (list (symbol->string name)))
          #f #f
          (lambda (opt name* arg result)
            (alist-cons name #t result))))

(define *options-spec*
  (list (flag 'help)
        (flag 'line-number #\n)
        (flag 'count #\c)
        (flag 'files-with-matches #\l)
        (flag 'files-without-match #\L)
        (flag 'with-file-name #\H)
        (flag 'no-file-name #\h)
        (flag 'only-matching #\o)
        (flag 'quiet #\q)
        (flag 'silent)
        (flag 'version #\V)
        (flag 'invert-match #\v)
        (option '("extended-regexp" #\E) #f #f
                (lambda (opt name arg result)
                  (alist-cons 'matching 'extended result)))
        (option '("regexp" #\e) #t #f
                (lambda (opt name arg result)
                  (alist-cons 'regexp arg result)))
        (option '("fixed-strings" #\F) #f #f
                (lambda (opt name arg result)
                  (alist-cons 'matching 'string result)))
        (option '("basic-regexp" #\G) #f #f
                (lambda (opt name arg result)
                  (alist-cons 'matching 'basic result)))))

(define (get-options args spec)
  (args-fold (cdr args) spec
             (lambda (opt name arg result)
               (format (current-error-port)
                       "~a: no such option: -~a~%"
                       (car args) (if (string? name)
                                      (string-append "-" name)
                                      name))
               (exit EXIT_FAILURE))
             (lambda (arg result)
               (if (assq 'regexp result)
                   (alist-cons 'input-file arg result)
                   (alist-cons 'regexp arg result)))
             '()))

(define (option-ref options key dflt)
  (or (and=> (assq key options) cdr) dflt))

(define (option-ref/list options key)
  (filter-map (match-lambda
                (((? (cut eq? <> key)) . v) v)
                (_ #f))
              options))

(define (grep . args)
  (let* ((options (get-options args *options-spec*))
         (matching (option-ref options 'matching 'basic))
         (inverted? (option-ref options 'invert-match #f))
         (regexps (option-ref/list options 'regexp))
         (quiet? (option-ref options 'quiet #f))
         (silent? (option-ref options 'silent #f))

         (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
         (files (option-ref/list options 'input-file)))
    (cond (version? (format #t "grep (GASH) ~a\n" %version))
          (help? (display "Usage: grep [OPTION]... PATTERN [FILE]...

Options:
  -h, --no-filename          suppress the file name prefix on output
  -H, --with-filename        print file name with output lines
  -l, --files-with-matches   print only names of FILEs with selected lines
  -L, --files-without-match  print only names of FILEs with no selected lines
  -n, --line-number          print line number with output lines
  -o, --only-matching        show only the part of a line matching PATTERN
  -q, --quiet, --silent      suppress all normal output
  --help                     display this help and exit
  -V, --version              display version information and exit
"))
          ((null? regexps) #t)
          (else
           (let* ((patterns regexps)
                  (files (if (pair? files) files
                             (list "-")))
                  (matches (append-map (cut grep+ patterns <>
                                            #:matching matching
                                            #:inverted? inverted?)
                                       files)))
             (define (display-match o)
               (let* ((s (grep-match-string o))
                      (s (if (option-ref options 'only-matching #f)
                             (substring s (grep-match-column o) (grep-match-end-column o))
                             s))
                      (s (if (option-ref options 'line-number #f)
                             (string-append (number->string (grep-match-line o)) ":" s)
                             s))
                      (s (if (option-ref options 'with-file-name #f)
                             (string-append (grep-match-file-name o) ":" s)
                             s)))
                 (stdout s)))
             (define (files-with-matches)
               (delete-duplicates (map grep-match-file-name matches)))
             (cond ((option-ref options 'count #f)
                    (unless (or quiet? silent?)
                      (format #t "~a~%" (length matches)))
                    0)
                   ((option-ref options 'files-with-matches #f)
                    (let ((result (files-with-matches)))
                      (and (pair? result)
                           (for-each stdout result)
                           0)))
                   ((option-ref options 'files-without-match #f)
                    (let* ((result (files-with-matches))
                           (result (filter (negate (cut member <> result)) files)))
                      (and (pair? result)
                           (for-each stdout result)
                           0)))
                   (else
                    (and (pair? matches)
                         (unless (or quiet? silent?)
                           (for-each display-match matches))
                         0))))))))

(define main grep)
