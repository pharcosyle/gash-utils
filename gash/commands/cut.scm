;;; Gash -- Guile As SHell
;;; Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gash commands cut)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (gash config)
  #:use-module (gash shell-utils)

  #:export (
            cut
            ))

(define (cut-fields inport outport delimiter fields)
  (let ((dstring (make-string 1 delimiter)))
    (let loop ((line (read-line inport)))
      (unless (eof-object? line)
        (let* ((split (string-split line delimiter))
               (count (length split))
               (show (let loop ((fields fields))
                       (if (null? fields) '()
                           (let ((field (car fields)))
                             (if (>= field count) (loop (cdr fields))
                                 (cons (list-ref split field)
                                       (loop (cdr fields)))))))))
          (display (string-join show dstring) outport)
          (newline outport)
          (loop (read-line inport)))))))

(define %LINE-MAX 80)
(define (cut-ranges inport outport ranges)
  (define (range->set o)
    (match o
      (((and (? number?) from) (and (? number?) to))
       (iota (1+ (- to from)) from))
      (((and (? number?) from) #f)
       (iota %LINE-MAX from))
      ((#f (and (? number?) to))
       (iota to 1))))
  (let* ((sets (map range->set ranges))
         (union (sort (apply lset-union (cons = sets)) <))
         (indices (map 1- union)))
    (let loop ((line (read-line inport)))
      (unless (eof-object? line)
        (let* ((len (string-length line))
               (show (map (cute string-ref line <>)
                          (take-while (cute < <> len) indices))))
          (display (list->string show) outport)
          (newline outport)
          (loop (read-line inport)))))))

(define (cut . args)
  (let* ((option-spec
	  '((bytes (single-char #\b) (value #t))
            (characters (single-char #\c) (value #t))
            (delimiter (single-char #\d) (value #t))
            (fields (single-char #\f) (value #t))
            (ignored-n (single-char #\n))

            (help (single-char #\h))
            (version (single-char #\V))))
	 (options (getopt-long args option-spec))
         (bytes (option-ref options 'bytes #f))
         (characters (option-ref options 'characters #f))
         (delimiter (option-ref options 'delimiter "\t"))
         (fields (option-ref options 'fields #f))

	 (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
	 (files (option-ref options '() '()))
         (usage? (and (not help?) (not (= 1 (length (filter
                                                     identity
                                                     (list bytes characters fields))))))))
    (cond (version? (format #t "cut (GASH) ~a\n" %version) (exit 0))
          ((or help? usage?) (format (if usage? (current-error-port) #t)
                                     "\
Usage: cut OPTION... [FILE]...
Print selected parts of lines from each FILE to standard output.

With no FILE, or when FILE is -, read standard input.

Options:
  -b, --bytes=LIST        select only these bytes
  -c, --characters=LIST   select only these characters
  -d, --delimiter=DELIM   use DELIM instead of TAB for field delimiter
  -f, --fields=LIST       select only these fields;  also print any line
                            that contains no delimiter character
  -n                      (ignored)
      --help              display this help and exit
      --version           output version information and exit
")
           (exit (if usage? 2 0)))
          (else
           (let ((files (if (pair? files) files
                            (list "-")))
                 (ranges (or bytes characters))
                 (outport (current-output-port)))
             (for-each
              (lambda (file)
                (let ((inport (if (equal? file "-") (current-input-port)
                                  (and (or (file-exists?* file) (exit 2))
                                       (open-input-file file)))))
                  (cond (ranges (let ((ranges (map (compose (cute map string->number <>)
                                                            (cute string-split <> #\-))
                                                   (string-split ranges #\,))))
                                  (cut-ranges inport outport ranges)))
                        (fields (let ((delimiter (car (string->list delimiter)))
                                      (fields (map (compose 1- string->number)
                                                   (string-split fields #\,))))
                                  (cut-fields inport outport delimiter fields))))))
              files))))))

(define main cut)
