;;; Gash -- Guile As SHell
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

;;; Commentary:

;;; Code:

(define-module (gash commands tr)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 rdelim)
  #:use-module (gash config)
  #:export (
            tr
            ))

(define (string-replace-string string from to)
  (cond ((string-contains string from)
         =>
         (lambda (i)
           (string-replace string to i (+ i (string-length from)))))
        (else string)))

(define (tr . args)
  (let* ((option-spec
	  '((delete (single-char #\d))
            (help (single-char #\h))
            (version (single-char #\V))))
	 (options (getopt-long args option-spec))
         (delete? (option-ref options 'delete #f))
         (files (option-ref options '() '()))
	 (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
         (usage? (and (not help?) (not (or (and delete? (= (length files) 1))
                                           (= (length files) 2))))))
    (cond (version? (format #t "tr (GASH) ~a\n" %version) (exit 0))
          ((or help? usage?) (format (if usage? (current-error-port) #t)
                                     "\
Usage: tr [OPTION]... SET1 [SET2]

Options:
  -d, --delete    delete characters in SET1, do not translate
  -h, --help      display this help and exit
  -V, --version   display version information and exit
")
           (exit (if usage? 2 0)))
          (delete?
           (let* ((s (car files))
                  (s (string-replace-string s "\\n" "\n"))
                  (s (string-replace-string s "\\r" "\r"))
                  (s (string-replace-string s "\\t" "\t"))
                  (s (string->char-set s)))
             (let loop ((line (read-line (current-input-port) 'concat)))
               (if (eof-object? line) #t
                   (begin
                     (display (string-delete s line))
                     (loop (read-line (current-input-port) 'concat)))))))
          (else
           (format #t "TODO: TR A B\n")))))

(define main tr)
