;;; Gash-Utils
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

;;; Commentary:

;;; Code:

(define-module (gash commands basename)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 receive)
  #:use-module (gash commands config)
  #:export (basename))

(define (basename . args)
  (let* ((option-spec
	  '((multiple (single-char #\a))
            (help (single-char #\h))
            (version (single-char #\V))
            (suffix (single-char #\s) (value #t))
            (zero (single-char #\z))))
	 (options (getopt-long args option-spec))
	 (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
         (suffix (option-ref options 'suffix #f))
         (mutliple? (or suffix (option-ref options 'multiple #f)))
         (zero? (option-ref options 'zero #f))
         (files (option-ref options '() '()))
         (usage? (and (not help?) (null? files))))
    (cond (version? (format #t "basename (GASH) ~a\n" %version) (exit 0))
          ((or help? usage?) (format (if usage? (current-error-port) #t)
                                     "\
Usage: basename NAME [SUFFIX]
   or: basename OPTION... NAME...

Options:
  -a, --multiple          support multiple arguments and treat each as a NAME
      --help              display this help and exit
  -s, --suffix=SUFFIX     remove a trailing SUFFIX; implies -a
      --version           output version information and exit
  -z, --zero              end each output line with NUL, not newline
")
           (exit (if usage? 2 0)))
          (else
           (receive (files suffix)
               (if suffix (values files suffix)
                   (values (list-head files 1) (and (pair? (cdr files)) (cadr files))))
             (for-each (lambda (file)
                         (let ((file
                                (if (and (> (string-length file) 1)
                                         (string-suffix? "/" file)) (string-drop-right file 1)
                                         file)))
                           (cond ((string=? file "/") (display "/"))
                                 (suffix (display ((@ (guile) basename) file suffix)))
                                 (else (display ((@ (guile) basename) file)))))
                        (if zero? (display #\nul) (newline)))
                      files))))))

(define main basename)
