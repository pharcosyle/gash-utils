;;; Gash -- Guile As SHell
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

;;; The initial bournish.scm was taken from Guix.

;;; Code:

(define-module (gash commands ls)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (gash config)
  #:use-module (gash io)
  #:use-module (gash shell-utils)

  #:export (
            ls
            ))

(cond-expand
 (guile
  ;; Support -1, see https://lists.gnu.org/archive/html/bug-guile/2018-07/msg00009.html
  (module-define! (resolve-module '(ice-9 getopt-long)) 'short-opt-rx (make-regexp "^-([a-zA-Z0-9]+)(.*)")))
 (else))

(define (ls . args)
  (let* ((option-spec
          '((all (single-char #\a))
            (help)
            (long (single-char #\l))
            (one-file-per-line (single-char #\1))
            (version)))
         (options (getopt-long args option-spec))
         (all? (option-ref options 'all #f))
         (help? (option-ref options 'help #f))
         (long? (option-ref options 'long #f))
         (one-file-per-line? (option-ref options 'one-file-per-line #f))
         (version? (option-ref options 'version #f))
         (files (option-ref options '() '())))
    (cond (version? (format #t "ls (GASH) ~a\n" %version))
          (help? (display "Usage: ls [OPTION]... [FILE]...

Options:
  -a, --all      do not ignore entries starting with .
      --help     display this help and exit
  -l, --long     use a long listing format
      --version  display version information and exit
  -1             list one file per line
"))
          (else
           (let* ((files (if (null? files) (scandir ".")
                             (append-map (lambda (file)
                                           (catch 'system-error
                                             (lambda ()
                                               (match (stat:type (lstat file))
                                                 ('directory
                                                  ;; Like GNU ls, list the contents of
                                                  ;; FILE rather than FILE itself.
                                                  (match (scandir file
                                                                  (match-lambda
                                                                    ((or "." "..") #f)
                                                                    (_ #t)))
                                                    (#f
                                                     (list file))
                                                    ((files ...)
                                                     (map (cut string-append file "/" <>)
                                                          files))))
                                                 (_
                                                  (list file))))
                                             (lambda args
                                               (let ((errno (system-error-errno args)))
                                                 (format (current-error-port) "~a: ~a~%"
                                                         file (strerror errno))
                                                 '()))))
                                         files)))
                  (files (if all? files
                             (filter (negate (cut string-prefix? "." <>)) files))))
             (cond (long? (for-each (lambda (f) (display-file f) (newline)) files))
                   (one-file-per-line? (for-each stdout files))
                   (else (display-tabulated files))))))))

(define main ls)
