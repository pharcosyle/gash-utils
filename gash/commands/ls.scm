;;; Gash --- Guile As SHell
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

(define (file-modification-time<? a b)
  (cond ((= (stat:mtime (cdr a)) (stat:mtime (cdr b)))
         (< (stat:mtimensec (cdr a)) (stat:mtimensec (cdr b))))
        (else
         (< (stat:mtime (cdr a)) (stat:mtime (cdr b))))))

(define (ls . args)
  (let* ((option-spec
          '((all (single-char #\a))
            (classify (single-char #\F))
            (directory (single-char #\d))
            (dereference (single-char #\L))
            (inode (single-char #\i))
            (long (single-char #\l))
            (one-file-per-line (single-char #\1))
            (reverse (single-char #\r))
            (sort-by-modification-time (single-char #\t))

            (help)
            (version)))
         (options (getopt-long args option-spec))
         (all? (option-ref options 'all #f))
         (classify? (option-ref options 'classify #f))
         (directory? (option-ref options 'directory #f))
         (inode? (option-ref options 'inode #f))
         (long? (option-ref options 'long #f))
         (one-file-per-line? (option-ref options 'one-file-per-line #f))
         (reverse? (option-ref options 'reverse #f))
         (sort-by-modification-time? (option-ref options 'sort-by-modification-time #f))

         (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
         (files (option-ref options '() '()))
         (follow-links? (not directory?)))
    (cond (version? (format #t "ls (GASH) ~a\n" %version))
          (help? (display "Usage: ls [OPTION]... [FILE]...

Options:
  -a, --all          do not ignore entries starting with .
      --help         display this help and exit
  -F, --classify     append indicator (one of */=>@|) to entries
  -l, --long         use a long listing format
  -L, --dereference  ignored to support configure scripts
      --version      display version information and exit
  -r, --reverse      reverse order while sorting
  -t                 sort by modification time, newest first
  -1                 list one file per line
"))
          (else
           (let* ((stat* (if follow-links? stat lstat))
                  (files (if (null? files)
                             (if directory?
                                 (list `("." . ,(stat* ".")))
                                 (map (lambda (filename)
                                        `(,filename . ,(stat* filename)))
                                      (scandir ".")))
                             (append-map (lambda (file)
                                           (catch 'system-error
                                             (lambda ()
                                               (match (stat:type (stat* file))
                                                 ('directory
                                                  (if directory?
                                                      (list `(,file . ,(stat* file)))
                                                      (match (scandir file)
                                                        (#f (list `(,file . ,(stat* file))))
                                                        (files (map (lambda (f)
                                                                      `(,f . ,(stat* f)))
                                                                    files)))))
                                                 (_ (list `(,file . ,(stat* file))))))
                                             (lambda args
                                               (let ((errno (system-error-errno args)))
                                                 (format (current-error-port) "~a: ~a~%"
                                                         file (strerror errno))
                                                 '()))))
                                         files)))
                  (dot-file? (lambda (x) (let ((file (car x)))
                                           (and (string-prefix? "." file)
                                                (or (string=? file ".")
                                                    (not (eq? (string-ref file 1) #\/)))))))
                  (files (if (or all? directory?) files
                             (filter (negate dot-file?) files)))
                  (files (if (not sort-by-modification-time?) files
                             (reverse (sort files file-modification-time<?))))
                  (files (if (not reverse?) files
                             (reverse files))))
             (cond (long?
                    (for-each (match-lambda
                                ((f . st)
                                 (when inode?
                                   (format #t "~a " (stat:ino st)))
                                 (display-file f #:classify? classify?)
                                 (newline)))
                              files))
                   (one-file-per-line?
                    (for-each (match-lambda
                                ((f . st)
                                 (when inode?
                                   (format #t "~a " (stat:ino st)))
                                 (display f)
                                 (when classify?
                                   (display (file-class st)))
                                 (newline)))
                              files))
                   (else
                    (display-tabulated (map (match-lambda
                                              ((f . st)
                                               (cond (inode?
                                                      (format #f "~a ~a" (stat:ino st) f))
                                                     (classify?
                                                      (format #f "~a~a" f (file-class st)))
                                                     (else f))))
                                            files)))))))))

(define main ls)
