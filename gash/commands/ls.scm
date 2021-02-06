;;; Gash-Utils
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

;;; The initial bournish.scm was taken from Guix.

;;; Code:

(define-module (gash commands ls)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (gash commands config)
  #:use-module (gash io)
  #:use-module (gash shell-utils)
  #:use-module (gash-utils options)
  #:export (ls))

(define *help-message* "\
Usage: ls [OPTION]... [FILE]...

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
")

(define *version-message*
  (format #f "ls (~a) ~a~%" %package-name %version))

(define *options-grammar*
  (make-options-grammar
   `((flag all #\a)
     (flag classify #\F)
     (flag directory #\d)
     (flag dereference #\L)
     (flag inode #\i)
     (flag long #\l)
     (flag one-file-per-line #\1)
     (flag reverse #\r)
     (flag sort-by-modification-time #\t)
     (message ("help" #\h) ,*help-message*)
     (message ("version" #\V) ,*version-message*))
   #:default 'files))

(define (file-modification-time<? a b)
  (cond ((= (stat:mtime (cdr a)) (stat:mtime (cdr b)))
         (< (stat:mtimensec (cdr a)) (stat:mtimensec (cdr b))))
        (else
         (< (stat:mtime (cdr a)) (stat:mtime (cdr b))))))

(define (ls . args)
  (let* ((options (parse-options args *options-grammar*))
         (all? (assoc-ref options 'all))
         (classify? (assoc-ref options 'classify))
         (directory? (assoc-ref options 'directory))
         (inode? (assoc-ref options 'inode))
         (long? (assoc-ref options 'long))
         (one-file-per-line? (assoc-ref options 'one-file-per-line))
         (reverse? (assoc-ref options 'reverse))
         (sort-by-modification-time? (assoc-ref options
                                                'sort-by-modification-time))
         (files (or (assoc-ref options 'files) '()))
         (follow-links? (not directory?)))
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
                                                               `(,f . ,(stat* (string-append file "/" f))))
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
                                         (not (string-prefix? "./" file))
                                         (not (string-prefix? "../" file))))))
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
                                     files)))))))

(define main ls)
