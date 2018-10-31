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

(define-module (gash commands find)
  #:use-module (ice-9 getopt-long)

  #:use-module (gash config)
  #:use-module (gash io)
  #:use-module (gash shell-utils)

  #:export (
            find
            ))

(define (find . args)
  (let* ((option-spec
          '((help)
            (version)))
         (options (getopt-long args option-spec))
         (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
         (files (option-ref options '() '()))
         (files (if (null? files) '(".") files))
         (file (car files)))
    (when (> (length files) 1)
      (format (current-error-port) "find: too many FILEs: ~s\n" files)
      (error "find failed"))
    ;; TODO: find [OPTION]... [FILE]... [EXPRESSION]...
    ;; and options: esp: -x, -L
    (cond (version? (format #t "find (GASH) ~a\n" %version))
          (help? (display "Usage: find [OPTION]... [FILE]

Options:
  --help     display this help and exit
  --version  display version information and exit
"))
          (else
           (let* ((files (find-files file #:directories? #t #:fail-on-error? #t)))
             (for-each stdout files))))))

(define main find)
