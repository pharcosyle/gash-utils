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

(define-module (gash commands cp)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (gash config)
  #:use-module (gash shell-utils)
  #:export (
            cp
            ))

(define (copy-file-force? force?)
  (lambda (src dest)
    (if (not force?) (copy-file src dest)
        (catch 'system-error
          (lambda _
            (copy-file src dest))
          (lambda (key func fmt msg errno . rest)
            (format #t "errno:~s\n" (car errno))
            (match errno
              ((13)
               (delete-file dest)
               (copy-file src dest))
              (_ (throw key func fmt msg errno))))))))

(define (cp name . args)
  (define (usage port)
    (display "Usage: cp [OPTION]... SOURCE... DEST

Options:
  -f, --force     if an existing destination file cannot be opened,
                    remove it and try again
  -h, --help      display this help and exit
  -V, --version   display version information and exit
" port))
  (match args
    (((or "-f" "--force") args ...)
     (apply cp (cons 'force args)))
    (((or "-h" "--help") t ...)
     (usage (current-output-port))
     (exit 0))
    (((or "-V" "--version") t ...)
     (format #t "cp (GASH) ~a\n" %version) (exit 0))
    ((source (and (? directory-exists?) dir))
     ((copy-file-force? (eq? name 'force))
      source (string-append dir "/" (basename source))))
    ((source dest)
     ((copy-file-force? (eq? name 'force)) source dest))
    ((sources ... dir)
     (unless (directory-exists? dir)
       (error (format #f "mv: target `~a' is not a directory\n" dir)))
     (for-each
      (copy-file-force? (eq? name 'force))
      sources
      (map (compose (cute string-append dir "/" <>) basename)
           sources)))
    (_ (usage (current-error-port)) (exit 2))))

(define main cp)
