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

(define-module (gash commands rm)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (gash shell-utils)
  #:export (
            rm
            ))

(define (rm name . args)
  (let ((recursive? (or (member "-r" args)
                        (member "-fr" args)
                        (member "-rf" args)))
        (force? (or (member "-f" args)
                    (member "-rf" args)
                    (member "-fr" args)))
        (files (filter (negate (cut string-prefix? "-" <>)) args)))
    (catch #t
      (lambda _
        (if recursive? (for-each delete-file-recursively files)
            (for-each delete-file files))
        #t)
      (lambda ( . rest)
        (or force?
            (apply throw rest))))))

(define main rm)
