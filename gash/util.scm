;;; Gash --- Guile As SHell
;;; Copyright © 2016,2017,2018 R.E.W. van Beusekom <rutger.van.beusekom@gmail.com>
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of Gash.
;;;
;;; Gash is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Gash is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Gash.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gash util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:export (
            conjoin
            disjoin
            wrap-command
            char->string
            string->string-list
            string-replace-string
            ))

(define (disjoin . predicates)
  (lambda (. arguments)
    (any (cut apply <> arguments) predicates)))

(define (conjoin . predicates)
  (lambda (. arguments)
    (every (cut apply <> arguments) predicates)))

(define (string->string-list string)
  (map char->string (string->list string)))

(define (char->string c)
  (make-string 1 c))

(define (string-replace-string string from to)
  (cond ((string-contains string from)
         =>
         (lambda (i)
           (string-replace string to i (+ i (string-length from)))))
        (else string)))
