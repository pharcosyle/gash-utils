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

;;; This is a fallback module for the bootstrap guile where (ice-9
;;; readline) is not available.

;;; Code:

(define-module (gash readline)
  #:use-module (ice-9 rdelim)
  #:export (add-history
            clear-history
            read-history
            readline
            with-readline-completion-function
            write-history))

(define (add-history x) #t)
(define (clear-history) #t)
(define (read-history x) #t)
(define (readline prompt) (display prompt) (read-line))
(define (with-readline-completion-function completion thunk) (thunk))
(define (write-history x) #t)

