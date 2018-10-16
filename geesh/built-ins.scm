;;; The Geesh Shell Interpreter
;;; Copyright 2018 Timothy Sample <samplet@ngyro.com>
;;;
;;; This file is part of Geesh.
;;;
;;; Geesh is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Geesh is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Geesh.  If not, see <http://www.gnu.org/licenses/>.

(define-module (geesh built-ins)
  #:use-module (geesh built-ins echo)
  #:export (search-built-ins
            search-special-built-ins))

;;; Commentary:
;;;
;;; This module provides built-in searching functions.
;;;
;;; Code:

(define (undefined env . args)
  (throw 'undefined-built-in))

;; Special built-ins take precedence over any other command.
(define *special-built-ins*
  `(("." . ,undefined)
    (":" . ,undefined)
    ("break" . ,undefined)
    ("continue" . ,undefined)
    ("eval" . ,undefined)
    ("exec" . ,undefined)
    ("exit" . ,undefined)
    ("export" . ,undefined)
    ("readonly" . ,undefined)
    ("return" . ,undefined)
    ("set" . ,undefined)
    ("shift" . ,undefined)
    ("times" . ,undefined)
    ("trap" . ,undefined)
    ("unset" . ,undefined)))

;; Regular built-ins take precendence over utilities in the search
;; path, but not over functions.
(define *built-ins*
  `( ;; POSIX-specified built-ins.
    ("alias" . ,undefined)
    ("bg" . ,undefined)
    ("cd" . ,undefined)
    ("command" . ,undefined)
    ("false" . ,undefined)
    ("fc" . ,undefined)
    ("fg" . ,undefined)
    ("getopts" . ,undefined)
    ("hash" . ,undefined)
    ("jobs" . ,undefined)
    ("kill" . ,undefined)
    ("newgrp" . ,undefined)
    ("pwd" . ,undefined)
    ("read" . ,(@@ (geesh built-ins read) main))
    ("true" . ,undefined)
    ("umask" . ,undefined)
    ("unalias" . ,undefined)
    ("wait" . ,undefined)
    ;; Other built-ins.
    ("echo" . ,echo)))

(define (search-special-built-ins name)
  (assoc-ref *special-built-ins* name))

(define (search-built-ins name)
  (assoc-ref *built-ins* name))
