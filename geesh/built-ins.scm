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
  `(("." . ,(@@ (geesh built-ins dot) main))
    (":" . ,(@@ (geesh built-ins colon) main))
    ("break" . ,(@@ (geesh built-ins break) main))
    ("continue" . ,(@@ (geesh built-ins continue) main))
    ("eval" . ,(@@ (geesh built-ins eval) main))
    ("exec" . ,undefined)
    ("exit" . ,undefined)
    ("export" . ,(@@ (geesh built-ins export) main))
    ("readonly" . ,(@@ (geesh built-ins readonly) main))
    ("return" . ,undefined)
    ("set" . ,(@@ (geesh built-ins set) main))
    ("shift" . ,undefined)
    ("times" . ,undefined)
    ("trap" . ,undefined)
    ("unset" . ,(@@ (geesh built-ins unset) main))))

;; Regular built-ins take precendence over utilities in the search
;; path, but not over functions.
(define *built-ins*
  `( ;; POSIX-specified built-ins.
    ("alias" . ,undefined)
    ("bg" . ,undefined)
    ("cd" . ,(@@ (geesh built-ins cd) main))
    ("command" . ,undefined)
    ("false" . ,(@@ (geesh built-ins false) main))
    ("fc" . ,undefined)
    ("fg" . ,undefined)
    ("getopts" . ,undefined)
    ("hash" . ,undefined)
    ("jobs" . ,undefined)
    ("kill" . ,undefined)
    ("newgrp" . ,undefined)
    ("pwd" . ,(@@ (geesh built-ins pwd) main))
    ("read" . ,(@@ (geesh built-ins read) main))
    ("true" . ,(@@ (geesh built-ins true) main))
    ("umask" . ,undefined)
    ("unalias" . ,undefined)
    ("wait" . ,undefined)
    ;; Other built-ins.
    ("echo" . ,echo)))

(define (search-special-built-ins name)
  (assoc-ref *special-built-ins* name))

(define (search-built-ins name)
  (assoc-ref *built-ins* name))
