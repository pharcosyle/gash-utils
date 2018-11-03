;;;; peg.scm --- Parsing Expression Grammar (PEG) parser generator
;;;;
;;;; 	Copyright (C) 2010, 2011 Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;;

(define-module (peg)
  #:use-module (peg codegen)
  #:use-module (peg string-peg)
  ;; Note: the most important effect of using string-peg is not whatever
  ;; functions it exports, but the fact that it adds a new handler to
  ;; peg-sexp-compile.
  #:use-module (peg simplify-tree)
  #:use-module (peg using-parsers)
  #:use-module (peg cache)
  #:re-export (define-peg-pattern
               define-peg-string-patterns
               match-pattern
               search-for-pattern
               compile-peg-pattern
               keyword-flatten
               context-flatten
               peg:start
               peg:end
               peg:string
               peg:tree
               peg:substring
               peg-record?))
