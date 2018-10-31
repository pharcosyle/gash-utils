;;; Gash --- Guile As SHell
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

(define-module (gash geesh)
  #:use-module (srfi srfi-1)

  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)

  #:use-module (gash builtins)
  #:use-module (gash gash)
  #:use-module (gash io)
  #:use-module (geesh parser)
  #:export (
            parse
            parse-string
            ))

(define (parse port)
  (let ((parse-tree (read-sh-all port)))
    (when (> %debug-level 1)
      (format (current-error-port) "parse-tree:\n")
      (pretty-print parse-tree (current-error-port)))
    (let ((ast (parse-tree->script parse-tree)))
    (when (> %debug-level 1)
      (format (current-error-port) "transformed:\n")
      (pretty-print ast (current-error-port)))
      (let* ((script (match ast
                       (((or 'command 'pipeline) _ ...) `(script ,ast))
                       ((_ ...) `(script ,@ast))
                       (_ `(script ,ast))))
             (tracing-script (annotate-tracing script)))
        (when (> %debug-level 0)
          (format (current-error-port) "script:\n")
          (pretty-print tracing-script (current-error-port)))
        tracing-script))))

(define (parse-string string)
  (call-with-input-string string parse))

(define (parse-tree->script tree)
  (define (transform o)
    (when (> %debug-level 2)
      (format (current-error-port) "transform:\n")
      (pretty-print o (current-error-port)))
    (match o
      (('<sh-begin> body ...) `(begin ,@(map transform body)))
      (('<sh-pipeline> (('<sh-pipeline> (left ...))) right)
       `(pipeline ,@(map transform left) ,(transform right)))
      (('<sh-pipeline> ('<sh-pipeline> (left ...) right))
       `(pipeline ,@(map transform left) ,(transform right)))
      (('<sh-pipeline> (left right))
       `(pipeline ,(transform left) ,(transform right)))
      (('<sh-exec> command) `(command ,(transform command)))
      (('<sh-exec> command ...) `(command ,@(map transform command)))
      (((and ref ('<sh-ref> _)) words ...)
       `(word ,(transform ref) ,@(map transform words)))
      (('<sh-ref> var) `(variable ,var))
      (('<sh-set!> (var (and value ((? symbol?) _ ...))))
       `(assignment ,(transform var) ,(transform value)))
      (('<sh-set!> (var (value ...)))
       `(assignment ,(transform var) (word ,@(map transform value))))
      (('<sh-set!> (var value)) `(assignment ,(transform var) ,(transform value)))
      (((and kwote ('<sh-quote> _)) word)
       `(word ,(transform kwote) ,(transform word)))
      (('<sh-quote>)
       `(doublequotes ""))
      (('<sh-quote> words ...)
       `(doublequotes (word ,@(map transform words))))
      (((and quote ('<sh-quote> _)) tail ...)
       `(word ,(transform quote) ,@(map transform tail)))
      (('<sh-cmd-sub> cmd) `(substitution ,(transform cmd)))
      (('<sh-cond> (expression then)) `(if-clause ,(transform expression) ,(transform then)))
      (('<sh-with-redirects> (('<< 0 string)) pipeline)
       (let ((pipeline (transform pipeline)))
         `(pipeline (display ,(transform string))
                    ,@(match pipeline
                        (('command command ...) `(,pipeline))
                        (('pipeline commands ...) commands)))))

      (('<sh-for> (name (sequence)) body)
       `(for ,(transform name)
             (lambda _ (split ,(transform sequence)))
             (lambda _ ,(transform body))))

      (('<sh-for> (name sequence) body)
       `(for ,(transform name)
             (lambda _ (split ,(transform sequence)))
             (lambda _ ,(transform body))))

      ((? string?) o)
      (((? string?) _ ...) `(word ,@(map re-word o)))
      ((_ ...) (map transform o))
      (_ o)))
  (transform tree))

(define (re-word word)
  (match word
    ((? string?) word)
    (((and h (? string?)) t ...)
     `(word ,h ,@(map (compose re-word parse-tree->script) t)))
    (_ (parse-tree->script word))))

(define (annotate-tracing script)
  (match script
    (('pipeline command)
     `(pipeline ,(trace (list command)) ,command))
    (('pipeline commands ...)
     `(pipeline ,(trace commands) ,@commands))
    (('command command ...)
     `(pipeline ,(trace (list script)) ,script))
    ((_ ...) (map annotate-tracing script))
    (_ script)))
