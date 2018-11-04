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

(define-module (gash script)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 local-eval)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (gash bournish-commands)
  #:use-module (gash builtins)
  #:use-module (gash config)
  #:use-module (gash environment)
  #:use-module (gash gash)
  #:use-module (gash io)
  #:use-module (gash job)
  #:use-module (gash pipe)
  #:use-module (gash util)

  #:export (
            and-terms
            background
            builtin
            command
            doublequotes
            for
            glob
            if-clause
            ignore-error
            literal
            or-terms
            pipeline
            run
            script
            script-status
            sequence
            singlequotes
            splice
            split
            substitution
            word
            xtrace
            ))

(define (background term)
  (format (current-error-port) "background: ~s\n" term)
  (match (pke 'background-term term)
    (('pipeline command) (pke 'background: `(pipeline+ #f ,command)))
    (_ term)))

(define (command . args)
  (define (exec command)
    (cond ((procedure? command) command)
          ((every string? command)
           (let* ((program (car command))
                  (escape-builtin? (and (string? program) (string-prefix? "\\" program)))
                  (program (if escape-builtin? (string-drop program 1) program))
                  (command (cons program (cdr command))))
             (or (builtin command #:prefer-builtin? (or %prefer-builtins?
                                                        escape-builtin?))
                 (lambda _ (status:exit-val (apply system* command))))))
          (else (lambda () #t))))
  (exec (append-map glob args)))

(define (glob pattern)
  (define (glob? pattern)
    (and (string? pattern) (string-match "\\?|\\*" pattern)))
  (define (glob2regex pattern)
    (let* ((pattern (regexp-substitute/global #f "\\." pattern 'pre "\\." 'post))
           (pattern (regexp-substitute/global #f "\\?" pattern 'pre "." 'post))
           (pattern (regexp-substitute/global #f "\\*" pattern 'pre ".*" 'post)))
      (make-regexp (string-append "^" pattern "$"))))
  (define (glob-match regex path) ;; pattern path -> bool
    (regexp-match? (regexp-exec regex path)))
  (define (glob- pattern file-names)
    (map (lambda (file-name)
           (if (string-prefix? "./" file-name) (string-drop file-name 2) file-name))
         (append-map (lambda (file-name)
                       (map (cut string-append (if (string=? "/" file-name) "" file-name) "/" <>)
                            (filter (conjoin (negate (cut string-prefix? "." <>))
                                             (cute glob-match (glob2regex pattern) <>))
                                    (or (scandir file-name) '()))))
                     file-names)))
  (cond
   ((not pattern) '(""))
   ((glob? pattern) (let ((absolute? (string-prefix? "/" pattern)))
                      (let loop ((patterns (filter (negate string-null?) (string-split pattern #\/)))
                                 (file-names (if absolute? '("/") '("."))))
                        (if (null? patterns)
                            file-names
                            (begin
                              (loop (cdr patterns) (glob- (car patterns) file-names)))))))
   (#t (list pattern))))

(define (singlequotes . o)
  (string-join o ""))

(define (doublequotes . o)
  (string-join (append-map glob  o) ""))

(define (sequence . args)
  (let ((glob (append-map glob (apply append args))))
    glob))

(define (run ast)
  (map (cut local-eval <> (the-environment)) ast))

(define (script-status)
  ((compose string->number variable) "?"))

(define (script . o)
  o)

(define (for name sequence body)
  (for-each (lambda (value)
              (assignment name value)
              (body))
            (sequence)))

(define (split o)
  ((compose string-tokenize string-trim-right) o))

(define (xtrace o)
  (o))

(define (literal o)
  o)

(define (word . o)
  (define (flatten o)
    (match o
      ((h t ...) (append (flatten h) (append-map flatten t)))
      (_ (list o))))
  (string-join (flatten o) ""))

(define-syntax-rule (substitution commands)
  (with-output-to-string (lambda _ commands)))

(define-syntax-rule (ignore-error o)
  (let ((errexit (shell-opt? "errexit")))
    (when errexit
      (set-shell-opt! "errexit" #f))
    (let ((r o))
      (assignment "?" "0")
      (when errexit
        (set-shell-opt! " errexit" #t))
      r)))

(define-syntax if-clause
  (lambda (x)
    (syntax-case x ()
      ((_ expr then)
       (with-syntax ((it (datum->syntax x 'it)))
         #'(let ((it (ignore-error expr)))
             (if (zero? it) then))))
      ((_ expr then else)
       (with-syntax ((it (datum->syntax x 'it)))
         #'(let ((it (ignore-error expr)))
             (if (zero? it) then else)))))))

(define-syntax and-terms
  (lambda (x)
    (syntax-case x ()
      ((_ left right)
       (with-syntax ((it (datum->syntax x 'it)))
         #'(let ((it left))
             (if (zero? it) right it)))))))

(define-syntax or-terms
  (lambda (x)
    (syntax-case x ()
      ((_ left right)
       (with-syntax ((it (datum->syntax x 'it)))
         #'(let ((it (ignore-error left)))
             (if (zero? it) it right)))))))

(define (pipeline . commands)
  (define (handle job)
    (when (> %debug-level 1)
      (format (current-error-port) "job=~s\n" job))
    (let* ((stati (cond ((job? job) (map status:exit-val (job-status job)))
                        ((boolean? job) (list (if job 0 1)))
                        ((number? job) (list job))
                        (else (list 0))))
           (foo (when (> %debug-level 1)
                  (format (current-error-port) "stati=~s\n" stati)))
           (status (if (shell-opt? "pipefail") (or (find (negate zero?) stati) 0)
                       (car stati)))
           (pipestatus (string-append
                        "("
                        (string-join
                         (map (lambda (s i)
                                (format #f "[~a]=\"~a\"" s i))
                              stati
                              (iota (length stati))))
                        ")")))
      (assignment "PIPESTATUS" pipestatus)
      (assignment "?" (number->string status))
      (when (and (not (zero? status))
                 (shell-opt? "errexit"))
        (exit status))
      status))
  (let ((commands (filter (lambda (x) (not (eq? x *unspecified*))) commands)))
    (when (> %debug-level 1)
      (format (current-error-port) "pijp: commands=~s\n" commands))
    ;; FIXME: after running a builtin, we still end up here with the builtin's result
    ;; that should probably not happen, however, cater for it here for now
    (match commands
      (((and (? boolean?) boolean))
       (handle boolean))
      (((and (? number?) number))
       (handle number))
      (((? unspecified?))
       (handle #t))
      (((? unspecified?) t ... #t)
       #t)
      (_ (handle (apply pipeline+ #t commands))))))

(define* (builtin ast #:key prefer-builtin?)
  ;; FIXME: distinguish between POSIX compliant builtins and
  ;; `best-effort'/`fallback'?
  "Possibly modify command to use a builtin."
  (when (> %debug-level 0)
    (format (current-error-port) "builtin ast=~s\n" ast))
  (receive (command args)
      (match ast
        (((and (? string?) command) args ...) (values command args))
        (_ (values #f #f)))
    (let ((program (and command
                        (cond ((string-prefix? "/" command)
                               (when (not (file-exists? command))
                                 (format (current-error-port) "gash: ~a: no such file or directory\n" command))
                               command)
                              (else (PATH-search-path command))))))
      ;; FIXME: find some generic strerror/errno way: what about permissions and stuff?
      ;; after calling  system* we're too late for that?
      (when (> %debug-level 0)
        (format (current-error-port) "command ~a => ~s ~s\n" (or program 'builtin) command args))
      (cond ((and program (not prefer-builtin?))
             (when (not program)
               (format (current-error-port) "gash: ~a: command not found\n" command))
             (when (not (access? program X_OK))
               (format (current-error-port) "gash: ~a: permission denied\n" command))
             #f)
            ((and command (or (assoc-ref %builtin-commands command)
                              (assoc-ref (%bournish-commands) command)))
             =>
             (lambda (command)
               (if args
                   (apply command (map (cut local-eval <> (the-environment)) args))
                   (command))))
            (else #f)))))
