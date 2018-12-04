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

(define-module (geesh environment)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (get-status
            set-status!
            getvar
            setvar!
            unsetvar!
            exported?
            set-exported!
            read-only?
            set-read-only!
            with-variables
            get-environ
            with-environ
            getfun
            defun!
            unsetfun!
            with-arguments
            getopt
            setopt!
            *option-names*
            *option-letters*
            call-with-continue
            sh:continue
            call-with-break
            sh:break
            call-with-return
            sh:return
            set-atexit!
            sh:exit
            *fd-count*
            fd->current-port))

;;; Commentary:
;;;
;;; This module contains functions to inspect and manipulate the
;;; environment of the Shell language.
;;;
;;; Code:


;;; Status.

(define *status* 0)

(define (get-status)
  "Return the current status."
  *status*)

(define (set-status! n)
  "Set the current status."
  (set! *status* n))


;;; Variables.

(define (environ->alist env)
  "Convert @var{environ} (a value of the type returned by
@code{environ}) to an alist."
  (define (string-split-1 str char_pred)
    (and=> (string-index str char_pred)
           (lambda (index)
             `(,(substring str 0 index) . ,(substring str (1+ index))))))
  (filter-map (cut string-split-1 <> #\=) env))

(define *variables*
  (alist->hash-table
   (map (match-lambda
          ((name . value) `(,name . ,(vector value #t #f))))
        (environ->alist (environ)))))

(define (exported? name)
  "Check if the variable @var{name} has been exported."
  (match (hash-ref *variables* name)
    (#(_ exported? _) exported?)
    (_ #f)))

(define* (set-exported! name #:optional value)
  "Export the variable @var{name}.  If the optional parameter
@var{value} is provided, update the variable's value as well."
  (match (hash-ref *variables* name)
    ((? vector? vec)
     (vector-set! vec 1 #t)
     (when value
       (vector-set! vec 0 value)))
    (v (hash-set! *variables* name (vector (or value v) #t #f)))))

(define (read-only? name)
  "Check if the variable @var{name} has been marked read-only."
  (match (hash-ref *variables* name)
    (#(_ _ read-only?) read-only?)
    (_ #f)))

(define* (set-read-only! name #:optional value)
  "Mark the variable @var{name} as read-only.  If the optional
parameter @var{value} is provided, update the variable's value as
well."
  (match (hash-ref *variables* name)
    ((? vector? vec)
     (vector-set! vec 2 #t)
     (when value
       (vector-set! vec 0 value)))
    (v (hash-set! *variables* name (vector (or value v) #f #t)))))

(define* (getvar name #:optional dflt)
  "Return the value of the variable @var{name}.  If it does not exist
and @var{dflt} is provided, return @var{dflt}.  Otherwise, return
@code{#f}."
  (match (hash-ref *variables* name dflt)
    (#(value _ _) value)
    (value value)))

(define (setvar! name value)
  "Set the variable @var{name} to @var{value}.  If @var{value} is
@code{#f}, the variable will be removed from the set of current
variables.  If @var{name} has been marked read-only, an exception will
be thrown."
  (match (hash-ref *variables* name)
    ((? vector? vec)
     (when (vector-ref vec 2)
       (scm-error
        'shell-error "setvar!"
        "Attempted to assign the read-only only variable \"~A\"."
        `(,name)
        '(variable-assignment-error)))
     (if value
         (vector-set! vec 0 value)
         (hash-remove! *variables* name)))
    (_ (if value
           (hash-set! *variables* name value)
           (hash-remove! *variables* name)))))

(define (unsetvar! name)
  "Remove the variable @var{name} from the set of current variables."
  (setvar! name #f))

(define (with-variables variables thunk)
  "Call @var{thunk} in a dynamic extent in which the set of current
variables contains only @var{variables}.  The previous set of current
variables is unaffected by any changes made from within the dynamic
extent of @var{thunk}."
  (let ((outside-variables #f)
        (inside-variables (alist->hash-table variables)))
    (dynamic-wind
      (lambda ()
        (set! outside-variables *variables*)
        (set! *variables* inside-variables))
      thunk
      (lambda ()
        (set! inside-variables *variables*)
        (set! outside-variables *variables*)))))

(define* (get-environ #:optional (bindings '()))
  "Return a value that represents the set of current variables is
suitable for passing to @code{environ}.  If @var{bindings} is set,
consider them as part of the set of current variables."
  (let ((exported (hash-fold (lambda (name v acc)
                               (match v
                                 (#(value #t _)
                                  (cons `(,name . ,value) acc))
                                 (_ acc)))
                             '()
                             *variables*)))
    (map (match-lambda
           ((name . value) (string-append name "=" value)))
         (delete-duplicates!
          (append bindings exported)
          (lambda (x y)
            (string=? (car x) (car y)))))))

(define (with-environ env thunk)
  "Call @var{thunk} in a dynamic extent in which the environment (the
regular @code{getenv}/@code{setenv} one -- not the Geesh one) has been
set to @var{env} (a value suitable for passing to @code{environ}."
  (let ((outside-env #f)
        (inside-env env))
    (dynamic-wind
      (lambda ()
        (set! outside-env (environ))
        (environ inside-env))
      thunk
      (lambda ()
        (set! inside-env (environ))
        (environ outside-env)))))


;;; Functions.

(define *functions* (make-hash-table))

(define (getfun name)
  "Return the function @var{name}.  If it does not exist, return
@code{#f}."
  (hash-ref *functions* name))

(define (defun! name proc)
  "Define the function @var{name} to be @var{proc} (a procedure that
takes a variable number of arguments)."
  (hash-set! *functions* name proc))

(define (unsetfun! name)
  "Remove the function @var{name} from the set of current functions."
  (hash-remove! *functions* name))


;;; Arguments.

(define (with-arguments args thunk)
  "Call @var{thunk} in a dynamic extent in which the current arguments
list (as obtained by calling @code{program-arguments}) is set to
@var{args}.  The previous arguments list is unaffected by any changes
made from within the dynamic extent of @var{thunk}."
  (let ((outside-args #f)
        (inside-args args))
    (dynamic-wind
      (lambda ()
        (set! outside-args (program-arguments))
        (set-program-arguments inside-args))
      thunk
      (lambda ()
        (set! inside-args (program-arguments))
        (set-program-arguments outside-args)))))


;;; Options.

(define *options*
  (map (cut cons <> #f)
       '(allexport
         errexit
         ignoreeof
         monitor
         noclobber
         noglob
         noexec
         nolog
         notify
         nounset
         verbose
         vi
         xtrace)))

(define (getopt name)
  "Get the value of the option named @var{name}."
  (match (assq name *options*)
    ((_ . value) value)))

(define (setopt! name value)
  "Set the value of the option named @var{name} to @var{value}."
  (match (assq name *options*)
    ((? pair? p) (set-cdr! p value))))

(define *option-names*
  (map car *options*))

(define *option-letters*
  '((#\a . allexport)
    (#\e . errexit)
    (#\m . monitor)
    (#\C . noclobber)
    (#\f . noglob)
    (#\n . noexec)
    (#\b . notify)
    (#\u . nounset)
    (#\v . verbose)
    (#\x . xtrace)))


;;; Prompts

(define *continue-tag* (make-prompt-tag))

(define (call-with-continue thunk)
  "Call @var{thunk} in such a way that a call to @code{continue} will
exit the dynamic extent of @var{thunk}."
  (call-with-prompt *continue-tag*
    thunk
    (lambda (cont n)
      (when (> n 0)
        (false-if-exception
         (abort-to-prompt *continue-tag* (1- n)))))))

(define* (sh:continue #:optional (n 0))
  "Exit to the closest invocation of @code{call-with-continue}.  If
@var{n} is set, exit to the @math{n + 1}th closest invocation."
  (abort-to-prompt *continue-tag* n))

(define *break-tag* (make-prompt-tag))

(define (call-with-break thunk)
  "Call @var{thunk} in such a way that a call to @code{break} will
exit the dynamic extent of @var{thunk}."
  (call-with-prompt *break-tag*
    thunk
    (lambda (cont n)
      (when (> n 0)
        (false-if-exception
         (abort-to-prompt *break-tag* (1- n)))))))

(define* (sh:break #:optional (n 0))
  "Exit to the closest invocation of @code{call-with-break}.  If
@var{n} is set, exit to the @math{n + 1}th closest invocation."
  (abort-to-prompt *break-tag* n))

(define *return-tag* (make-prompt-tag))

(define (call-with-return thunk)
  "Call @var{thunk} in such a way that a call to @code{return} will
exit the dynamic extent of @var{thunk}."
  (call-with-prompt *return-tag*
    thunk
    (lambda (cont status)
      (set-status! status))))

(define* (sh:return #:optional (status (get-status)))
  "Exit to the closest invocation of @code{call-with-return} setting
status to @var{status}.  If @var{status} is not set, keep the current
status."
  (abort-to-prompt *return-tag* status))

(define *atexit* #f)
(define *exiting?* #f)

(define (set-atexit! handler)
  (set! *atexit* handler))

(define* (sh:exit #:optional status)
  (if (and (not *exiting?*) (thunk? *atexit*))
      (begin
        (set! *exiting?* #t)
        (*atexit*)
        (exit (or status (get-status))))
      (exit (or status (get-status)))))


;;; Files.

(define *fd-count* 10)

(define current-3-port (make-parameter #f))
(define current-4-port (make-parameter #f))
(define current-5-port (make-parameter #f))
(define current-6-port (make-parameter #f))
(define current-7-port (make-parameter #f))
(define current-8-port (make-parameter #f))
(define current-9-port (make-parameter #f))

(define fd->current-port
  (let ((cps (vector current-input-port
                     current-output-port
                     current-error-port
                     current-3-port
                     current-4-port
                     current-5-port
                     current-6-port
                     current-7-port
                     current-8-port
                     current-9-port)))
    (lambda (fd)
      "Return the current port (e.g. @code{current-input-port})
corresponding to the the Shell file descriptor @var{fd}.  The value of
@var{fd} must be a nonnegative integer less than @code{*fd-count*}."
      (vector-ref cps fd))))
