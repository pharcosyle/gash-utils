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
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (<environment>
            make-environment
            environment?
            var-ref
            var-ref*
            set-var!
            set-var-export!
            set-var-read-only!
            delete-environment-vars!
            environment->environ
            environ->alist
            environment-status
            set-environment-status!
            environment-function-ref
            define-environment-function!
            delete-environment-functions!
            environment-arguments
            with-environment-arguments
            environment-break-prompt
            environment-continue-prompt))

;;; Commentary:
;;;
;;; This module contains data structures and functions for the
;;; environment of the Shell language.
;;;
;;; Code:

(define-record-type <variable>
  (make-variable value export? read-only?)
  variable?
  (value variable-value)
  (export? variable-exported?)
  (read-only? variable-read-only?))

(define-record-type <environment>
  (%make-environment vars functions arguments status
                     break-prompt continue-prompt)
  environment?
  (vars environment-vars set-environment-vars!)
  (functions environment-functions set-environment-functions!)
  (arguments environment-arguments set-environment-arguments!)
  (status environment-status set-environment-status!)
  (break-prompt environment-break-prompt)
  (continue-prompt environment-continue-prompt))

(define* (make-environment vars #:optional (arguments '()))
  ;; In order to insure that each pair in the 'vars' alist is mutable,
  ;; we copy each one into a new list.
  (%make-environment (map (match-lambda
                            ((key . val)
                             (cons key (make-variable val #t #f))))
                          vars)
                     '()
                     arguments
                     0
                     (make-prompt-tag)
                     (make-prompt-tag)))

(define (var-ref env name)
  "Get the value of the variable @var{name} in @var{env}.  If
@var{name} is unset, return @code{#f}."
  (and=> (assoc-ref (environment-vars env) name)
         (match-lambda
           (($ <variable> value _ _) value))))

(define (var-ref* env name)
  "Get the value of the variable @var{name} in @var{env}.  If
@var{name} is unset return @code{\"\"}."
  (or (var-ref env name) ""))

(define (set-var! env name val)
  "Set the variable @var{name} to @var{val} in @var{env}."
  (match (assoc-ref (environment-vars env) name)
    (#f (set-environment-vars!
         env (acons name (make-variable val #f #f)
                    (environment-vars env))))
    (($ <variable> _ export? read-only?)
     (when read-only? (throw 'variable-assignment-error))
     (set-environment-vars!
      env (acons name (make-variable val export? #f)
                 (environment-vars env))))))

(define* (set-var-export! env name #:optional val)
  "Set the export attribute for variable @var{name} in @var{env}.  If
@var{val} is specified, update the variable's value as well."
  (match (assoc-ref (environment-vars env) name)
    (#f (set-environment-vars! env (acons name (make-variable val #t #f)
                                          (environment-vars env))))
    (($ <variable> value export? read-only?)
     (when (and read-only? val) (throw 'variable-assignment-error))
     (set-environment-vars!
      env (acons name (make-variable (or val value) #t read-only?)
                 (environment-vars env))))))

(define* (set-var-read-only! env name #:optional val)
  "Set the read-only attribute for variable @var{name} in @var{env}.
If @var{val} is specified, update the variable's value as well."
  (match (assoc-ref (environment-vars env) name)
    (#f (set-environment-vars! env (acons name (make-variable val #f #t)
                                          (environment-vars env))))
    (($ <variable> value export? read-only?)
     (when (and read-only? val) (throw 'variable-assignment-error))
     (set-environment-vars!
      env (acons name (make-variable (or val value) export? #t)
                 (environment-vars env))))))

(define (delete-environment-vars! env names)
  (set-environment-vars! env (remove (match-lambda
                                       ((key . _) (member key names)))
                                     (environment-vars env))))

(define* (environment->environ env #:optional (bindings '()))
  "Convert the environment variables from @var{env} into a list of
@code{\"name=value\"} strings (an @dfn{environ}).  If @var{bindings}
is set to a list of pairs of strings, those name-value pairs take
precedence over the ones in @var{env}."
  (let ((exported (filter-map (match-lambda
                                ((name . ($ <variable> val export? _))
                                 (and export? val `(,name . ,val))))
                              (environment-vars env))))
    (let loop ((env-vars (append bindings exported))
               (acc '())
               (seen '()))
      (match env-vars
        (((name . value) . rest)
         (if (member name seen)
             (loop rest acc seen)
             (loop rest
                   (cons (string-append name "=" value) acc)
                   (cons name seen))))
        (() acc)))))

(define (environ->alist environ)
  (define (string-split-1 str char_pred)
    (and=> (string-index str char_pred)
           (lambda (index)
             `(,(substring str 0 index) . ,(substring str (1+ index))))))
  (filter-map (cut string-split-1 <> #\=) environ))

(define (environment-function-ref env name)
  "Get the function named @var{name} in @var{env}.  If there is no
such function, return @code{#f}."
  (assoc-ref (environment-functions env) name))

(define (define-environment-function! env name proc)
  "Make @var{name} refer to @var{proc} in @var{env}."
  (set-environment-functions! env (acons name proc
                                         (environment-functions env))))

(define (delete-environment-functions! env . names)
  (set-environment-functions! env (remove (match-lambda
                                            ((key . _) (member key names)))
                                          (environment-functions env))))

(define (with-environment-arguments env arguments thunk)
  "Call @var{thunk} with the arguments in @var{env} set to
@var{arguments}."
  (let ((saved-arguments #f))
    (dynamic-wind
      (lambda ()
        (set! saved-arguments (environment-arguments env))
        (set-environment-arguments! env arguments))
      thunk
      (lambda ()
        (let ((tmp saved-arguments))
          (set! saved-arguments (environment-arguments env))
          (set-environment-arguments! env tmp))))))
