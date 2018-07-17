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
  #:use-module (srfi srfi-9)
  #:export (<environment>
            make-environment
            environment?
            var-ref
            set-var!
            environment->environ))

;;; Commentary:
;;;
;;; This module contains data structures and functions for the
;;; environment of the Shell language.
;;;
;;; Code:

(define-record-type <environment>
  (%make-environment vars)
  environment?
  (vars environment-vars set-environment-vars!))

(define (make-environment vars)
  ;; In order to insure that each pair in the 'vars' alist is mutable,
  ;; we copy each one into a new list.
  (%make-environment (map (match-lambda
                            ((key . val) (cons key val)))
                          vars)))

(define (var-ref env name)
  (assoc-ref (environment-vars env) name))

(define (set-var! env name val)
  (set-environment-vars! env (acons name val
                                    (environment-vars env))))

(define* (environment->environ env #:optional (bindings '()))
  "Convert the environment variables from @var{env} into a list of
@code{\"name=value\"} strings (an @dfn{environ}).  If @var{bindings}
is set to a list of pairs of strings, those name-value pairs take
precedence over the ones in @var{env}."
  (let loop ((env-vars (append bindings (environment-vars env)))
             (acc '())
             (seen '()))
    (match env-vars
      (((name . value) . rest)
       (if (member name seen)
           (loop rest acc seen)
           (loop rest
                 (cons (string-append name "=" value) acc)
                 (cons name seen))))
      (() acc))))
