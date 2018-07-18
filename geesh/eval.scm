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

(define-module (geesh eval)
  #:use-module (geesh environment)
  #:use-module (geesh shell)
  #:use-module (geesh word)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (eval-sh))

;;; Commentary:
;;;
;;; This module provides an interpreter for the Shell language.
;;;
;;; Code:

(define* (eval-word env word #:key (split? #t) (rhs-tildes? #f))
  ;; FIXME: Set the 'eval-cmd-sub' parameter.
  (expand-word env word #:split? split? #:rhs-tildes? rhs-tildes?))

(define (eval-redir env redir)
  "Evaluate the redirect @var{redir} in environment @var{env}."
  (match-let* (((op fd word) redir)
               (field (eval-word env word #:split? #f)))
    (match op
      ((or '>& '<&)
       (let ((n (string->number field)))
         (cond
          ((and n (exact-integer? n)) `(,op ,fd ,n))
          ((string=? field "-") `(,op ,fd -))
          (else (throw 'bad-dup)))))
      (_ `(,op ,fd ,field)))))

(define (exp->thunk env exp)
  (lambda () (eval-sh env exp)))

(define (eval-sh env exp)
  "Evaluate the Shell expression @var{exp} in the context of the Shell
environment @var{env}."
  (match exp
    (('<sh-begin> . sub-exps)
     (for-each (cut eval-sh env <>) sub-exps))
    (('<sh-exec> words ..1)
     (let ((args (append-map (cut eval-word env <>) words)))
       (match args
         ((name . args) (apply sh:exec env name args))
         (() #f))))
    (('<sh-set!> (names words) ..1)
     (for-each (lambda (name word)
                 (set-var! env name (eval-word env word
                                               #:split? #f
                                               #:rhs-tildes? #t)))
               names words))
    (('<sh-with-redirects> (redirs ..1) sub-exp)
     (match sub-exp
       ;; For "simple commands" we have to observe a special order of
       ;; evaluation: first command words, then redirects, and finally
       ;; assignment words.
       (('<sh-exec> words ..1)
        (let ((args (append-map (cut eval-word env <>) words))
              (redirs (map (cut eval-redir env <>) redirs)))
          (match args
            ((name . args)
             (sh:with-redirects env redirs
               (lambda ()
                 (apply sh:exec env name args))))
            (() #f))))
       (_ (sh:with-redirects env (map (cut eval-redir env <>) redirs)
            (exp->thunk env sub-exp)))))))
