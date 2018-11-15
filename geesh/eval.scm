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
  (parameterize ((eval-cmd-sub (lambda (exps)
                                 (sh:substitute-command env
                                   (lambda ()
                                     (for-each (cut eval-sh env <>) exps))))))
    (expand-word env word #:split? split? #:rhs-tildes? rhs-tildes?)))

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

(define (exps->thunk env exps)
  (lambda () (eval-sh env `(<sh-begin> ,@exps))))

(define (eval-sh env exp)
  "Evaluate the Shell expression @var{exp} in the context of the Shell
environment @var{env}."
  (match exp
    (('<sh-and> exp1 exp2)
     (sh:and env (exp->thunk env exp1) (exp->thunk env exp2)))
    (('<sh-begin> . sub-exps)
     (for-each (cut eval-sh env <>) sub-exps))
    (('<sh-defun> name . sub-exps)
     (let ((proc (lambda (env . args)
                   (eval-sh env `(<sh-begin> ,@sub-exps)))))
       (define-environment-function! env name proc)))
    (('<sh-exec> words ..1)
     (let ((args (append-map (cut eval-word env <>) words)))
       (match args
         ((name . args) (apply sh:exec env name args))
         (() #f))))
    (('<sh-for> (name (words ...)) . sub-exps)
     (sh:for env `(,name ,(append-map (cut eval-word env <>) words))
       (exps->thunk env sub-exps)))
    (('<sh-not> exp)
     (sh:not env (exp->thunk env exp)))
    (('<sh-or> exp1 exp2)
     (sh:or env (exp->thunk env exp1) (exp->thunk env exp2)))
    (('<sh-pipeline> cmd*s ..1)
     (apply sh:pipeline env (map (cut exp->thunk env <>) cmd*s)))
    (('<sh-set!> (names words) ..1)
     (for-each (lambda (name word)
                 (set-var! env name (eval-word env word
                                               #:split? #f
                                               #:rhs-tildes? #t)))
               names words))
    (('<sh-subshell> . sub-exps)
     (sh:subshell env (exps->thunk env sub-exps)))
    (('<sh-with-redirects> (redirs ..1) sub-exp)
     (match sub-exp
       ;; For "simple commands" we have to observe a special order of
       ;; evaluation: first command words, then redirects, and finally
       ;; assignment words.
       (('<sh-exec> words ..1)
        (let ((args (append-map (cut eval-word env <>) words)))
          (match (false-if-exception
                  (map (cut eval-redir env <>) redirs))
            (#f (set-environment-status! env 1))
            (redirs
             (match args
               ((name . args)
                (sh:with-redirects env redirs
                  (lambda ()
                    (apply sh:exec env name args))))
               (() #f))))))
       (_ (match (false-if-exception
                  (map (cut eval-redir env <>) redirs))
            (#f (set-environment-status! env 1))
            (redirs
             (sh:with-redirects env redirs
               (exp->thunk env sub-exp)))))))))
