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

(define* (eval-word word #:key (output 'fields) (rhs-tildes? #f))
  (parameterize ((eval-cmd-sub (lambda (exps)
                                 (sh:substitute-command
                                   (lambda ()
                                     (for-each eval-sh exps))))))
    (expand-word word #:output output #:rhs-tildes? rhs-tildes?)))

(define (eval-redir redir)
  "Evaluate the redirect @var{redir}."
  (match-let* (((op fd word) redir)
               (field (eval-word word #:output 'string)))
    (match op
      ((or '>& '<&)
       (let ((n (string->number field)))
         (cond
          ((and n (exact-integer? n)) `(,op ,fd ,n))
          ((string=? field "-") `(,op ,fd -))
          (else (throw 'bad-dup)))))
      (_ `(,op ,fd ,field)))))

(define (exp->thunk exp)
  (lambda () (eval-sh exp)))

(define (exps->thunk exps)
  ;; XXX: It probably makes more sense to exclude '#f' expressions at
  ;; the syntax level.  For now, we filter them out here.
  (if exps
      (match (filter values exps)
        (() noop)
        (exps (lambda () (eval-sh `(<sh-begin> ,@exps)))))
      noop))

(define (eval-sh exp)
  "Evaluate the Shell expression @var{exp}."
  (match exp
    (('<sh-and> exp1 exp2)
     (sh:and (exp->thunk exp1) (exp->thunk exp2)))
    (('<sh-begin> . sub-exps)
     (for-each eval-sh sub-exps))
    (('<sh-case> word (pattern-lists . sub-exp-lists) ...)
     (let ((value (eval-word word #:output 'string)))
       (apply sh:case value
              (map (lambda (patterns sub-exps)
                     `(,(map (cut eval-word <> #:output 'pattern)
                             patterns)
                       ,(exps->thunk sub-exps)))
                   pattern-lists
                   sub-exp-lists))))
    (('<sh-cond> (test-exps . sub-exp-lists) ..1)
     (apply sh:cond
            (map (lambda (test-exp sub-exps)
                   `(,(match test-exp
                        ('<sh-else> #t)
                        (exp (exp->thunk exp)))
                     ,(exps->thunk sub-exps)))
                 test-exps
                 sub-exp-lists)))
    (('<sh-defun> name . sub-exps)
     (let ((proc (lambda args
                   (eval-sh `(<sh-begin> ,@sub-exps)))))
       (defun! name proc)))
    (('<sh-exec> words ..1)
     (let ((args (append-map eval-word words)))
       (match args
         ((name . args) (apply sh:exec name args))
         (() #f))))
    (('<sh-exec-let> ((names var-words) ..1) cmd-words ..1)
     (let* ((args (append-map eval-word cmd-words))
            (bindings (map (lambda (name word)
                             `(,name . ,(eval-word word
                                                   #:output 'string
                                                   #:rhs-tildes? #t)))
                           names var-words)))
       (match args
         ((name . args) (apply sh:exec-let bindings name args))
         (() (for-each (match-lambda
                         ((name . value) (setvar! name value)))
                       bindings)))))
    (('<sh-for> (name (words ...)) . sub-exps)
     (sh:for `(,name ,(append-map eval-word words))
       (exps->thunk sub-exps)))
    (('<sh-not> exp)
     (sh:not (exp->thunk exp)))
    (('<sh-or> exp1 exp2)
     (sh:or (exp->thunk exp1) (exp->thunk exp2)))
    (('<sh-pipeline> cmd*s ..1)
     (apply sh:pipeline (map exp->thunk cmd*s)))
    (('<sh-set!> (names words) ..1)
     (for-each (lambda (name word)
                 (setvar! name (eval-word word
                                          #:output 'string
                                          #:rhs-tildes? #t)))
               names words))
    (('<sh-subshell> . sub-exps)
     (sh:subshell (exps->thunk sub-exps)))
    (('<sh-while> test-exp sub-exps ..1)
     (sh:while (exp->thunk test-exp) (exps->thunk sub-exps)))
    (('<sh-with-redirects> (redirs ..1) sub-exp)
     (match sub-exp
       ;; For "simple commands" we have to observe a special order of
       ;; evaluation: first command words, then redirects, and finally
       ;; assignment words.
       (('<sh-exec> words ..1)
        (let ((args (append-map eval-word words)))
          (match (false-if-exception (map eval-redir redirs))
            (#f (set-status! 1))
            (redirs
             (match args
               ;; This built-in, called with no arguments, is a very
               ;; special case.  We need to treat the redirects
               ;; directly rather than pass them to
               ;; 'sh:with-redirects'.
               (("exec") (sh:set-redirects redirs))
               ((name . args)
                (sh:with-redirects redirs
                  (lambda ()
                    (apply sh:exec name args))))
               (() #f))))))
       (('<sh-exec-let> ((names var-words) ..1) cmd-words ..1)
        (let ((args (append-map eval-word cmd-words)))
          (match (false-if-exception (map eval-redir redirs))
            (#f (set-status! 1))
            (redirs
             (let ((bindings (map (lambda (name word)
                                    `(,name . ,(eval-word word
                                                          #:output 'string
                                                          #:rhs-tildes? #t)))
                                  names var-words)))
               (match args
                 ;; See the '<sh-exec>' case for why this built-in is
                 ;; treated specially.
                 (("exec") (sh:set-redirects redirs))
                 ((name . args)
                  (sh:with-redirects redirs
                    (lambda ()
                      (apply sh:exec-let bindings name args))))
                 (() (for-each (match-lambda
                                 ((name . value) (setvar! name value)))
                               bindings))))))))
       (_ (match (false-if-exception (map eval-redir redirs))
            (#f (set-status! 1))
            (redirs
             (sh:with-redirects redirs
               (exp->thunk sub-exp)))))))
    (('<sh-until> test-exp sub-exps ..1)
     (sh:until (exp->thunk test-exp) (exps->thunk sub-exps)))))
