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

(define-module (geesh word)
  #:use-module (geesh environment)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (eval-cmd-sub
            expand-word))

;;; Commentary:
;;;
;;; This module contains functions for manipulating shell words.  This
;;; includes tilde expansion, parameter expansions, field splitting,
;;; globbing, etc.
;;;
;;; In the code below, the term "qword" is used to refer to words that
;;; only contain quotations (i.e., no substitutions).
;;;
;;; Code:

(define (normalize-word word)
  "Normalize @var{word} (which may be a word or a qword) so that it is
guaranteed to be a list."
  (match word
    ((? string?) (list word))
    (((? symbol?) _) (list word))
    (_ word)))

(define (infix x xs)
  "Place @var{x} between each element of the list @var{xs}."
  (if (null? xs)
      xs
      (let loop ((xs (cdr xs)) (acc (list (car xs))))
        (if (null? xs)
            (reverse acc)
            (loop (cdr xs) (cons* (car xs) x acc))))))

(define (list-split xs sym)
  "Split the list @var{xs} into sublists delimited by the symbol
@var{sym}."
  (let loop ((xs xs) (small-acc '()) (big-acc '()))
    (cond
     ((null? xs)
      (reverse (cons (reverse small-acc) big-acc)))
     ((eq? (car xs) sym)
      (loop (cdr xs) '() (cons (reverse small-acc) big-acc)))
     (else
      (loop (cdr xs) (cons (car xs) small-acc) big-acc)))))

(define (split-fields qword ifs)
  "Split @var{qword} into a list of qwords delimited by the character
set @var{ifs}."

  (define (wedge-apart qword-part ifs)
    (match qword-part
      (('<sh-quote> _) (list qword-part))
      ("" '(""))
      (str (let ((str-parts (string-split str ifs)))
             (if (every string-null? str-parts)
                 '(wedge)
                 (filter (lambda (x)
                           (or (eq? x 'wedge) (not (string-null? x))))
                         (infix 'wedge str-parts)))))))

  (let ((wedged (append-map (cut wedge-apart <> ifs)
                            (normalize-word qword))))
    (filter pair? (list-split wedged 'wedge))))

(define (remove-quotes qword)
  "Remove quote forms from @var{qword} and concatenate the result into a
single field (string)."
  (let loop ((qword (normalize-word qword)) (acc '()))
    (match qword
      (() (string-concatenate-reverse acc))
      ((('<sh-quote> qword*) . t) (loop t (cons (remove-quotes qword*) acc)))
      (((? string? h) . t) (loop t (cons h acc))))))

(define eval-cmd-sub
  ;; A procedure for evaluating (expanding) a command substitution.
  ;; This is parameterized to avoid a circular dependency.
  (make-parameter (lambda (exps) (throw 'eval-cmd-sub-unset))))

(define (string-not-null? str)
  "Check if @var{str} is a non-null string."
  (and (string? str) (not (string-null? str))))

(define (word->qword env word)
  "Convert @var{word} into a qword by resolving all parameter, command,
and arithmetic substitions using the environment @var{env}."
  (match word
    ((? string?)
     word)
    (('<sh-quote> quoted-word)
     `(<sh-quote> ,(word->qword env quoted-word)))
    (('<sh-cmd-sub> . exps)
     ((eval-cmd-sub) exps))
    (('<sh-ref> name)
     (or (var-ref env name) ""))
    (('<sh-ref-or> name default)
     (or (var-ref env name)
         (word->qword env (or default ""))))
    (('<sh-ref-or*> name default)
     (let ((value (var-ref env name)))
       (if (string-not-null? value)
           value
           (word->qword env (or default "")))))
    (('<sh-ref-or!> name default)
     (or (var-ref env name)
         (let ((new-value (expand-word env (or default "")
                                       #:split? #f #:rhs-tildes? #t)))
           (set-var! env name new-value)
           new-value)))
    (('<sh-ref-or!*> name default)
     (let ((value (var-ref env name)))
       (if (string-not-null? value)
           value
           (let ((new-value (expand-word env (or default "")
                                         #:split? #f #:rhs-tildes? #t)))
             (set-var! env name new-value)
             new-value))))
    (('<sh-ref-assert> name message) (error "Not implemented"))
    (('<sh-ref-assert*> name message) (error "Not implemented"))
    (('<sh-ref-and> name value)
     (if (string-not-null? (var-ref env name))
         (word->qword env (or value ""))
         ""))
    (('<sh-ref-and*> name value)
     (or (and (var-ref env name)
              (word->qword env (or value "")))
         ""))
    (('<sh-ref-except-min> name pattern) (error "Not implemented"))
    (('<sh-ref-except-max> name pattern) (error "Not implemented"))
    (('<sh-ref-skip-min> name pattern) (error "Not implemented"))
    (('<sh-ref-skip-max> name pattern) (error "Not implemented"))
    (('<sh-ref-length> name)
     (number->string (string-length (or (var-ref env name) ""))))
    (_ (map (cut word->qword env <>) word))))

(define* (expand-word env word #:key (split? #t) (rhs-tildes? #f))
  "Expand @var{word} into a list of fields using the environment
@var{env}."
  (let ((qword (word->qword env word)))
    (if split?
        (map remove-quotes
             (split-fields qword (char-set #\newline #\tab #\space)))
        (remove-quotes qword))))
