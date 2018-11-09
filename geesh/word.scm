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
  "Split @var{qword} into a list of qwords delimited by any character
in the string @var{ifs}."

  (define ifs? (cut string-index ifs <>))

  (define (wedge-apart-quote qword)
    (let loop ((qword (normalize-word qword)) (acc '()))
      (match qword
        (() (reverse! acc))
        ((('<sh-quote> qword*) . t)
         (loop t (append-reverse (wedge-apart-quote qword*) acc)))
        ((('<sh-at> vals) . t)
         (loop t (append-reverse (infix 'wedge (map (cut list '<sh-quote> <>)
                                                    vals))
                                 acc)))
        (((? string? h) . t)
         (loop t (cons `(<sh-quote> ,h) acc))))))

  (define (wedge-apart qword-part)
    (match qword-part
      (('<sh-quote> quote) (wedge-apart-quote quote))
      (('<sh-at> vals) (apply append (infix '(wedge) (map wedge-apart vals))))
      ("" '(""))
      (str (let ((str-parts (string-split str ifs?)))
             (if (every string-null? str-parts)
                 '(wedge)
                 (filter (lambda (x)
                           (or (eq? x 'wedge) (not (string-null? x))))
                         (infix 'wedge str-parts)))))))

  (let ((wedged (append-map wedge-apart (normalize-word qword))))
    (filter pair? (list-split wedged 'wedge))))

(define (argument-separator ifs)
  "Find the argument separator string by taking the first character of
the string @var{ifs}.  If @var{ifs} is @code{#f} the separator will be
a space (@code{\" \"}), and if @var{ifs} is null (@code{\"\"}) the
separator will be null as well."
  (let ((ifs (or ifs " ")))
    (if (string-null? ifs)
        ""
        (string (string-ref ifs 0)))))

(define (remove-quotes qword ifs)
  "Remove quote forms from @var{qword} and concatenate the result into
a single field (string).  When converting an argument list to a
string, the separator is derived from @var{ifs} using
@code{argument-separator}."
  (let loop ((qword (normalize-word qword)) (acc '()))
    (match qword
      (() (string-concatenate-reverse acc))
      ((('<sh-quote> qword*) . t)
       (loop t (cons (remove-quotes qword* ifs) acc)))
      ((('<sh-at> vals) . t)
       (let ((sep (argument-separator ifs)))
         (loop t (cons (string-join vals sep) acc))))
      (((? string? h) . t)
       (loop t (cons h acc))))))

(define eval-cmd-sub
  ;; A procedure for evaluating (expanding) a command substitution.
  ;; This is parameterized to avoid a circular dependency.
  (make-parameter (lambda (exps) (throw 'eval-cmd-sub-unset))))

(define (string-not-null? str)
  "Check if @var{str} is a non-null string."
  (and (string? str) (not (string-null? str))))

(define (parameter-ref env name)
  "Get the value of the variable or special parameter @var{name} in
@var{env}.  If @var{name} is unset, return @code{#f}."
  (match name
    ("?" (number->string (environment-status env)))
    (_ (var-ref env name))))

(define (parameter-ref* env name)
  "Get the value of the variable or special parameter @var{name} in
@var{env}.  If @var{name} is unset, return @code{\"\"}."
  (or (parameter-ref env name) ""))

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
     (parameter-ref* env name))
    (('<sh-ref-or> name default)
     (or (parameter-ref env name)
         (word->qword env (or default ""))))
    (('<sh-ref-or*> name default)
     (let ((value (parameter-ref env name)))
       (if (string-not-null? value)
           value
           (word->qword env (or default "")))))
    (('<sh-ref-or!> name default)
     (or (parameter-ref env name)
         (let ((new-value (expand-word env (or default "")
                                       #:split? #f #:rhs-tildes? #t)))
           (set-var! env name new-value)
           new-value)))
    (('<sh-ref-or!*> name default)
     (let ((value (parameter-ref env name)))
       (if (string-not-null? value)
           value
           (let ((new-value (expand-word env (or default "")
                                         #:split? #f #:rhs-tildes? #t)))
             (set-var! env name new-value)
             new-value))))
    (('<sh-ref-assert> name message) (error "Not implemented"))
    (('<sh-ref-assert*> name message) (error "Not implemented"))
    (('<sh-ref-and> name value)
     (if (string-not-null? (parameter-ref env name))
         (word->qword env (or value ""))
         ""))
    (('<sh-ref-and*> name value)
     (or (and (parameter-ref env name)
              (word->qword env (or value "")))
         ""))
    (('<sh-ref-except-min> name pattern) (error "Not implemented"))
    (('<sh-ref-except-max> name pattern) (error "Not implemented"))
    (('<sh-ref-skip-min> name pattern) (error "Not implemented"))
    (('<sh-ref-skip-max> name pattern) (error "Not implemented"))
    (('<sh-ref-length> name)
     (number->string (string-length (parameter-ref* env name))))
    (_ (map (cut word->qword env <>) word))))

(define* (expand-word env word #:key (split? #t) (rhs-tildes? #f))
  "Expand @var{word} into a list of fields using the environment
@var{env}."
  ;; The value of '$IFS' may depend on side-effects performed during
  ;; 'word->qword', so use 'let*' here.
  (let* ((qword (word->qword env word))
         (ifs (or (and env (var-ref env "IFS"))
                  (string #\space #\tab #\newline))))
    (if split?
        (map (cut remove-quotes <> ifs)
             (split-fields qword ifs))
        (remove-quotes qword ifs))))
