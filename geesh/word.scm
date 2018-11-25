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
  #:use-module (geesh pattern)
  #:use-module (ice-9 ftw)
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

(define (string-tokenize* s token-set)
  "Split the string @var{s} into a list of substrings, where each
substring is a maximal non-empty contiguous sequence of characters
from the character set @var{token-set} or its compliment."

  (define token-set-complement
    (let ((token-set* (char-set-complement token-set)))
      (lambda (cs)
        (if (eq? cs token-set) token-set* token-set))))

  (let loop ((index 0) (start 0) (cs token-set) (acc '()))
    (cond
     ((>= index (string-length s))
      (reverse! (if (> index start)
                    (cons (substring s start index) acc)
                    acc)))
     ((char-set-contains? cs (string-ref s index))
      (loop (1+ index) start cs acc))
     (else
      (loop index index
            (token-set-complement cs)
            (if (> index start)
                (cons (substring s start index) acc)
                acc))))))

(define (split-fields qword ifs)
  "Split @var{qword} into a list of qwords delimited by any character
in the string @var{ifs}."

  (define char-set:ifs
    (string->char-set ifs))

  (define char-set:ifs/nw
    (char-set-difference char-set:ifs char-set:whitespace))

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

  (define (wedge-apart qword)
    (match qword
      (('<sh-quote> quote) (wedge-apart-quote quote))
      (('<sh-at> vals) (apply append (infix '(wedge) (map wedge-apart vals))))
      ((? string? str)
       (let ((tokens (string-tokenize* str char-set:ifs)))
         (append-map (lambda (token)
                       (if (string-any char-set:ifs token)
                           ;; Every occurrence of a non-whitespace
                           ;; separator must delimit a field.  This
                           ;; means that we have to add a blank field
                           ;; for every non-whitespace separator in
                           ;; 'token' beyond the first.
                           (let ((count (string-count token char-set:ifs/nw)))
                             (cons 'wedge
                                   (append-map (const '("" wedge))
                                               (iota (max 0 (- count 1))))))
                           (list token)))
                     ;; When a word starts with a non-whitespace
                     ;; separator, it still delimits two fields, the
                     ;; one on the left being empty.
                     (match tokens
                       (((? (cut string-any char-set:ifs/nw <>)) . rest)
                        (cons "" tokens))
                       (_ tokens)))))
      (_ (append-map wedge-apart qword))))

  (let ((wedged (wedge-apart qword)))
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

(define (qword->pattern qword ifs)
  (let loop ((qword (normalize-word qword)) (acc '()))
    (match qword
      (() (parse-pattern (string-concatenate-reverse acc)))
      ((('<sh-quote> qword*) . t)
       (loop t (cons (pattern-quote (remove-quotes qword* ifs)) acc)))
      (((? string? h) . t)
       (loop t (cons h acc))))))

(define (expand-pathnames qword pwd ifs)

  (define (list-matches patterns)
    (let loop ((stack `(("" ,@patterns))) (acc '()))
      (match stack
        (() (reverse! acc))
        (((path) . stack-tail)
         (loop stack-tail (cons path acc)))
        (((path pattern . next-patterns) . stack-tail)
         (match (scandir (string-append pwd "/" path)
                         (cut pattern-match? pattern <>
                              #:explicit-initial-period? #t))
           (#f (loop stack-tail acc))
           (files (loop (append (map (lambda (file)
                                       (if (string-null? path)
                                           (cons file next-patterns)
                                           (cons (string-append path "/" file)
                                                 next-patterns)))
                                     files)
                                stack-tail)
                        acc)))))))

  (let ((patterns (map (cut qword->pattern <> ifs)
                       (split-fields qword "/"))))
    (if (every pattern-plain? patterns)
        `(,(remove-quotes qword ifs))
        (match (list-matches patterns)
          (() `(,(remove-quotes qword ifs)))
          (matches matches)))))

(define eval-cmd-sub
  ;; A procedure for evaluating (expanding) a command substitution.
  ;; This is parameterized to avoid a circular dependency.
  (make-parameter (lambda (exps) (throw 'eval-cmd-sub-unset))))

(define (string-not-null? str)
  "Check if @var{str} is a non-null string."
  (and (string? str) (not (string-null? str))))

(define* (parameter-ref name #:optional dflt)
  "Get the value of the variable or special parameter @var{name} from
the environment.  If @var{name} is unset, return @code{#f}."
  (match name
    ("@" `(<sh-at> ,(cdr (program-arguments))))
    ("*" (let* ((ifs (or (getvar "IFS")
                         (string #\space #\tab #\newline)))
                (sep (argument-separator ifs)))
           (string-join (cdr (program-arguments)) sep)))
    ("?" (number->string (get-status)))
    (_ (getvar name dflt))))

(define (word->qword word)
  "Convert @var{word} into a qword by resolving all parameter, command,
and arithmetic substitions."
  (match word
    ((? string?)
     word)
    (('<sh-quote> quoted-word)
     `(<sh-quote> ,(word->qword quoted-word)))
    (('<sh-cmd-sub> . exps)
     ((eval-cmd-sub) exps))
    (('<sh-ref> name)
     (parameter-ref name ""))
    (('<sh-ref-or> name default)
     (or (parameter-ref name)
         (word->qword (or default ""))))
    (('<sh-ref-or*> name default)
     (let ((value (parameter-ref name)))
       (if (string-not-null? value)
           value
           (word->qword (or default "")))))
    (('<sh-ref-or!> name default)
     (or (parameter-ref name)
         (let ((new-value (expand-word (or default "")
                                       #:output 'string #:rhs-tildes? #t)))
           (setvar! name new-value)
           new-value)))
    (('<sh-ref-or!*> name default)
     (let ((value (parameter-ref name)))
       (if (string-not-null? value)
           value
           (let ((new-value (expand-word (or default "")
                                         #:output 'string #:rhs-tildes? #t)))
             (setvar! name new-value)
             new-value))))
    (('<sh-ref-assert> name message) (error "Not implemented"))
    (('<sh-ref-assert*> name message) (error "Not implemented"))
    (('<sh-ref-and> name value)
     (if (string-not-null? (parameter-ref name))
         (word->qword (or value ""))
         ""))
    (('<sh-ref-and*> name value)
     (or (and (parameter-ref name)
              (word->qword (or value "")))
         ""))
    (('<sh-ref-except-min> name pattern) (error "Not implemented"))
    (('<sh-ref-except-max> name pattern) (error "Not implemented"))
    (('<sh-ref-skip-min> name pattern) (error "Not implemented"))
    (('<sh-ref-skip-max> name pattern) (error "Not implemented"))
    (('<sh-ref-length> name)
     (number->string (string-length (parameter-ref name ""))))
    (_ (map word->qword word))))

(define* (expand-word word #:key (output 'fields) (rhs-tildes? #f))
  "Expand @var{word} into a list of fields."
  ;; The value of '$IFS' may depend on side-effects performed during
  ;; 'word->qword', so use 'let*' here.
  (let* ((qword (word->qword word))
         (ifs (getvar "IFS" (string #\space #\tab #\newline)))
         (pwd (getvar "PWD")))
    (match output
      ('fields (if pwd
                   (append-map (cut expand-pathnames <> pwd ifs)
                               (split-fields qword ifs))
                   (map (cut remove-quotes <> ifs)
                        (split-fields qword ifs))))
      ('string (remove-quotes qword ifs))
      ('pattern (qword->pattern qword ifs)))))
