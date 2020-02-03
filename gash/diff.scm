;;; Gash-Utils
;;; Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of Gash-Utils.
;;;
;;; Gash-Utils is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Gash-Utils is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Gash-Utils.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; The initial ustar.scm was taken from the Guile100 challenge
;;; https://github.com/spk121/guile100 from a contribution by Mark H
;;; Weaver.

;;; Code:

(define-module (gash diff)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 rdelim)

  #:use-module (gash shell-utils)
  #:export (diff-files
            hunk->lines))

(define (plus a)
  (string-append "+" a))
(define (minus a)
  (string-append "-" a))
(define (keep a)
  (string-append " " a))

(define-record-type <hunk> (make-hunk context after removed added)
  hunk?
  (context hunk.context)
  (after hunk.after)
  (removed hunk.removed)
  (added hunk.added))

(define (hunk->lines o)
  (append `(,(format #f "@@ -~a,~a +~a,~a" (length (hunk.removed o)) (+ 3 (car (hunk.context o))) (length (hunk.added o)) (+ 3 (cadr (hunk.context o))))
            ,@(map keep (filter identity (cddr (hunk.context o)))))
          (map minus (hunk.removed o))
          (map plus (hunk.added o))
          (map keep (hunk.after o))))

(define (safe-list-head lst n)
  (list-head lst (min n (length lst))))

(define (line-equal? a b)
  (equal? (string-trim-right a) (string-trim-right b)))

(define (diff-ports a b)
  (let ((a-lines (string-split (read-string a) #\newline))
        (b-lines (string-split (read-string b) #\newline)))
    (let loop ((context '(1 1 #f #f #f)) (a-lines a-lines) (b-lines b-lines))
      ;;(format (current-error-port) "loop context=~s\n" context)
      (cond ((and (null? a-lines) (null? b-lines)) '())
            ((null? a-lines)
             (list (make-hunk context (safe-list-head a-lines 3) '() b-lines)))
            ((null? b-lines)
             (list (make-hunk context (safe-list-head a-lines 3) a-lines '())))
            ((line-equal? (car a-lines) (car b-lines))
             (loop `(,(1+ (car context))
                     ,(1+ (cadr context))
                     ,@(cdddr context)
                     ,(car a-lines))
                   (cdr a-lines) (cdr b-lines)))
            (else
             (cond ((and (pair? (cdr b-lines)) (line-equal? (car a-lines) (cadr b-lines)))
                    (cons (make-hunk context (safe-list-head a-lines 3) '() (list (car b-lines)))
                          (loop `(,(+ 1 (car context))
                                  ,(+ 2 (cadr context))
                                  ,@(cdddr context)
                                  ,(car a-lines))
                                (cdr a-lines) (cddr b-lines))))
                   ((and (pair? (cdr a-lines)) (line-equal? (cadr a-lines) (car b-lines)))
                    (cons (make-hunk context (safe-list-head a-lines 3) (list (car a-lines)) '())
                          (loop `(,(+ 2 (car context))
                                  ,(+ 1 (cadr context))
                                  ,@(cddddr context)
                                  ,(car a-lines)
                                  ,(cadr a-lines))
                                (cddr a-lines) (cdr b-lines))))
                   (else (cons (make-hunk context (safe-list-head a-lines 3) (list (car a-lines)) (list (car b-lines)))
                               (loop `(,(1+ (car context))
                                       ,(1+ (cadr context))
                                       ,@(cdddr context)
                                       ,(car a-lines))
                                     (cdr a-lines) (cdr b-lines))))))))))

(define (diff-files a b)
  (diff-ports (open-input-file* a) (open-input-file* b)))
