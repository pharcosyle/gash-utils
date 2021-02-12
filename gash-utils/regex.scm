;;; Gash-Utils
;;; Copyright © 2018, 2020 Timothy Sample <samplet@ngyro.com>
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

(define-module (gash-utils regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%extended?
            read-re-until))

;;; Commentary:
;;;
;;; This module provides tools for dealing with basic and extended
;;; regular expressions.
;;;
;;; Code:

(define %extended? (make-parameter #f))

(define (next-char port)
  "Discard one character from PORT, and return the next character to
be read."
  (get-char port)
  (lookahead-char port))

(define *special-escapes*
  `((#\a . #\alarm)
    (#\b . #\backspace)
    (#\f . #\page)
    (#\n . #\newline)
    (#\r . #\return)
    (#\t . #\tab)
    (#\v . #\vtab)))

(define (special-escape? chr)
  (assv-ref *special-escapes* chr))

(define (read-bracket-expression port)
  "Read a regular expression bracket expression from PORT,
assuming that it is positioned just after the initial open
bracket (`[').  Return as a string the complete bracket expression,
including both brackets.

This procedure takes into account all the ways that a close
bracket (`]') may occur in a bracket expression without terminating
it, such as named character classes and backslash escapes."

  (define (read-until-pair chr1 chr2 port)
    (let loop ((chunk (read-delimited (string chr1) port 'concat)) (acc '()))
      (unless (and (not (string-null? chunk))
                   (char=? (string-ref chunk (1- (string-length chunk)))
                           chr1))
        (error "Unterminated bracket expression"))
      (if (char=? (lookahead-char port) chr2)
          (string->list (string-concatenate-reverse acc chunk))
          (loop (read-delimited (string chr1) port 'concat)
                (cons chunk acc)))))

  (define (read-rest)
    (let loop ((chr (get-char port)) (acc '()))
      (match chr
        ((? eof-object?) (error "Unterminated bracket expression"))
        (#\] (reverse-list->string (cons #\] acc)))
        (#\[ (match (get-char port)
               ((? eof-object?) (error "Unterminated bracket expression"))
               ((and cc (or #\= #\. #\:))
                (let ((class (read-until-pair cc #\] port))
                      (acc* (cons* cc #\[ acc)))
                  (loop (get-char port) (append-reverse class acc*))))
               (chr (loop (get-char port) (cons* chr #\[ acc)))))
        (#\\ (match (lookahead-char port)
               ((? eof-object?) (error "Unterminated regular expression"))
               ((? special-escape? nchr)
                (get-char port)
                (loop (get-char port)
                      (cons (assv-ref *special-escapes* nchr) acc)))
               (_ (loop (get-char port) (cons chr acc)))))
        (chr (loop (get-char port) (cons chr acc))))))

  (match (lookahead-char port)
    (#\^ (match (next-char port)
           (#\] (get-char port) "[^]")
           (_ (string-append "[^" (read-rest)))))
    (#\] (get-char port) "[]")
    (_ (string-append "[" (read-rest)))))

(define (read-re-until delim port)
  "Read text from PORT as a regular expression until encountering the
delimiting character DELIM.  Return the text of the regular expression
with the trailing delimiter discarded.

This procedure takes into account the ways that the delimiter could
appear in the regular expression without ending it, such as in a
bracket expression or capture group.  In order to determine what
constitutes a capture group, it uses the `%extended?' parameter."
  (define (delim? chr)
    (char=? chr delim))

  (let loop ((chr (lookahead-char port)) (depth 0) (acc '()))
    (cond
     ((eof-object? chr)
      (error "Unterminated regular expression"))
     ((char=? chr #\[)
      (get-char port)
      (let* ((be (read-bracket-expression port))
             (be-chars (string->list be)))
        (loop (lookahead-char port) depth (append-reverse! be-chars acc))))
     ((and (%extended?) (char=? chr #\())
      (loop (next-char port) (1+ depth) (cons #\( acc)))
     ((and (%extended?) (char=? chr #\)))
      (loop (next-char port) (1- depth) (cons #\) acc)))
     ((char=? chr #\\)
      (if (%extended?)
          (match (next-char port)
            ((? eof-object?) (error "Unterminated regular expression"))
            ((? delim?) (loop (next-char port) depth (cons delim acc)))
            ((? special-escape? nchr)
             (loop (next-char port) depth
                   (cons (assv-ref *special-escapes* nchr) acc)))
            (nchr (loop (next-char port) depth (cons* nchr chr acc))))
          (match (next-char port)
            ((? eof-object?) (error "Unterminated regular expression"))
            ((? delim?) (loop (next-char port) depth (cons delim acc)))
            ((? special-escape? nchr)
             (loop (next-char port) depth
                   (cons (assv-ref *special-escapes* nchr) acc)))
            (#\( (loop (next-char port) (1+ depth) (cons* #\( chr acc)))
            (#\) (loop (next-char port) (1- depth) (cons* #\) chr acc)))
            (nchr (loop (next-char port) depth (cons* nchr chr acc))))))
     ((and (= depth 0)
           (char=? chr delim))
      (get-char port)
      (reverse-list->string acc))
     (else (loop (next-char port) depth (cons chr acc))))))
