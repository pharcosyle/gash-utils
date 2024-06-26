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

(define-module (gash commands sed reader)
  #:use-module (gash-utils regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (read-sed
            read-sed-all))

;;; Commentary:
;;;
;;; This module provides a reader for the `sed' stream editing
;;; language.
;;;
;;; Code:

(define (next-char port)
  "Discard one character from PORT, and return the next character to
be read."
  (get-char port)
  (lookahead-char port))

(define (get-char-while cs port)
  "Read text from PORT until a character is found that does not belong
to the character set CS."
  (let loop ((chr (lookahead-char port)) (acc '()))
    (if (or (eof-object? chr)
            (not (char-set-contains? cs chr)))
        (reverse-list->string acc)
        (loop (next-char port) (cons chr acc)))))

(define (read-number port)
  "Read a nonnegative integer from PORT."
  (let* ((str (get-char-while char-set:digit port))
         (n (string->number str)))
    (unless n
      (error "Expected a number"))
    n))

(define (read-string-until delim port)
  "Read text from PORT until encountering the character DELIM,
taking into account escaping with backslashes (`\\')."
  (let loop ((chr (lookahead-char port)) (acc '()))
    (cond
     ((eof-object? chr) (error "Unterminated string"))
     ((char=? chr #\\)
      (let ((next-chr (next-char port)))
        (if (eof-object? next-chr)
            (error "Unterminated string")
            (loop (next-char port) (cons* next-chr chr acc)))))
     ((and (char=? chr delim))
      (get-char port)
      (reverse-list->string acc))
     (else (loop (next-char port) (cons chr acc))))))

(define (read-re port)
  "Read a delimited regular expression from PORT."
  (let ((delim (get-char port)))
    (if (eof-object? delim)
        (error "Expected regular expression")
        (read-re-until delim port))))

(define (read-re+string port)
  "Read a delimited regular expression and a replacement string from
PORT."
  (let ((delim (get-char port)))
    (if (eof-object? delim)
        (error "Expected regular expression and replacement")
        (let* ((re (read-re-until delim port))
               (str (read-string-until delim port)))
          `(,re . ,str)))))

(define (read-string+string port)
  "Read two delimited strings from PORT."
  (let ((delim (get-char port)))
    (if (eof-object? delim)
        (error "Expected characters and their replacements")
        (let* ((str1 (read-string-until delim port))
               (str2 (read-string-until delim port)))
          `(,str1 . ,str2)))))

(define (read-text port)
  "Read text from PORT until either an unescaped newline or end of
file is encountered."
  (get-char-while char-set:whitespace port)
  (let loop ((chr (get-char port)) (acc '()))
    (match chr
      ((or (? eof-object?)
           #\newline)
       (reverse-list->string acc))
      (#\\
       (let ((next-chr (get-char port)))
         (if (eof-object? next-chr)
             (error "Unterminated text")
             (loop (get-char port) (cons next-chr acc)))))
      (_ (loop (get-char port) (cons chr acc))))))

(define char-set:label
  (string->char-set
   (string-append "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                  "abcdefghijklmnopqrstuvwxyz"
                  "0123456789._-")))

(define label-char? (cut char-set-contains? char-set:label <>))

(define (read-label port)
  "Read a label from PORT."
  (get-char-while char-set:blank port)
  (get-char-while char-set:label port))

(define (read-flags port)
  "Read flags (for the `s' command) from PORT."
  (let loop ((chr (lookahead-char port)) (acc '()))
    (match chr
      ((? eof-object?) (reverse! acc))
      (#\g (loop (next-char port) (cons 'g acc)))
      (#\i (loop (next-char port) (cons 'i acc)))
      (#\p (loop (next-char port) (cons 'p acc)))
      ((? (cut char-set-contains? char-set:digit <>))
       (let ((n (read-number port)))
         (loop (lookahead-char port) (cons n acc))))
      (#\w
       (get-char port)
       (let ((filename (read-text port)))
         (reverse! (cons `(w ,filename) acc))))
      (_ (reverse! acc)))))

(define (read-address port)
  "Read an address from PORT."
  (match (lookahead-char port)
    (#\$ (get-char port) '$)
    ((? (cut char-set-contains? char-set:digit <>)) (read-number port))
    (#\\ (get-char port) (read-re port))
    (_ (read-re port))))

(define* (read-function port #:key (depth 0))
  "Read a function and its arguments from PORT."
  (get-char-while char-set:whitespace port)
  (match (get-char port)
    (#\{ `(begin ,@(%read-sed-all port #:depth (1+ depth))))
    (#\a `(a ,(read-text port)))
    (#\b `(b ,(read-label port)))
    (#\c `(c ,(read-text port)))
    (#\d '(d))
    (#\D '(D))
    (#\g '(g))
    (#\G '(G))
    (#\h '(h))
    (#\H '(H))
    (#\i `(i ,(read-text port)))
    (#\l '(l))
    (#\n '(n))
    (#\N '(N))
    (#\p '(p))
    (#\P '(P))
    (#\q '(q))
    (#\r `(r ,(read-text port)))
    (#\s (match-let (((re . str) (read-re+string port)))
           `(s ,re ,str ,(read-flags port))))
    (#\t `(t ,(read-label port)))
    (#\w `(w ,(read-text port)))
    (#\x '(x))
    (#\y (match-let (((str1 . str2) (read-string+string port)))
           `(y ,str1 ,str2)))
    (#\: `(: ,(read-label port)))
    (#\= `(= ,(1+ (port-line port))))
    (#\# `(comment ,(read-line port)))))

(define char-set:function
  (string->char-set "abcdDgGhHilnNpPqrstwxy:=#"))

(define function-char? (cut char-set-contains? char-set:function <>))

(define (read-addresses port)
  "Read zero, one, or two address from PORT, separated by a
comma (`,') and delimited by a function name."
  (match (lookahead-char port)
    ((? function-char?) '())
    (_ (let ((address1 (read-address port)))
         (match (lookahead-char port)
           (#\, (let* ((_ (get-char port))
                       (address2 (read-address port)))
                  `(,address1 ,address2)))
           (_ `(,address1)))))))

(define (read-address-predicate port)
  "Read an \"address predicate\" from PORT.  An address predicate is
the first part of a command, which controls when a function a is run."
  (let* ((apred (match (read-addresses port)
                  (() `always)
                  ((address) `(at ,address))
                  ((address1 address2) `(in ,address1 ,address2)))))
    (get-char-while char-set:whitespace port)
    (match (lookahead-char port)
      (#\! (get-char port) `(not ,apred))
      (_ apred))))

(define char-set:whitespace+semi (char-set-adjoin char-set:whitespace #\;))

(define* (%read-sed port #:key (depth 0))
  "Read a sed command from PORT."
  (get-char-while char-set:whitespace+semi port)
  (match (lookahead-char port)
    ((? eof-object?) (eof-object))
    (#\}
     (get-char port)
     (if (> depth 0)
         (eof-object)
         (error "Unmatched close brace")))
    (_ (let* ((apred (read-address-predicate port))
              (function (read-function port #:depth depth)))
         `(,apred . ,function)))))

(define* (%read-sed-all port #:key (depth 0))
  "Read a sequence of sed commands from PORT."
  (let loop ((cmd (%read-sed port #:depth depth)) (acc '()))
    (match cmd
      ((? eof-object?) (reverse! acc))
      (_ (loop (%read-sed port #:depth depth) (cons cmd acc))))))

(define* (read-sed port #:key (extended? #f))
  "Read a sed command from PORT.  If EXTENDED? is set, treat regular
expressions as extended rather than basic."
  (parameterize ((%extended? extended?))
    (%read-sed port)))

(define* (read-sed-all port #:key (extended? #f))
  "Read a sequence of sed commands from PORT.  If EXTENDED? is set,
treat regular expressions as extended rather than basic."
  (parameterize ((%extended? extended?))
    (%read-sed-all port)))
