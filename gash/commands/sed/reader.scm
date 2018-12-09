;;; Gash --- Guile As SHell
;;; Copyright © 2018 Timothy Sample <samplet@ngyro.com>
;;;
;;; This file is part of Gash.
;;;
;;; Gash is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Gash is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Gash.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gash commands sed reader)
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

(define (read-bracket-expression port)
  "Read a regular expression bracket expression from PORT,
assuming that it is positioned just after the initial open
bracket (`[').  Return as a string the complete bracket expression,
including both brackets.

This procedure takes into account all the ways that a close
bracket (`]') may occur in a bracket expression without terminating
it, such as named character classes and backslash escapes."

  (define (read-until-pair chr1 chr2 port)
    (let loop ((chunk (read-delimited chr1 port 'concat)) (acc '()))
      (unless (and (not (string-null? chunk))
                   (char=? (string-ref chunk (1- (string-length chunk)))
                           chr1))
        (error "Unterminated bracket expression"))
      (if (char=? (lookahead-char port) chr2)
          (string->list (string-concatenate (reverse! acc)))
          (loop (read-delimited chr1 port 'concat) (cons chunk acc)))))

  (define (read-rest)
    (let loop ((chr (get-char port)) (acc '()))
      (match chr
        ((? eof-object?) (error "Unterminated bracket expression"))
        (#\] (reverse-list->string (cons #\] acc)))
        (#\[ (match (get-char port)
               ((? eof-object?) (error "Unterminated bracket expression"))
               ((and cc (or #\= #\. #\:))
                (let ((class (read-until-pair cc #\] port)))
                  (loop (get-char port) (append-reverse class acc))))
               (chr (loop (get-char port) (cons* chr #\[ acc)))))
        (#\\ (match (get-char port)
               ((? eof-object?) (error "Unterminated bracket expression"))
               (chr (loop (get-char port) (cons* chr #\\ acc)))))
        (chr (loop (get-char port) (cons chr acc))))))

  (match (lookahead-char port)
    (#\^ (match (next-char port)
           (#\] (get-char port) (string-append "[^]" (read-rest)))
           (_ (string-append "[^" (read-rest)))))
    (#\] (get-char port) (string-append "[]" (read-rest)))
    (_ (string-append "[" (read-rest)))))

(define %extended? (make-parameter #f))

(define (read-re-until delim port)
  "Read text from PORT as a regular expression until encountering the
delimiting character DELIM.  Return the text of the regular expression
with the trailing delimiter discarded.

This procedure takes into account the ways that the delimiter could
appear in the regular expression without ending it, such as in a
bracket expression or capture group.  It order to determine what
constitutes a capture group, it uses the `%extended?' parameter."
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
            (nchr (loop (next-char port) depth (cons* nchr chr acc))))
          (match (next-char port)
            ((? eof-object?) (error "Unterminated regular expression"))
            (#\( (loop (next-char port) (1+ depth) (cons* #\( chr acc)))
            (#\) (loop (next-char port) (1- depth) (cons* #\) chr acc)))
            (nchr (loop (next-char port) depth (cons* nchr chr acc))))))
     ((and (= depth 0)
           (char=? chr delim))
      (get-char port)
      (reverse-list->string acc))
     (else (loop (next-char port) depth (cons chr acc))))))

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
  (get-char-while char-set:whitespace port)
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
    (#\$ '$)
    ((? (cut char-set-contains? char-set:digit <>)) (read-number port))
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
    (_ (let* ((addresses (read-addresses port))
              (function (read-function port #:depth depth)))
         (match addresses
           (() `(always ,function))
           ((address) `(at ,address ,function))
           ((address1 address2) `(in (,address1 . ,address2) ,function)))))))

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
