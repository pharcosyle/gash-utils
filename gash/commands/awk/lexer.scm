;;; Gash-Utils
;;; Copyright © 2017, 2018, 2020 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gash commands awk lexer)
  #:use-module (gash compat textual-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (system base lalr)
  #:export (get-token
            %regex))

;;; Commentary:

;; This module contains the lexer for the Awk language.

;;; Code:

(define %regex #f)

(define-record-type <port-location>
  (make-port-location filename line column offset)
  port-location?
  (filename port-location-filename)
  (line port-location-line)
  (column port-location-column)
  (offset port-location-offset))

(define (port->port-location port)
  "Create a <port-location> from a port"
  (make-port-location (port-filename port)
                      (port-line port)
                      (port-column port)
                      (false-if-exception (ftell port))))

(define (complete-source-location port-location len)
  "Create a <source-location> by mixing a <port-location> and a length"
  (make-source-location (port-location-filename port-location)
                        (port-location-line port-location)
                        (port-location-column port-location)
                        (port-location-offset port-location)
                        len))

(define *operators*
  '(("++" . ++)
    ("--" . --)
    ("^" . ^)
    ("+" . +)
    ("-" . -)
    ("*" . *)
    ("/" . /)
    ("%" . %)
    ("!" . !)
    ("!=" . !=)
    ("$" . $)
    ("?" . ?)
    (":" . :)
    ("<" . <)
    ("<=" . <=)
    (">" . >)
    (">=" . >=)

    ("=" . =)
    ("^=" . ^=)
    ("/=" . /=)
    ("*=" . *=)
    ("%=" . %=)
    ("+=" . +=)
    ("-=" . -=)

    ("==" . ==)
    ("!=" . !=)

    ("~" . ~)
    ("!~" . !~)

    ("?" . ?)
    (":" . :)

    ("&&" . &&)
    ("||" . ||)

    (">>" . >>)
    ("|" . PIPE)

    ("," . COMMA)
    (";" . SEMI)
    ("[" . LBRACKET)
    ("]" . RBRACKET)

    ("{" . LBRACE)
    ("}" . RBRACE)

    ("(" . LPAREN)
    (")" . RPAREN)))

(define *reserved-words*
  '(
    ("BEGIN" . Begin)
    ("END" . End)

    ("break" . Break)
    ("continue" . Continue)
    ("do" . Do)
    ("delete" . Delete)
    ("for" . For)
    ("else" . Else)
    ("exit" . Exit)
    ("function" . Function)
    ("getline" . Getline)
    ("if" . If)
    ("in" . In)
    ("next" . Next)
    ("print" . Print)
    ("printf" . Printf)
    ("return" . Return)   
    ("while" . While)))

(define *builtins*
  '(("atan2" . atan2)
    ("close" . close)
    ("cos" . cos)
    ("exp" . exp)
    ("gsub" . gsub)
    ("index" . index)
    ("int" . int)
    ("length" . length)
    ("log" . log)
    ("match" . match)
    ("rand" . rand)
    ("sin" . sin)
    ("split" . split)
    ("sprintf" . sprintf)
    ("sqrt" . sqrt)
    ("srand" . srand)
    ("sub" . sub)
    ("substr" . substr)
    ("system" . system)
    ("tolower" . tolower)
    ("toupper" . toupper)))

(define (operator-prefix? str)
  (any (cut string-prefix? str <>) (map car *operators*)))

(define operator-prefix-char? (compose operator-prefix? string))

(define blank? (cut char-set-contains? char-set:blank <>))

(define name-start-char?
  (let ((char-set:name-start
         (char-set-intersection char-set:ascii
                                (char-set-union char-set:letter
                                                (char-set #\_)))))
    (lambda (chr)
      (and (char? chr)
           (char-set-contains? char-set:name-start chr)))))

(define name-char?
  (let ((char-set:name
         (char-set-intersection char-set:ascii
                                (char-set-union char-set:letter+digit
                                                (char-set #\_)))))
    (lambda (chr)
      (and (char? chr)
           (char-set-contains? char-set:name chr)))))

(define (name? str)
  (and (string? str)
       (not (string-null? str))
       (name-start-char? (string-ref str 0))
       (string-every name-char? str)))

(define ascii-digit-char?
  (let ((char-set:ascii-digit
         (char-set-intersection char-set:ascii
                                char-set:digit)))
    (lambda (chr)
      (and (char? chr)
           (char-set-contains? char-set:ascii-digit chr)))))

(define (reserved-word? word)
  (assoc word *reserved-words*))

(define (builtin? word)
  (assoc word *builtins*))

(define (join-contiguous-strings lst)
  "Join all contiguous strings in @var{lst}."
  (fold-right (lambda (x lst)
                (let ((head (if (null? lst) #f (car lst))))
                  (if (and (string? x) (string? head))
                      (cons (string-append x head) (cdr lst))
                      (cons x lst))))
              '()
              lst))

(define (next-char port)
  "Advance @var{port} by one character and return the lookahead
character."
  (get-char port)
  (lookahead-char port))

(define *escape-map*
  '((#\")
    (#\\)
    (#\/)
    (#\a . #\alarm)
    (#\b . #\backspace)
    (#\f . #\page)
    (#\n . #\newline)
    (#\r . #\return)
    (#\t . #\tab)
    (#\v . #\vtab)))

(define (standard-escape-map chr)
  (match (assoc chr *escape-map*)
    (#f #f)
    ((x . ()) x)
    ((_ . x) x)))

(define* (get-escape port #:optional (escape-map standard-escape-map))
  "Get an escape sequence ('\\x') from @var{port}. If @var{pred} is set,
then the backslash will be treated as a literal backslash unless the
next character statisfies @var{pred} (or is a newline)."
  (match (get-char port)
    (#\\
     (let ((chr (lookahead-char port)))
       (match chr
         (#\newline (begin (get-char port) '()))
         ((? char?)
          (cond
           ((escape-map chr)
            => (lambda (x)
                 (get-char port)
                 (list x)))
           (else (list #\\))))
         (_ `(,(string #\\))))))))

(define (get-regex port)
  "Get a regex from @var{port}."

  (let* ((begin-location (port->port-location port))
         (chars (let loop ((chr (lookahead-char port)))
                  (match chr
                    ((? eof-object?) (throw 'lex-error))
                    (#\/ '())
                    (#\\ (let ((escape (get-escape port)))
                           (append escape (loop (lookahead-char port)))))
                    (_ (cons (get-char port) (loop (lookahead-char port)))))))
         (end-location (port->port-location port))
         (string (apply string chars)))
    (set! %regex #f)
    (make-lexical-token
     'REGEX
     (complete-source-location begin-location (string-length string))
     string)))

(define (get-string port)
  "Get a double quoted string from @var{port}."

  (let* ((begin-location (port->port-location port))
         (foo (get-char port))
         (chars (let loop ((chr (lookahead-char port)))
                  (match chr
                    ((? eof-object?) (throw 'lex-error))
                    (#\" (begin (get-char port) '()))
                    (#\\ (let ((escape (get-escape port)))
                           (append escape (loop (lookahead-char port)))))
                    (_ (cons (get-char port) (loop (lookahead-char port)))))))
         (end-location (port->port-location port))
         (string (apply string chars)))
    (make-lexical-token
     'STRING
     (complete-source-location begin-location (string-length string))
     string)))

(define (get-operator port)
  "Get an operator from @var{port}."
  (let loop ((chr (lookahead-char port)) (acc '()))
    (if (and (not (eof-object? chr))
             (operator-prefix? (list->string (reverse (cons chr acc)))))
        (loop (next-char port) (cons chr acc))
        (let ((operator (list->string (reverse! acc))))
          `(,(assoc-ref *operators* operator) . ,operator)))))

(define (get-word port)
  "Get a word (name, keyword, number, etc.) from @var{port}."

  (define (get-word-string port)
    (let loop ((chr (lookahead-char port)) (acc '()))
      (match chr
        ((or (? eof-object?)
             (? operator-prefix-char?)
             (? blank?)
             #\newline
             #\" #\\) (list->string (reverse! acc)))
        (_ (loop (next-char port) (cons chr acc))))))

  (define* (acc->token acc #:optional (delimiter #f))
    (match (join-contiguous-strings (reverse! acc))
      ((str) (match str
               ((? reserved-word?) `(,(assoc-ref *reserved-words* str) . ,str))
               ((? builtin?) `(Builtin . ,str))
               ((? name?) `(NAME . ,str))
               (_ `(WORD . ,str))))
      (lst lst)))

  (let loop ((chr (lookahead-char port)) (acc '()))
    (match chr
      ((or (? eof-object?)
           (? operator-prefix-char?)
           (? blank?)
           #\"
           #\newline) (acc->token acc chr))
      (_ (let ((str (get-word-string port)))
           (loop (lookahead-char port) (if (not (string-null? str))
                                           (cons str acc)
                                           acc)))))))

(define (call-with-metered-input-port port proc)
  "Call @var{proc} with @var{port} instrumented to count the number of
characters read."
  (define meter 0)
  (define wrapped-port
    (make-soft-port
     (vector
      ;; put-char, put-string, and flush-output-port
      #f #f #f
      ;; get-char
      (lambda ()
        (set! meter (+ meter 1))
        (get-char port))
      ;; close-port
      #f)
     "r"))
  (set-port-line! wrapped-port (port-line port))
  (let ((result (proc wrapped-port)))
    ;; The soft port seems to have an independent buffer from the
    ;; input port. This means that the "lookahead" character on the
    ;; soft port will disappear unless we move it back to the input
    ;; port.
    (when (char? (lookahead-char wrapped-port))
      (unget-char port (lookahead-char wrapped-port)))
    (values meter result)))

(define (get-token->get-lexical-token proc)
  "Convert @var{proc} from a procedure that returns a token-value pair
to a procedure that returns a lexical token to be consumed by the LALR
module."
  (lambda (port)
    (let ((port-location (port->port-location port)))
      (receive (length token)
          (call-with-metered-input-port port proc)
        (match token
          ((category . value)
           (make-lexical-token
            category
            ;; We must use "length - 1" since proc will get a final
            ;; delimiting character that is not part of the token.
            (complete-source-location port-location (- length 1))
            value)))))))

(define get-operator-lexical-token
  (get-token->get-lexical-token get-operator))

(define get-word-lexical-token
  (get-token->get-lexical-token get-word))

(define (get-newline-lexical-token port)
  "Get a newline as a lexical token to be consumed by the LALR module."
  (let ((port-location (port->port-location port)))
    (match (get-char port)
      (#\newline (make-lexical-token
                  'NEWLINE
                  (complete-source-location port-location 1)
                  #\newline)))))

(define (digit->number c)
  (- (char->integer c) (char->integer #\0)))

(define (get-numeric-lexical-token port)
  "Get a newline as a lexical token to be consumed by the LALR module."
  (let ((port-location (port->port-location port)))
    (let loop ((c (peek-char port)) (value 0))
      (if (or (eof-object? c)
              (not (char-numeric? c)))
          (make-lexical-token 'NUMBER (complete-source-location port-location 1) value)
          (begin
            (read-char port)
            (loop (peek-char port) (+ (* 10 value) (digit->number c))))))))

(define (skip-to-end-of-line port)
  "Skip characters from @var{port} until the next character to be read
is a newline (or EOF)."
  (let loop ((chr (lookahead-char port)))
    (match chr
      ((or (? eof-object?) #\newline) #f)
      (_ (loop (next-char port))))))

(define (get-token port)
  "Get the next lexical token from @var{port}."
  (let loop ((chr (lookahead-char port)))
    (match chr
      ((? eof-object?) '*eoi*)
      ((? blank?) (loop (next-char port)))
      (#\# (begin
             (skip-to-end-of-line port)
             (loop (lookahead-char port))))
      (#\newline (get-newline-lexical-token port))
      (#\\ (match (next-char port)
             (#\newline (loop (next-char port)))
             (_ (unget-char port #\\)
                (get-word-lexical-token port))))
      ((? (const %regex)) (get-regex port))
      ((? operator-prefix-char?) (get-operator-lexical-token port))
      ((? char-numeric?)
       (get-numeric-lexical-token port))
      (#\" (get-string port))
      (_ (get-word-lexical-token port)))))
