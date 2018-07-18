;;; The Geesh Shell Interpreter
;;; Copyright 2017, 2018 Timothy Sample <samplet@ngyro.com>
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

(define-module (geesh lexer)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (system base lalr)
  #:export (read-bracketed-command
            read-backquoted-command
            get-token
            get-here-end
            get-here-doc))

;;; Commentary:
;;;
;;; This module contains the lexer for the Shell language.
;;;
;;; Code:

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
  '(("&" . AND)
    ("|" . PIPE)
    (";" . SEMI)
    ("<" . LESS)
    (">" . GREAT)
    ("(" . LPAREN)
    (")" . RPAREN)
    ("&&" . AND-IF)
    ("||" . OR-IF)
    (";;" . DSEMI)
    ("<<" . DLESS)
    (">>" . DGREAT)
    ("<&" . LESSAND)
    (">&" . GREATAND)
    ("<>" . LESSGREAT)
    ("<<-" . DLESSDASH)
    (">|" . CLOBBER)))

(define *reserved-words*
  '(("!" . Bang)
    ("{" . Lbrace)
    ("}" . Rbrace)
    ("case" . Case)
    ("do" . Do)
    ("done" . Done)
    ("elif" . Elif)
    ("else" . Else)
    ("esac" . Esac)
    ("fi" . Fi)
    ("for" . For)
    ("if" . If)
    ("in" . In)
    ("then" . Then)
    ("until" . Until)
    ("while" . While)))

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

(define io-number?
  (let ((char-set:ascii-digit (char-set-intersection char-set:ascii
                                                     char-set:digit)))
    (lambda (str)
      (and (string? str)
           (string-every ascii-digit-char? str)))))

(define (assignment-word? word)
  (define (name-then-=? str)
    (match (string-index str #\=)
      (#f #f)
      (index (name? (substring str 0 index)))))
  (match word
    (((? string? str) . tail) (name-then-=? str))
    ((? string? str) (name-then-=? str))
    (_ #f)))

(define (reserved-word? word)
  (assoc word *reserved-words*))

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

(define read-bracketed-command
  ;; A procedure for reading a bracketed command (e.g, "$(command)").
  ;; This is parameterized to avoid a circular dependency.
  (make-parameter (lambda (port) (throw 'bracketed-command-parser-unset))))

(define read-backquoted-command
  ;; A procedure for reading a backquoted command (e.g, "`command`").
  ;; This is parameterized to avoid a circular dependency.
  (make-parameter (lambda (port) (throw 'backquoted-command-parser-unset))))

(define* (get-parameter port #:key (multidigit? #f))
  "Get a parameter name (excluding the leading '$') from @var{port}.
If @var{multidigit?} is true, treat strings of numbers as a valid
name.  If a valid parameter name cannot be read from @var{port},
nothing will be read and @code{#f} will be returned."
  (match (lookahead-char port)
    ;; Special parameter names
    ((or #\@ #\* #\# #\? #\- #\$ #\! #\0)
     (string (get-char port)))

    ;; Numeric parameter names (excluding "0")
    ((? ascii-digit-char? digit)
     (if multidigit?
         (let loop ((chr (next-char port)) (acc `(,digit)))
           (match chr
             ((? ascii-digit-char?) (loop (next-char port) (cons chr acc)))
             (_ (list->string (reverse! acc)))))
         (string (get-char port))))

    ;; Regular names ("[a-zA-Z_][a-zA-Z0-9_]*").
    ((? name-start-char? start-chr)
     (let loop ((chr (next-char port)) (acc `(,start-chr)))
       (match chr
         ((? name-char?) (loop (next-char port) (cons chr acc)))
         (_ (list->string (reverse! acc))))))
    ;; Not a parameter name.
    (_ #f)))

(define *parameter-operators*
  ;; Associate Scheme-like names to all of the Shell parameter
  ;; operators.  Note that "#" means the infix version and not the
  ;; prefixed "#", which means "length".
  '(("-" . <sh-ref-or>)
    (":-" . <sh-ref-or*>)
    ("=" . <sh-ref-or!>)
    (":=" . <sh-ref-or!*>)
    ("?" . <sh-ref-assert>)
    (":?" . <sh-ref-assert*>)
    ("+" . <sh-ref-and>)
    (":+" . <sh-ref-and*>)
    ("%" . <sh-ref-except-min>)
    ("%%" . <sh-ref-except-max>)
    ("#" . <sh-ref-skip-min>)
    ("##" . <sh-ref-skip-max>)))

(define (try-get-parameter-operator port)
  "Try to get a Shell parameter operator from @var{port}. Upon failure,
return #f and use 'unget-char' to return at most one character back to
@var{port}."
  (match (lookahead-char port)
    ((or #\- #\= #\? #\+) (string (get-char port)))
    (#\: (match (next-char port)
           ((or #\- #\= #\? #\+) (string #\: (get-char port)))
           (_ (unget-char port #\:)
              #f)))
    (#\% (match (next-char port)
           (#\% (string #\% (get-char port)))
           (_ "%")))
    (#\# (match (next-char port)
           (#\# (string #\# (get-char port)))
           (_ "#")))
    (_ #f)))

(define (get-parameter-word port)
  "Get a parameter word (the bit that comes after the operator in a
parameter expression) from @var{port}."

  (define (get-parameter-word-string port)
    (let loop ((chr (lookahead-char port)) (acc '()))
      (match chr
        ((? eof-object?) (throw 'lex-error))
        ((or #\{ #\} #\" #\' #\$ #\` #\\) (list->string (reverse! acc)))
        (_ (loop (next-char port) (cons chr acc))))))

  (let loop ((chr (lookahead-char port)) (brace-balance 0) (acc '()))
    (match chr
      ((or #\$ #\`) (let ((expansion (get-expansion port)))
                      (loop (lookahead-char port)
                            brace-balance
                            (cons (or expansion (string chr)) acc))))
      (#\\ (let ((escape (get-escape port)))
             (loop (lookahead-char port) brace-balance (append escape acc ))))
      (#\' (let ((quotation (get-single-quotation port)))
             (loop (lookahead-char port) brace-balance (cons quotation acc))))
      (#\" (let ((quotation (get-double-quotation port)))
             (loop (lookahead-char port) brace-balance (cons quotation acc))))
      (#\{ (loop (next-char port) (+ brace-balance 1) (cons "{" acc)))
      (#\} (if (= brace-balance 0)
               (match (join-contiguous-strings (reverse! acc))
                 ((str) str)
                 (x x))
               (loop (next-char port) (- brace-balance 1) (cons "}" acc))))
      (_ (let ((str (get-parameter-word-string port)))
           (loop (lookahead-char port)
                 brace-balance
                 (if (not (string-null? str))
                     (cons str acc)
                     acc)))))))

(define (get-parameter-expression port)
  "Get a parameter expression ('${...}') from @var{port} (excluding the
leading '$')."
  (match (get-char port)
    (#\{ (match (lookahead-char port)
           (#\#
            (get-char port)
            (let ((parameter (get-parameter port #:multidigit? #t)))
              (match (get-char port)
                (#\} `(<sh-ref-length> ,parameter)))))
           (_
            (let* ((parameter (get-parameter port #:multidigit? #t))
                   (operator  (assoc-ref *parameter-operators*
                                         (try-get-parameter-operator port)))
                   (word      (if operator
                                  (match (lookahead-char port)
                                    (#\} #f)
                                    (_ (get-parameter-word port)))
                                  #f)))
              (match (get-char port)
                (#\} (match `(,parameter ,operator ,word)
                       ((p #f #f) `(<sh-ref> ,p))
                       ((p o #f)  `(,o ,p #f))
                       ((p o w)   `(,o ,p ,w)))))))))))

(define (get-parameter-expansion port)
  "Get a parameter expansion (either '$name' or '${...}') from
@var{port} (excluding the leading '$')."
  (match (lookahead-char port)
    (#\{ (get-parameter-expression port))
    (_ (and=> (get-parameter port)
              (lambda (name)
                `(<sh-ref> ,name))))))

(define (get-bracketed-command port)
  "Get a bracketed command ('$(...)') from @var{port} (excluding the
leading '$')."
  (match (get-char port)
    (#\(
     (let ((result ((read-bracketed-command) port)))
       (match (get-char port)
         (#\) `(<sh-cmd-sub> ,result)))))))

(define (get-backquoted-command port)
  "Get a backquoted command ('`...`') from @var{port}."
  (match (get-char port)
    (#\`
     (let ((result ((read-backquoted-command) port)))
       (match (get-char port)
         (#\` `(<sh-cmd-sub> ,result)))))))

(define (get-expansion port)
  "Get an expansion ('$name', '${...}', '$(...)', or '`...`') from
@var{port}."
  (match (lookahead-char port)
    (#\$ (begin
           (get-char port)
           (match (lookahead-char port)
             (#\( (get-bracketed-command port))
             (_ (get-parameter-expansion port)))))
    (#\` (get-backquoted-command port))))

;; When this parameter is true, expansion processing is enabled.
(define expansions? (make-parameter #t))

(define* (get-escape port #:optional (pred (lambda _ #t)))
  "Get an escape sequence ('\\x') from @var{port}. If @var{pred} is set,
then the backslash will be treated as a literal backslash unless the
next character statisfies @var{pred} (or is a newline)."
  (match (get-char port)
    (#\\
     (let ((chr (lookahead-char port)))
       (match chr
         (#\newline (begin (get-char port) '()))
         ((and (? char?) (? pred)) (begin (get-char port)
                                          `((<sh-quote> ,(string chr)))))
         (_ `(,(string #\\))))))))

(define (get-single-quotation port)
  "Get a single-quote wrapped string from @var{port}."
  (match (get-char port)
    (#\'
     (let loop ((chr (get-char port)) (acc '()))
       (match chr
         (#\' `(<sh-quote> ,(list->string (reverse! acc))))
         (x (loop (get-char port) (cons x acc))))))))

(define (get-double-quotation port)
  "Get a double-quote wrapped string from @var{port}."

  (define (get-double-quotation-string port)
    (let loop ((chr (lookahead-char port)) (acc '()))
      (match chr
        ((? eof-object?) (throw 'lex-error))
        ((or #\" #\$ #\` #\\) (list->string (reverse! acc)))
        (_ (loop (next-char port) (cons chr acc))))))

  (match (get-char port)
    (#\"
     (let loop ((chr (lookahead-char port)) (acc '()))
       (match chr
         (#\" (begin
                (get-char port)
                `(<sh-quote> ,(match (join-contiguous-strings (reverse! acc))
                                ((word) word)
                                (words words)))))
         ((or #\$ #\`)
          (if (expansions?)
              (let ((expansion (get-expansion port)))
                (loop (lookahead-char port)
                      (cons (or expansion (string chr)) acc)))
              (loop (next-char port) (cons (string chr) acc))))
         (#\\ (let ((escape (get-escape port
                                        (cut member <> '(#\" #\$ #\` #\\)))))
                (loop (lookahead-char port) (append escape acc))))
         (_ (let ((str (get-double-quotation-string port)))
              (loop (lookahead-char port) (if (not (string-null? str))
                                              (cons str acc)
                                              acc)))))))))

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
             #\newline #\#
             #\$ #\` #\' #\" #\\) (list->string (reverse! acc)))
        (_ (loop (next-char port) (cons chr acc))))))

  (define* (acc->token acc #:optional (delimiter #f))
    (match (join-contiguous-strings (reverse! acc))
      ((str) (match str
               ((? io-number?) (if (member delimiter '(#\< #\>))
                                   `(IO-NUMBER . ,str)
                                   `(WORD . ,str)))
               ((? reserved-word?) `(,(assoc-ref *reserved-words* str) . ,str))
               ((? name?) `(NAME . ,str))
               ((? assignment-word?) `(ASSIGNMENT-WORD . ,str))
               (_ `(WORD . ,str))))
      (lst (match lst
             ((? assignment-word?) `(ASSIGNMENT-WORD . ,lst))
             (_ `(WORD . ,lst))))))

  (let loop ((chr (lookahead-char port)) (acc '()))
    (match chr
      ((or (? eof-object?)
           (? operator-prefix-char?)
           (? blank?)
           #\newline
           #\#) (acc->token acc chr))
      ((or #\$ #\`)
       (if (expansions?)
           (let ((expansion (get-expansion port)))
             (loop (lookahead-char port)
                   (cons (or expansion (string chr)) acc)))
           (loop (next-char port) (cons (string chr) acc))))
      (#\\ (let ((escape (get-escape port)))
             (loop (lookahead-char port) (append escape acc))))
      (#\' (let ((quotation (get-single-quotation port)))
             (loop (lookahead-char port) (cons quotation acc))))
      (#\" (let ((quotation (get-double-quotation port)))
             (loop (lookahead-char port) (cons quotation acc))))
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
      ((? operator-prefix-char?) (get-operator-lexical-token port))
      ((? blank?) (loop (next-char port)))
      (#\# (begin
             (skip-to-end-of-line port)
             (loop (lookahead-char port))))
      (#\newline (get-newline-lexical-token port))
      (#\\ (match (next-char port)
             (#\newline (loop (next-char port)))
             (_ (unget-char port #\\)
                (get-word-lexical-token port))))
      (_ (get-word-lexical-token port)))))


;;; Here-documents.

(define (get-here-end port)
  "Get the next lexical token from @var{port}, using the special rules
for lexing a here-end word.  Namely, do not treat expansions
(parameters, command substitutions, etc.) specially."
  (parameterize ((expansions? #f))
    (get-token port)))

(define (get-quoted-here-doc end port)
  "Get a quoted here-document string from @var{port}, where @var{end}
marks the end of the here-document."
  (let loop ((line (read-line port 'concat)) (acc '()))
    (if (eof-object? line)
        ;; XXX: Following Bash, we should issue a warning here.
        `(<sh-quote> ,(string-concatenate-reverse acc))
        (let ((line* (string-trim-right line #\newline)))
          (if (string=? line* end)
              `(<sh-quote> ,(string-concatenate-reverse acc))
              (loop (read-line port 'concat) (cons line acc)))))))

(define (get-unquoted-here-doc end port)
  "Get an unquoted here-document string from @var{port}, where
@var{end} marks the end of the here-document."

  (define end-list (string->list end))

  (define (get-unquoted-here-doc-string port)
    (let loop ((chr (lookahead-char port)) (acc '()))
      (match chr
        ((or #\$ #\` #\\ (? eof-object?)) (list->string (reverse! acc)))
        (#\newline (list->string (reverse! (cons (get-char port) acc))))
        (_ (loop (next-char port) (cons chr acc))))))

  (let loop ((chr (lookahead-char port))
             (end end-list)
             (end-acc '())
             (acc '()))
    (cond
     ;; We've read the end string and are looking at newline or EOF.
     ((and (null? end)
           (or (eof-object? chr)
               (char=? chr #\newline)))
      (get-char port)
      `(<sh-quote> ,(match (join-contiguous-strings (reverse! acc))
                      ((word) word)
                      (words words))))
     ;; We've hit EOF prematurely.
     ((eof-object? chr)
      ;; XXX: Following Bash, we should issue a warning here.
      (let* ((end-str (list->string (reverse! end-acc)))
             (acc (if (string-null? end-str) acc (cons end-str acc))))
        `(<sh-quote> ,(match (join-contiguous-strings (reverse! acc))
                        ((word) word)
                        (words words)))))
     ;; We've read another character from the end string.
     ((and (pair? end)
           (char=? (car end) chr))
      (loop (next-char port) (cdr end) (cons chr end-acc) acc))
     ;; We've read a non-end-string character, and have some
     ;; characters from the end string already read.
     ((pair? end-acc)
      (loop chr #f '() (cons (list->string (reverse! end-acc)) acc)))
     ;; We've nothing to do with the end string.
     (else
      (match chr
        ((or #\$ #\`) (let ((expansion (get-expansion port)))
                        (loop (lookahead-char port) #f '()
                              (cons (or expansion (string chr)) acc))))
        (#\\ (let ((escape (get-escape port (cut member <> '(#\$ #\` #\\)))))
               (loop (lookahead-char port) #f '() (append escape acc))))
        (_ (let ((str (get-unquoted-here-doc-string port)))
             (loop (lookahead-char port) end-list '()
                   (if (not (string-null? str))
                       (cons str acc)
                       acc)))))))))

(define (wrap-port-with-tab-trimming port)
  "Wrap @var{port} filtering out all tabs that occur at the beginning
of a line."
  (define after-newline? #t)
  (make-soft-port
   (vector
    ;; put-char, put-string, and flush-output-port
    #f #f #f
    ;; get-char
    (lambda ()
      (if after-newline?
          (let loop ((chr (get-char port)))
            (match chr
              (#\tab (loop (get-char port)))
              (#\newline chr)
              (_ (set! after-newline? #f)
                 chr)))
          (match (get-char port)
            (#\newline
             (set! after-newline? #t)
             #\newline)
            (chr chr))))
    ;; close-port
    #f)
   "r"))

(define* (get-here-doc end port #:key (trim-tabs? #f) (quoted? #f))
  "Get a here-document token from @var{port}, using @var{end} to
signal the ending.  If @var{trim-tabs?} is set, remove leading tabs
from each line.  If @var{quoted?} is set, ignore substitutions."
  ((get-token->get-lexical-token
    (lambda (port)
      (let ((port (if trim-tabs? (wrap-port-with-tab-trimming port) port)))
        `(HERE-DOC . ,(if quoted?
                          (get-quoted-here-doc end port)
                          (get-unquoted-here-doc end port))))))
   port))
