;;; Gash -- Guile As SHell
;;; Copyright © 2018 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gash commands awk parser)
  #:use-module (gash commands awk lexer)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-41)
  #:use-module (system base lalr)
  #:export (read-awk))

;;; Commentary:

;; This module contains the parser for the Awk language.

;;; Code:

;; The (ice-9 textual-ports) module does not allow instantiating
;; end-of-file objects, but (rnrs io ports) does.
(define eof-object (@ (rnrs io ports) eof-object))

(define (make-lexer port)
  "Make a lexer thunk that reads tokens from @var{port}."
  (lambda _ (get-token port)))

(define* (make-parser)
  "Make an LALR parser for the Awk language."
  (lalr-parser
   (
    ;; lowest precedence first
    COMMA
    LBRACE
    LBRACKET
    LPAREN
    NEWLINE
    NO_MATCH
    RBRACE
    RBRACKET
    (left: RPAREN)
    SEMI

    Begin
    Break
    Builtin
    Continue
    Delete
    Do
    For
    Func
    Function
    (left: Else)
    End
    Exit
    If
    Next
    Print
    Return
    While

    NAME
    NUMBER
    REGEX
    STRING

    (left: ||)
    (left: &&)
    (left: < <= != == > >= ~ !~)
    (right: ^)
    (left: !)
    (left: + -)
    (left: * / %)
    (left: ++ --)

    (left: In)
    (right: ? :)
    (right: = ^= %= *= /= += -=)
    (left: $))

   (program
    (item-list) : $1
    (actionless-item-list): $1)

   (item-list
    (newline-opt) : '()
    (actionless-item-list item terminator) : `(,@$1 ,$2)
    (item-list item terminator) : `(,@$1 ,$2)
    (item-list action terminator) : `(,@$1 ,$2))

   (actionless-item-list
    (item-list pattern terminator) : `(,$1 ,$2)
    (actionless-item-list pattern terminator) : `(,@$1 ,$2))

   (item
    (pattern action) : `(<awk-item> ,$1 ,$2)
    (Function NAME LPAREN param-list-opt RPAREN newline-opt action) : '(<awk-function>)
    ;;(Function FUNC_NAME LPAREN param-list-opt RPAREN newline-opt action) '(<awk-function>)
    )

   (param-list-opt
    () : '(<awk-param-list>)
    (param-list) : $1)

   (param-list
    (NAME) : $1
    (param-list COMMA NAME) : `(,$2 ,@$1))

   (pattern
    (Begin) : '(<awk-begin>)
    (End) : '(<awk-end>)
    (expr) : `(<awk-pattern> ,$1)
    (expr COMMA newline-opt expr) : `(<awk-pattern> ,$1 ,$2))

   (action
    (LBRACE newline-opt RBRACE) : '(<awk-action>)
    (LBRACE newline-opt terminated-statement-list RBRACE) : `(<awk-action> ,@$3)
    (LBRACE newline-opt unterminated-statement-list RBRACE) : `(<awk-action> ,@$3))

   (terminator
    (terminator SEMI) : '(<awk-terminator>)
    (terminator NEWLINE) : '(<awk-terminator>)
    (SEMI) : '(<awk-terminator>)
    (NEWLINE) : '(<awk-terminator>))

   (terminated-statement-list
    (terminated-statement) : `(,$1)
    (terminated-statement-list terminated-statement) : `(,@$1 ,$2))

   (unterminated-statement-list
    (unterminated-statement) : `(,$1)
    (terminated-statement-list unterminated-statement) : `(,@$1 ,$2))

   (terminated-statement
    (action newline-opt) : $1
    (If LPAREN expr RPAREN newline-opt terminated-statement) : `(<awk-if> ,$3 ,$6)
    (If LPAREN expr RPAREN newline-opt terminated-statement Else newline-opt terminated-statement) : `(<awk-if> ,$3 ,$6 ,$9)
    (While LPAREN expr RPAREN newline-opt terminated-statement) : `(<awk-while> ,$3 ,$5)
    (For LPAREN simple-statement-opt SEMI expr-opt SEMI simple-statement-opt RPAREN newline-opt terminated-statement) : `(<awk-for> ,$3 ,$5 ,$7 ,$10)
    (For LPAREN NAME In NAME RPAREN newline-opt terminated-statement) : `(<awk-for-in> ,$3 ,$5 ,$8)
    (SEMI newline-opt) : '()
    (terminatable-statement NEWLINE newline-opt) : $1
    (terminatable-statement SEMI newline-opt) : $1)

   (unterminated-statement
    (terminatable-statement) : $1
    (If LPAREN expr RPAREN newline-opt unterminated-statement) : `(<awk-if> ,$3 ,$6)
    (If LPAREN expr RPAREN newline-opt terminated-statement Else newline-opt unterminated-statement) : `(<awk-if> ,$3 ,$6 ,$9)
    (While LPAREN expr RPAREN newline-opt unterminated-statement) : `(<awk-while> ,$3 ,$5)
    (For LPAREN simple-statement-opt SEMI expr-opt SEMI simple-statement-opt RPAREN newline-opt unterminated-statement) : `(<awk-for> ,$3 ,$5 ,$7 ,$10)
    (For LPAREN NAME In NAME RPAREN newline-opt unterminated-statement) : `(<awk-for-in> ,$3 ,$5 ,$8))

   (terminatable-statement
    (simple-statement) : $1
    (Break) : `(<awk-break>)
    (Continue) : `(<awk-continue>)
    (Next) : `(<awk-next>)
    (Exit expr-opt) : `(<awk-exit> ,$2)
    (Return expr-opt) : `(<awk-return> ,$2)
    (Do newline-opt terminated-statement While LPAREN expr RPAREN) : `(<awk-do> ,$3 ,$6))

   (simple-statement-opt
    () : '()
    (simple-statement) : $1)

   (simple-statement
    (Delete NAME LBRACKET expr-list RBRACKET) : $1
    (expr) : $1
    (print-statement) : $1)

   (print-statement
    (simple-print-statement) : $1
    ;;(simple-print-statement output-redirection) : ...
    )

   (simple-print-statement
    (Print expr-list-opt) : `(<awk-print> ,@$2)
    (Print LPAREN expr-list RPAREN) : `(<awk-print> ,@$3))

   ;; output-redirection

   (expr-list-opt
    () : '()
    (expr-list) : $1)

   (expr-list
    (expr) : `(,$1)
    (multiple-expr-list) : $1)

   (multiple-expr-list
    (expr COMMA newline-opt expr) : `(,$1 ,$4)
    (multiple-expr-list COMMA newline-opt expr) : `(,@$1 ,$4))

   (expr-opt
    () : '()
    (expr) : $1)

   (expr
    (+ expr) : `(+ ,$2)
    (- expr) : `(- ,$2)
    (LPAREN expr RPAREN) : $2
    (! expr) : `(! ,$2)
    (expr ^ expr) : `(^ ,$1 ,$3)
    (expr * expr) : `(* ,$1 ,$3)
    (expr / expr) : `(/ ,$1 ,$3)
    (expr % expr) : `(% ,$1 ,$3)
    (expr + expr) : `(+ ,$1 ,$3)
    (expr - expr) : `(- ,$1 ,$3)
    (expr < expr) : `(< ,$1 ,$3)
    (expr <= expr) : `(<= ,$1 ,$3)
    (expr != expr) : `(!= ,$1 ,$3)
    (expr == expr) : `(== ,$1 ,$3)
    (expr > expr) : `(> ,$1 ,$3)
    (expr >= expr) : `(>= ,$1 ,$3)
    (expr ~ expr) : `(~ ,$1 ,$3)
    (expr !~ expr) : `(!~ ,$1 ,$3)
    (expr In NAME) : `(<awk-in> ,$1 ,$3)
    (LPAREN multiple-expr-list RPAREN In NAME)
    (expr && expr) : `(&& ,$1 ,$3)
    (expr || expr) : `(|| ,$1 ,$3)
    (expr ? expr : expr) : `(? ,$1 ,$3 ,$5)
    (concat-expr) : $1
    (regex) : $1
    (lvalue ++) : `(<awk-post-inc> ,$1)
    (lvalue --) : `(<awk-post-dec> ,$1)
    (++ lvalue) : `(<awk-pre-inc> ,$1)
    (-- lvalue) : `(<awk-pre-dec> ,$1)
    (lvalue ^= expr) : `(^= ,$1 ,$3)
    (lvalue %= expr) : `(%= ,$1 ,$3)
    (lvalue *= expr) : `(*= ,$1 ,$3)
    (lvalue /= expr) : `(/= ,$1 ,$3)
    (lvalue += expr) : `(+= ,$1 ,$3)
    (lvalue -= expr) : `(-= ,$1 ,$3)
    (lvalue = expr) : `(= ,$1 ,$3))

   (regex
    (regex-start REGEX /) : (begin (set! %regex #f) `(<awk-regex> ,$2)))

   (regex-start
    (/) : (set! %regex #t))

   (concat-expr
    (STRING) : $1
    (STRING concat-expr) : `(<awk-concat> ,$1 ,$2)
    (lvalue) : $1
    (lvalue concat-expr) : `(<awk-concat> ,$1 ,$2)
    (Builtin LPAREN expr-list-opt RPAREN) : `(<awk-call> ,$1 ,$3)
    (Builtin LPAREN expr-list-opt RPAREN concat-expr) : `(<awk-concat> (<awk-call> ,$1 ,$3) ,$5)
    (Func LPAREN expr-list-opt RPAREN) : `(<awk-call> ,$1 ,$3)
    (Func LPAREN expr-list-opt RPAREN concat-expr) : `(<awk-concat> (<awk-call> ,$1 ,$3) ,$5)
    (NUMBER) : $1
    (NUMBER concat-expr) : `(<awk-concat> ,$1 ,$2)
    (Builtin) : `(<awk-call> ,$1)
    (Builtin concat-expr) : `(<awk-concat> (<awk-call> ,$1) ,$2))

   (lvalue
    (NAME) : `(<awk-name> ,$1)
    (NAME LBRACKET expr RBRACKET) : `(<awk-array-ref> ,$1 ,$3)
    ($ expr) : `(<awk-field> ,$2))

   (newline-opt
    () : '()
    (newline-opt NEWLINE) : `(,$1 ,$2))))

(define* (syntax-error message #:optional token)
  "Handle a parser error"
  (if (lexical-token? token)
      (throw 'syntax-error #f message
             (and=> (lexical-token-source token)
                    source-location->source-properties)
             (or (lexical-token-value token)
                 (lexical-token-category token))
             #f)
      (throw 'syntax-error #f message #f token #f)))

(define* (parse port)
  "Parse an Awk script from @var{port}."
  ((make-parser) (make-lexer port) syntax-error))

(define (->command-list code)
  "Make the Awk syntax tree @var{code} a list of commands."
  (match code
    ((? eof-object?) '())
    (((? symbol? tag) . rest) `((,tag . ,rest)))
    (code code)))

(define (call-with-backquoted-input-port port proc)
  "Call @var{proc} with a wrapped version of @var{port} that will
return the end-of-file object upon encountering an unescaped backquote
\"`\" (without consuming the backquote)."
  (define wrapped-port
    (make-soft-port
     (vector
      ;; put-char, put-string, and flush-output-port
      #f #f #f
      ;; get-char
      (lambda ()
        (match (lookahead-char port)
          (#\` (eof-object))
          (#\\ (begin
                 (get-char port)
                 (match (lookahead-char port)
                   ((or #\$ #\` #\\) (get-char port))
                   (_ #\\))))
          (_ (get-char port))))
      ;; close-port
      #f)
     "r"))
  (proc wrapped-port))

(define* (read-awk #:optional (port (current-input-port)))
  "Read Awk script from @var{port} (or from current input port if
@var{port} is unspecified)."
  (->command-list (parse port)))
