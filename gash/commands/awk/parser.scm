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
   (expect: 20)
   (
    ;; lowest precedence first
    $
    COMMA
    LBRACE
    LBRACKET
    LPAREN
    NEWLINE
    NO_MATCH
    RBRACE
    RBRACKET
    RPAREN
    SEMI

    Begin
    Break
    Continue
    Delete
    Do
    For
    Else
    End
    Exit
    If
    Next
    Print
    Return
    While

    NAME
    NUMBER
    STRING

    ;; Must take lower precedence than arithmetic
    (left: ||)
    (left: &&)
    (nonassoc: <)
    (nonassoc: <=)
    (nonassoc: !=)
    (nonassoc: ==)
    (nonassoc: >)
    (nonassoc: >=)
    (nonassoc: ~ !~)

    (right: ^)
    (left: !)
    (left: +)
    (left: -)
    (left: *)
    (left: /)
    (left: %)
    ;; (left: ||)
    ;; (left: &&)

    ;; this is what the spec says, but weird: NR == 1 * 2 + 1 =>
    ;; (+ (* (== (<awk-name> "NR") 1) 2) 1)
    ;; (nonassoc: <)
    ;; (nonassoc: <=)
    ;; (nonassoc: !=)
    ;; (nonassoc: ==)
    ;; (nonassoc: >)
    ;; (nonassoc: >=)
    ;; (nonassoc: ~ !~)

    (left: In)
    (right: ? :)
    (right: =)

    )

   (program
    (item-list) : $1
    (actionless-item-list): $1)

   ;; This works
   (calc-expr
    (calc-expr + calc-expr)           : `(+ ,$1 ,$3)
    (calc-expr * calc-expr)           : `(* ,$1 ,$3)
    (NUMBER) : $1)

   (item-list
    (newline-opt) : '()
    (actionless-item-list item terminator) : `(,@$1 ,$2)
    (item-list item terminator) : `(,@$1 ,$2)
    (item-list action terminator) : `(,@$1 ,$2))

   (actionless-item-list
    (: calc-expr item-list) : `((<calc-expr> ,$2) ,$3)
    (item-list pattern terminator) : `(,$1 ,$2)
    (actionless-item-list pattern terminator) : `(,@$1 ,$2))

   (item
    (pattern action) : `(<awk-item> ,$1 ,$2)
    ;;(Function NAME LPAREN param-list-opt RPAREN newline opt action) : '(<awk-function0>)
    ;;(Function FUNC_NAME LPAREN param-list-opt RPAREN newline opt action) '(<awk-function1>)
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
    (For LPAREN simple-statement-opt SEMI expr-opt SEMI simple-statement-opt SEMI RPAREN newline-opt terminated-statement) : `(<awk-for> ,$3 ,$5 ,$7 ,$11)
    (For LPAREN NAME In NAME RPAREN newline-opt terminated-statement) : `(<awk-for-in> ,$3 ,$5 ,$8)
    (SEMI newline-opt) : '()
    (terminatable-statement NEWLINE newline-opt) : $1
    (terminatable-statement SEMI newline-opt) : $1)

   (unterminated-statement
    (terminatable-statement) : $1
    (If LPAREN expr RPAREN newline-opt unterminated-statement) : `(<awk-if> ,$3 ,$6)
    (If LPAREN expr RPAREN newline-opt terminated-statement Else newline-opt unterminated-statement) : `(<awk-if> ,$3 ,$6 ,$9)
    (While LPAREN expr RPAREN newline-opt unterminated-statement) : `(<awk-while> ,$3 ,$5)
    (For LPAREN simple-statement-opt SEMI expr-opt SEMI simple-statement-opt SEMI RPAREN newline-opt unterminated-statement) : `(<awk-for> ,$3 ,$5 ,$7 ,$11)
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
    (expr) : `(<awk-expr> ,$1)
    (print-statement) : $1)

   (print-statement
    (simple-print-statement) : $1
    ;;(simple-print-statement output-redirection) : ...
    )

   (simple-print-statement
    (Print print-expr-list-opt) : `(<awk-print> ,@$2)
    (Print LPAREN multiple-expr-list RPAREN) : `(<awk-print> ,@$3))

   ;; output-redirection

   (expr-list-opt
    () : '()
    (expr-list) : $1)

   (expr-list
    (expr) : $1
    (multiple-expr-list) : $1
    )

   (multiple-expr-list
    (expr COMMA newline-opt expr) : `(,$1 ,$3)
    (multiple-expr-list COMMA newline-opt expr) : `(,@$1 ,$4))

   (expr-opt
    () : '()
    (expr) : $1)

   ;; This is what the spec says...but it messes up the precedence
   ;; (1 * 2 + 3) == 0 { print } ==>
   ;; (<awk-item>
   ;;   (<awk-pattern> (== (* 1 (+ 2 3)) 0))
   ;;   (<awk-action> (<awk-print>)))
   ;; (expr
   ;;  (unary-expr) : $1
   ;;  (non-unary-expr) : $1)

   ;; (unary-expr
   ;;  (+ expr) : `(+ ,$2)
   ;;  (- expr) : `(- ,$2)
   ;;  (unary-expr ^ expr) : `(^ ,$1 ,$3)
   ;;  (unary-expr * expr) : `(* ,$1 ,$3)
   ;;  (unary-expr / expr) : `(/ ,$1 ,$3)
   ;;  (unary-expr % expr) : `(% ,$1 ,$3)
   ;;  (unary-expr + expr) : `(+ ,$1 ,$3)
   ;;  (unary-expr - expr) : `(- ,$1 ,$3)
   ;;  (unary-expr non-unary-expr) : `(,@$1 ,$2)
   ;;  (unary-expr < expr) : `(< ,$1 ,$3)
   ;;  (unary-expr <= expr) : `(<= ,$1 ,$3)
   ;;  (unary-expr != expr) : `(!= ,$1 ,$3)
   ;;  (unary-expr == expr) : `(== ,$1 ,$3)
   ;;  (unary-expr > expr) : `(> ,$1 ,$3)
   ;;  (unary-expr >= expr) : `(>= ,$1 ,$3)
   ;;  (unary-expr ~ expr) : `(~ ,$1 ,$3)
   ;;  (unary-expr NO_MATCH expr) : `(<awk-no-match> ,$1 ,$3)
   ;;  (unary-expr In NAME) : `(<awk-in> ,$1 ,$3)
   ;;  (unary-expr && expr) : `(&& ,(warn 'UN-&& "1=" $1) ,(warn 'UN-&& "3= " $3))
   ;;  (unary-expr || expr) : `(|| ,$1 ,$3)
   ;;  (unary-expr ? expr : expr) : `(? ,$1 ,$3 ,$5))

   ;; (non-unary-expr
   ;;  (LPAREN expr RPAREN) : $2
   ;;  (! expr) : `(! ,$2)
   ;;  (non-unary-expr ^ expr) : `(^ ,$1 ,$3)
   ;;  (non-unary-expr * expr) : `(* ,$1 ,$3)
   ;;  (non-unary-expr / expr) : `(/ ,$1 ,$3)
   ;;  (non-unary-expr % expr) : `(% ,$1 ,$3)
   ;;  (non-unary-expr + expr) : `(+ ,$1 ,$3)
   ;;  (non-unary-expr - expr) : `(- ,$1 ,$3)
   ;;  (non-unary-expr non-unary-expr) : `(,@$1 ,$2)
   ;;  (non-unary-expr < expr) : `(< ,$1 ,$3)
   ;;  (non-unary-expr <= expr) : `(<= ,$1 ,$3)
   ;;  (non-unary-expr != expr) : `(!= ,(warn 'ne "1=" $1) ,(warn 'ne "3= "$3))
   ;;  (non-unary-expr == expr) : `(== ,$1 ,$3)
   ;;  (non-unary-expr > expr) : `(> ,$1 ,$3)
   ;;  (non-unary-expr >= expr) : `(>= ,$1 ,$3)
   ;;  (non-unary-expr ~ expr) : `(~ ,$1 ,$3)
   ;;  (non-unary-expr NO_MATCH expr) : `(<awk-no-match> ,$1 ,$3)
   ;;  (non-unary-expr In NAME) : `(<awk-in> ,$1 ,$3)
   ;;  ;; ( multiple...)
   ;;  ;;(non-unary-expr && expr) : `(&& ,$1 ,$3)
   ;;  (non-unary-expr && expr) : `(&& ,(warn 'NON-&& "1=" $1) ,(warn 'NON-&& "3= " $3))
   ;;  (non-unary-expr || expr) : `(|| ,$1 ,$3)
   ;;  (non-unary-expr ? expr : expr) : `(? ,$1 ,$3 ,$5)
   ;;  (NUMBER) : $1
   ;;  (STRING) : $1
   ;;  (lvalue) : $1
   ;;  ;; ERE
   ;;  (lvalue = expr) : `(<awk-assign> ,$1 ,$3)
   ;;  ;;
   ;;  )

   ;; This works
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
    ;;(expr expr) : `(,@$1 ,$2)
    (expr < expr) : `(< ,$1 ,$3)
    (expr <= expr) : `(<= ,$1 ,$3)
    (expr != expr) : `(!= ,$1 ,$3)
    (expr == expr) : `(== ,$1 ,$3)
    (expr > expr) : `(> ,$1 ,$3)
    (expr >= expr) : `(>= ,$1 ,$3)
    (expr ~ expr) : `(~ ,$1 ,$3)
    (expr NO_MATCH expr) : `(<awk-no-match> ,$1 ,$3)
    (expr In NAME) : `(<awk-in> ,$1 ,$3)
    ;; ( multiple...)
    (expr && expr) : `(&& ,$1 ,$3)
    (expr || expr) : `(|| ,$1 ,$3)
    (expr ? expr : expr) : `(? ,$1 ,$3 ,$5)
    (NUMBER) : $1
    (STRING) : $1
    (lvalue) : $1
    ;; ERE
    (lvalue = expr) : `(= ,$1 ,$3)

    )

   (print-expr-list-opt
    () : '()
    (print-expr-list) : $1)

   (print-expr-list
    (print-expr) : `(,$1)
    (print-expr-list print-expr) : `(,@$1 ,$2)
    (print-expr-list COMMA newline-opt print-expr) : `(,@$1 ,$4))

   ;; (print-expr
   ;;  (unary-print-expr) : $1
   ;;  (non-unary-print-expr) : $1)

   ;; (unary-print-expr
   ;;  (+ print-expr) : `(+ ,$2)
   ;;  (- print-expr) : `(- ,$2)
   ;;  ;;;
   ;;  (unary-print-expr non-unary-print-expr) : `(,$1 ,$2)
   ;;  ;;;
   ;;  )

   ;; (non-unary-print-expr
   ;;  (LPAREN expr RPAREN) : $2
   ;;  (! print-expr) : `(! ,$2)
   ;;  (non-unary-print-expr non-unary-print-expr) : `(,$1 ,$2)
   ;;  (NUMBER) : $1
   ;;  (STRING) : $1
   ;;  (lvalue) : $1
   ;;  ;;;
   ;;  )
   ;; ;;;

   (print-expr
    (LPAREN expr RPAREN) : $2
    (! print-expr) : `(! ,$2)
    ;;(print-expr print-expr) : `(,$1 ,@$2)
    (NUMBER) : $1
    (STRING) : $1
    (lvalue) : $1
    )

   (lvalue
    (NAME) : `(<awk-name> ,$1)
    ;;(NAME LBRACKET expr-list RBRACKET) : `(<awk-array-ref> ,$1 ,$3)
    (NAME LBRACKET expr RBRACKET) : `(<awk-array-ref> ,$1 ,$3)
    ($ expr) : `(<awk-field> ,$2))

   (newline-opt
    () : '()
    (newline-opt NEWLINE) : `(,$1 ,$2))

   ))

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
