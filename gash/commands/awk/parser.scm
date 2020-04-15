;;; Gash-Utils
;;; Copyright © 2018, 2020 Timothy Sample <samplet@ngyro.com>
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

(define-module (gash commands awk parser)
  #:use-module (gash commands awk lexer)
  #:use-module (gash compat textual-ports)
  #:use-module (ice-9 match)
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

(define (strip-progn expr)
  (match expr
    (('progn . body) body)
    (_ (list expr))))

(define (unwrap-singleton lst)
  (match lst
    ((x) x)
    (_ lst)))

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
    RPAREN
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
    Getline
    If
    Next
    Print
    Printf
    Return
    While

    NAME
    NUMBER
    REGEX
    STRING

    >> PIPE

    = ^= %= *= /= += -=
    ! || && < <= != == > >= ~ !~
    ;; We need to mark '/' as right associative for the parser
    ;; generator to accept the ERE (regex) work-around.
    ^ + - * (right: /) %
    ;; Shift these in when possible.  This means that 'n ++ n' reads
    ;; as '(n++) n'.
    (right: ++ --)
    ;; There's an ambiguity in for loops, and marking 'In' as right
    ;; associative lets the parser generator do the right thing
    ;; without complaining.
    (right: In) ? : $)

   ;; Splitting items into terminated and unterminated is not
   ;; standard.  The standard requires that consecutive items be
   ;; separated by a terminator, but implementations allow an action
   ;; to be followed another item without a terminator.  We also need
   ;; to allow this, since the GNU Build System makes use of it.
   (program
    (item-list) : $1
    (item-list unterminated-item) : `(,@$1 ,$2))

   (item-list
    (newline-opt) : '()
    (item-list terminated-item terminator-opt) : `(,@$1 ,$2)
    (item-list unterminated-item terminator) : `(,@$1 ,$2))

   (terminated-item
    (action) : `(#t ,@$1)
    (pattern action) : `(,$1 ,@$2)
    (Function name LPAREN param-list-opt RPAREN newline-opt action)
    : `(defun ,$2 ,$4 ,@$7)
    ;;(Function FUNC_NAME LPAREN param-list-opt RPAREN newline-opt action) '(<awk-function>)
    )

   (unterminated-item
    (normal-pattern) : `(,$1 (print)))

   (param-list-opt
    () : '()
    (param-list) : $1)

   (param-list
    (name) : `(,$1)
    (param-list COMMA name) : `(,@$1 ,$3))

   (pattern
    (normal-pattern) : $1
    (special-pattern) : $1)

   (normal-pattern
    (expr) : $1
    (expr COMMA newline-opt expr) : `(,$1 ,$4))

   (special-pattern
    (Begin) : 'begin
    (End) : 'end)

   (action
    (LBRACE newline-opt RBRACE) : '()
    (LBRACE newline-opt terminated-statement-list RBRACE) : $3
    (LBRACE newline-opt unterminated-statement-list RBRACE) : $3)

   (terminator-opt
    () : #f
    (terminator) : $1)

   (terminator
    (terminator SEMI) : $2
    (terminator NEWLINE) : $2
    (SEMI) : $1
    (NEWLINE) : $1)

   (terminated-statement-list
    (terminated-statement) : `(,$1)
    (terminated-statement-list terminated-statement) : `(,@$1 ,$2))

   (unterminated-statement-list
    (unterminated-statement) : `(,$1)
    (terminated-statement-list unterminated-statement) : `(,@$1 ,$2))

   (terminated-statement
    (action newline-opt) : (match $1
                             ((expr) expr)
                             (exprs `(progn ,@exprs)))
    (If LPAREN expr RPAREN newline-opt terminated-statement)
    : `(if ,$3 ,$6)
    (If LPAREN expr RPAREN newline-opt terminated-statement
        Else newline-opt terminated-statement)
    : `(if ,$3 ,$6 ,$9)
    (While LPAREN expr RPAREN newline-opt terminated-statement)
    : `(while ,$3 ,@(strip-progn $6))
    (For LPAREN simple-statement-opt SEMI expr-opt SEMI
         simple-statement-opt RPAREN newline-opt terminated-statement)
    : `(for (,$3 ,$5 ,$7) ,@(strip-progn $10))
    (For LPAREN name In name RPAREN newline-opt terminated-statement)
    : `(for-each (,$3 ,$5) ,@(strip-progn $8))
    (SEMI newline-opt) : '(progn)
    (terminatable-statement NEWLINE newline-opt) : $1
    (terminatable-statement SEMI newline-opt) : $1)

   (unterminated-statement
    (terminatable-statement) : $1
    (If LPAREN expr RPAREN newline-opt unterminated-statement)
    : `(if ,$3 ,$6)
    (If LPAREN expr RPAREN newline-opt terminated-statement
        Else newline-opt unterminated-statement)
    : `(if ,$3 ,$6 ,$9)
    (While LPAREN expr RPAREN newline-opt unterminated-statement)
    : `(while ,$3 ,@(strip-progn $6))
    (For LPAREN simple-statement-opt SEMI expr-opt SEMI
         simple-statement-opt RPAREN newline-opt unterminated-statement)
    : `(for (,$3 ,$5 ,$7) ,@(strip-progn $10))
    (For LPAREN name In name RPAREN newline-opt unterminated-statement)
    : `(for-each (,$3 ,$5) ,@(strip-progn $8)))

   (terminatable-statement
    (simple-statement) : $1
    (Break) : `(break)
    (Continue) : `(continue)
    (Next) : `(next)
    (Exit expr-opt) : (if (eq? $2 #t) '(exit) `(exit ,$2))
    (Return expr-opt) : (if (eq? $2 #t) '(return) `(return ,$2))
    (Do newline-opt terminated-statement While LPAREN expr RPAREN)
    : `(do ,$6 ,@(strip-progn $3)))

   (simple-statement-opt
    () : '(progn)
    (simple-statement) : $1)

   (simple-statement
    (Delete name LBRACKET expr-list RBRACKET)
    : `(array-delete! ,(unwrap-singleton $4) ,$2)
    (expr) : $1
    (print-statement) : $1)

   (print-statement
    (simple-print-statement) : $1
    (simple-print-statement output-redirection) : `(with-redirect ,$2 ,$1))

   (simple-print-statement
    (Print print-expr-list-opt) : `(print ,@$2)
    (Print LPAREN multiple-expr-list RPAREN) : `(print ,@$3)
    (Printf print-expr-list) : `(printf ,@$2)
    (Printf LPAREN multiple-expr-list RPAREN) : `(printf ,@$3))

   (output-redirection
    (> expr) : `(truncate ,$2)
    (>> expr) : `(append ,$2)
    (PIPE expr) : `(pipe-to ,$2))

   (expr-list-opt
    () : '()
    (expr-list) : $1)

   (print-expr-list-opt
    () : '()
    (print-expr-list) : $1)

   (expr-list
    (expr) : `(,$1)
    (multiple-expr-list) : $1)

   (print-expr-list
    (print-expr) : `(,$1)
    (print-expr-list COMMA newline-opt print-expr) : `(,@$1 ,$4))

   (multiple-expr-list
    (expr COMMA newline-opt expr) : `(,$1 ,$4)
    (multiple-expr-list COMMA newline-opt expr) : `(,@$1 ,$4))

   (expr-opt
    () : #t
    (expr) : $1)

   ;; The standard splits expressions into unary and non-unary to
   ;; differentiate (for example) between subtraction and concatenating
   ;; with negation.  This technique seems to be at odds with the way
   ;; our parser generator does explicit precedence (having different
   ;; types of expressions keeps the precedence rules from taking
   ;; effect).  The following takes care of precedence implicitly,
   ;; having a hierarchy of rules for the different precedences and
   ;; associativities.  It's a little verbose, but it does limit the
   ;; unary/non-unary distinction to the precedence levels between
   ;; concatenation and the unary plus and minus.
   (expr
    (assign-expr) : $1)

   ;; Since printing supports redirects, '>' would have two meanings
   ;; if we used the normal 'expr' nonterminal.  Instead, awk avoids
   ;; this by disallowing comparisons in print statements.  No
   ;; comparisons means no 'x > y', which solves the problem.  For us,
   ;; this means copying the 'expr' hierarchy without 'comp-expr'.
   (print-expr
    (print-assign-expr) : $1)

   (assign-expr
    (lvalue ^= assign-expr) : `(set-op! expt ,$1 ,$3)
    (lvalue %= assign-expr) : `(set-op! modulo ,$1 ,$3)
    (lvalue *= assign-expr) : `(set-op! * ,$1 ,$3)
    (lvalue /= assign-expr) : `(set-op! / ,$1 ,$3)
    (lvalue += assign-expr) : `(set-op! + ,$1 ,$3)
    (lvalue -= assign-expr) : `(set-op! - ,$1 ,$3)
    (lvalue = assign-expr) : `(set! ,$1 ,$3)
    (cond-expr) : $1)

   (print-assign-expr
    (lvalue ^= print-assign-expr) : `(set-op! expt ,$1 ,$3)
    (lvalue %= print-assign-expr) : `(set-op! modulo ,$1 ,$3)
    (lvalue *= print-assign-expr) : `(set-op! * ,$1 ,$3)
    (lvalue /= print-assign-expr) : `(set-op! / ,$1 ,$3)
    (lvalue += print-assign-expr) : `(set-op! + ,$1 ,$3)
    (lvalue -= print-assign-expr) : `(set-op! - ,$1 ,$3)
    (lvalue = print-assign-expr) : `(set! ,$1 ,$3)
    (print-cond-expr) : $1)

   (cond-expr
    (or-expr ? cond-expr : cond-expr) : `(if ,$1 ,$3 ,$5)
    (or-expr) : $1)

   (print-cond-expr
    (print-or-expr ? print-cond-expr : print-cond-expr) : `(if ,$1 ,$3 ,$5)
    (print-or-expr) : $1)

   (or-expr
    (or-expr || and-expr) : `(or ,$1 ,$3)
    (and-expr) : $1)

   (print-or-expr
    (print-or-expr || print-and-expr) : `(or ,$1 ,$3)
    (print-and-expr) : $1)

   (and-expr
    (and-expr && member-expr) : `(and ,$1 ,$3)
    (member-expr) : $1)

   (print-and-expr
    (print-and-expr && print-member-expr) : `(and ,$1 ,$3)
    (print-member-expr) : $1)

   (member-expr
    (member-expr In name)
    : `(array-member? ,(unwrap-singleton $1) ,$3)
    (LPAREN multiple-expr-list RPAREN In name)
    : `(array-member? ,(unwrap-singleton $2) ,$5)
    (re-expr) : $1)

   (print-member-expr
    (print-member-expr In name)
    : `(array-member? ,(unwrap-singleton $1) ,$3)
    (LPAREN multiple-expr-list RPAREN In name)
    : `(array-member? ,(unwrap-singleton $2) ,$5)
    (print-re-expr) : $1)

   (re-expr
    (comp-expr ~ comp-expr) : `(string-match ,$1 ,$3)
    (comp-expr !~ comp-expr) : `(not-string-match ,$1 ,$3)
    (comp-expr) : $1)

   (print-re-expr
    (cat-expr ~ cat-expr) : `(string-match ,$1 ,$3)
    (cat-expr !~ cat-expr) : `(not-string-match ,$1 ,$3)
    (cat-expr) : $1)

   (comp-expr
    (cat-expr < cat-expr) : `(< ,$1 ,$3)
    (cat-expr <= cat-expr) : `(<= ,$1 ,$3)
    (cat-expr != cat-expr) : `(not-equal? ,$1 ,$3)
    (cat-expr == cat-expr) : `(equal? ,$1 ,$3)
    (cat-expr > cat-expr) : `(> ,$1 ,$3)
    (cat-expr >= cat-expr) : `(>= ,$1 ,$3)
    (cat-expr) : $1)

   (cat-expr
    (cat-expr non-unary-expr-expr) : `(string-append ,$1 ,$2)
    (expr-expr) : $1)

   (expr-expr
    (unary-expr-expr) : $1
    (non-unary-expr-expr) : $1)

   (unary-expr-expr
    (unary-expr-expr + term-expr) : `(+ ,$1 ,$3)
    (unary-expr-expr - term-expr) : `(- ,$1 ,$3)
    (unary-term-expr) : $1)

   (non-unary-expr-expr
    (non-unary-expr-expr + term-expr) : `(+ ,$1 ,$3)
    (non-unary-expr-expr - term-expr) : `(- ,$1 ,$3)
    (non-unary-term-expr) : $1)

   (term-expr
    (unary-term-expr) : $1
    (non-unary-term-expr) : $1)

   (unary-term-expr
    (unary-term-expr * factor-expr) : `(* ,$1 ,$3)
    (unary-term-expr / factor-expr) : `(/ ,$1 ,$3)
    (unary-term-expr % factor-expr) : `(modulo ,$1 ,$3)
    (unary-factor-expr) : $1)

   (non-unary-term-expr
    (non-unary-term-expr * factor-expr) : `(* ,$1 ,$3)
    (non-unary-term-expr / factor-expr) : `(/ ,$1 ,$3)
    (non-unary-term-expr % factor-expr) : `(modulo ,$1 ,$3)
    (non-unary-factor-expr) : $1)

   (factor-expr
    (unary-factor-expr) : $1
    (non-unary-factor-expr) : $1)

   (unary-factor-expr
    (+ exp-expr) : `(+ ,$2)
    (- exp-expr) : `(- ,$2))

   (non-unary-factor-expr
    (! exp-expr) : `(not ,$2)
    (exp-expr) : $1)

   (exp-expr
    (prepostfix-expr ^ exp-expr) : `(expt ,$1 ,$3)
    (prepostfix-expr) : $1)

   (prepostfix-expr
    (++ lvalue) : `(pre-incr! ,$2)
    (-- lvalue) : `(pre-decr! ,$2)
    (lvalue ++) : `(post-incr! ,$1)
    (lvalue --) : `(post-decr! ,$1)
    (lvalue) : $1
    (base-expr) : $1)

   (lvalue
    (name) : $1
    (name LBRACKET expr-list RBRACKET)
    : `(array-ref ,(unwrap-singleton $3) ,$1)
    ($ lvalue) : `($ ,$2)
    ($ base-expr) : `($ ,$2))

   (base-expr
    (LPAREN expr RPAREN) : $2
    (NUMBER) : $1
    (STRING) : $1
    (regex) : $1
    (Func LPAREN expr-list-opt RPAREN) : `(apply ,$1 ,$3)
    ;; XXX: These built-in rules cause a shift/reduce conflict.  It
    ;; works out because of the documented way the parser generator
    ;; handles conflicts, but it would be better to fix it explicitly.
    (Builtin LPAREN expr-list-opt RPAREN) : `(apply ,(string->symbol $1) ,@$3)
    (Builtin) : `(apply ,(string->symbol $1)))

   (regex
    (regex-start REGEX /) : (begin (set! %regex #f) `(re ,$2)))

   (regex-start
    (/) : (set! %regex #t))

   (newline-opt
    () : '()
    (newline-opt NEWLINE) : `(,$1 ,$2))

   (name
    (NAME) : (string->symbol $1))))

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
