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

(define-module (geesh parser)
  #:use-module (geesh lexer)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-41)
  #:use-module (system base lalr)
  #:export (read-sh
            read-sh-all))

;;; Commentary:
;;;
;;; This module contains the parser for the Shell language.
;;;
;;; Code:

(define io-redirect-defaults
  '((< . 0) (<& . 0) (> . 1)
    (>& . 1) (>> . 1) (<> . 0)
    (>! . 1) (<< . 0) (<<- . 0)))

(define (io-redirect? sexp)
  "Determine if @var{sexp} is an I/O redirect form."
  (match sexp
    ((or ('< x y) ('<& x y) ('> x y)
         ('>& x y) ('>> x y) ('<> x y)
         ('>! x y) ('<< x y) ('<<- x y)) #t)
    (_ #f)))

(define (split-assignment word)
  "Split assignment @var{word} into a list where the first element is
the variable name and the second element is the value expression."

  (define (assignment-name-and-value str)
    (let* ((index (string-index str #\=))
           (name (substring str 0 index))
           (value (substring str (1+ index))))
      `(,name . ,value)))

  (match word
    (((? string?) . tail)
     (match (assignment-name-and-value (car word))
       ((name . value)
        (cond
         ((null? tail) `(,name ,value))
         ((string-null? value) (if (null? (cdr tail))
                                   `(,name ,(car tail))
                                   `(,name ,tail)))
         (else `(,name ,(cons value tail)))))))
    ((? string?)
     (match (assignment-name-and-value word)
       ((name . value) `(,name ,value))))))

;; The (ice-9 textual-ports) module does not allow instantiating
;; end-of-file objects, but (rnrs io ports) does.
(define eof-object (@ (rnrs io ports) eof-object))

(define (map+fold proc init xs)
  "Apply @var{proc} to each element of @var{xs}, mapping and folding
at the same time.  The procedure @var{proc} must return two values:
the first is the result for mapping, and the second is the result for
folding."
  (let loop ((xs xs) (map-acc '()) (fold-acc init))
    (match xs
      ((x . rest) (let-values (((map-value fold-value) (proc x fold-acc)))
                    (loop rest (cons map-value map-acc) fold-value)))
      (() (values (reverse! map-acc) fold-acc)))))

(define (merge-here-docs cmd here-docs)
  "Replace @code{'(<sh-here-end> ...)} forms in @var{cmd} with words
from the list @var{here-docs}.  Returns two values: the modified
@var{cmd} and the unused strings from @var{here-docs}."
  (match cmd
    (('<sh-with-redirects> redirs . rest)
     (let*-values (((redirs here-docs)
                    (let loop ((redirs redirs) (here-docs here-docs) (acc '()))
                      (match redirs
                        ((((or '<< '<<-) fdes ('<sh-here-end> _)) . rest)
                         (loop rest (cdr here-docs)
                               (cons `(<< ,fdes ,(car here-docs)) acc)))
                        ((redir . rest)
                         (loop rest here-docs
                               (cons redir acc)))
                        (() (values (reverse! acc) here-docs)))))
                   ((rest here-docs)
                    (map+fold merge-here-docs here-docs rest)))
       (values `(<sh-with-redirects> ,redirs ,@rest) here-docs)))
    ((xs ...) (map+fold merge-here-docs here-docs xs))
    (x (values x here-docs))))

(define (remove-quotes here-end)
  "Remove quote forms from @var{here-end} and concatenate the result
into a single field (string).  If there are no quote forms in
@var{here-end}, it is returned as-is.  This means that when @code{(eq?
here-end (remove-quotes here-end))}, then @var{here-end} does not
contain any quote forms."
  (let loop ((word here-end) (quotes? #f) (acc '()))
    (match word
      (() (if quotes? (string-concatenate-reverse acc) here-end))
      (('<sh-quote> word*) (loop '() #t (cons (remove-quotes word*) acc)))
      ((('<sh-quote> word*) . t) (loop t #t (cons (remove-quotes word*) acc)))
      ((? string?) (loop '() quotes? (cons word acc)))
      (((? string? h) . t) (loop t quotes? (cons h acc))))))

(define (read-here-docs op+ends port)
  "Read a here-document from @var{port} for each operator and here-end
pair in @var{op+ends}."
  (map (match-lambda
         ((op . end) (let ((end* (remove-quotes end)))
                       (get-here-doc (remove-quotes end) port
                                     #:trim-tabs? (eq? op '<<-)
                                     #:quoted? (not (eq? end end*))))))
       op+ends))

(define (make-lexer port read-sh/bracketed read-sh/backquoted)
  "Make a lexer thunk that reads tokens from @var{port}. When the lexer
needs to read subcommands, it uses @var{read-sh/bracketed} to read
bracketed subcommands and @var{read-sh/backquoted} to read backquoted
subcommands."
  (define next-tokens '())
  (define here-ends '())
  (lambda ()
    (parameterize ((read-bracketed-command read-sh/bracketed)
                   (read-backquoted-command read-sh/backquoted))
      (match next-tokens
        (()
         (let* ((token (get-token port))
                (category (and (lexical-token? token)
                               (lexical-token-category token))))
           (match category
             ((or 'DLESS 'DLESSDASH)
              (let ((here-end (get-here-end port)))
                (unless (lexical-token? here-end)
                  (error "Unexpected EOF."))
                (let ((op (if (eq? category 'DLESS) '<< '<<-))
                      (end (lexical-token-value here-end)))
                  (set! here-ends (cons `(,op . ,end) here-ends))
                  (set! next-tokens `(,here-end)))
                token))
             ('NEWLINE
              (if (null? here-ends)
                  token
                  (let ((here-docs (read-here-docs (reverse here-ends) port)))
                    (set! here-ends '())
                    (set! next-tokens (append here-docs `(,token)))
                    (make-lexical-token 'HERE-DOC-SEP
                                        (lexical-token-source token)
                                        ""))))
             (_ token))))
        ((next-token . rest)
         (set! next-tokens rest)
         next-token)))))

(define* (make-parser #:key (command-hook noop)
                      (open-bracket-hook noop)
                      (close-bracket-hook noop))
  "Make an LALR parser for the Shell language. The optional hooks are
all thunks. The @var{command-hook} thunk is called after reducing a
complete command. The @var{open-bracket-hook} thunk is called after
reducing an opening bracket. The @var{close-bracket-hook} is called
after reducing a closing bracket. (Note that a @var{open-bracket-hook}
is also called when reducing case patterns that end with an unbalanced
closing bracket. This ensures that when parsing valid Shell code,
@var{open-bracket-hook} and @var{close-bracket-hook} should be called
the same number of times.)"
  (lalr-parser

   (AND                                 ; '&'
    SEMI                                ; ';'
    LESS                                ; '<'
    GREAT                               ; '>'
    PIPE                                ; '|'
    LPAREN                              ; '('
    RPAREN                              ; ')'

    AND-IF                              ; '&&'
    OR-IF                               ; '||'
    DSEMI                               ; ';;'
    DLESS                               ; '<<'
    DGREAT                              ; '>>'
    LESSAND                             ; '<&'
    GREATAND                            ; '>&'
    LESSGREAT                           ; '<>'
    DLESSDASH                           ; '<<-'
    CLOBBER                             ; '>|'

    If                                  ; 'if'
    Then                                ; 'then'
    Else                                ; 'else'
    Elif                                ; 'elif'
    Fi                                  ; 'fi'
    Do                                  ; 'do'
    Done                                ; 'done'
    Case                                ; 'case'
    Esac                                ; 'esac'
    While                               ; 'while'
    Until                               ; 'until'
    For                                 ; 'for'
    Lbrace                              ; '{'
    Rbrace                              ; '}'
    Bang                                ; '!'
    In                                  ; 'in'

    WORD
    ASSIGNMENT-WORD
    NAME
    NEWLINE
    IO-NUMBER
    HERE-DOC
    HERE-DOC-SEP)

   (program
    (linebreak complete-commands linebreak)
    : (if (null? (cdr $2)) (car $2) (reverse! $2))
    (linebreak)
    : (eof-object))

   (complete-commands
    (complete-commands newline-list complete-command)
    : (begin
        (command-hook)
        (cons $3 $1))
    (complete-command)
    : (begin
        (command-hook)
        `(,$1)))

   (complete-command
    (complete-command HERE-DOC-SEP here-doc-list)
    : (let-values (((complete-command here-docs) (merge-here-docs $1 $3)))
        (unless (null? here-docs)
          (error "Unused here-documents"))
        complete-command)
    (list separator-op)
    : (let ((lst (match $2
                   ('AND (reverse! (cons `(<sh-async> ,(car $1)) (cdr $1))))
                   ('SEMI (reverse! $1)))))
        (if (null? (cdr lst))
            (car lst)
            (cons '<sh-begin> lst)))
    (list)
    : (let ((lst (reverse! $1)))
        (if (null? (cdr lst))
            (car lst)
            (cons '<sh-begin> lst))))

   (list
    (list separator-op and-or)
    : (match $2
        ('AND (cons* $3 `(<sh-async> ,(car $1)) (cdr $1)))
        ('SEMI (cons $3 $1)))
    (and-or)
    : `(,$1))

   (and-or
    (pipeline)
    : $1
    (and-or AND-IF linebreak pipeline)
    : `(<sh-and> ,$1 ,$4)
    (and-or OR-IF linebreak pipeline)
    : `(<sh-or> ,$1 ,$4))

   (pipeline
    (pipe-sequence)
    : (if (null? (cdr $1)) (car $1) $1)
    (Bang pipe-sequence)
    : `(<sh-not> ,$2))

   (pipe-sequence
    (command)
    : `(,$1)
    (pipe-sequence PIPE linebreak command)
    : `(<sh-pipe> ,(append $1 (list $4))))

   (command
    (simple-command)
    : $1
    (compound-command)
    : $1
    (compound-command redirect-list)
    : `(<sh-with-redirects> ,$2 ,$1)
    (function-definition)
    : $1)

   (compound-command
    (brace-group)
    : $1
    (subshell)
    : $1
    (for-clause)
    : $1
    (case-clause)
    : $1
    (if-clause)
    : $1
    (while-clause)
    : $1
    (until-clause)
    : $1)

   (subshell
    (LPAREN! compound-list RPAREN!)
    : `(<sh-subshell> ,$2))

   (compound-list
    (linebreak term)
    : (match $2
        ((cmd) cmd)
        (cmds `(<sh-begin> ,@(reverse! cmds))))
    (linebreak term separator)
    : (match (match $3
               ('AND (cons `(<sh-async> ,(car $2)) (cdr $2)))
               ((or 'SEMI 'NEWLINE) $2))
        ((cmd) cmd)
        (cmds `(<sh-begin> ,@(reverse! cmds)))))

   (term
    (term HERE-DOC-SEP here-doc-list)
    : (let-values (((term here-docs) (merge-here-docs $1 (reverse $3))))
        (unless (null? here-docs)
          (error "Unused here-documents"))
        term)
    (term separator and-or)
    : (match $2
        ('AND (cons* $3 `(<sh-async> ,(car $1)) (cdr $1)))
        ((or 'SEMI 'NEWLINE) (cons $3 $1)))
    (and-or)
    : `(,$1))

   (for-clause
    (For name do-group)
    : `(<sh-for> (,$2 (<sh-ref> "@")) ,$3)
    (For name sequential-sep do-group)
    : `(<sh-for> (,$2 (<sh-ref> "@")) ,$4)
    (For name linebreak in sequential-sep do-group)
    : `(<sh-for> (,$2 (<sh-ref> "@")) ,$6)
    (For name linebreak in wordlist sequential-sep do-group)
    : `(<sh-for> (,$2 ,$5) ,$7))

   (name
    (NAME-with-keywords)
    : $1)

   (in
    (In)
    : #f)

   (wordlist
    (wordlist WORD*)
    : (append $1 `(,$2))
    (WORD*)
    : `(,$1))

   (case-clause
    (Case WORD* linebreak in linebreak case-list Esac)
    : `(<sh-case> ,$2 ,@$6)
    (Case WORD* linebreak in linebreak case-list-ns Esac)
    : `(<sh-case> ,$2 ,@$6)
    (Case WORD* linebreak in linebreak Esac)
    : `(<sh-case> ,$2))

   (case-list-ns
    (case-list case-item-ns)
    : (append $1 `(,$2))
    (case-item-ns)
    : `(,$1))

   (case-list
    (case-list case-item)
    : (append $1 `(,$2))
    (case-item)
    : `(,$1))

   (case-item-ns
    (pattern! RPAREN! linebreak)
    : `(,$1 #f)
    (pattern! RPAREN! compound-list)
    : `(,$1 ,$3)
    (LPAREN! pattern RPAREN! linebreak)
    : `(,$2 #f)
    (LPAREN! pattern RPAREN! compound-list)
    : `(,$2 ,$4))

   (case-item
    (pattern! RPAREN! linebreak DSEMI linebreak)
    : `(,$1 #f)
    (pattern! RPAREN! compound-list DSEMI linebreak)
    : `(,$1 ,$3)
    (LPAREN! pattern RPAREN! linebreak DSEMI linebreak)
    : `(,$2 #f)
    (LPAREN! pattern RPAREN! compound-list DSEMI linebreak)
    : `(,$2 ,$4))

   (pattern
    (WORD*-without-Esac)
    : `(,$1)
    (pattern PIPE WORD*)
    : (append $1 `(,$3)))

   (if-clause
    (If compound-list Then compound-list else-part Fi)
    : `(<sh-cond> (,$2 ,$4) ,@$5)
    (If compound-list Then compound-list Fi)
    : `(<sh-cond> (,$2 ,$4)))

   (else-part
    (Elif compound-list Then compound-list)
    : `((,$2 ,$4))
    (Elif compound-list Then compound-list else-part)
    : (cons `(,$2 ,$4) $5)
    (Else compound-list)
    : `((<sh-else> ,$2)))

   (while-clause
    (While compound-list do-group)
    : `(<sh-while> ,$2 ,$3))

   (until-clause
    (Until compound-list do-group)
    : `(<sh-until> ,$2 ,$3))

   (function-definition
    (fname LPAREN! RPAREN! linebreak function-body)
    : `(<sh-define> (,$1) ,$5))

   (function-body
    (compound-command)
    : $1
    (compound-command redirect-list)
    : `(<sh-with-redirects> ,$2 ,$1))

   (fname
    (NAME)
    : $1)

   (brace-group
    (Lbrace compound-list Rbrace)
    : $2)

   (do-group
    (Do compound-list Done)
    : $2)

   (simple-command
    (cmd-prefix cmd-word cmd-suffix)
    : (let*-values (((redirects-1 assignments*) (partition io-redirect? $1))
                    ((redirects-2 args) (partition io-redirect? $3))
                    ((assignments) (map split-assignment assignments*)))
        (match (append redirects-1 redirects-2)
          (() `(<sh-exec-let> ,assignments ,$2 ,@args))
          (redirects `(<sh-with-redirects> ,redirects
                        (<sh-exec-let> ,assignments ,$2 ,@args)))))
    (cmd-prefix cmd-word)
    : (let*-values (((redirects assignments*) (partition io-redirect? $1))
                    ((assignments) (map split-assignment assignments*)))
        (match redirects
          (() `(<sh-exec-let> ,assignments ,$2))
          (_ `(<sh-with-redirects> ,redirects
                ,(if (null? assignments)
                     `(<sh-exec> ,$2)
                     `(<sh-exec-let> ,assignments ,$2))))))
    (cmd-prefix)
    : (let*-values (((redirects assignments*) (partition io-redirect? $1))
                    ((assignments) (map split-assignment assignments*)))
        (match redirects
          (() `(<sh-set!> ,@assignments))
          (_ `(<sh-with-redirects> ,redirects
                ,(if (null? assignments)
                     #f
                     `(<sh-set!> ,assignments))))))
    (cmd-name cmd-suffix)
    : (let-values (((redirects args) (partition io-redirect? $2)))
        (match redirects
          (() `(<sh-exec> ,$1 ,@args))
          (_ `(<sh-with-redirects> ,redirects
                (<sh-exec> ,$1 ,@args)))))
    (cmd-name)
    : `(<sh-exec> ,$1))

   (cmd-name
    (WORD*-without-keywords-or-ASSIGNMENT-WORD)
    : $1)

   (cmd-word
    (WORD*-without-keywords-or-ASSIGNMENT-WORD)
    : $1)

   (cmd-prefix
    (io-redirect)
    : `(,$1)
    (cmd-prefix io-redirect)
    : (append $1 `(,$2))
    (ASSIGNMENT-WORD)
    : `(,$1)
    (cmd-prefix ASSIGNMENT-WORD)
    : (append $1 `(,$2)))

   (cmd-suffix
    (io-redirect)
    : `(,$1)
    (cmd-suffix io-redirect)
    : (append $1 `(,$2))
    (WORD*)
    : `(,$1)
    (cmd-suffix WORD*)
    : (append $1 `(,$2)))

   (redirect-list
    (io-redirect)
    : `(,$1)
    (redirect-list io-redirect)
    : (append $1 `(,$2)))

   (io-redirect
    (io-file)
    : `(,(car $1) ,(assoc-ref io-redirect-defaults (car $1)) ,(cdr $1))
    (IO-NUMBER io-file)
    : `(,(car $2) ,(string->number $1) ,(cdr $2))
    (io-here)
    : `(,(car $1) ,(assoc-ref io-redirect-defaults (car $1)) ,(cdr $1))
    (IO-NUMBER io-here)
    : `(,(car $2) ,(string->number $1) ,(cdr $2)))

   (io-file
    (LESS filename)
    : `(< . ,$2)
    (LESSAND filename)
    : `(<& . ,$2)
    (GREAT filename)
    : `(> . ,$2)
    (GREATAND filename)
    : `(>& . ,$2)
    (DGREAT filename)
    : `(>> . ,$2)
    (LESSGREAT filename)
    : `(<> . ,$2)
    (CLOBBER filename)
    : `(>! . ,$2))

   (filename
    (WORD*)
    : $1)

   (io-here
    (DLESS here-end)
    : `(<< . (<sh-here-end> ,$2))
    (DLESSDASH here-end)
    : `(<<- . (<sh-here-end> ,$2)))

   (here-end
    (WORD*)
    : $1)

   (newline-list
    (NEWLINE)
    : #f
    (newline-list NEWLINE)
    : #f)

   (linebreak
    (newline-list)
    : #f
    ()
    : #f)

   (separator-op
    (AND)
    : 'AND
    (SEMI)
    : 'SEMI)

   (separator
    (separator-op linebreak)
    : $1
    (newline-list)
    : 'NEWLINE)

   (sequential-sep
    (SEMI linebreak)
    : #f
    (newline-list)
    : #f)

   ;; Rules added to emulate the POSIX context-sensitive lexer
   ;; approach.

   ;; Accept all the specializations of a normal word and all
   ;; keywords.  This is the default case.
   (WORD*
    (WORD) : $1
    (NAME) : $1
    (ASSIGNMENT-WORD) : $1
    (If) : $1
    (Then) : $1
    (Else) : $1
    (Elif) : $1
    (Fi) : $1
    (Do) : $1
    (Done) : $1
    (Case) : $1
    (Esac) : $1
    (While) : $1
    (Until) : $1
    (For) : $1
    (Lbrace) : $1
    (Rbrace) : $1
    (Bang) : $1
    (In) : $1)

   ;; Just like 'WORD*', but no keywords.  This corresponds to "rule
   ;; 1" in the POSIX specification.
   (WORD*-without-keywords
    (WORD) : $1
    (NAME) : $1
    (ASSIGNMENT-WORD) : $1)

   ;; Just like 'WORD*', but without the "esac" keyword.  This
   ;; corresponds to "rule 4" in the POSIX specification.
   (WORD*-without-Esac
    (WORD) : $1
    (NAME) : $1
    (ASSIGNMENT-WORD) : $1
    (If) : $1
    (Then) : $1
    (Else) : $1
    (Elif) : $1
    (Fi) : $1
    (Do) : $1
    (Done) : $1
    (Case) : $1
    ;; (Esac) : $1
    (While) : $1
    (Until) : $1
    (For) : $1
    (Lbrace) : $1
    (Rbrace) : $1
    (Bang) : $1
    (In) : $1)

   ;; Accept a "NAME" or any keyword. This corresponds to "rule 5" in
   ;; the POSIX specification.
   (NAME-with-keywords
    (NAME) : $1
    (If) : $1
    (Then) : $1
    (Else) : $1
    (Elif) : $1
    (Fi) : $1
    (Do) : $1
    (Done) : $1
    (Case) : $1
    (Esac) : $1
    (While) : $1
    (Until) : $1
    (For) : $1
    (Lbrace) : $1
    (Rbrace) : $1
    (Bang) : $1
    (In) : $1)

   ;; Accept any "WORD*" token except for "ASSIGNMENT-WORD". This
   ;; corresponds to "rule 7" in the POSIX specification.
   (WORD*-without-keywords-or-ASSIGNMENT-WORD
    (WORD) : $1
    (NAME) : $1)

   ;; Rules for updating bracket balance.

   (LPAREN!
    (LPAREN)
    : (begin (open-bracket-hook) $1))

   (RPAREN!
    (RPAREN)
    : (begin (close-bracket-hook) $1))

   ;; Sometimes a "pattern" non-terminal comes before an unbalanced
   ;; "RPAREN". This reduction hook can be used to pretend that we
   ;; encountered an "LPAREN".

   (pattern!
    (pattern)
    : (begin (open-bracket-hook) $1))

   ;; A helper rule for handling here-docs.

   (here-doc-list
    (here-doc-list HERE-DOC)
    : (append $1 `(,$2))
    (HERE-DOC)
    : `(,$1))))

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

(define (read-sh/bracketed port)
  "Read Shell code from @var{port} until the first unmatched closing
bracket."
  (let* ((bracket-depth 0)
         (incr-bracket-depth! (lambda ()
                                (set! bracket-depth (1+ bracket-depth))))
         (decr-bracket-depth! (lambda ()
                                (set! bracket-depth (1- bracket-depth))))
         (balanced? (lambda () (= 0 bracket-depth)))
         (pre-lex (make-lexer port read-sh/bracketed read-sh/backquoted))
         (lex (lambda ()
                (let ((token (pre-lex)))
                  (if (and (balanced?)
                           (lexical-token? token)
                           (eq? (lexical-token-category token) 'RPAREN))
                      (begin
                        (unget-char port #\))
                        '*eoi*)
                      token))))
         (parse (make-parser #:open-bracket-hook incr-bracket-depth!
                             #:close-bracket-hook decr-bracket-depth!)))
    (match (parse lex syntax-error)
      ((? eof-object?) '())
      (((? symbol? tag) . rest) `((,tag . ,rest)))
      (code code))))

(define (read-sh/backquoted port)
  "Read Shell code from @var{port} until the first unescaped backquote."
  (call-with-backquoted-input-port port
    (lambda (port)
      (let ((lex (make-lexer port read-sh/bracketed read-sh/backquoted))
            (parse (make-parser)))
        (match (parse lex syntax-error)
          ((? eof-object?) '())
          (((? symbol? tag) . rest) `((,tag . ,rest)))
          (code code))))))

(define* (read-sh #:optional (port #f))
  "Read a complete Shell command from @var{port} (or the current input
port if @var{port} is unspecified)."
  (let* ((port (or port (current-output-port)))
         (stop? #f)
         (stop! (lambda () (set! stop? #t)))
         (pre-lex (make-lexer port read-sh/bracketed read-sh/backquoted))
         (lex (lambda () (if stop? '*eoi* (pre-lex))))
         (parse (make-parser #:command-hook stop!)))
    (parse lex syntax-error)))

(define* (read-sh-all #:optional (port #f))
  "Read all complete Shell commands from @var{port} (or the current
input port if @var{port} is unspecified)."
  (let* ((port (or port (current-input-port)))
         (lex (make-lexer port read-sh/bracketed read-sh/backquoted))
         (parse (make-parser)))
    (match (parse lex syntax-error)
      ((? eof-object?) '())
      (((? symbol? tag) . rest) `((,tag . ,rest)))
      (x x))))
