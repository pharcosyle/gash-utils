(define-module (gash grammar)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 rdelim)

  #:use-module (srfi srfi-8)

  #:use-module (gash gash)
  #:use-module (gash peg)
  #:use-module (gash peg codegen)

  #:export (parse
            parse-string))

(define (parse port)
  (parse-string (read-string port)))

(define (parse-string input)

  (define io-label "")

  (define (io-label-name str len pos)
    (let ((at (string-skip str char-alphabetic? pos len)))
      (set! io-label (substring str pos at))
      (if (< at len) (list at '())
          #f)))

  (define (io-label-match str len pos)
    (if (string-prefix? io-label (substring str pos))
        (list (+ pos (string-length io-label)) '())
        #f))

  (define-peg-pattern io-here-label none io-label-name)
  (define-peg-pattern io-here-delim none io-label-match)
  (define-peg-pattern io-here-document all
    (and (+ (and (not-followed-by io-here-delim)
                 peg-any))
         io-here-delim))

  (define-peg-string-patterns
    "script           <-- ws* compound
     ws               <   sp / eol
     sp               <   '\\\n'* (comment / [ \t\v])
     comment          <   [#] (!eol .)*
     eol              <   [\n\r\f]

     compound         <-- (term (&rpar / sep#))*

     sep              <-  sp* (amp ws* / semi ws* / eof) / ws+
     amp              <-  '&'
     semi             <   ';'!';'
     eof              <   !.

     term             <-  and / or / pipeline
     and              <-- pipeline and-op ws* term
     or               <-- pipeline or-op ws* term
     and-op           <   '&&'
     or-op            <   '||'

     pipeline         <-- '!'? sp* (command (&sep / &or-op / &and-op / &rpar / eof / pipe#))+

     and-or           <-  '&&' / '||'

     exclamation      <-  '!'
     pipe             <   sp* '|' !'|' ws*

     command          <-- function-def / compound-command / simple-command

     compound-command <-  (subshell / brace-group / for-clause / case-clause /
                          if-clause / while-clause / until-clause) (sp* io-redirect)*

     simple-command   <-  ((io-redirect / assignment) sp*)*
                          ((io-redirect / nonreserved) sp*)+ /
                          ((io-redirect / assignment) sp*)+
                          ((io-redirect / nonreserved) sp*)*

     assignment       <-- name assign word?
     assign           <   '='

     io-redirect      <-- [0-9]* (io-here / io-file)
     io-file          <-- io-op ([0-9]+ / sp* word)
     io-op            <-  '<&' / '>&' / '>>' / '>' / '<>'/ '<' / '>|'
     io-here          <-- io-here-op io-here-label sp* eol io-here-document
     io-here-op       <-  '<<-' / '<<'

     reserved         <   ('case' / 'esac' / 'in' / 'if' / 'fi' / 'then' / 'else' /
                           'elif' / 'for' / 'done' / 'do' / 'until' / 'while') &ws
     nonreserved      <-  !reserved word

     word             <-- test / substitution / 
                          (number / variable / variable-subst / delim / literal)+

     function-def     <-- name sp* lpar rpar# ws* function-body
     name             <-- !reserved identifier
     function-body    <-- brace-group (sp* io-redirect)*

     subshell         <-- lpar compound rpar#
     brace-group      <-- lbrace ws* compound rbrace#

     case-clause      <-- case-keyword sp* word sp* in-keyword# ws* case-item+ ws* esac-keyword#
     case-item        <-- pattern sp* colon? ws* compound? case-sep?
     colon            <   ':'
     case-sep         <   ';;' ws*
     pattern          <-- (word (!rpar '|'# / !'|' &rpar))+ rpar#

     for-clause       <-- for-keyword sp+ identifier ws+ (in-keyword sp+ expression)? sep# do-group
     expression       <-- command
     do-group         <-- do-keyword ws+ compound done-keyword#

     if-clause        <-- if-keyword sp+ compound then-keyword# ws+ compound else-part? fi-keyword#
     else-part        <-- else-keyword ws+ compound /
                          elif-keyword ws+ compound then-keyword# ws+ compound else-part?

     while-clause     <--  while-keyword compound do-group

     until-clause     <--  until-keyword compound do-group

     test             <-- ltest sp+ (word sp+)+ rtest#
     ltest            <   '['
     rtest            <   ']'

     literal          <-  !reserved (escaped / !allowed .)+
     escaped          <-  escape [ \"$]
     escape           <   [\\]
     allowed          <-  ']' / [ \t\v\f\n`'\")};|&$] / '\\\n'

     identifier       <-  [_a-zA-Z][_a-zA-Z0-9]*

     dollar           <   '$'
     number           <-- [0-9]+

     substitution     <-- dollar lpar compound rpar# / bt ([\\] bt / !bt .)+ bt#
     lpar             <   '('
     rpar             <   ')'
     bt               <   [`]

     variable         <-- dollar ('*' / '@' / [0-9] / name /
                          lbrace name (variable-literal / &rbrace) rbrace)
     variable-subst   <-  dollar lbrace (variable-or / variable-and / variable-regex / &rbrace) rbrace
     variable-or      <-- name min variable-word
     variable-and     <-- name plus variable-word
     variable-word    <-  (variable-regex / substitution / variable-subst / variable / variable-literal)+
     variable-regex   <-- name ('%%' / '%' / '##' / '#' / '^^' / '^' /',,' / ',' / '*' / '@' / '?')+ variable-word
     variable-literal <-  (!rbrace !min !plus .)+
     min              <   '-'
     plus             <   '+'
     lbrace           <   '{'
     rbrace           <   '}'


     delim            <-- singlequotes / doublequotes / substitution
     sq               <   [']
     dq               <   [\"]
     singlequotes     <-  sq (!sq .)* sq#
     doublequotes     <-  dq (substitution / variable / (!dq (escape '\"' / .)))* dq#

     case-keyword     <   'case'
     do-keyword       <   'do'
     done-keyword     <   'done'
     elif-keyword     <   'elif'
     else-keyword     <   'else'
     esac-keyword     <   'esac'
     fi-keyword       <   'fi'
     for-keyword      <   'for'
     if-keyword       <   'if'
     in-keyword       <   'in'
     then-keyword     <   'then'
     until-keyword    <   'until'
     while-keyword    <   'while'
")

  (catch 'syntax-error
    (lambda ()
      (let* ((match (match-pattern script input))
             (end (peg:end match))
             (tree (peg:tree match)))
        (when (> %debug-level 0)
          (pretty-print tree))
        (if (eq? (string-length input) end)
            tree
            (if match
                (begin
                  (format (current-error-port) "parse error: at offset: ~a\n" end)
                  (pretty-print tree)
                  #f)
                (begin
                  (format (current-error-port) "parse error: no match\n")
                  #f)))))
    (lambda (key . args)
      (define (line-column input pos)
        (let ((length (string-length input)))
          (let loop ((lines (string-split input #\newline)) (ln 1) (p 0))
            (if (null? lines) (values #f #f input)
                (let* ((line (car lines))
                       (length (string-length line))
                       (end (+ p length 1))
                       (last? (null? (cdr lines))))
                  (if (<= pos end) (values ln (+ (if last? 0 1) (- pos p))
                                           (if last? line
                                               (string-append line "\\n" (cadr lines))))
                      (loop (cdr lines) (1+ ln) end)))))))
      (define (format-peg o)
        (match o
          (('or l ...) (string-join (map format-peg l) ", or "))
          (('and l ...) (string-join (map format-peg l) " "))
          ((? symbol?) (symbol->string o))
          ((? string?) o)))

      (receive (ln col line) (line-column input (caar args))
        (let* ((col (- col 1))
               (indent (make-string col #\space)))
          (format #t "~a:~a:~a: syntax-error:\n~a\n~a^\n~aexpected: ~a\n"
                  ""
                  ln col line
                  indent
                  indent
                  (format-peg (cadar args)))
          (exit 1))))))
