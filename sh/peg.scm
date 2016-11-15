(define-module (sh peg)
  :use-module (ice-9 peg)
  :use-module (ice-9 peg codegen)
  :use-module (ice-9 pretty-print)
  :export (parse))

(define (error? x)
  (let loop ((x x))
    (if (null? x) #f
        (if (not (pair? x))
            (eq? 'error x)
            (or (loop (car x))
                (loop (cdr x)))))))

(define (parse input)
  (let ((tree (parse- input)))
    (cond ((error? tree)
           (format (current-error-port) "error: ~a\n" tree)
           #f)
          (#t
           tree))))

(define (parse- input)
  (define label "")
  (define (label-name str len pos)
    (let ((at (string-skip str char-alphabetic? pos len)))
      (set! label (substring str pos at))
      (if (< at len) (list at '())
          #f)))

  (define (label-match str len pos)
    (if (string-prefix? label (substring str pos)) (list (+ pos (string-length label)) '())
        #f))

  (define-peg-pattern here-label none label-name)
  (define-peg-pattern here-delim none label-match)
  (define-peg-pattern here-document all (and (+ (and (not-followed-by here-delim) peg-any)) here-delim))

  (define-peg-string-patterns
    "script           <-- ws* (term (separator term)* separator?)? eof
     eof              <   !. / error
     error            <-- .*
     term             <-- pipeline (sp* (and / or) ws* pipeline)*
     and              <-- '&&'
     or               <-- '||'
     pipeline         <-- '!'? sp* command (sp* pipe ws* command)*
     pipe             <-- '|'
     command          <-- simple-command / (compound-command (sp+ io-redirect)*) / function-def
     compound-command <-- brace-group / subshell / for-clause / case-clause / if-clause / while-clause / until-clause
     subshell         <-- '(' ne-compound-list ')'
     compound-list    <-- term (separator term)*
     ne-compound-list <-- compound-list separator / error
     case-clause      <-- 'case' sp+ word ws+ 'in' ws+ case-item* 'esac'
     case-item        <-- pattern (ne-compound-list? case-sep ws* / error)
     case-sep         <   ';;'
     pattern          <-- sp* word (sp* '|' sp* word)* sp* ')' sp*
     for-clause       <-- 'for' (sp+ identifier ws+ ('in' expression sequential-sep)? do-group / error)
     expression       <-- sp+ substitution sp* / (sp+ word)* sp*
     do-group         <-- 'do' (ne-compound-list 'done' / error)
     if-clause        <-- 'if' (ne-compound-list 'then' ne-compound-list else-part? 'fi' / error)
     else-part        <-- 'elif' (ne-compound-list 'then' ne-compound-list else-part? / error) / 'else' (ne-compound-list / error)
     while-clause     <-- 'while' (ne-compound-list do-group / error)
     until-clause     <-- 'until' (ne-compound-list do-group / error)
     function-def     <-- name sp* '(' sp* ')' ws* (function-body / error)
     function-body    <-- compound-command io-redirect*
     brace-group      <-- '{' (sp* (compound-list / error) sp* '}' / error)
     simple-command   <-- (io-redirect sp+)* nonreserved (sp+ (io-redirect / nonreserved))*
     reserved         <   ('case' / 'esac' / 'if' / 'fi' / 'then' / 'else' / 'elif' / 'for' / 'done' / 'do' / 'until' / 'while')
     nonreserved      <-  &(reserved word) word / !reserved word
     io-redirect      <-- [0-9]* sp* (io-here / io-file)
     io-file          <-- ('<&' /  '>&' / '>>' / '>' / '<>'/ '<' / '>|') sp* ([0-9]+ / filename)
     io-here          <-  ('<<' / '<<-') io-suffix here-document
     io-op            <   '<<-' / '<<' / '<&' /  '>&' / '>>' / '>' / '<>'/ '<' / '>|'
     io-suffix        <-  sp* here-label sp* nl
     filename         <-- word
     name             <-- identifier
     identifier       <-  [_a-zA-Z][_a-zA-Z0-9]*
     word             <-- test / substitution / assignment / number / literal
     number           <-- [0-9]+
     test             <-- ltest (!rtest .)* rtest
     ltest            <   '[ '
     rtest            <   ' ]'
     substitution     <-- ('$(' (script ')' / error)) / ('`' (script '`' / error))
     assignment       <-- name assign word?
     assign           <   '='
     literal          <-- (variable / delim / (![0-9] (![()] !io-op !sp !nl !break !pipe !assign .)+) / ([0-9]+ &separator)) literal*
     variable         <-- '$' ('$' / '*' / '@' / [0-9] / identifier / ([{] (![}] .)+ [}]))
     delim            <-- singlequotes / doublequotes / backticks
     sq               <   [']
     dq               <   [\"]
     bt               <   [`]
     singlequotes     <-- sq  ((doublequotes / backticks / (!sq .))* sq)
     doublequotes     <-- dq ((singlequotes / backticks / (!dq .))* dq)
     backticks        <-- bt  ((singlequotes / doublequotes / (!bt .))* bt)
     separator        <-  (sp* break ws*) / ws+
     break            <-  amp / semi !semi
     sequential-sep   <-- (semi !semi ws*) / ws+
     amp              <-  '&'
     semi             <   ';'
     nl               <   '\n'
     sp               <   [\t ]
     ws               <   sp / nl")

  (let* ((match (match-pattern script input))
         (end (peg:end match))
         (tree (peg:tree match)))
    (if (eq? (string-length input) end)
        tree
        (if match
            (begin
              (format (current-error-port) "parse error: at offset: ~a\n~s\n" end tree)
              #f)
            (begin
              (format (current-error-port) "parse error: no match\n")
              #f)))))
