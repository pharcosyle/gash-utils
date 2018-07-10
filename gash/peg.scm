(define-module (gash peg)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 peg codegen)

  #:use-module (srfi srfi-26)

  #:export (parse peg-trace?))

(define (wrap-parser-for-users for-syntax parser accumsym s-syn)
  #`(lambda (str strlen pos)
      (when (> (@ (gash gash) %debug-level) 0)
        (format (current-error-port) "~a ~a : ~s\n"
                (make-string (- pos (or (string-rindex str #\newline 0 pos) 0)) #\space)
                '#,s-syn
                (substring str pos (min (+ pos 40) strlen))))

      (let* ((res (#,parser str strlen pos)))
        ;; Try to match the nonterminal.
        (if res
            ;; If we matched, do some post-processing to figure out
            ;; what data to propagate upward.
            (let ((at (car res))
                  (body (cadr res)))
              #,(cond
                 ((eq? accumsym 'name)
                  #`(list at '#,s-syn))
                 ((eq? accumsym 'all)
                  #`(list (car res)
                          (cond
                           ((not (list? body))
                            (list '#,s-syn body))
                           ((null? body) '#,s-syn)
                           ((symbol? (car body))
                            (list '#,s-syn body))
                           (else (cons '#,s-syn body)))))
                 ((eq? accumsym 'none) #`(list (car res) '()))
                 (else #`(begin res))))
            ;; If we didn't match, just return false.
            #f))))

(module-define! (resolve-module '(ice-9 peg codegen))
                'wrap-parser-for-users
                wrap-parser-for-users)

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
           (format (current-error-port) "error:\n")
           (pretty-print tree (current-error-port))
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
    "script           <-- ws* (term (separator term)* separator?)?
     term             <-  pipeline (sp* ('&&' / '||') ws* pipeline)*
     pipe             <   '|'
     pipeline         <-- '!'? sp* command (sp* pipe ws* command)*
     command          <-- (compound-command (sp+ io-redirect)*) / simple-command / function-def
     compound-command <- brace-group / subshell / for-clause / case-clause / if-clause / while-clause / until-clause
     simple-command   <-  (sp* (io-redirect sp+)* nonreserved)+
     nonreserved      <-  &(reserved word) word / !reserved word
     reserved         <   'case' / 'esac' / 'if' / 'fi' / 'then' / 'else' / 'elif' / 'for' / 'done' / 'do' / 'until' / 'while'

     function-def     <-- name sp* '(' sp* ')' ws* (function-body / error)
     function-body    <-- compound-command io-redirect*

     io-redirect      <-- [0-9]* sp* (io-here / io-file)
     io-file          <-- ('<&' /  '>&' / '>>' / '>' / '<>'/ '<' / '>|') sp* ([0-9]+ / filename)
     io-here          <-  ('<<' / '<<-') io-suffix here-document
     io-op            <   '<<-' / '<<' / '<&' /  '>&' / '>>' / '>' / '<>'/ '<' / '>|'
     io-suffix        <-  sp* here-label sp* nl

     brace-group      <-- '{' (sp* (compound-list / error) sp* '}' / error)
     subshell         <-- '(' compound-list separator ')'
     compound-list    <-  term (separator term)*

     case-keyword     <   'case'
     case-clause      <-- case-keyword sp+ word ws+ 'in' ws+ case-item* 'esac'
     case-item        <-- pattern ((compound-list separator)? case-sep ws* / error)
     case-sep         <   ';;'
     pattern          <-- sp* word (sp* '|' sp* word)* sp* ')' sp*

     for-keyword      <   'for'
     for-clause       <-- for-keyword sp+ name in-expression? sp* sequential-sep do-group
     in-keyword       <   'in'
     in-expression    <-- ws+ in-keyword expression?
     expression       <-- sp+ (substitution / word+)
     do-keyword       <   'do'
     done-keyword     <   'done'
     do-group         <-- do-keyword ws* compound-list separator done-keyword

     if-keyword       <   'if'
     fi-keyword       <   'fi'
     if-clause        <-- if-keyword compound-list separator then-part elif-part* else-part? fi-keyword
     then-keyword     <   'then'
     then-part        <-- then-keyword ws* compound-list separator
     elif-keyword     <   'elif'
     elif-part        <-- elif-keyword compound-list separator then-keyword ws* compound-list separator else-part?
     else-keyword     <   'else'
     else-part        <-- else-keyword compound-list separator

     while-keyword    <   'while'
     while-clause     <-- while-keyword compound-list separator do-group

     until-keyword    <   'until'
     until-clause     <-- until-keyword compound-list separator do-group

     filename         <-- word
     name             <-- identifier
     identifier       <-  [_a-zA-Z][_a-zA-Z0-9]*
     word             <-- test / substitution / assignment / number / variable / delim / literal
     number           <-- [0-9]+
     test             <-- ltest (!rtest .)* rtest
     ltest            <   '[ '
     rtest            <   ' ]'
     substitution     <-- ('$(' script ')') / ('`' script '`')
     assignment       <-- name assign (substitution / word)?
     assign           <   '='
     literal          <-- (!pipe !semi !nl !sp .)+
     variable         <-- '$' ('$' / '*' / '?' / '@' / [0-9] / identifier / ([{] (![}] .)+ [}]))
     delim            <-- singlequotes / doublequotes / substitution
     sq               <   [']
     dq               <   [\"]
     bt               <   [`]
     singlequotes     <-- sq  (doublequotes / (!sq .))* sq
     doublequotes     <-- dq (singlequotes / substitution / variable / (!dq .))* dq
     separator        <-  (sp* break ws*) / ws+
     break            <-  amp / semi !semi
     sequential-sep   <-  (semi !semi ws*) / ws+
     amp              <-  '&'
     semi             <   ';'
     nl               <   '\n'
     sp               <   [\t ]
     ws               <   sp / nl
     error            <-- .*")

  (let* ((match (match-pattern script input))
         (end (peg:end match))
         (pt (peg:tree match)))
    (if (eq? (string-length input) end)
        (let* ((foo (pretty-print pt))
               (ast (transform (keyword-flatten '(pipeline) pt)))
               (foo (pretty-print ast)))
          ast)
        (if match
            (begin
              (format (current-error-port) "parse error: at offset: ~a\n" end)
              (pretty-print tree (current-error-port))
              #f)
            (begin
              (format (current-error-port) "parse error: no match\n")
              #f)))))

(define (transform ast)
  (match ast
    (('script o ...) (map transform o))
    (('pipeline o ...) `(pipeline ,@(map transform o)))
    (('command o ...) `(command ,@(map transform o)))
    (('word o) (transform o))
    (('literal o) (transform o))
    (('name o) o)
    (('number o) o)
    (('assignment a b) `(assignment ,(transform a) ,(transform b)))
    (_ ast)))
