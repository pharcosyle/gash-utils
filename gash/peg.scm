(define-module (gash peg)
  #:use-module (ice-9 local-eval)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 peg codegen)
  #:use-module (ice-9 regex)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (gash builtins)
  #:use-module (gash environment)
  #:use-module (gash gash)
  #:use-module (gash io)
  #:use-module (gash job)

  #:export (
            parse
            peg-trace?
            ))

(define (wrap-parser-for-users for-syntax parser accumsym s-syn)
  #`(lambda (str strlen pos)
      (when (> (@ (gash gash) %debug-level) 2)
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
     term             <-  pipeline (sp* (and / or) ws* pipeline)*
     and              <-- '&&'
     or               <-- '||'
     pipe             <   '|'
     pipeline-head    <-  sp* command
     pipeline-tail    <-  sp* pipe ws* command
     negate           <-- '!'
     pipeline         <-- negate? pipeline-head pipeline-tail*
     command          <-- (compound-command (sp+ io-redirect)*) / simple-command / function-def
     compound-command <-  brace-group / subshell / for-clause / case-clause / if-clause / while-clause / until-clause
     simple-command   <-  (sp* (io-redirect sp+)* nonreserved)+
     nonreserved      <-  &(reserved word) word / !reserved word
     reserved         <   'case' / 'esac' / 'if' / 'fi' / 'then' / 'else' / 'elif' / 'for' / 'done' / 'do' / 'until' / 'while'

     function-def     <-- name sp* lpar sp* rpar ws* (function-body / error)
     function-body    <-- compound-command io-redirect*

     io-redirect      <-- [0-9]* sp* (io-here / io-file)
     io-file          <-- ('<&' /  '>&' / '>>' / '>' / '<>'/ '<' / '>|') sp* ([0-9]+ / filename)
     io-here          <-  ('<<' / '<<-') io-suffix here-document
     io-op            <   '<<-' / '<<' / '<&' /  '>&' / '>>' / '>' / '<>'/ '<' / '>|'
     io-suffix        <-  sp* here-label sp* nl

     brace-group      <-- '{' (sp* (compound-list / error) sp* '}' / error)
     subshell         <-- lpar compound-list separator rpar
     compound-list    <-  term (separator term)*

     case-keyword     <   'case'
     case-clause      <-- case-keyword sp+ word ws+ 'in' ws+ case-item* 'esac'
     case-item        <-- pattern ((compound-list separator)? case-sep ws* / error)
     case-sep         <   ';;'
     pattern          <-- sp* word (sp* '|' sp* word)* sp* ')' sp*

     for-keyword      <   'for'
     in-keyword       <   'in'
     for-clause       <-- for-keyword sp+ name (ws+ in-keyword sequence)? sp* sequential-sep do-group
     sequence         <-- (sp+ word)+
     do-keyword       <   'do'
     done-keyword     <   'done'
     do-group         <-  do-keyword ws* compound-list separator done-keyword

     if-keyword       <   'if'
     fi-keyword       <   'fi'
     if-clause        <-- if-keyword pipeline separator then-part elif-part* else-part? fi-keyword
     then-keyword     <   'then'
     then-part        <-- then-keyword ws* compound-list separator
     elif-keyword     <   'elif'
     elif-part        <-- elif-keyword ws* compound-list separator then-keyword ws* compound-list separator else-part?
     else-keyword     <   'else'
     else-part        <-- else-keyword ws* compound-list separator

     while-keyword    <   'while'
     while-clause     <-- while-keyword ws* compound-list separator do-group

     until-keyword    <   'until'
     until-clause     <-- until-keyword ws* compound-list separator do-group

     filename         <-- word
     name             <-- identifier
     identifier       <-  [_a-zA-Z][_a-zA-Z0-9]*
     word             <-  substitution / assignment / number / variable / delim / literal
     number           <-- [0-9]+
     lsubst           <   '$('
     rsubst           <   ')'
     tick             <   '`'
     substitution     <-- lsubst script rsubst / tick script tick
     assignment       <-- name assign (substitution / word)*
     assign           <   '='
     dollar           <-  '$'
     literal          <-- (!tick !dollar !pipe !semi !par !nl !sp .)+
     variable         <-- dollar (dollar / '*' / '?' / '@' / [0-9] / identifier / ([{] (![}] .)+ [}]))
     delim            <-  singlequotes / doublequotes / substitution
     sq               <   [']
     dq               <   [\"]
     bt               <   [`]
     singlequotes     <-- sq  (doublequotes / (!sq .))* sq
     doublequotes     <-- dq (singlequotes / substitution / variable / (!dq .))* dq
     break            <-  amp / semi !semi
     separator        <-  (sp* break ws*) / ws+
     sequential-sep   <-  (semi !semi ws*) / ws+
     amp              <-  '&'
     semi             <   ';'
     lpar             <   '('
     rpar             <   ')'
     par              <   lpar / rpar
     nl               <   '\n'
     sp               <   [\t ]
     ws               <   sp / nl
     error            <-- .*")

  (let* ((match (match-pattern script input))
         (end (peg:end match))
         (pt (peg:tree match)))
    (if (eq? (string-length input) end)
        pt
        (if match
            (begin
              (format (current-error-port) "parse error: at offset: ~a\n" end)
              (pretty-print pt (current-error-port))
              #f)
            (begin
              (format (current-error-port) "parse error: no match\n")
              #f)))))

(define (parse input)
  (let* ((pt (parse- input))
         (foo (when (> %debug-level 1) (display "tree:\n") (pretty-print pt)))
         (flat (keyword-flatten '(and assignent command literal name or pipeline substitution) pt))
         (foo (when (> %debug-level 0) (display "flat:\n") (pretty-print flat)))
         (ast (transform flat))
         (foo (when (> %debug-level 0) (display "ast:\n") (pretty-print ast))))
    (cond ((error? ast)
           (stderr "error:") (pretty-print ast (current-error-port)) #f)
          ((eq? ast 'script)
           #t)
          (else
           (map (cut local-eval <> (the-environment)) ast)
           ast))))

(define (unspecified? o)
  (eq? o *unspecified*))

(define (trace commands)
  `(xtrace
    ,(lambda _
       (when (shell-opt? "xtrace")
         (for-each
          (lambda (o)
            (match o
              (('command (and command (? string?)) ...)
               (format (current-error-port) "+ ~a\n" (string-join command)))
              (_ format (current-error-port) "+ ~s <FIXME>\n" o)))
          (reverse commands))))))

(define (transform ast)
  (when (> %debug-level 1)
    (pretty-print ast (current-error-port)))
  (match ast
    ;; FIXME: flatten?
    ((('pipeline _ ...) _ ...)
     (map transform (keyword-flatten '(and assignent command literal name or pipeline substitution) ast)))

    ((('literal _ ...) _ ...)
     (map transform (keyword-flatten '(and assignent command literal name or pipeline substitution) ast)))

    ((('assignent _ ...) _ ...)
     (map transform (keyword-flatten '(and assignent command literal name or pipeline substitution) ast)))

    (('script o ...) `(script ,@(map transform o)))

    (('pipeline o ...)
     (let ((commands (map transform o)))
      `(pipeline ,@(cons (trace commands) commands))))
    
    (('command o ...) `(command ,@(map transform o)))
    (('literal o) (transform o))
    (('name o) o)
    (('number o) o)

    (('assignment a b) `(assignment ,(transform a) ',(transform b)))

    (('for-clause name expr body)
     `(for ,(transform name) (lambda _ ,(transform expr)) (lambda _ ,@(transform body))))
    (('sequence o ...)
     `(sequence ,@(fold-right (lambda (o r)
                                (cons
                                 (match o
                                   (('substitution x) (transform o))
                                   (_ `(list ,(transform o))))
                                 r))
                              '() o)))
    (('substitution o) `(substitution ,(transform o)))
    (('if-clause expr then) `(if-clause ,(transform expr) ,(transform then)))
    (('if-clause expr then else) `(if-clause ,(transform expr) ,(transform then) ,(transform else)))
    (('then-part o ...) `(begin ,@(map transform o)))
    (('else-part o ...) `(begin ,@(map transform o)))
    (_ ast)))
