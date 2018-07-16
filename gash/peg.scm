(define-module (gash peg)
  #:use-module (ice-9 local-eval)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 peg codegen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (gash builtins)
  #:use-module (gash environment)
  #:use-module (gash gash)
  #:use-module (gash io)
  #:use-module (gash script)

  #:export (
            parse
            parse-string
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


;;;;;;;;;;;;;;;;;;;;;;;;;; WIP
(define (expand identifier o) ;;identifier-string -> symbol
  (define (expand- o)
    (let ((dollar-identifier (string-append "$" identifier)))
      (match o
        ((? symbol?) o)
        ((? string?) (if (string=? o dollar-identifier) (string->symbol identifier) o))
        ((? list?) (map expand- o))
        (_ o))))
  (map expand- o))

(define (tostring . args)
  (with-output-to-string (cut map display args)))

;; transform ast -> list of expr
;; such that (map eval expr)
(define (DEAD-transform ast)
  (format (current-error-port) "transform=~s\n" ast)
  (match ast
     (('script term "&") (list (background (transform term))))
     (('script term) `(,(transform term)))
     (('script terms ...) (transform terms))
     (('substitution "$(" script ")") (local-eval (cons 'substitute (cddr (car (transform script)))) (the-environment)))
     (('substitution "`" script "`") (local-eval (cons 'substitute (cddr (car (transform script)))) (the-environment)))
     ((('term command)) `(,(transform command)))
     ((('term command) ...) (map transform command))
     ((('term command) (('term commands) ...)) (map transform (cons command commands)))
     (('compound-list terms ...) (transform terms))
     (('if-clause "if" (expression "then" consequent "fi"))
      `(if (equal? 0 (status:exit-val ,@(transform expression)))
           (begin ,@(transform consequent))))
     (('if-clause "if" (expression "then" consequent ('else-part "else" alternative) "fi"))
      `(if (equal? 0 (status:exit-val ,@(transform expression)))
           (begin ,@(transform consequent))
           (begin ,@(transform alternative))))
     (('for-clause ("for" identifier sep do-group)) #t)
     (('for-clause "for" ((identifier "in" lst sep) do-group))
      `(for-each (lambda (,(string->symbol identifier))
                   (begin ,@(expand identifier (transform do-group))))
                 (glob ,(transform lst))))
     (('do-group "do" (command "done")) (transform command))
     (('pipeline command) (pk 1) (let* ((command (transform command))) (or (builtin command) `(pipeline #t ,@command))))
     (('pipeline command piped-commands) (pk 2) `(pipeline #t ,@(transform command) ,@(transform piped-commands)))
     (('simple-command ('word (assignment name value))) `((lambda _ (let ((name ,(tostring (transform name)))
                                                                          (value ,(tostring (transform value))))
                                                                      (stderr "assignment: " name "=" value)
                                                                      (set! global-variables (assoc-set! global-variables name (glob value)))))))
     (('simple-command ('word s)) `((glob ,(transform s))))
     (('simple-command ('word s1) ('io-redirect "<<" ('here-document s2))) `((append (glob "echo") (cons "-n" (glob ,s2))) (glob ,(transform s1))))
     (('simple-command ('word s1) ('word s2)) `((append (glob ,(transform s1)) (glob ,(transform s2)))))
     (('simple-command ('word s1) (('word s2) ...)) `((append (glob ,(transform s1)) (append-map glob (list ,@(map transform s2))))))
     (('variable s) s)
     (('literal s) (transform s))
     (('singlequotes s) (string-concatenate `("'" ,s "'")))
     (('doublequotes s) (string-concatenate `("\"" ,s "\"")))
     (('backticks s) (string-concatenate `("`" ,s "`")))
     (('delim ('singlequotes s ...)) (string-concatenate (map transform s)))
     (('delim ('doublequotes s ...)) (string-concatenate (map transform s)))
     (('delim ('backticks s ...)) (string-concatenate (map transform s)))
     ((('pipe _) command) (transform command))
     (((('pipe _) command) ...) (map (compose car transform) command))
     ((_ o) (transform o)) ;; peel the onion: (symbol (...)) -> (...)
     (_ ast))) ;; done

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
     oldword             <-  substitution / assignment / number / variable / delim / literal

     word-for-test-assign-sh        <-- assignment / (delim / number / variable / literal)+
     word-for-test-if2-sh            <-- assignment / delim / (number / variable / literal)+

     word    <-- assignment / (delim / number / variable / literal)+


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

(define (flatten o)
  (keyword-flatten '(and assignent command doublequotes for-clause literal name or pipeline singlequotes substitution word) o))

(define (unspecified? o)
  (eq? o *unspecified*))

(define (transform ast)
  (when (> %debug-level 1)
    (pretty-print ast (current-error-port)))
  (match ast
    ;; FIXME: flatten?

    ((('assignent _ ...) _ ...) (map transform (flatten ast)))
    ((('command _ ...) _ ...) (map transform (flatten ast)))
    ((('doublequotes _ ...) _ ...) (map transform (flatten ast)))
    ((('for-clause _ ...) _ ...) (map transform (flatten ast)))
    ((('literal _ ...) _ ...) (map transform (flatten ast)))
    ((('pipeline _ ...) _ ...) (map transform (flatten ast)))
    ((('singlequotes _ ...) _ ...) (map transform (flatten ast)))
    ((('word _ ...) _ ...) (map transform (flatten ast)))

    (('script ('pipeline ('command command ... (word (literal "&")))))
     (background `(pipeline ',(map transform command))))

    (('script terms ...) `(script ,@(map transform terms)))

    (('pipeline o ...)
     (let ((commands (map transform o)))
       `(pipeline ,@(cons (trace commands) commands))))
    
    (('command o ...) `(command ,@(map transform o)))
    (('literal o) (transform o))
    (('name o) o)
    (('number o) o)

    ;;(('assignment a b) `(assignment ,(transform a) ',(transform b)))
    ;; FIXME: to quote or not?
    (('assignment a) `(substitution (variable ,(transform a))))
    (('assignment a b) `(assignment ,(transform a) ,(transform b)))

    ;; (('assignment a (and b ('literal _ ...))) `(assignment ,(transform a) ,(transform b)))
    ;; (('assignment a b)
    ;;  `(assignment ,(transform a) ,(map transform b)))


    (('for-clause name sequence (and body ('pipeline _ ...)))
     `(for ,(transform name) (lambda _ ,(transform sequence)) (lambda _ ,(transform body))))
    (('for-clause name expr body)
     `(for ,(transform name) (lambda _ ,(transform expr)) (lambda _ ,@(map transform body))))
    (('sequence o)
     `(sequence ,@(fold-right (lambda (o r)
                                      (cons
                                       (match o
                                         (('substitution x) (transform o))
                                         (_ `(list ,(transform o))))
                                       r))
                                    '() o)))
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
    (('word 'singlequotes) "")
    (('word o) (transform o))
    (('word o ...) `(string-append ,@(map transform o)))
    (_ ast)))


(define (remove-shell-comments s)
  (string-join (map
                (lambda (s)
                  (let* ((n (string-index s #\#)))
                    (if n (string-pad-right s (string-length s) #\space  0 n)
                        s)))
                (string-split s #\newline)) "\n"))

(define (remove-escaped-newlines s)
  (reduce (lambda (next prev)
            (let* ((escaped? (string-suffix? "\\" next))
                   (next (if escaped? (string-drop-right next 1) next))
                   (sep (if escaped? "" "\n")))
              (string-append prev sep next)))
          "" (string-split s #\newline)))

(define (parse-string string)
  (let* ((pt ((compose parse- remove-escaped-newlines remove-shell-comments) string))
         (foo (when (> %debug-level 1) (display "tree:\n") (pretty-print pt)))
         (flat (flatten pt))
         (foo (when (> %debug-level 0) (display "flat:\n") (pretty-print flat)))
         (ast (transform flat))
         (foo (when (> %debug-level 0) (display "ast:\n") (pretty-print ast))))
    (cond ((error? ast)
           (stderr "error:") (pretty-print ast (current-error-port)) #f)
          ((eq? ast 'script)
           #t)
          (else ast))))

(define (parse port)
  (parse-string (read-string port)))

