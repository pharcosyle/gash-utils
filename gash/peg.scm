(define-module (gash peg)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 local-eval)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 peg codegen)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (gash builtins)
  #:use-module (gash environment)
  #:use-module (gash gash)
  #:use-module (gash io)
  #:use-module (gash job)
  #:use-module (gash util)

  #:export (
            parse
            peg-trace?
            ))

(define (wrap-parser-for-users for-syntax parser accumsym s-syn)
  #`(lambda (str strlen pos)
      (when (> (@ (gash gash) %debug-level) 1)
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
     for-clause       <-- for-keyword sp+ name (ws+ in-keyword expression)? sp* sequential-sep do-group
     expression       <-- (sp+ word)+
     do-keyword       <   'do'
     done-keyword     <   'done'
     do-group         <-  do-keyword ws* compound-list separator done-keyword

     if-keyword       <   'if'
     fi-keyword       <   'fi'
     if-clause        <-- if-keyword expression separator then-part elif-part* else-part? fi-keyword
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
     word             <-  test / substitution / assignment / number / variable / delim / literal
     number           <-- [0-9]+
     test             <-- ltest expression  rtest
     ltest            <   '[ '
     rtest            <   ' ]'
     lsubst           <   '$('
     rsubst           <   ')'
     tick             <   '`'
     substitution     <-- lsubst script rsubst / tick script tick
     assignment       <-- name assign (substitution / word)*
     assign           <   '='
     dollar           <-  '$'
     literal          <-- (!ltest !tick !dollar !pipe !semi !par !nl !sp .)+
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

(define (sh-exec ast)
  (define (exec cmd)
    (when (> %debug-level 0)
      (format (current-error-port) "sh-exec:exec cmd=~s\n" cmd))
    (let* ((job (local-eval cmd (the-environment)))
           (stati (cond ((job? job) (map status:exit-val (job-status job)))
                        ((boolean? job) (list (if job 0 1)))
                        ((number? job) (list job))
                        (else (list 0))))
           (status (if (shell-opt? "pipefail") (or (find (negate zero?) stati) 0)
                       (car stati)))
           (pipestatus (string-append
                        "("
                        (string-join
                         (map (lambda (s i)
                                (format #f "[~a]=\"~a\"" s i))
                              stati
                              (iota (length stati))))
                        ")")))
      (assignment "PIPESTATUS" pipestatus)
      (assignment "?" (number->string status))
      (when (and (not (zero? status))
                 (shell-opt? "errexit"))
        (exit status))
      status))
  (when (> %debug-level 1)
    (format (current-error-port) "sh-exec:exec ast=~s\n" ast))
  (match ast
    ('script #t) ;; skip
    (('pipeline commands ...)
     (when (shell-opt? "xtrace")
       (for-each
        (lambda (o)
          (match o
            (('command command ...)
             ;;(format (current-error-port) "+ ~a\n" (string-join command))
             ;; FIXME: side-effects done twice?!
             ;; '(variable "$?"): not a string...hmm
             (format (current-error-port) "+ ~a\n" (string-join (map (cut local-eval <> (the-environment)) command)))
             )
            (_ (format (current-error-port) "FIXME trace:~s" o))))
        (reverse commands)))
     (exec ast))
    (_ (for-each exec ast))))


(define (parse input)
  (let* ((pt (parse- input))
         (foo (pretty-print pt))
         (ast (transform (keyword-flatten '(pipeline) pt)))
         (foo (pretty-print ast)))
    (cond ((error? ast)
           (stderr "error:") (pretty-print ast (current-error-port)) #f)
          (else
           (map sh-exec ast)
           ;;(map (cut local-eval <> (the-environment)) ast)
           ast))))

(define (unspecified? o)
  (eq? o *unspecified*))

(define (transform ast)
  (when (> %debug-level 1)
    (format (current-error-port) "transform ast=~s\n" ast))
  (match ast
    (('script o ...) (map transform o))
    (('substitution o) `(substitution ,@(transform o)))
    (('pipeline o) (pk `(pipeline ,(let ((c (warn 'transform (transform o)))) (or (builtin c) c)))))
    (('pipeline h t) (pk `(pipeline ,(transform h) ,@(map transform t))))
    (('command o ...) `(command ,@(map transform o)))
    (('literal o) (transform o))
    (('name o) o)
    (('number o) o)
    (('expression o ...) `(expression ,@(map transform o)))
    (('assignment a b) `(lambda _ (assignment ,(transform a) ,(transform b))))
    (('for-clause name expr do) `(for ,(transform name) (lambda _ ,(transform expr)) (lambda _ ,(transform do))))
    (('if-clause expr then) `(if ,(transform expr) ,(transform then)))
    (('if-clause expr then else) `(if ,(transform expr) ,(transform then) ,(transform else)))
    (('then-part o ...) `(begin ,@(map transform o)))
    (('else-part o ...) `(begin ,@(map transform o)))
    (_ ast)))

(define (builtin ast)
  (when (> %debug-level 0)
    (format (current-error-port) "builtin ast=~s\n" ast))
  (receive (command args)
      (match ast
        (('command (and (? string?) command) args ...) (values command args))
        ;; ((('append ('glob command) args ...)) (values command args))
        ;; ((('glob command)) (values command #f))
        (_ (values #f #f)))
    (let ((program (and command (PATH-search-path command))))
      (when (> %debug-level 0)
        (format (current-error-port) "command ~a => ~s ~s\n" program command args))
      (cond ((and program (not %prefer-builtins?))
             #f)
            ((and command (assoc-ref %builtin-commands command))
             =>
             (lambda (command)
               (if args
                   `(,apply ,command ',(map (cut local-eval <> (the-environment)) args))
                   command)))
            (else #f)))))

(define (glob pattern)
  (define (glob? pattern)
    (and (string? pattern) (string-match "\\?|\\*" pattern)))
  (define (glob2regex pattern)
    (let* ((pattern (regexp-substitute/global #f "\\." pattern 'pre "\\." 'post))
           (pattern (regexp-substitute/global #f "\\?" pattern 'pre "." 'post))
           (pattern (regexp-substitute/global #f "\\*" pattern 'pre ".*" 'post)))
      (make-regexp (string-append "^" pattern "$"))))
  (define (glob-match regex path) ;; pattern path -> bool
    (regexp-match? (regexp-exec regex path)))
  (define (glob- pattern paths)
    (map (lambda (path)
           (if (string-prefix? "./" path) (string-drop path 2) path))
         (append-map (lambda (path)
                       (map (cute string-append (if (string=? "/" path) "" path) "/" <>)
                            (filter (conjoin (negate (cut string-prefix? "." <>))
                                             (cute glob-match (glob2regex pattern) <>))
                                    (or (scandir path) '()))))
                     paths)))
  (cond
   ((not pattern) '(""))
   ((glob? pattern) (let ((absolute? (string-prefix? "/" pattern)))
                      (let loop ((patterns (filter (negate string-null?) (string-split pattern #\/)))
                                 (paths (if absolute? '("/") '("."))))
                        (if (null? patterns)
                            paths
                            (loop (cdr patterns) (glob- (car patterns) paths))))))
   (#t (list pattern))))

(define (singlequotes . o)
  (string-join o ""))

(define (doublequotes . o)
  (string-join (append-map glob  o) ""))

(define (expression . args)
  (append-map glob args))

(define (for name expr body)
  (for-each (lambda (value)
              (assignment name value)
              (body)) (expr)))

(define (command . args)
  (define (exec command)
    (cond ((procedure? command) command)
          ((every string? command) (cut apply (compose status:exit-val system*) command))
          ;; not sure whether to do $?/PIPESTATUS here or in sh-exec
          ((every string? command)
           (cut apply (compose (lambda (status)
                                 ((compose (cut assignment "?" <>) number->string) status)
                                 status)
                               (cut warn 'exit-val <>)
                               status:exit-val
                               (cut warn 'status <>)
                               system*) command))
          (else (lambda () #t))))
  (exec (append-map glob args)))

(define (substitution . commands)
  (apply (@ (gash pipe) pipeline->string) (map cdr commands))) ;;HACK

(define (pipeline . commands)
  (when (> %debug-level 1)
    (format (current-error-port) "pijp: commands=~s\n" commands))
  ;; FIXME: after running a builtin, we still end up here with the builtin's result
  ;; that should probably not happen, however, cater for it here for now
  (match commands
    (((and (? boolean?) boolean)) (if boolean 0 1))
    (((and (? number?) number)) number)
    (((? unspecified?)) 0)
    (_ (apply (@ (gash pipe) pipeline) #t commands))))
