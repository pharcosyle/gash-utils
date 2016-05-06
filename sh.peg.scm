(use-modules (ice-9 peg))
(use-modules (ice-9 peg codegen))
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))


(define (remove-shell-comments s)
  (string-join (map
                (lambda (s)
                  (let* ((n (string-index s #\#)))
                    (if n (string-pad-right s (string-length s) #\space  0 n)
                        s)))
                (string-split s #\newline)) "\n"))

(define (flatten lst)
  (cond
    ((null? lst)
      '())
    ((list? (car lst))
      (append (flatten (car lst)) (flatten (cdr lst))))
    (else
      (cons (car lst) (flatten (cdr lst))))))

(define (sh-exec ast)
  (define (sh-exec- ast)
    (match ast
      (('name o) o)
      (('word o) o)
      (('command o ...) (map sh-exec- o))
      ((head tail ...) (map sh-exec- (append (list head) tail)))
      ;;(('list o ...) (map sh-exec o))
      ((_ o) (sh-exec- o))
      (_ #f)))
  (let ((cmd (filter identity (flatten (sh-exec- ast)))))
    cmd
    (apply system* cmd)
    ))

;; insert / error at convenient location to short circuit backtracking
(define (parse input)
(define-peg-string-patterns
"script           <-- (sp / linebreak)* (term (separator term)* separator?)?
 term             <-- pipeline (sp* (and / or) (sp / linebreak)* pipeline)*
 and              <-- '&&'
 or               <-- '||'
 pipeline         <-- '!'? sp* command (sp* pipe (sp / linebreak)* command)*
 pipe             <-- '|'
 command          <-- simple-command / (compound-command (sp+ io-redirect)*) / function-def
 compound-command <-- brace-group / subshell / for-clause / case-clause / if-clause / while-clause / until-clause
 subshell         <-- '(' sp* compound-list sp* ')'
 compound-list    <-- (sp / linebreak)* term (separator term)* separator?
 case-clause      <-- 'case' sp+ word (sp / linebreak)* 'in' (sp / linebreak)* (case-item sp)* 'esac'
 case-item        <-- '('? sp* pattern sp* ')' (((sp / linebreak) ';;' (sp / linebreak)) / ((compound-list sp* ';;'?)? (sp / linebreak)))
 pattern          <-- word (sp* '|' sp* word)*
 for-clause       <-- 'for' sp+ identifier (sp / linebreak)+ ('in' (sp+ word)* sp* sequential-sep)? do-group
 do-group         <-- 'do' compound-list 'done'
 if-clause        <-- 'if' compound-list 'then' compound-list else-part? 'fi'
 else-part        <-- ('elif' compound-list 'then' compound-list else-part?) / ('else' compound-list)
 while-clause     <-- 'while' compound-list do-group
 until-clause     <-- 'until' compound-list do-group
 function-def     <-- name sp* '(' sp* ')' (sp / linebreak)* function-body
 function-body    <-- compound-command io-redirect*
 brace-group      <-- '{' sp* compound-list sp* '}'
 simple-command   <-- (io-redirect sp+)* !reserved word (sp+ (io-redirect / (!reserved word)))*
 xsimple-command   <-- !reserved ((cmd-prefix (sp+ cmd-suffix)?) / (word (sp+ cmd-suffix)?))
 reserved         <   ('if' / 'then' / 'else' / 'elif' / 'fi' / 'for' / 'done' / 'do' / 'until' / 'while') (sp / linebreak)
 cmd-prefix       <-- (io-redirect (sp* io-redirect)*) / (word (sp+ word)*)
 cmd-suffix       <-- (io-redirect (sp* io-redirect)*) / (word (sp+ word)*)
 io-redirect      <-- [0-9]* sp* (io-here / io-file)
 io-file          <-- ('<&' /  '>&' / '>>' / '>' / '<>'/ '<' / '>|') sp* ([0-9]+ / filename)
 filename         <-- word
 io-here          <-- ('<<' / '<<-') sp* word
 name             <-- identifier
 identifier       <-- [_a-zA-Z][_a-zA-Z0-9]*
 word             <-- test / substitution / assignment / literal
 test             <-- ltest (!' ]' .)* rtest
 ltest            <   '[ '
 rtest            <   ' ]'
 substitution     <-- ('$' '(' script ')') / ('`' word (sp+ word)* '`')
 assignment       <-- name assign word?
 assign           <   '='
 literal          <-  (subst / delim / (![0-9] (!sp !linebreak ![;&|$()=] .)+) / ([0-9]+ &separator)) literal*
 subst            <-  '$' ('$' / '*' / '@' / [0-9] / identifier / ([{] (![}] .)+ [}]))
 delim            <-  (['] (!['] .)* [']) / ([\"] (![\"] .)* [\"]) / ([`] (![`] .)* [`])
 separator        <-- (sp* break (sp / linebreak)*) / (sp / linebreak)+
 break            <-- '&' / ';'
 sequential-sep   <--  (semi (sp / linebreak)*) / (sp / linebreak)+
 semi             < ';'
 linebreak        <   [\r\n]
 sp               <   [\t ]")
(let ((match (match-pattern script input)))
  (if (not (eq? (string-length input) (peg:end match)))
      (let ((tree (peg:tree match)))
        (pretty-print (peg:tree match))
        (pretty-print "parse error" (current-error-port))
        (pretty-print (peg:end match)))
      (peg:tree match))))


;; (let* ((input (read-string (open-input-file (cadr (command-line)))))
;;        (input (remove-shell-comments input))
;;        (ast (parse input)))
;;   (sh-exec ast))

(pretty-print (parse (remove-shell-comments (read-string (open-input-file (cadr (command-line)))))))
