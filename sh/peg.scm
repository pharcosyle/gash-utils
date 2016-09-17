(define-module (sh peg)
  :use-module (ice-9 peg)
  :use-module (ice-9 peg codegen)
  :use-module (ice-9 pretty-print)
  :export (parse))

(define (parse input)
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
 term             <-- pipeline (sp* (and / or) ws* pipeline)*
 and              <-- '&&'
 or               <-- '||'
 pipeline         <-- '!'? sp* command (sp* pipe ws* command)*
 pipe             <-- '|'
 command          <-- simple-command / (compound-command (sp+ io-redirect)*) / function-def
 compound-command <-- brace-group / subshell / for-clause / case-clause / if-clause / while-clause / until-clause
 subshell         <-- '(' compound-list ')'
 compound-list    <-- ws* term (separator term)* separator?
 case-clause      <-- 'case' sp+ word ws+ 'in' ws+ case-item* 'esac'
 case-item        <-- sp* pattern sp* ')' compound-list? ws* case-sep ws
 case-sep         <   ';;'
 pattern          <-- word (sp* '|' sp* word)*
 for-clause       <-- 'for' sp+ identifier ws+ ('in' (sp+ word)* sp* sequential-sep)? do-group
 do-group         <-- 'do' compound-list 'done'
 if-clause        <-- 'if' compound-list 'then' compound-list else-part? 'fi'
 else-part        <-- ('elif' compound-list 'then' compound-list else-part?) / ('else' compound-list)
 while-clause     <-- 'while' compound-list do-group
 until-clause     <-- 'until' compound-list do-group
 function-def     <-- name sp* '(' sp* ')' ws* function-body
 function-body    <-- compound-command io-redirect*
 brace-group      <-- '{' sp* compound-list sp* '}'
 simple-command   <-- (io-redirect sp+)* (!(reserved ws+) word) (sp+ (io-redirect / (!(reserved ws+) word)))*
 reserved         <   ('case' / 'esac' / 'if' / 'fi' / 'then' / 'else' / 'elif' / 'for' / 'done' / 'do' / 'until' / 'while')
 io-redirect      <-- [0-9]* sp* (io-here / io-file)
 io-file          <-- ('<&' /  '>&' / '>>' / '>' / '<>'/ '<' / '>|') sp* ([0-9]+ / filename)
 io-here          <-  ('<<' / '<<-') io-suffix here-document
 io-op            <   '<<-' / '<<' / '<&' /  '>&' / '>>' / '>' / '<>'/ '<' / '>|'
 io-suffix        <-  sp* here-label sp* nl
 filename         <-- word
 name             <-- identifier
 identifier       <-  [_a-zA-Z][_a-zA-Z0-9]*
 word             <-- test / substitution / assignment / literal / number
 number           <-- [0-9]+
 test             <-- ltest (!rtest .)* rtest
 ltest            <   '[ '
 rtest            <   ' ]'
 substitution     <-- ('$(' 'ls' ')') / ('`' script '`')
 assignment       <-- name assign word?
 assign           <   '='
 literal          <-- (subst / delim / (![0-9] (![()] !io-op !sp !nl !break !pipe !assign .)+) / ([0-9]+ &separator)) literal*
 subst            <-- '$' ('$' / '*' / '@' / [0-9] / identifier / ([{] (![}] .)+ [}]))
 delim            <-  (['] (!['] .)* [']) / ([\"] (![\"] .)* [\"]) / ([`] (![`] .)* [`])
 separator        <-- (sp* break !semi ws*) / ws*
 break            <-- amp / semi
 sequential-sep   <-- (semi ws*) / ws+
 amp              <   '&'
 semi             <   ';'
 nl               <   '\n'
 sp               <   [\t ]
 ws               <   sp / nl
")

(let ((match (match-pattern script input)))
  (if (not (eq? (string-length input) (peg:end match)))
      (let ((tree (peg:tree match)))
        (pretty-print (peg:tree match))
        (pretty-print "parse error" (current-error-port))
        (pretty-print (peg:end match))
        #f)
      (peg:tree match))))


;; (display 'foo)
;; (newline)
;; (display (parse "for f in a b; do echo $f; ls; done"))
;; (newline)
