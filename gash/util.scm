(define-module (gash util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-26)

  :export (disjoin conjoin))

(define (disjoin . predicates)
  (lambda (. arguments)
    (any (cut apply <> arguments) predicates)))

(define (conjoin . predicates)
  (lambda (. arguments)
    (every (cut apply <> arguments) predicates)))
