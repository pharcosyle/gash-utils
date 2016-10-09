(use-modules (ice-9 match))

(use-modules (system vm frame)
             (system vm trace))

(define (to-string o)
  (match o
    ((? string?) o)
    ((? symbol?) (symbol->string o))
    ((? number?) (number->string o))
    ((? list?) (string-join (map to-string o) " "))
    ((? pair?) (string-join (list (to-string (car o)) (to-string (cdr o))) " "))
    (_ "???")))

(define (location frame)
  (let ((source (frame-source frame)))
    (if source
        (let* ((args (frame-arguments frame))
               (args (if (null? args) "" (string-append " args: " (to-string args)))))
          (string-append (cadr source) ":"
                              (number->string  (caddr source)) ":" args))
        source)))

(define (stack-trace)
  (let ((skip-stack-capture-crap 0)
        (stack (make-stack #t)))
    (filter identity (let loop ((frame (stack-ref stack skip-stack-capture-crap)))
       (if (not (frame? frame)) '()
           (cons (location frame) (loop (frame-previous frame))))))))

(define (main)
  (catch #t
    (lambda ()
      (with-throw-handler
          #t
        foo
        (lambda (key . args)
          (stdout "error: " args)
          (throw 'exception (stack-trace)))))
    (lambda (key . args)
      (map stdout (car args)))))

(define (foo)
  (bar '(a b))
  (format (current-output-port) "foo\n"))

(define (stdout . o)
  (map (lambda (o) (display o (current-output-port))) o)
  (newline)
  o)

(define (bar arg)
  (match arg
    ('a 'a)
    ((? pair?) (map bar arg)))
  (format (current-output-port) "bar\n"))

(main)
