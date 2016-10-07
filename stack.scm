(use-modules (ice-9 match))

(use-modules (system vm frame)
             (system vm trace))

(define (location frame)
  (let ((source (frame-source frame)))
    (if source
        (string-append (cadr source) ":"
                       (number->string  (caddr source)) ":")
        source)))

(define (stack-trace)
  (let ((skip-stack-capture-crap 4)
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
  (bar)
  (format (current-output-port) "foo\n"))

(define (stdout . o)
  (map (lambda (o) (display o (current-output-port))) o)
  (newline)
  o)

(define (bar)
  (define (blurp o)
    (match o
      ('a 'a)
      ('c 'c)
      ((? pair?) (map blurp o))))
  (blurp '(a b))
  (format (current-output-port) "bar\n"))

(main)
