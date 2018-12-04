(define-module (geesh pattern)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (parse-pattern
            pattern-quote
            pattern-plain?
            pattern-match?))

(define* (parse-rdelim s1 s2 #:optional (start 0) (end (string-length s1)))

  (define (not-backslash? chr) (not (char=? chr #\\)))

  (define (escaped? index)
    (even? (- index (or (string-rindex s1 not-backslash? start index) 0))))

  (let loop ((index (string-contains s1 s2 start end)))
    (match index
      (#f (values #f 0))
      (_ (if (escaped? index)
             (loop (string-contains s1 s2 (1+ index) end))
             (values (substring s1 start index) (+ (- index start) 2)))))))

(define* (parse-collating-symbol s #:optional (start 0)
                                 (end (string-length s)))
  (parse-rdelim s ".]" start end))

(define* (parse-equivalence-class s #:optional (start 0)
                                  (end (string-length s)))
  (parse-rdelim s "=]" start end))

(define* (parse-character-class s #:optional (start 0)
                                (end (string-length s)))
  (parse-rdelim s ":]" start end))

(define character-range
  (let ((lower "abcdefghijklmnopqrstuvwxyz")
        (upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (digits "0123456789"))
    (lambda (start end)
      (let loop ((strs (list lower upper digits)))
        (match strs
          (() #f)
          ((str . tail)
           (match (string-index str start)
             (#f (loop tail))
             (sindex
              (match (string-index str end)
                (#f (loop tail))
                (eindex (if (<= sindex eindex)
                            (string->list str sindex (1+ eindex))
                            (loop tail))))))))))))

(define* (parse-matching-bracket-expression s #:optional (start 0)
                                            (end (string-length s)))
  (let loop ((i start) (acc '()))
    (match (and (< i end) (string-ref s i))
      (#f (values #f 0))
      (#\] (if (= i start)
               (loop (1+ i) (cons #\] acc))
               (values (list->char-set acc) (1+ (- i start)))))
      (#\[ (match (and (< (1+ i) end) (string-ref s (1+ i)))
             (#\. (receive (result length)
                      (parse-collating-symbol s (+ i 2) end)
                    (if result
                        (throw 'pattern-collating-symbol)
                        (loop (1+ i) (cons #\[ acc)))))
             (#\= (receive (result length)
                      (parse-equivalence-class s (+ i 2) end)
                    (if result
                        (throw 'pattern-equivalence-class)
                        (loop (1+ i) (cons #\[ acc)))))
             (#\: (receive (result length)
                      (parse-character-class s (+ i 2) end)
                    (if result
                        (throw 'pattern-character-class)
                        (loop (1+ i) (cons #\[ acc)))))
             (_ (loop (1+ i) (cons #\[ acc)))))
      (#\- (if (or (= i start)
                   (and (< (1+ i) end) (char=? (string-ref s (1+ i)) #\])))
               (loop (1+ i) (cons #\- acc))
               (let ((alpha (and (pair? acc) (car acc)))
                     ;; XXX: Escaped range end?
                     (omega (and (< (1+ i) end) (string-ref s (1+ i)))))
                 (match (character-range alpha omega)
                   (#f (throw 'pattern-range-expression))
                   (chrs (loop (+ i 2) (append chrs acc)))))))
      (#\\ (if (< (1+ i) end)
               (loop (+ i 2) (cons (string-ref s (1+ i)) acc))
               (loop (1+ i) acc)))
      (chr (loop (1+ i) (cons chr acc))))))

(define* (parse-bracket-expression s #:optional (start 0)
                                   (end (string-length s)))
  (let* ((matching? (not (char=? (string-ref s start) #\!)))
         (start* (if matching? start (1+ start))))
    (receive (result length)
        (parse-matching-bracket-expression s start* end)
      (if (or (not result) matching?)
          (values result length)
          (values (char-set-complement! result) (1+ length))))))

(define* (parse-pattern s #:optional (start 0) (end (string-length s)))
  "Parse the string @var{s} as a pattern."
  (let loop ((i start) (parts '()) (acc '()))
    (match (and (< i end) (string-ref s i))
      (#f (if (null? acc)
              (reverse! parts)
              (reverse! (cons (reverse! acc) parts))))
      (#\* (if (null? acc)
               (loop (1+ i) (cons '* parts) '())
               (loop (1+ i) (cons* '* (reverse! acc) parts) '())))
      (#\? (loop (1+ i) parts (cons '? acc)))
      (#\[ (if (< (1+ i) end)
               (receive (result length)
                   (parse-bracket-expression s (1+ i) end)
                 (if result
                     (loop (+ i length 1) parts (cons result acc))
                     (loop (1+ i) parts (cons #\[ acc))))
               (loop (1+ i) parts (cons #\[ acc))))
      (#\\ (if (< (1+ i) end)
               (loop (+ i 2) parts (cons (string-ref s (1+ i)) acc))
               (loop (1+ i) parts acc)))
      (chr (loop (1+ i) parts (cons chr acc))))))

(define pattern-quote
  (let ((specials '(#\\ #\* #\? #\[ #\] #\! #\-)))
    (lambda (s)
      "Quote all the special characters in @var{s} so that none of
them are treated specially when @var{s} is interpreted as a pattern."
      (reverse-list->string
       (string-fold (lambda (chr acc)
                      (if (member chr specials)
                          (cons* chr #\\ acc)
                          (cons chr acc)))
                    '()
                    s)))))

(define (pattern-plain? pattern)
  "Check if @var{pattern} free of special pattern constructions like
asterisks and bracket expressions.  If a pattern is ``plain'' its
source string is the only string that will match it."
  (every (match-lambda
           ('* #f)
           (parts (every (match-lambda
                           ((or (? char?) (? string?)) #t)
                           (_ #f))
                         parts)))
         pattern))

(define* (string-starts-with-part s part #:optional (start 0)
                                  (end (string-length s)))
  (let loop ((part part) (i start))
    (match (and (< i end) part)
      (#f (match part
            (() (- i start))
            (((? string? s2) . rest)
             (and (string-null? s2)
                  (loop rest i)))
            (_ #f)))
      (() (- i start))
      (('? . rest) (loop rest (1+ i)))
      (((? string? s2) . rest)
       (and (string-prefix? s2 s 0 (string-length s2) i end)
            (loop rest (+ i (string-length s2)))))
      (((? char? chr) . rest)
       (and (char=? (string-ref s i) chr)
            (loop rest (1+ i))))
      (((? char-set? cs) . rest)
       (and (char-set-contains? cs (string-ref s i))
            (loop rest (1+ i)))))))

(define (pattern-part-length part)
  (let loop ((part part) (length 0))
    (match part
      (() length)
      (((? string? str) . rest) (loop rest (+ length (string-length str))))
      ((first . rest) (loop rest (1+ length))))))

(define* (string-ends-with-part s part #:optional (start 0)
                                (end (string-length s)))
  (let ((start* (- end (pattern-part-length part))))
    (and (>= start* start)
         (string-starts-with-part s part start* end))))

(define* (string-contains-part s part #:optional (start 0)
                               (end (string-length s)))
  (let loop ((part part) (i start))
    (match part
      (() (cons start (- i start)))
      (('? . rest) (loop rest (1+ i)))
      (((? string? s2) . rest)
       (and=> (string-contains s s2 i end)
              (lambda (index)
                (or (and=> (string-starts-with-part s part index end)
                           (cut cons index <>))
                    (loop part (1+ index))))))
      (((or (? char? cp) (? char-set? cp)) . rest)
       (and=> (string-index s cp i end)
              (lambda (index)
                (or (and=> (string-starts-with-part s part index end)
                           (cut cons index <>))
                    (loop part (1+ index)))))))))

(define* (pattern-match? pattern str #:optional (start 0)
                         (end (string-length str))
                         #:key explicit-initial-period?)
  "Check if @var{str} matches @var{pattern}."
  (if (and explicit-initial-period?
           (< start end)
           (char=? (string-ref str start) #\.))
      (match pattern
        (((#\. . _) . _)
         (pattern-match? pattern str start end
                         #:explicit-initial-period? #f))
        ((((? string? s) . _) ._)
         (and (string-prefix? "." s)
              (pattern-match? pattern str start end
                              #:explicit-initial-period? #f)))
        (_ #f))
      (let loop ((pattern pattern) (i start))
        (match pattern
          (() (= i end))
          (('*) #t)
          (('* (? pair? part)) (string-ends-with-part str part i end))
          (('* (? pair? part) . rest)
           (and=> (string-contains-part str part i end)
                  (match-lambda
                    ((match-index . match-length)
                     (loop rest (+ match-index match-length))))))
          (((? pair? part) . rest)
           (and=> (string-starts-with-part str part i end)
                  (lambda (length)
                    (loop rest (+ i length)))))))))
