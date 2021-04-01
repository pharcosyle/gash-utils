;;; Gash-Utils
;;; Copyright © 2018, 2021 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of Gash-Utils.
;;;
;;; Gash-Utils is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Gash-Utils is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Gash-Utils.  If not, see <https://www.gnu.org/licenses/>.

(define-module (gash commands sort)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)

  #:use-module (gash commands config)
  #:use-module (gash io)
  #:use-module (gash shell-utils)
  #:use-module (gash-utils options))

;;; Commentary:

;;; Code:

(define (char-pred->pred char-pred)
  "Convert @var{char-pred} into a predicate.  If @var{char-pred} is a
character, the predicate will check for equality.  If @var{char-pred} is
a character set, it will check for membership.  If @var{char-pred} is a
procedure, it will be used directly."
  (match char-pred
    ((? char?) (lambda (x) (char=? x char-pred)))
    ((? char-set?) (lambda (x) (char-set-contains? char-pred x)))
    ((? procedure?) char-pred)
    (_ (scm-error 'wrong-type-arg 'char-pred->pred
                  "Wrong type argument in position 1: ~A"
                  (list char-pred) (list char-pred)))))

(define (string-fields str char-pred)
  "Split STR on CHAR-PRED, returning the start and end index for each
field as a pair.  If these pairs of indexes are passed on to
@code{substring}, the result is exactly the same as calling
@code{(string-split @var{str} @var{char-pred})}."
  (let loop ((k 0) (acc '()))
    (match (string-index str char-pred k)
      (#f (reverse (cons (cons k (string-length str)) acc)))
      (idx (loop (1+ idx) (cons (cons k idx) acc))))))

(define* (string-fields/transition str #:optional
                                   (char-pred char-set:blank))
  "Split STR on the true-to-@code{#f} transition (falling edge) of
CHAR-PRED, returning the start and end index for each field as a pair."
  (define pred (char-pred->pred char-pred))
  (define not-pred (negate pred))
  (let loop ((k 0) (acc '()))
    (match (string-index str not-pred k)
      (#f (reverse (cons (cons k (string-length str)) acc)))
      (idx1 (match (string-index str pred idx1)
              (#f (reverse (cons (cons k (string-length str)) acc)))
              (idx2 (loop idx2 (cons (cons k idx2) acc))))))))

(define-record-type <sort-key-index>
  (make-sort-key-index field character ignore-blanks?)
  sort-key-index?
  (field sort-key-index-field)
  (character sort-key-index-character)
  (ignore-blanks? sort-key-index-ignore-blanks?))

(define char-set:non-blank (char-set-complement char-set:blank))

(define* (string-key-index str fields key-index #:key end?)
  "Return the index into @var{str} specified by @var{key-index} with the
fields of @var{str} delimited according to @var{fields}.  If @var{end?}
is true, use the end of a field as the default character offset.  The
value of @var{fields} should be a list of pairs of indexes into
@var{str} that indicate the start and end of each field."
  (match key-index
    (#f (if end? (string-length str) 0))
    (($ <sort-key-index> field character ignore-blanks?)
     (match (and (< field (length fields)) (list-ref fields field))
       (#f (string-length str))
       ((start . end)
        (if (and end? (not character))
            end
            (+ (or character 0)
               (if ignore-blanks?
                   (or (string-index str char-set:non-blank start end) end)
                   start))))))))

(define (make-key-extractor fields start-key-index end-key-index)
  "Return a procedure that, given a string, returns the substring from
@var{start-key-index} to @var{end-key-index} (both interpreted in the
context of @var{fields})."
  (lambda (str)
    (let ((start (string-key-index str fields start-key-index))
          (end (string-key-index str fields end-key-index #:end? #t)))
      (substring str (min (string-length str) start end)
                 (min (string-length str) end)))))

(define-record-type <sort-key>
  (make-sort-key start end dictionary-order? ignore-case?
                 ignore-nonprinting? numeric-sort? reverse?)
  sort-key?
  (start sort-key-start)
  (end sort-key-end)
  (dictionary-order? sort-key-dictionary-order?)
  (ignore-case? sort-key-ignore-case?)
  (ignore-nonprinting? sort-key-ignore-nonprinting?)
  (numeric-sort? sort-key-numeric-sort?)
  (reverse? sort-key-reverse?))

(define %default-sort-key
  (make-sort-key #f #f #f #f #f #f #f))

(define (locale-string->inexact* str)
  (let-values (((n len) (locale-string->inexact str)))
    n))

(define string-filter-dictionary
  (let ((char-set:dictionary (char-set-union char-set:blank
                                             char-set:letter+digit)))
    (lambda (str)
      (string-filter char-set:dictionary str))))

(define (string-filter-nonprinting str)
  (string-filter char-set:printing str))

(define (sort-key-relations sort-key fields1 fields2)
  "Convert @var{sort-key} into two procedures (as two values): the first
takes two strings and determines if the first is less than the second;
the second takes two strings and determines if they are equal.  For both
procedures, @var{fields1} will be used as the fields of the first
string, and @var{fields2} will be used as the fields of the second."
  (match sort-key
    (($ <sort-key> start end dictionary-order? ignore-case?
                   ignore-nonprinting? numeric-sort? reverse?)
     (let ((kex1 (make-key-extractor fields1 start end))
           (kex2 (make-key-extractor fields2 start end))
           (filter (cond
                    (numeric-sort? locale-string->inexact*)
                    (dictionary-order? string-filter-dictionary)
                    (ignore-nonprinting? string-filter-nonprinting)
                    (else identity)))
           (klt (if numeric-sort?
                    (if reverse? > <)
                    (if reverse?
                        (if ignore-case? string-locale-ci>? string-locale>?)
                        (if ignore-case? string-locale-ci<? string-locale<?))))
           (keq (if numeric-sort?
                    =
                    (if ignore-case? string-locale-ci=? string=?))))
       (values (lambda (s1 s2)
                 (klt (filter (kex1 s1)) (filter (kex2 s2))))
               (lambda (s1 s2)
                 (keq (filter (kex1 s1)) (filter (kex2 s2)))))))))

(define guile-sort (@ (guile) sort))

(define* (sort/keys lst keys #:optional field-separator)
  "Sort @var{lst} according to @var{keys}.  The value of @var{keys} must
be a list of sort keys.  Elements of @var{lst} will be compared
according to @var{keys}, starting with the first key and moving on to
later keys only if all ealier keys compare equal.  To resolve field
references in @var{keys} elements of @var{lst} will be split using the
character @var{field-separator} if it is set, or a default field
separator otherwise."
  (define (less str1 str2)
    (let ((fields1 (if field-separator
                       (string-fields str1 field-separator)
                       (string-fields/transition str1)))
          (fields2 (if field-separator
                       (string-fields str2 field-separator)
                       (string-fields/transition str2))))
      (let loop ((keys keys))
        (match keys
          (() #f)
          ((key . rest)
           (let-values (((klt keq) (sort-key-relations key fields1 fields2)))
             (or (klt str1 str2)
                 (and (keq str1 str2) (loop rest)))))))))
  (guile-sort lst less))


;; Command-line interface

(define *help-message* "\
Usage: sort OPTION... [FILE]...
Write sorted concatenation of all FILEs to standard output.

With no FILE, or when FILE is -, read standard input.

Options:
  -d, --dictionary-order  compare only alphanumeric and blank characters
  -f, --ignore-case       compare ignoring letter case
  -i, --ignore-nonprinting  compare ignoring non-printing characters
  -n, --numeric-sort      compare according to string numerical value
  -r, --reverse           reverse the result of comparisons
  -b, --ignore-blanks     ignore blanks for character offsets
  -t, --field-separator=SEP  use SEP as the field separator
  -k, --key=KEY           sort according to the key definition KEY
  -c, --check             check that input is already sorted
  -u, --unique            delete duplicate lines
      --help              display this help and exit
      --version           output version information and exit
")

(define *version-message*
  (format #f "sort (~a) ~a~%" %package-name %version))

(define *options-grammar*
  (make-options-grammar
   `((flag dictionary-order #\d)
     (flag ignore-case #\f)
     (flag ignore-nonprinting #\i)
     (flag numeric-sort #\n)
     (flag reverse #\r)
     (flag ignore-blanks #\b)
     (value field-separator #\t)
     (list key #\k)
     (flag check #\c)
     (flag unique #\u)
     (message ("help" #\h) ,*help-message*)
     (message ("version" #\V) ,*version-message*))
   #:default 'files))

(define char-set:non-ascii-digit+dot
  (char-set-complement
   (char-set-union (char-set #\.)
                   (char-set-intersection char-set:digit
                                          char-set:ascii))))

(define char-set:modifiers
  (string->char-set "bdfinr"))

(define* (string->key-index str #:key ignore-blanks?)
  "Convert STR into a sort key index.  Use @var{ignore-blanks?} for the
@code{ignore-blanks?} field."
  (match (string-split str #\.)
    ((field-str)
     (and-let* ((field* (string->number field-str))
                (field (and (> field* 0) (1- field*))))
       (make-sort-key-index field #f ignore-blanks?)))
    ((field-str character-str)
     (and-let* ((field* (string->number field-str))
                (field (and (> field* 0) (1- field*)))
                (character* (string->number character-str))
                (character (if (zero? character*) #f (1- character*))))
       (make-sort-key-index field character ignore-blanks?)))
    (_ #f)))

(define* (string->key-index+modifiers str #:key ignore-blanks?)
  "Convert STR into two values: a sort key index and a string containing
modifiers.  The value of @var{ignore-blanks?} will be used for the sort
key index unless the modifiers from @var{str} override it."
  (match (string-index str char-set:non-ascii-digit+dot)
    (#f (values (string->key-index str #:ignore-blanks? ignore-blanks?)
                ""))
    (idx (let* ((modifiers (substring str idx))
                (ignore-blanks? (or (string-index modifiers #\b)
                                    (and (string-null? modifiers)
                                         ignore-blanks?))))
           (if (string-every char-set:modifiers modifiers)
               (values (string->key-index (substring str 0 idx)
                                          #:ignore-blanks? ignore-blanks?)
                       modifiers)
               (values #f #f))))))

(define (make-sort-key/modifiers start end modifiers)
  "Create a sort key from the sort key indexes @var{start} and
@var{end}, with other fields taken from the string @var{modifiers}."
  (make-sort-key start end
                 (string-index modifiers #\d)
                 (string-index modifiers #\f)
                 (string-index modifiers #\i)
                 (string-index modifiers #\n)
                 (string-index modifiers #\r)))

(define (combine-modifiers defaults . mods)
  "Combine the modifier strings @var{mods}, using @var{defaults} if they
are all empty."
  (if (every string-null? mods) defaults (string-concatenate mods)))

(define* (string->sort-key str #:key dictionary-order? ignore-case?
                           ignore-nonprinting? numeric-sort? reverse?
                           ignore-blanks?)
  "Convert @var{str} to a sort key.  The values @var{dictionary-order?},
@var{ignore-case?}, @var{ignore-nonprinting?}, @var{numeric-sort?},
@var{reverse?}, and @var{ignore-blanks?} will be used for their
respective fields unless overridden by modifiers specified in
@var{str}."
  (define default-mods
    (list->string
     (filter-map (lambda (flag chr)
                   (and flag chr))
                 (list dictionary-order? ignore-case?
                       ignore-nonprinting? numeric-sort? reverse?)
                 (string->list "dfinr"))))

  (match (string-split str #\,)
    ((key-start)
     (let-values (((start smods)
                   (string->key-index+modifiers key-start
                                                #:ignore-blanks?
                                                ignore-blanks?)))
       (and start smods
            (let ((mods (combine-modifiers default-mods smods)))
              (make-sort-key/modifiers start #f mods)))))
    ((key-start key-end)
     (let-values (((start smods)
                   (string->key-index+modifiers key-start
                                                #:ignore-blanks?
                                                ignore-blanks?))
                  ((end emods)
                   (string->key-index+modifiers key-end
                                                #:ignore-blanks?
                                                ignore-blanks?)))
       (and start smods end emods
            (let ((mods (combine-modifiers default-mods smods emods)))
              (make-sort-key/modifiers start end mods)))))))

(define (sort . args)
  (let* ((options (parse-options args *options-grammar*))
         (dictionary-order? (assoc-ref options 'dictionary-order))
         (ignore-case? (assoc-ref options 'ignore-case))
         (ignore-nonprinting? (assoc-ref options 'ignore-nonprinting))
         (numeric-sort? (assoc-ref options 'numeric-sort))
         (reverse? (assoc-ref options 'reverse))
         (ignore-blanks? (assoc-ref options 'ignore-blanks))
         (field-separator-str (assoc-ref options 'field-separator))
         (field-separator (cond
                           ((not field-separator-str) #f)
                           ((= (string-length field-separator-str) 1)
                            (string-ref field-separator-str 0))
                           (else
                            (format (current-error-port)
                                    "sort: Invalid field separator: ~a~%"
                                    field-separator-str)
                            (exit 2))))
         (key-strs (reverse (or (assoc-ref options 'key) '("1"))))
         (keys (map (lambda (key-str)
                      (or (string->sort-key key-str
                                            #:dictionary-order?
                                            dictionary-order?
                                            #:ignore-case?
                                            ignore-case?
                                            #:ignore-nonprinting?
                                            ignore-nonprinting?
                                            #:numeric-sort?
                                            numeric-sort?
                                            #:reverse?
                                            reverse?
                                            #:ignore-blanks?
                                            ignore-blanks?)
                          (begin
                            (format (current-error-port)
                                    "sort: Invalid key: ~a~%" key-str)
                            (exit 2))))
                    key-strs))
         (check? (assoc-ref options 'check))
         (unique? (assoc-ref options 'unique))
         (files (or (assoc-ref options 'files) '())))
    (let* ((files (if (pair? files) files
                      (list "-")))
           (lines (append-map (compose (cut read-lines <>) (lambda (file) (if (equal? file "-") (current-input-port) (open-input-file file)))) files))
           (sorted (sort/keys lines (append keys (list %default-sort-key))
                              field-separator))
           (sorted (if unique? (delete-duplicates! sorted string=?) sorted))
           (sorted (if reverse? (reverse! sorted) sorted)))
      (when (and check? (not (equal? lines sorted)))
        (exit 1))
      (unless check?
        (for-each stdout sorted)))))

(define main sort)
