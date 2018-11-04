;;; Gash --- Guile As SHell
;;; Copyright © 2013 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of Gash.
;;;
;;; Gash is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Gash is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Gash.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; The initial ustar.scm was taken from the Guile100 challenge
;;; https://github.com/spk121/guile100 from a contribution by Mark H
;;; Weaver.

;;; Code:

(define-module (gash ustar)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (gash shell-utils)
  #:export (read-ustar-archive
            read-ustar-port
            write-ustar-archive
            write-ustar-port
            list-ustar-archive
            list-ustar-port))

(define (fmt-error fmt . args)
  (error (apply format #f fmt args)))

;; Like 'string-pad-right', but for bytevectors.  However, unlike
;; 'string-pad-right', truncation is not allowed here.
(define* (bytevector-pad
          bv len #:optional (byte 0) (start 0) (end (bytevector-length bv)))
  (when (< len (- end start))
    (fmt-error
     "bytevector-pad: truncation would occur: len ~a, start ~a, end ~a, bv ~s"
     len start end bv))
  (let ((result (make-bytevector len byte)))
    (bytevector-copy! bv start result 0 (- end start))
    result))

(define (bytevector-append . bvs)
  (let* ((lengths (map bytevector-length bvs))
         (total (fold + 0 lengths))
         (result (make-bytevector total)))
    (fold (lambda (bv len pos)
            (bytevector-copy! bv 0 result pos len)
            (+ pos len))
          0 bvs lengths)
    result))

(define ustar-charset
  #;
  (char-set-union (ucs-range->char-set #x20 #x23)
  (ucs-range->char-set #x25 #x40)
  (ucs-range->char-set #x41 #x5B)
  (ucs-range->char-set #x5F #x60)
  (ucs-range->char-set #x61 #x7B))
  char-set:ascii)

(define (valid-ustar-char? c)
  (char-set-contains? ustar-charset c))

(define (ustar-string n str name)
  (unless (>= n (string-length str))
    (fmt-error "~a is too long (max ~a): ~a" name n str))
  (unless (string-every valid-ustar-char? str)
    (fmt-error "~a contains unsupported character(s): ~s in ~s"
               name
               (string-filter (negate valid-ustar-char?) str)
               str))
  (bytevector-pad (string->bytevector str (make-transcoder (latin-1-codec))) n))

(define (ustar-0string n str name)
  (bytevector-pad (ustar-string (- n 1) str name)
                  n))

(define (ustar-number n num name)
  (unless (and (integer? num)
               (exact? num)
               (not (negative? num)))
    (fmt-error "~a is not a non-negative exact integer: ~a" name num))
  (unless (< num (expt 8 (- n 1)))
    (fmt-error "~a is too large (max ~a): ~a" name (expt 8 (- n 1)) num))
  (bytevector-pad (string->bytevector (string-pad (number->string num 8)
                                            (- n 1)
                                            #\0)
                                      (make-transcoder (latin-1-codec)))
                  n))

(define (checksum-bv bv)
  (let ((len (bytevector-length bv)))
    (let loop ((i 0) (sum 0))
      (if (= i len) sum
          (loop (+ i 1) (+ sum (bytevector-u8-ref bv i)))))))

(define (checksum . bvs)
  (fold + 0 (map checksum-bv bvs)))

(define nuls (make-bytevector 512 0))

;; read a ustar record of exactly 512 bytes.
(define (read-ustar-record port)
  (get-bytevector-n port 512))

;; write a ustar record of exactly 512 bytes, starting with the
;; segment of BV between START (inclusive) and END (exclusive), and
;; padded at the end with nuls as needed.
(define* (write-ustar-record
          port bv #:optional (start 0) (end (bytevector-length bv)))
  (when (< 512 (- end start))
    (fmt-error "write-ustar-record: record too long: start ~s, end ~s, bv ~s"
               start end bv))
  ;; We could have used 'bytevector-pad' here,
  ;; but instead use a method that avoids allocation.
  (put-bytevector port bv start end)
  (put-bytevector port nuls 0 (- 512 (- end start))))

;; write 1024 zero bytes, which indicates the end of a ustar archive.
(define (write-ustar-footer port)
  (put-bytevector port nuls)
  (put-bytevector port nuls))

(define (compose-path-name dir name)
  (if (or (string-null? dir)
          (file-name-separator? (string-ref dir (- (string-length dir) 1))))
      (string-append dir name)
      (string-append dir "/" name)))

;; Like 'call-with-port', but also closes PORT if an error occurs.
(define (call-with-port* port proc)
  (dynamic-wind
    (lambda () #f)
    (lambda () (proc port))
    (lambda () (close port))))

(define (call-with-dirstream* dirstream proc)
  (dynamic-wind
    (lambda () #f)
    (lambda () (proc dirstream))
    (lambda () (closedir dirstream))))

(define (files-in-directory dir)
  (call-with-dirstream* (opendir dir)
    (lambda (dirstream)
      (let loop ((files '()))
        (let ((name (readdir dirstream)))
          (cond ((eof-object? name)
                 (reverse files))
                ((member name '("." ".."))
                 (loop files))
                (else
                 (loop (cons (compose-path-name dir name) files)))))))))

;; split the path into prefix and name fields for purposes of the
;; ustar header.  If the entire path fits in the name field (100 chars
;; max), then leave the prefix empty.  Otherwise, try to put the last
;; component into the name field and everything else into the prefix
;; field (155 chars max).  If that fails, put as much as possible into
;; the prefix and the rest into the name field.  This follows the
;; behavior of GNU tar when creating a ustar archive.
(define (ustar-path-name-split path orig-path)
  (define (too-long)
    (fmt-error "~a: file name too long" orig-path))
  (let ((len (string-length path)))
    (cond ((<= len 100) (values "" path))
          ((> len 256) (too-long))
          ((string-rindex path
                          file-name-separator?
                          (- len 101)
                          (min (- len 1) 156))
           => (lambda (i)
                (values (substring path 0 i)
                        (substring path (+ i 1) len))))
          (else (too-long)))))

(define (bv->ustar-string bv name)
  (string-trim-right (bv->ustar-0string bv name) (compose zero? char->integer)))

(define (bv->ustar-number bv name)
  (let ((string (bv->ustar-string bv name)))
    (or (string->number string 8) 0)))

(define (bv->ustar-0string bv name)
  (bytevector->string bv (make-transcoder (latin-1-codec))))

(define-immutable-record-type <ustar-header>
  (make-ustar-header name
                     mode
                     uid
                     gid
                     size
                     mtime
                     checksum
                     ;; space
                     type-flag
                     link-name
                     magic
                     version
                     uname
                     gname
                     dev-major
                     dev-minor
                     prefix)
  ustar-header?
  (name      ustar-header-name     )
  (mode      ustar-header-mode     )
  (uid       ustar-header-uid      )
  (gid       ustar-header-gid      )
  (size      ustar-header-size     )
  (mtime     ustar-header-mtime    )
  (checksum  ustar-header-checksum )
  ;;(space     ustar-header-space    )
  (type-flag ustar-header-type-flag)
  (link-name ustar-header-link-name)
  (magic     ustar-header-magic    )
  (version   ustar-header-version  )
  (uname     ustar-header-uname    )
  (gname     ustar-header-gname    )
  (dev-major ustar-header-dev-major)
  (dev-minor ustar-header-dev-minor)
  (prefix    ustar-header-prefix   ))

(define (ustar-header-type header)
  (let ((file-types #(regular - symlink char-special block-special directory fifo))
        (type (string->number (ustar-header-type-flag header))))
    (when (or (not type)
              (< type 0)
              (>= type (vector-length file-types)))
      (fmt-error "~a: unsupported file type ~a"
                 (ustar-header-file-name header) type))
    (vector-ref file-types (string->number (ustar-header-type-flag header)))))

(define ustar-header-field-size-alist
  '((name           . 100)
    (mode           .   8)
    (uid            .   8)
    (gid            .   8)
    (size           .  12)
    (mtime          .  12)
    (checksum       .   7)
    (space          .   1)
    (type-flag      .   1)
    (link-name      . 100)
    (magic          .   6)
    (version        .   2)
    (uname          .  32)
    (gname          .  32)
    (dev-major      .   8)
    (dev-minor      .   8)
    (prefix         . 155)))

(define (ustar-footer? bv)
  (every zero? (array->list bv)))

(define (sub-bytevector bv offset size)
  (let ((sub (make-bytevector size)))
    (bytevector-copy! bv offset sub 0 size)
    sub))

(define (read-ustar-header port)
  (define offset
    (let ((offset 0))
      (lambda (. args)
        (if (null? args) offset
            (let ((n (car args)))
              (set! offset (+ offset n))
              n)))))
  (let ((%record (read-ustar-record port)))
    (and (not (eof-object? %record))
         (not (ustar-footer? %record))
         (let* ((field-bv-alist
                 `((dummy-checksum . ,(string->utf8 "        "))
                   ,@(map
                      (match-lambda ((field . size)
                                     (cons field (sub-bytevector %record (offset) (offset size)))))
                      ustar-header-field-size-alist)))
                (checksum-fields '(name mode uid gid size mtime
                                        dummy-checksum
                                        type-flag link-name magic version
                                        uname gname dev-major dev-minor
                                        prefix))
                (checksum (apply checksum (map (cut assoc-ref field-bv-alist <>)
                                               checksum-fields)))
                (header
                 (make-ustar-header
                  (bv->ustar-string (assoc-ref field-bv-alist 'name     ) "file name"        )
                  (bv->ustar-number (assoc-ref field-bv-alist 'mode     ) "file mode"        )
                  (bv->ustar-number (assoc-ref field-bv-alist 'uid      ) "user id"          )
                  (bv->ustar-number (assoc-ref field-bv-alist 'gid      ) "group id"         )
                  (bv->ustar-number (assoc-ref field-bv-alist 'size     ) "file size"        )
                  (bv->ustar-number (assoc-ref field-bv-alist 'mtime    ) "modification time")
                  (bv->ustar-number (assoc-ref field-bv-alist 'checksum ) "checksum"         )
                  ;; (bv->ustar-string (assoc-ref field-bv-alist 'space    ) "space"            )
                  (bv->ustar-string (assoc-ref field-bv-alist 'type-flag) "type flag"        )
                  (bv->ustar-string (assoc-ref field-bv-alist 'link-name) "link name"        )
                  (bv->ustar-string (assoc-ref field-bv-alist 'magic    ) "magic field"      )
                  (bv->ustar-string (assoc-ref field-bv-alist 'version  ) "version number"   )
                  (bv->ustar-string (assoc-ref field-bv-alist 'uname    ) "user name"        )
                  (bv->ustar-string (assoc-ref field-bv-alist 'gname    ) "group name"       )
                  (bv->ustar-number (assoc-ref field-bv-alist 'dev-major) "dev major"        )
                  (bv->ustar-number (assoc-ref field-bv-alist 'dev-minor) "dev minor"        )
                  (bv->ustar-string (assoc-ref field-bv-alist 'prefix   ) "directory name"   ))))
           (when (not (= (ustar-header-checksum header) checksum))
             (error "checksum mismatch, expected: ~s, got: ~s\n"
                    (ustar-header-checksum header)
                    checksum))
           header))))

(define* (write-ustar-header port path st #:key group mtime numeric-owner? owner)
  (let* ((type  (stat:type st))
         (perms  (stat:perms st))
         (mtime  (or mtime (stat:mtime st)))
         (uid    (or owner (stat:uid st)))
         (gid    (or group (stat:gid st)))
         (uname  (or (false-if-exception (passwd:name (getpwuid uid)))
                     ""))
         (gname  (or (false-if-exception (group:name (getgrgid gid)))
                     ""))

         (size  (case type
                  ((regular) (stat:size st))
                  (else 0)))

         (type-flag (case type
                      ((regular)       "0")
                      ((symlink)       "2")
                      ((char-special)  "3")
                      ((block-special) "4")
                      ((directory)     "5")
                      ((fifo)          "6")
                      (else (fmt-error "~a: unsupported file type ~a"
                                       path type))))

         (link-name (case type
                      ((symlink) (readlink path))
                      (else "")))

         (dev-major (case type
                      ((char-special block-special)
                       (quotient (stat:rdev st) 256))
                      (else 0)))
         (dev-minor (case type
                      ((char-special block-special)
                       (remainder (stat:rdev st) 256))
                      (else 0)))

         ;; Convert file name separators to slashes.
         (slash-path (string-map (lambda (c)
                                   (if (file-name-separator? c) #\/ c))
                                 path))

         ;; Make the path name relative.
         ;; TODO: handle drive letters on windows.
         (relative-path (if (string-every #\/ slash-path)
                            "."
                            (string-trim slash-path #\/)))

         ;; If it's a directory, add a trailing slash,
         ;; otherwise remove trailing slashes.
         (full-path (case type
                      ((directory) (string-append relative-path "/"))
                      (else (string-trim-right relative-path #\/)))))

    (receive (prefix name) (ustar-path-name-split full-path path)

      (let* ((%name      (ustar-string  100 name      "file name"))
             (%mode      (ustar-number    8 perms     "file mode"))
             (%uid       (ustar-number    8 uid       "user id"))
             (%gid       (ustar-number    8 gid       "group id"))
             (%size      (ustar-number   12 size      "file size"))
             (%mtime     (ustar-number   12 mtime     "modification time"))
             (%type-flag (ustar-string    1 type-flag "type flag"))
             (%link-name (ustar-string  100 link-name "link name"))
             (%magic     (ustar-0string   6 "ustar"   "magic field"))
             (%version   (ustar-string    2 "00"      "version number"))
             (%uname     (ustar-0string  32 uname     "user name"))
             (%gname     (ustar-0string  32 gname     "group name"))
             (%dev-major (ustar-number    8 dev-major "dev major"))
             (%dev-minor (ustar-number    8 dev-minor "dev minor"))
             (%prefix    (ustar-string  155 prefix    "directory name"))

             (%dummy-checksum (string->utf8 "        "))

             (%checksum
              (bytevector-append
               (ustar-number
                7
                (checksum %name %mode %uid %gid %size %mtime
                          %dummy-checksum
                          %type-flag %link-name %magic %version
                          %uname %gname %dev-major %dev-minor
                          %prefix)
                "checksum")
               (string->utf8 " "))))

        (write-ustar-record port
                            (bytevector-append
                             %name %mode %uid %gid %size %mtime
                             %checksum
                             %type-flag %link-name %magic %version
                             %uname %gname %dev-major %dev-minor
                             %prefix))))))

(define* (write-ustar-file port file-name #:key group mtime numeric-owner? owner sort-order verbosity)
  (let* ((file-name (if (string-every file-name-separator? file-name)
                        file-name-separator-string
                        (string-trim-right file-name file-name-separator?)))
         (st  (lstat file-name))
         (type (stat:type st))
         (size (stat:size st)))
    (unless (zero? verbosity)
      (if (> verbosity 1) (display-file file-name st)
          (display file-name))
      (newline))
    (write-ustar-header port file-name st #:group group #:mtime mtime #:numeric-owner? numeric-owner? #:owner owner)
    (case type
      ((regular)
       (call-with-port* (open-file file-name "rb")
         (lambda (in)
           (let ((buf (make-bytevector 512)))
             (let loop ((left size))
               (when (positive? left)
                 (let* ((asked (min left 512))
                        (obtained (get-bytevector-n! in buf 0 asked)))
                   (when (or (eof-object? obtained)
                             (< obtained asked))
                     (fmt-error "~a: file appears to have shrunk" file-name))
                   (write-ustar-record port buf 0 obtained)
                   (loop (- left obtained)))))))))
      ((directory)
       (let* ((files (files-in-directory file-name))
              (files (if (eq? sort-order 'name) (sort files string<)
                         files)))
         (for-each (lambda (file-name) (write-ustar-file port file-name
                                                         #:group group #:mtime mtime #:numeric-owner? numeric-owner? #:owner owner #:verbosity verbosity))
                   files))))))

(define (ustar-header-file-name header)
  (let ((name (ustar-header-name header))
        (prefix (ustar-header-prefix header)))
    (if (string-null? prefix) name
        (string-append prefix "/" name))))

(define* (read-ustar-file port header #:key (extract? #t) (strip 0))
  (let* ((size (ustar-header-size header))
         (file-name (ustar-header-file-name header))
         (file-name (if (zero? strip) file-name
                        (string-join (list-tail (string-split file-name #\/) strip) "/")))
         (dir (dirname file-name))
         (extract? (and extract? (not (string-null? file-name))))
         (thunk (lambda _
                  (let loop ((read 0))
                    (and (< read size)
                         (let ((record (read-ustar-record port)))
                           (and record
                                (let* ((read (+ read 512))
                                       (block (if (< read size) record
                                                  (sub-bytevector record 0 (- size -512 read)))))
                                  (when extract?
                                    (display (bv->ustar-0string block "block")))
                                  (loop read)))))))))
    (when extract?
      (mkdir-p dir))
    (if extract?
        (case (ustar-header-type header)
          ((regular)
           (if (file-exists? file-name) (delete-file file-name))
           (with-output-to-file file-name thunk #:binary #t))
          ((directory) (mkdir-p file-name))
          ((symlink) (symlink (ustar-header-link-name header) file-name )))
        (thunk))
    (when (and extract?
               (not (eq? (ustar-header-type header) 'symlink)))
      (chmod file-name (ustar-header-mode header))
      (let ((mtime (ustar-header-mtime header)))
        (utime file-name mtime mtime)))))

(define (ustar-header->stat header)
  (let* ((stat-size 17)
         (si (list->vector (iota stat-size)))
         (st (make-vector stat-size 0)))
    (vector-set! st (stat:mode si) (ustar-header-mode header))
    (vector-set! st (stat:uid si) (ustar-header-uid header))
    (vector-set! st (stat:gid si) (ustar-header-gid header))
    (vector-set! st (stat:size si) (ustar-header-size header))
    (vector-set! st (stat:mtime si) (ustar-header-mtime header))
    (vector-set! st (stat:type si) (ustar-header-type header))
    st))

(define* (display-header header #:key verbose?)
  (let ((file-name (ustar-header-file-name header)))
    (if verbose? (display-file (ustar-header-file-name header) (ustar-header->stat header))
        (display file-name))
    (newline)))

(define* (write-ustar-port out files #:key group mtime numeric-owner? owner sort-order verbosity)
  (for-each
   (cut write-ustar-file out <>
        #:group group #:mtime mtime #:numeric-owner? numeric-owner? #:owner owner #:sort-order sort-order #:verbosity verbosity)
   files))

(define* (write-ustar-archive file-name files #:key group mtime numeric-owner? owner sort-order verbosity)
  (catch #t
    (lambda _
      (call-with-port* (open-file file-name "wb")
        (cut write-ustar-port <> files
             #:group group #:mtime mtime #:numeric-owner? numeric-owner? #:owner owner #:sort-order sort-order #:verbosity verbosity)))
    (lambda (key subr message args . rest)
      (false-if-exception (delete-file file-name))
      (format (current-error-port) "ERROR: ~a\n"
              (apply format #f message args))
      (exit 1))))

(define* (read-ustar-port in files #:key (extract? #t) (strip 0) verbosity)
  (let loop ((header (read-ustar-header in)))
    (when (and header
               (not (eof-object? header)))
      (unless (zero? verbosity)
        (display-header header #:verbose? (> verbosity 1)))
      (read-ustar-file in header #:extract? extract? #:strip strip)
      (loop (read-ustar-header in)))))

(define* (read-ustar-archive file-name files #:key (extract? #t) (strip 0) verbosity)
  (catch #t
    (lambda _
      (call-with-port* (open-file file-name "rb")
        (cut read-ustar-port <> files #:extract? extract? #:strip strip #:verbosity verbosity)))
    (lambda (key subr message args . rest)
      (format (current-error-port) "ERROR: ~a\n"
              (apply format #f message args))
      (exit 1))))

(define* (list-ustar-archive file-name files #:key (strip 0) verbosity)
  (read-ustar-archive file-name files #:extract? #f #:strip strip #:verbosity verbosity))

(define* (list-ustar-port in files #:key (strip 0) verbosity)
  (read-ustar-port in files #:extract? #f #:strip strip #:verbosity verbosity))

;;; Local Variables:
;;; mode: scheme
;;; eval: (put 'call-with-port* 'scheme-indent-function 1)
;;; eval: (put 'call-with-dirstream* 'scheme-indent-function 1)
;;; End:
