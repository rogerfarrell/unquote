;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright (C) 2026 Roger Farrell
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (uq parse)
  #:use-module (ice-9 match) #:use-module (ice-9 rdelim) #:use-module (ice-9 textual-ports) #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1) #:use-module (srfi srfi-9)
  #:export (define-tag attribute attribute-values attribute-list main-attribute current-command markup enable-markup-reader!
            read-markup-document markup-string->exprs
            at-line strip-line-wrappers uq-error-message))

;;; ================= Component registry =================
;; name -> (kind attributes wrap), attributes being #f, optional or required
;; Kinds are  placement:content.
;;   block:block   (alias block)   starts a line; holds blank-line-separated groups,
;;                                 each wrapped per the tag's #:wrap, else the
;;                                 declared #:default-wrap
;;   block:inline                  starts a line; holds one run of inline content
;;   inline:inline (alias inline)  mid-line; without [brackets] runs to end of line
;;   inline:word   (alias word)    mid-line; without [brackets] takes the next word
;;
;; #:wrap decides how a group is wrapped, and the wrapper's own kind decides
;; what a group may hold:
;;   wrapper is block:inline (p)   group = a run of text; a block line ends it
;;   wrapper is block:block  (li)  group = everything up to the next blank line,
;;                                 text and blocks together
(define block-kinds  '(block:block block:inline))
(define inline-kinds '(inline:inline inline:word))
(define (canonical-kind k)
  (case k ((block) 'block:block) ((inline) 'inline:inline) ((word) 'inline:word) (else k)))
(define registry (make-hash-table))
(define (entry name)    (hash-ref registry name))
(define (kind-of name)  (and=> (entry name) car))
(define (attributes? name) (and=> (entry name) cadr))   ; optional or required
(define (attributes-required? name) (eq? (attributes? name) 'required))
;; The wrapper for loose text where a block:block has no #:wrap of its own,
;; and at the top level of a document.  Declared by a tag with
;; #:default-wrap #t -- (uq tags) declares p -- the latest declaration
;; winning.  The parser names no component itself.
(define default-wrap #f)

;; How a block wraps its loose text, as convert-children takes it:
;;   (tag other …)  wrap each group in TAG; a group that is a lone OTHER
;;                  (or TAG) passes through unwrapped
;;   inline         no wrapper: the text stays inline in the block
;;   #f             no wrapper declared anywhere: loose text is an error
(define (default-wraps) (and default-wrap (list default-wrap)))
(define (wrap-of name)
  (match (and=> (entry name) caddr)
    ((or #f 'unset) (default-wraps))
    (w w)))

;; WRAP comes from the macro: 0 when #:wrap was not given, #f for #:wrap #f,
;; a tag, or a list of tags.
(define (register! name kind attributes wrap default?)
  (let ((k (canonical-kind kind))
        (w (match wrap
             (0 'unset)
             (#f 'inline)
             ((? symbol? tag) (list tag))
             (((? symbol?) ..1) wrap)
             (_ (error "define-tag: #:wrap takes a tag, a list of tags, or #f" name)))))
    (unless (memq k (append block-kinds inline-kinds))
      (error "define-tag: unknown kind" kind))
    (when (and (not (eq? w 'unset)) (not (eq? k 'block:block)))
      (error "define-tag: only block:block tags take #:wrap" name))
    (when (and default? (not (memq k block-kinds)))
      (error "define-tag: only block tags can be the #:default-wrap" name))
    (hash-set! registry name (list k attributes w))
    (when default? (set! default-wrap name))))

;; (define-tag kind (name . body) [option …] expr …)
;;
;; Defines the component procedure, registers it with the parser, and
;; exports it from the module it appears in.  Options, in any order:
;;   #:wrap tag                 block:block only: wrap each blank-line group in TAG
;;   #:wrap (tag other …)       the same, but a group that is a lone ,other
;;                              passes through unwrapped (a ,th in a row)
;;   #:wrap #f                  no wrapper: loose text stays inline (a cell)
;;   #:default-wrap #t          this block tag wraps loose text wherever no
;;                              #:wrap says otherwise, including the top
;;                              level of a document; (uq tags) declares p
;;   #:attributes var           the command may take [attributes][text]; the
;;                              brackets can be left out, giving no attributes
;;   #:required-attributes var  the same, but the attribute bracket must be
;;                              there and not empty; the parser says so,
;;                              with a line number
;; With either attributes option, VAR is bound to the attribute list (see
;; parse-attributes) and BODY is the text -- or, for a block, its contents.
;; For example:
;;   (define-tag inline (link . body) #:required-attributes attrs
;;     (let ((href (main-attribute attrs 'href)))
;;       `(a (@ (href ,href) ,@(attribute-list attrs '(href))) ,@body)))
(define-syntax define-tag
  (lambda (x)
    (define (fail message form . args)
      (syntax-violation 'define-tag (apply format #f message args) x form))
    (syntax-case x ()
      ((_ kind (name . body) . rest)
       ;; Walk the options; the rest is the body.  WRAP stays 0 until
       ;; #:wrap is seen (#f is a valid value: no wrapper).
       (let loop ((rest #'rest) (wrap 0) (attrs #f) (mode #f) (default #f))
         (syntax-case rest ()
           ((kw . more) (keyword? (syntax->datum #'kw))
            (let ((key (syntax->datum #'kw)))
              (syntax-case #'more ()
                ((value . more)
                 (case key
                   ((#:wrap)
                    (unless (eqv? wrap 0) (fail "~a given twice" #'kw key))
                    (loop #'more (syntax->datum #'value) attrs mode default))
                   ((#:attributes #:required-attributes)
                    (when attrs (fail "only one of #:attributes and #:required-attributes" #'kw))
                    (unless (identifier? #'value) (fail "~a takes a variable name" #'value key))
                    (loop #'more wrap #'value (if (eq? key #:attributes) 'optional 'required) default))
                   ((#:default-wrap)
                    (when default (fail "~a given twice" #'kw key))
                    (loop #'more wrap attrs mode (syntax->datum #'value)))
                   (else (fail "unknown option ~a" #'kw key))))
                (() (fail "~a needs a value" #'kw key)))))
           (() (fail "~a has no body" #'name (syntax->datum #'name)))
           ((expr ...)
            #`(begin
                (define (name #,@(if attrs (list attrs) '()) . body) expr ...)
                (export name)
                (register! 'name 'kind '#,(datum->syntax x mode) '#,(datum->syntax x wrap)
                           #,(datum->syntax x default))))))))))

;;; ================= Errors =================
;; Every error that knows its line is raised under the key uq-error, as
;; (uq-error #f "~a" ("line N: message") #f) -- the shape of misc-error, so
;; anything that formats Guile errors formats these too.
;; The file being parsed, when the message should name it (markup embedded
;; in a Scheme file).  compile-uq names the file itself, so leaves it #f.
(define current-uq-file (make-parameter #f))

;; LINE is a line number, or (FILE . LINE) baked in by wrap-line.
(define (location line)
  (let ((file (if (pair? line) (car line) (current-uq-file)))
        (n    (if (pair? line) (cdr line) line)))
    (if file
        (format #f "~a: line ~a: " file n)
        (format #f "line ~a: " n))))

(define (located line message)
  (scm-error 'uq-error #f "~a" (list (string-append (location line) message)) #f))

(define (oops line fmt . args) (located line (apply format #f fmt args)))

(define (uq-error-message key args)
  "A one-line message for an exception: Guile's own, or uq's."
  (define (fallback) (format #f "~a: ~s" key args))
  (match (cons key args)
    ;; syntax errors: (who message source-properties form subform)
    (('syntax-error _ (? string? msg) _ form . _)
     (format #f "~a ~s" (string-trim-right msg #\,) form))
    ;; uq-error, error, misc-error and friends: (who format-string format-args . _)
    ((_ _ (? string? fmt) (? list? fargs) . _)
     ;; Guile's error messages are simple-format strings (~a, ~s); unlike
     ;; format, simple-format raises on a mismatch without printing anything
     (catch #t (lambda () (apply simple-format #f fmt fargs)) (lambda _ (fallback))))
    (_ (fallback))))

;; The parser wraps every component call and ,(expr) escape in
;;   ((@ (uq parse) at-line) LINE NAME (lambda () EXPR))
;; so an error raised while evaluating it -- a component's own check, or a
;; failing escape -- is reported with the line it came from.  An error that
;; already has a line passes through, so the innermost line wins.
;; The command whose component is running, as the editor wrote it: set by
;; at-line around each command, so a component -- and helpers such as
;; main-attribute -- can name it in messages.  A component calling another
;; directly does not change it: errors name the command the editor typed.
(define current-command (make-parameter #f))

;; NAME is the command's name, or #f for a ,(expr) escape (which keeps the
;; enclosing command's).
(define (at-line line name thunk)
  (catch #t
    (if name (lambda () (parameterize ((current-command name)) (thunk))) thunk)
    (lambda (key . args)
      (if (memq key '(uq-error quit))
          (apply throw key args)
          (located line (uq-error-message key args))))))

(define* (wrap-line line expr #:optional name)
  (let ((loc (if (current-uq-file) (cons (current-uq-file) line) line)))
    `((@ (uq parse) at-line) ',loc ',name (lambda () ,expr))))

(define (strip-line-wrappers x)
  "X without the at-line wrappers, for showing the expression to people."
  (match x
    ((('@ ('uq 'parse) 'at-line) _ _ ('lambda () expr)) (strip-line-wrappers expr))
    ((? pair?) (cons (strip-line-wrappers (car x)) (strip-line-wrappers (cdr x))))
    (_ x)))

;; Show uq errors as their message, not "Throw to key `uq-error' …".
(set-exception-printer! 'uq-error
  (lambda (port key args default-printer) (display (uq-error-message key args) port)))

;;; ================= Lines -> indentation tree =================
(define-record-type <node>
  (make-node line indent text kids) node?
  (line node-line) (indent node-indent) (text node-text) (kids node-kids set-node-kids!))

(define (expand-tabs s) (regexp-substitute/global #f "\t" s 'pre "    " 'post))
(define (leading-spaces s) (- (string-length s) (string-length (string-trim s))))
(define (blank? n) (string-null? (node-text n)))

(define* (port->tree port #:optional (first-line 1))   ; FIRST-LINE: number of the port's first line
  (let* ((raw (let loop ((acc '()))
                (let ((l (read-line port)))
                  (if (eof-object? l) (reverse acc) (loop (cons (expand-tabs l) acc))))))
         ;; a blank line takes the indent of the next non-blank line,
         ;; so it lands among the siblings it separates
         (indents (let loop ((ls (reverse raw)) (next 0) (acc '()))
                    (match ls
                      (() acc)
                      ((l . rest)
                       (let ((i (if (string-null? (string-trim l)) next (leading-spaces l))))
                         (loop rest i (cons i acc)))))))
         (root (make-node 0 -1 "" '())))
    (let loop ((ls raw) (n first-line) (is indents) (stack (list root)))
      (match ls
        (() root)
        ((l . rest)
         (let* ((ind (car is))
                (node (make-node n ind (string-trim-both l) '()))
                (stack (drop-while (lambda (s) (>= (node-indent s) ind)) stack))
                (parent (car stack)))
           (set-node-kids! parent (append (node-kids parent) (list node)))
           (loop rest (1+ n) (cdr is) (if (blank? node) stack (cons node stack)))))))))

;;; ================= Reading Scheme from a string =================
;; Read one datum from S starting at index I.  Returns (values datum end),
;; END being the character index just after it.  (ftell on a string port
;; counts bytes, so the end is found from the characters left unread.)
(define (read-datum-at s i)
  (let* ((p (open-input-string (substring s i)))
         (datum (read p))
         (left (string-length (get-string-all p))))
    (values datum (- (string-length s) left))))

;;; ================= Attributes =================
;; The first bracket of an attributes command, e.g. [cheese.jpg :width 3in],
;; is split on whitespace.  ":key value" becomes (key "value"), ready to drop
;; into an SXML (@ …) list; any other word stays a string, in order, for the
;; component to interpret.  So [cheese.jpg :width 3in] -> ("cheese.jpg" (width "3in")).
;;
;; A word may be a "double-quoted" Scheme string, read with Guile's reader
;; (\" and \\ escapes), to hold spaces: [:title "Jar sizes"].  A quoted
;; string is never a :key, and it must close on the line it opens on.

;; STR -> list of tokens: plain words as strings, quoted strings as (q . str)
(define (attribute-tokens str name line)
  (let ((len (string-length str)))
    (let loop ((i 0) (acc '()))
      (let ((i (or (string-skip str char-whitespace? i) len)))
        (cond
         ((>= i len) (reverse acc))
         ((char=? (string-ref str i) #\")
          (call-with-values
              (lambda ()
                (catch #t
                  (lambda () (read-datum-at str i))
                  (lambda _ (oops (line-at str i line) ",~a: a \" is not closed on the same line" name))))
            (lambda (value end)
              (loop end (cons (cons 'q value) acc)))))
         (else
          (let ((end (or (string-index str char-whitespace? i) len)))
            (loop end (cons (substring str i end) acc)))))))))

(define (parse-attributes str name line)
  (define (key? t) (and (string? t) (> (string-length t) 1) (char=? (string-ref t 0) #\:)))
  (define (value t) (if (pair? t) (cdr t) t))
  (let loop ((tokens (attribute-tokens str name line)) (acc '()))
    (match tokens
      (() (reverse acc))
      (((? key? key) . rest)
       (match rest
         ((v . rest) (if (key? v)
                         (oops line ",~a: ~a needs a value" name key)
                         (loop rest (cons (list (string->symbol (substring key 1)) (value v)) acc))))
         (() (oops line ",~a: ~a needs a value" name key))))
      ((t . rest) (loop rest (cons (value t) acc))))))

;; Helpers for components.
(define* (attribute attrs key #:optional index)
  "The value of :KEY in ATTRS; failing that, the INDEXth bare value; else #f."
  (or (any (lambda (a) (and (pair? a) (eq? (car a) key) (cadr a))) attrs)
      (and index
           (let ((bare (filter string? attrs)))
             (and (< index (length bare)) (list-ref bare index))))))

(define* (main-attribute attrs key #:optional (command (current-command)))
  "A command's main value: given as :KEY, or as the one plain word in ATTRS.
Missing, given both ways, or given as several plain words is an error
naming COMMAND -- by default the command being evaluated -- and raised
while evaluating, so it carries the line."
  (match (list (attribute attrs key) (attribute-values attrs))
    ((#f (word)) word)
    (((? string? value) ()) value)
    ((#f ()) (error (format #f ",~a needs its ~a: as the first word or as :~a" command key key)))
    ((#f _) (error (format #f ",~a takes one ~a; quote it if it has spaces" command key)))
    (_ (error (format #f ",~a: give the ~a once, as a word or as :~a" command key key)))))

(define* (attribute-values attrs)
  "The bare (keyword-less) values in ATTRS, in order."
  (filter string? attrs))

(define* (attribute-list attrs #:optional (exclude '()))
  "The :key value pairs in ATTRS as SXML attributes, minus the keys in EXCLUDE."
  (filter (lambda (a) (and (pair? a) (not (memq (car a) exclude)))) attrs))

;;; ================= Brackets =================
;; Index of the "]" matching the "[" at OPEN, or #f.  A ,(expr) escape
;; inside is skipped whole, so brackets in its strings do not count.  With QUOTES?, a
;; "double-quoted" string is skipped whole, so [:title "see [1]"] works.
;; A quote must close on its own line; if it does not, the result is
;; (quote INDEX-OF-THE-QUOTE).  An unreadable ,(expr) gives
;; (escape INDEX-OF-THE-COMMA).
(define* (matching-bracket s open #:optional quotes?)
  (let loop ((j (1+ open)) (depth 0))
    (cond ((>= j (string-length s)) #f)
          ((and quotes? (char=? (string-ref s j) #\"))
           (let skip ((k (1+ j)))
             (cond ((>= k (string-length s)) (list 'quote j))
                   ((char=? (string-ref s k) #\newline) (list 'quote j))
                   ((char=? (string-ref s k) #\\)       ; an escape, but not of a line break
                    (if (and (< (1+ k) (string-length s)) (char=? (string-ref s (1+ k)) #\newline))
                        (list 'quote j)
                        (skip (+ k 2))))
                   ((char=? (string-ref s k) #\") (loop (1+ k) depth))
                   (else (skip (1+ k))))))
          ((escape-at? s j)
           (match (catch #t (lambda () (call-with-values (lambda () (read-datum-at s (1+ j))) list))
                    (lambda _ #f))
             ((_ end) (loop end depth))
             (#f (list 'escape j))))
          ((char=? (string-ref s j) #\[) (loop (1+ j) (1+ depth)))
          ((char=? (string-ref s j) #\]) (if (zero? depth) j (loop (1+ j) (1- depth))))
          (else (loop (1+ j) depth)))))

(define (bad-escape line)
  (oops line ",( is not a complete Scheme expression (unbalanced parentheses?)"))

;; Does a ,(expr) escape start at index I?  (A comma is a command only at
;; the start of a word.)
(define (escape-at? s i)
  (and (< (1+ i) (string-length s))
       (char=? (string-ref s i) #\,)
       (char=? (string-ref s (1+ i)) #\()
       (or (zero? i) (memv (string-ref s (1- i)) '(#\space #\newline #\( #\[)))))

;; Read ",name[…]" / ",name[attributes][…]" starting at the "[" at OPEN.
;; Attributes commands read the attribute bracket first (as in HTML:
;; <a href=…>text</a>), then an optional text bracket.
;; Returns (values text attributes-or-#f index-after).
(define (read-brackets s open name line)
  (let ((close (matching-bracket s open (attributes? name))))
    (match close
      (('quote q) (oops (+ line (string-count (substring s open q) #\newline))   ; LINE is the bracket's
                        ",~a: a \" is not closed on the same line" name))
      (('escape e) (bad-escape (+ line (string-count (substring s open e) #\newline))))
      (#f (oops line ",~a[ is never closed with ]" name))
      (_ #t))
    (let ((first (substring s (1+ open) close))
          (next  (1+ close)))
      (cond
       ((not (attributes? name)) (values first #f next))    ; a following "[…]" is just text
       ((and (< next (string-length s)) (char=? (string-ref s next) #\[))
        (let ((close2 (matching-bracket s next)))
          (unless close2 (oops line "the text after ,~a[...] is never closed with ]" name))
          (values (substring s (1+ next) close2) (checked-attributes first name line) (1+ close2))))
       (else (values "" (checked-attributes first name line) next))))))   ; text is optional

(define (missing-attributes name line)
  (oops line ",~a needs attributes: ,~a[attributes][text]" name name))

(define (checked-attributes str name line)
  (let ((attrs (parse-attributes str name line)))
    (when (and (null? attrs) (attributes-required? name)) (missing-attributes name line))
    attrs))

;;; ================= Inline text -> parts =================
;; S is a whole paragraph or item: its lines joined with "\n".  LINE is the
;; file line S starts on.  Newlines become spaces in the output, but a
;; inline:inline command without brackets still stops at one.
(define (name-char? c) (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\- #\_ #\? #\!))))
(define (name-end s i) (let scan ((j i)) (if (and (< j (string-length s)) (name-char? (string-ref s j))) (scan (1+ j)) j)))
(define (nl->sp str) (string-map (lambda (c) (if (char=? c #\newline) #\space c)) str))
(define (line-at s i line) (+ line (string-count (substring s 0 i) #\newline)))
;; ATTRS is #f for commands without attributes; otherwise a quoted list.
(define (call name attrs parts line)
  (wrap-line line `(,name ,@(if attrs `(',attrs) '()) ,@parts) name))

(define (parse-inline s line)
  (let loop ((i 0) (start 0) (acc '()))
    (define (text-upto j) (if (> j start) (cons (nl->sp (substring s start j)) acc) acc))
    (cond
     ((>= i (string-length s)) (reverse (text-upto i)))
     ((and (char=? (string-ref s i) #\,)
           (or (zero? i) (memv (string-ref s (1- i)) '(#\space #\newline #\( #\[)))
           (< (1+ i) (string-length s))
           (let ((n (string-ref s (1+ i)))) (or (char-alphabetic? n) (char=? n #\())))
      (if (char=? (string-ref s (1+ i)) #\()
          ;; ,(expr) — read one Scheme datum and splice it in
          (call-with-values
              (lambda ()
                (catch #t
                  (lambda () (read-datum-at s (1+ i)))
                  (lambda _ (bad-escape (line-at s i line)))))
            (lambda (datum end)
              (loop end end (cons (wrap-line (line-at s i line) datum) (text-upto i)))))
          (let* ((ne (name-end s (1+ i)))
                 (name (string->symbol (substring s (1+ i) ne)))
                 (here (line-at s i line))
                 (kind (kind-of name)))
            (cond
             ((not kind) (oops here "unknown command ,~a" name))
             ((memq kind block-kinds) (oops here ",~a has to start its own line" name))
             ((and (< ne (string-length s)) (char=? (string-ref s ne) #\[))
              (call-with-values (lambda () (read-brackets s ne name here))
                (lambda (inner addr after)
                  (loop after after (cons (call name addr (parse-inline inner here) here) (text-upto i))))))
             ((attributes-required? name) (missing-attributes name here))
             (else                       ; declared extent: next word / rest of line
              (let* ((after (if (and (< ne (string-length s)) (char=? (string-ref s ne) #\space)) (1+ ne) ne))
                     (stop (if (eq? kind 'inline:word) char-whitespace? (lambda (c) (char=? c #\newline))))
                     (we (or (string-index s stop after) (string-length s))))
                (loop we we (cons (call name (and (attributes? name) '())   ; optional: none given
                                        (parse-inline (substring s after we) here) here)
                                  (text-upto i)))))))))
     (else (loop (1+ i) start acc)))))

;;; ================= Tree -> Scheme expression =================
;; Is this line a block command?  Returns (name attributes head-text) or #f.
(define (block-head n)
  (let ((s (node-text n)))
    (and (> (string-length s) 1)
         (char=? (string-ref s 0) #\,)
         (char-alphabetic? (string-ref s 1))
         (let* ((ne (name-end s 1))
                (name (string->symbol (substring s 1 ne))))
           (case (kind-of name)
             ((block:block block:inline)
              (cond
               ((and (< ne (string-length s)) (char=? (string-ref s ne) #\[))
                (call-with-values (lambda () (read-brackets s ne name (node-line n)))
                  (lambda (inner addr after)
                    (unless (string-null? (string-trim (substring s after)))
                      (oops (node-line n) "unexpected text after ,~a[...]" name))
                    (list name addr inner))))
               ;; no brackets: optional attributes are none, and the rest of
               ;; the line is the first line of the body, as usual
               ((attributes-required? name) (missing-attributes name (node-line n)))
               ((attributes? name) (list name '() (string-trim (substring s ne))))
               (else (list name #f (string-trim (substring s ne))))))
             ((#f) (oops (node-line n) "unknown command ,~a" name))
             (else #f))))))                     ; inline command at line start: text

;; raw text of a node plus its continuation lines, joined with "\n"
(define (text-string n)
  (string-join (cons (node-text n)
                     (filter-map (lambda (k) (and (not (blank? k)) (not (block-head k)) (text-string k)))
                                 (node-kids n)))
               "\n"))

;; block nodes indented under a text line (through its continuation lines)
(define (text-blocks n)
  (append-map (lambda (k) (cond ((blank? k) '()) ((block-head k) (list k)) (else (text-blocks k))))
              (node-kids n)))

(define (run-parts nodes)               ; text nodes -> one parsed inline run
  (parse-inline (string-join (map text-string nodes) "\n") (node-line (car nodes))))

;; KIDS of a block:block, grouped by blank lines and wrapped in WRAP.
;; WRAP is as wrap-of returns it.
(define (convert-children kids wrap)
  (match wrap
    (#f (unwrapped kids))
    ('inline (block-groups kids #f '()))
    ((primary . _)
     (case (kind-of primary)
       ((block:inline) (inline-groups kids primary))
       ((block:block)  (block-groups kids primary wrap))
       (else (error (format #f "#:wrap ~a must be a block:block or block:inline tag" primary)))))))

;; no wrapper at all: blocks are fine, loose text is an error
(define (unwrapped kids)
  (filter-map (lambda (k)
                (cond ((blank? k) #f)
                      ((block-head k) (convert-block k))
                      (else (oops (node-line k) "text needs a wrapper, and none is declared (#:wrap or #:default-wrap)"))))
              kids))

;; wrapper holds only text (p): a run of text lines per group; block lines
;; and blank lines end the run, and blocks stand on their own
(define (inline-groups kids wrap)
  (let loop ((kids kids) (run '()) (out '()))
    (define (flush)
      (if (null? run) out
          (let ((run (reverse run)))
            (append (reverse (map convert-block (append-map text-blocks run)))
                    (cons (call wrap #f (run-parts run) (node-line (car run))) out)))))
    (match kids
      (() (reverse (flush)))
      ((k . rest)
       (cond ((blank? k)      (loop rest '() (flush)))
             ((block-head k)  (loop rest '() (cons (convert-block k) (flush))))
             (else            (loop rest (cons k run) out)))))))

;; wrapper holds blocks too (li): everything up to a blank line is one group.
;; A group that is a lone block named in PASS (,li in a list, ,th in a row)
;; is not wrapped again.  With no WRAP, each group's parts are spliced in
;; place, groups separated by a space.  Returns a list of nodes.
(define (block-groups kids wrap pass)
  (define (group->nodes group)
    (match group
      (((? block-head k)) (=> skip)
       (if (memq (car (block-head k)) pass) (list (convert-block k)) (skip)))
      (_ (let loop ((nodes group) (run '()) (parts '()))
           (define (flush) (if (null? run) parts (append parts (run-parts (reverse run)))))
           (match nodes
             (() (if wrap (list (call wrap #f (flush) (node-line (car group)))) (flush)))
             ((k . rest)
              (if (block-head k)
                  (loop rest '() (append (flush) (list (convert-block k))))
                  (let ((nested (text-blocks k)))
                    (if (null? nested)
                        (loop rest (cons k run) parts)
                        (loop rest '() (append parts (run-parts (reverse (cons k run)))
                                               (map convert-block nested))))))))))))
  (let loop ((kids kids) (group '()) (out '()))          ; OUT: list of node lists, newest first
    (define (flush) (if (null? group) out (cons (group->nodes (reverse group)) out)))
    (match kids
      (() (let ((groups (reverse (flush))))
            (if wrap
                (concatenate groups)
                (join-inline-groups groups))))
      ((k . rest) (if (blank? k) (loop rest '() (flush)) (loop rest (cons k group) out))))))

;; Unwrapped groups run together, so text from neighbouring groups gets a
;; space between them -- but not where a block ends or starts.
(define (join-inline-groups groups)
  (define (inline? x)                     ; a string, inline call or escape
    (match x
      ((('@ ('uq 'parse) 'at-line) _ _ ('lambda () (name . _)))
       (not (and (symbol? name) (memq (kind-of name) block-kinds))))
      (_ #t)))
  (let loop ((groups (remove null? groups)) (out '()))
    (match groups
      (() out)
      ((g . rest)
       (loop rest (if (and (pair? out) (inline? (last out)) (inline? (car g)))
                      (append out '(" ") g)
                      (append out g)))))))

;; The head run: text on the command's own line plus the indented text
;; lines straight after it, up to the first blank line or block.
;; Returns (values text first-line remaining-kids).
(define (head-run n head)
  (let loop ((kids (node-kids n))
             (acc  (if (string-null? head) '() (list head)))
             (line (if (string-null? head) #f (node-line n))))
    (match kids
      (((? (lambda (k) (not (or (blank? k) (block-head k)))) k) . rest)
       (loop rest (cons (text-string k) acc) (or line (node-line k))))
      (_ (values (string-join (reverse acc) "\n") (or line (node-line n)) kids)))))

(define (convert-block n)
  (match (block-head n)
    ((name addr head)
     (case (kind-of name)
       ((block:inline)
        (call-with-values (lambda () (head-run n head))
          (lambda (text line rest)
            (let ((extra (find (lambda (k) (not (blank? k))) rest)))
              (when extra
                (oops (node-line extra) ",~a holds one run of text; no blank lines or blocks inside it" name)))
            (call name addr (parse-inline text line) (node-line n)))))
       ((block:block)
        ;; text on the command's own line is simply the first line of its body
        (let ((kids (if (string-null? head) (node-kids n)
                        (cons (make-node (node-line n) (node-indent n) head '()) (node-kids n)))))
          (call name addr (convert-children kids (wrap-of name)) (node-line n))))))))

;; A document has no root element: its top-level blocks are siblings.
(define (tree->expr root) `(list ,@(convert-children (node-kids root) (default-wraps))))

;;; ================= Entry points =================
;; whole document -> one expression, (list …): a list of sibling nodes
(define (read-markup-document port) (tree->expr (port->tree port)))
;; fragment -> list of expressions, one per top-level node
(define* (markup-string->exprs str #:optional (first-line 1) file)
  "STR's top-level nodes as expressions.  FIRST-LINE is the line STR starts
on in FILE, so errors name the file and its lines."
  (parameterize ((current-uq-file file))
    (convert-children (node-kids (call-with-input-string str (lambda (p) (port->tree p first-line))))
                      (default-wraps))))

;; (markup "…")  expands at compile time into ordinary Scheme:
;;   (list (p "text " (em "x")) (ul (li …)) …)
;; datum->syntax with the use site's context, so components and any
;; ,(expr) escapes resolve in the module/scope where markup is written.
;; Errors name the Scheme file and its lines: the string's own source line
;; when Guile records one, else the markup form's.  The #m reader passes
;; the line and file explicitly.
(define-syntax markup
  (lambda (x)
    (define (source stx key) (and=> (syntax-source stx) (lambda (src) (assq-ref src key))))
    (define (expand k str line file)
      (datum->syntax k `(list ,@(markup-string->exprs str line file))))
    (syntax-case x ()
      ((k str line file) (string? (syntax->datum #'str))
       (expand #'k (syntax->datum #'str) (syntax->datum #'line) (syntax->datum #'file)))
      ((k str) (string? (syntax->datum #'str))
       (expand #'k (syntax->datum #'str)
               (or (and=> (or (source #'str 'line) (source x 'line)) 1+) 1)
               (or (source #'str 'filename) (source x 'filename)))))))

;; Optional reader syntax, so no string quotes are needed:
;;   #m<<END
;;   …markup…
;;   END
;; reads as (markup "…").  Enable with (enable-markup-reader!).
(define (enable-markup-reader!)
  (read-hash-extend #\m
    (lambda (chr port)
      (unless (and (eqv? (read-char port) #\<) (eqv? (read-char port) #\<))
        (error "#m must be followed by <<TERMINATOR"))
      (let* ((term  (string-trim-both (read-line port)))
             (first (1+ (port-line port))))         ; 1-based line of the markup's first line
        (let loop ((acc '()))
          (let ((l (read-line port)))
            (cond ((eof-object? l) (error "#m<<" term ": terminator never found"))
                  ((string=? (string-trim-both l) term)
                   `(markup ,(string-join (reverse acc) "\n") ,first ,(port-filename port)))
                  (else (loop (cons l acc))))))))))

