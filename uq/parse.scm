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
  #:export (define-tag attribute attribute-values attribute-list markup enable-markup-reader!
            read-markup-document markup-string->exprs))

;;; ================= Component registry =================
;; name -> (kind attributes wrap), attributes being #f, optional or required
;; Kinds are  placement:content.
;;   block:block   (alias block)   starts a line; holds blank-line-separated groups,
;;                                 each wrapped in the tag's #:wrap (default p)
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
(define (wrap-of name)  (or (and=> (entry name) caddr) default-wrap))

(define (register! name kind attributes wrap default?)
  (let ((k (canonical-kind kind)))
    (unless (memq k (append block-kinds inline-kinds))
      (error "define-tag: unknown kind" kind))
    (when (and wrap (not (eq? k 'block:block)))
      (error "define-tag: only block:block tags take #:wrap" name))
    (when (and default? (not (memq k block-kinds)))
      (error "define-tag: only block tags can be the #:default-wrap" name))
    (hash-set! registry name (list k attributes wrap))
    (when default? (set! default-wrap name))))

;; (define-tag kind (name . body) [option …] expr …)
;;
;; Defines the component procedure, registers it with the parser, and
;; exports it from the module it appears in.  Options, in any order:
;;   #:wrap tag                 block:block only: wrap each blank-line group in TAG
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
;;     (let ((href (attribute attrs 'href 0)))
;;       `(a (@ (href ,href) ,@(attribute-list attrs '(href))) ,@body)))
(define-syntax define-tag
  (syntax-rules ()
    ((_ kind (name . body) . rest)
     (define-tag/options kind name body #f #f #f #f rest))))

;; Collects the options, then defines.  W: wrap tag or #f; A: the attribute
;; variable or #f; M: optional, required or #f; D: default wrapper?
(define-syntax define-tag/options
  (syntax-rules ()
    ((_ k n b w #f m d (#:attributes a . rest))
     (define-tag/options k n b w a optional d rest))
    ((_ k n b w #f m d (#:required-attributes a . rest))
     (define-tag/options k n b w a required d rest))
    ((_ k n b w a m d (#:attributes . rest))
     (syntax-error "define-tag: only one of #:attributes and #:required-attributes" n))
    ((_ k n b w a m d (#:required-attributes . rest))
     (syntax-error "define-tag: only one of #:attributes and #:required-attributes" n))
    ((_ k n b #f a m d (#:wrap w . rest))
     (define-tag/options k n b w a m d rest))
    ((_ k n b w a m d (#:wrap . rest))
     (syntax-error "define-tag: #:wrap given twice" n))
    ((_ k n b w a m #f (#:default-wrap d . rest))
     (define-tag/options k n b w a m d rest))
    ((_ k n b w a m d (#:default-wrap . rest))
     (syntax-error "define-tag: #:default-wrap given twice" n))
    ((_ k n b #f #f #f d (expr ...))
     (begin (define (n . b) expr ...) (export n) (register! 'n 'k #f #f d)))
    ((_ k n b w #f #f d (expr ...))
     (begin (define (n . b) expr ...) (export n) (register! 'n 'k #f 'w d)))
    ((_ k n b #f a m d (expr ...))
     (begin (define (n a . b) expr ...) (export n) (register! 'n 'k 'm #f d)))
    ((_ k n b w a m d (expr ...))
     (begin (define (n a . b) expr ...) (export n) (register! 'n 'k 'm 'w d)))))

(define (oops line fmt . args)
  (error (string-append "line " (number->string line) ": " (apply format #f fmt args))))

;;; ================= Lines -> indentation tree =================
(define-record-type <node>
  (make-node line indent text kids) node?
  (line node-line) (indent node-indent) (text node-text) (kids node-kids set-node-kids!))

(define (expand-tabs s) (regexp-substitute/global #f "\t" s 'pre "    " 'post))
(define (leading-spaces s) (- (string-length s) (string-length (string-trim s))))
(define (blank? n) (string-null? (node-text n)))

(define (port->tree port)
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
    (let loop ((ls raw) (n 1) (is indents) (stack (list root)))
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
(define (call name attrs parts) `(,name ,@(if attrs `(',attrs) '()) ,@parts))

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
              (loop end end (cons datum (text-upto i)))))
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
                  (loop after after (cons (call name addr (parse-inline inner here)) (text-upto i))))))
             ((attributes-required? name) (missing-attributes name here))
             (else                       ; declared extent: next word / rest of line
              (let* ((after (if (and (< ne (string-length s)) (char=? (string-ref s ne) #\space)) (1+ ne) ne))
                     (stop (if (eq? kind 'inline:word) char-whitespace? (lambda (c) (char=? c #\newline))))
                     (we (or (string-index s stop after) (string-length s))))
                (loop we we (cons (call name (and (attributes? name) '())   ; optional: none given
                                        (parse-inline (substring s after we) here))
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
(define (convert-children kids wrap)
  (if (not wrap)
      (unwrapped kids)
      (case (kind-of wrap)
        ((block:inline) (inline-groups kids wrap))
        ((block:block)  (block-groups kids wrap))
        (else (error (format #f "#:wrap ~a must be a block:block or block:inline tag" wrap))))))

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
                    (cons (call wrap #f (run-parts run)) out)))))
    (match kids
      (() (reverse (flush)))
      ((k . rest)
       (cond ((blank? k)      (loop rest '() (flush)))
             ((block-head k)  (loop rest '() (cons (convert-block k) (flush))))
             (else            (loop rest (cons k run) out)))))))

;; wrapper holds blocks too (li): everything up to a blank line is one group
(define (block-groups kids wrap)
  (define (group->call group)
    (match group
      (((? block-head k)) (=> skip)              ; a lone ,li needs no second li
       (if (eq? (car (block-head k)) wrap) (convert-block k) (skip)))
      (_ (let loop ((nodes group) (run '()) (parts '()))
           (define (flush) (if (null? run) parts (append parts (run-parts (reverse run)))))
           (match nodes
             (() (call wrap #f (flush)))
             ((k . rest)
              (if (block-head k)
                  (loop rest '() (append (flush) (list (convert-block k))))
                  (let ((nested (text-blocks k)))
                    (if (null? nested)
                        (loop rest (cons k run) parts)
                        (loop rest '() (append parts (run-parts (reverse (cons k run)))
                                               (map convert-block nested))))))))))))
  (let loop ((kids kids) (group '()) (out '()))
    (define (flush) (if (null? group) out (cons (group->call (reverse group)) out)))
    (match kids
      (() (reverse (flush)))
      ((k . rest) (if (blank? k) (loop rest '() (flush)) (loop rest (cons k group) out))))))

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
            (call name addr (parse-inline text line)))))
       ((block:block)
        ;; text on the command's own line is simply the first line of its body
        (let ((kids (if (string-null? head) (node-kids n)
                        (cons (make-node (node-line n) (node-indent n) head '()) (node-kids n)))))
          (call name addr (convert-children kids (wrap-of name)))))))))

;; A document has no root element: its top-level blocks are siblings.
(define (tree->expr root) `(list ,@(convert-children (node-kids root) default-wrap)))

;;; ================= Entry points =================
;; whole document -> one expression, (list …): a list of sibling nodes
(define (read-markup-document port) (tree->expr (port->tree port)))
;; fragment -> list of expressions, one per top-level node
(define (markup-string->exprs str)
  (convert-children (node-kids (call-with-input-string str port->tree)) default-wrap))

;; (markup "…")  expands at compile time into ordinary Scheme:
;;   (list (p "text " (em "x")) (ul (li …)) …)
;; datum->syntax with the use site's context, so components and any
;; ,(expr) escapes resolve in the module/scope where markup is written.
(define-syntax markup
  (lambda (x)
    (syntax-case x ()
      ((k str) (string? (syntax->datum #'str))
       (datum->syntax #'k `(list ,@(markup-string->exprs (syntax->datum #'str))))))))

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
      (let ((term (string-trim-both (read-line port))))
        (let loop ((acc '()))
          (let ((l (read-line port)))
            (cond ((eof-object? l) (error "#m<<" term ": terminator never found"))
                  ((string=? (string-trim-both l) term)
                   `(markup ,(string-join (reverse acc) "\n")))
                  (else (loop (cons l acc))))))))))

