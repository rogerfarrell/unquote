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

;;; (uq compile post) -- post-processing: procedures from a finished
;;; document to a finished document, run by compile-uq's #:post in order,
;;; after evaluation and before #:page.  A document is a list of SXML
;;; nodes.  Passes are ordinary procedures; nothing here is registered, and
;;; nothing is left in the document to instruct them.
;;;
;;;   (compile-uq "book.uq" #:post (list (toc-pass '(h1 h2))))

(define-module (uq compile post)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)               ; let-values
  #:export (element? element-tag element-attributes element-children
            element-attribute set-element-attribute has-class?
            element-text map-elements find-elements slugify
            css-string element-bookmark-label
            toc-pass))

;;; ---------------- SXML utilities ----------------

(define (element? x)
  (and (pair? x) (symbol? (car x)) (not (memq (car x) '(@ *TOP* *PI* *ENTITY* *COMMENT*)))))
(define (element-tag el) (car el))

(define (element-attributes el)
  "EL's attributes as an alist of (name value)."
  (match (cdr el)
    ((('@ . attrs) . _) attrs)
    (_ '())))

(define (element-children el)
  (match (cdr el)
    ((('@ . _) . kids) kids)
    (kids kids)))

(define (element-attribute el name)
  (match (assq name (element-attributes el))
    ((_ value) value)
    (_ #f)))

(define (set-element-attribute el name value)
  "EL with attribute NAME set to VALUE."
  `(,(element-tag el)
    (@ ,@(remove (lambda (a) (eq? (car a) name)) (element-attributes el)) (,name ,value))
    ,@(element-children el)))

(define (has-class? el class)
  (and=> (element-attribute el 'class)
         (lambda (c) (and (member class (string-tokenize c)) #t))))

(define (element-text x)
  "The text in X (a node or list of nodes), without markup."
  (cond ((string? x) x)
        ((element? x) (string-concatenate (map element-text (element-children x))))
        ((pair? x) (string-concatenate (map element-text x)))   ; a list of nodes
        (else "")))

(define* (map-elements proc doc #:key (skip? (const #f)))
  "DOC with each element E replaced by (PROC E), outermost first; PROC's
result is then walked in turn.  Elements for which SKIP? is true are left
as they are, with their contents.  DOC may be a node or a list of nodes."
  (let walk ((x doc))
    (cond ((and (element? x) (skip? x)) x)
          ((element? x)
           (let ((e (proc x)))
             (if (element? e)
                 (let ((attrs (match (cdr e) ((('@ . a) . _) (list (cons '@ a))) (_ '()))))
                   `(,(element-tag e) ,@attrs ,@(map walk (element-children e))))
                 e)))
          ((and (pair? x) (not (symbol? (car x)))) (map walk x))
          (else x))))

(define* (find-elements pred doc #:key (skip? (const #f)))
  "The elements in DOC satisfying PRED, in document order, not looking
inside elements for which SKIP? is true."
  (let walk ((x doc))
    (cond ((and (element? x) (skip? x)) '())
          ((element? x)
           (append (if (pred x) (list x) '()) (append-map walk (element-children x))))
          ((and (pair? x) (not (symbol? (car x)))) (append-map walk x))
          (else '()))))

(define (slugify text)
  "TEXT as an id: lower case, runs of other characters as one hyphen."
  (let* ((s (string-map (lambda (c) (if (char-alphabetic? c) (char-downcase c) (if (char-numeric? c) c #\-)))
                        text))
         (s (string-join (remove string-null? (string-split s #\-)) "-")))
    (if (string-null? s) "section" s)))

;;; ---------------- bookmark-label ----------------
;;; The CSS property bookmark-label (CSS GCPM) names an element's PDF
;;; bookmark; WeasyPrint uses it.  toc-pass reads it from an element's
;;; style attribute for the entry text, so one label serves both.  Only
;;; values made of strings are understood; anything else (content(),
;;; attr(), counters) is left to the caller's fallback.

(define (css-string text)
  "TEXT as a double-quoted CSS string."
  (string-append
   "\""
   (string-concatenate
    (map (lambda (c)
           (case c
             ((#\\) "\\\\")
             ((#\") "\\\"")
             ((#\newline) "\\A ")
             (else (string c))))
         (string->list text)))
   "\""))

(define (style-declarations style)
  "STYLE (a style attribute) as a list of (name . value) strings, in order.
Semicolons inside quoted strings or parentheses do not split."
  (define (declaration str)
    (let ((colon (string-index str #\:)))
      (and colon
           (cons (string-downcase (string-trim-both (substring str 0 colon)))
                 (string-trim-both (substring str (1+ colon)))))))
  (let loop ((cs (string->list style)) (quote-char #f) (depth 0) (cur '()) (out '()))
    (define (flush) (let ((d (declaration (list->string (reverse cur))))) (if d (cons d out) out)))
    (match cs
      (() (reverse (flush)))
      ((#\\ c . rest) (loop rest quote-char depth (cons* c #\\ cur) out))
      ((c . rest)
       (cond (quote-char (loop rest (if (eqv? c quote-char) #f quote-char) depth (cons c cur) out))
             ((memv c '(#\" #\')) (loop rest c depth (cons c cur) out))
             ((eqv? c #\() (loop rest #f (1+ depth) (cons c cur) out))
             ((eqv? c #\)) (loop rest #f (max 0 (1- depth)) (cons c cur) out))
             ((and (eqv? c #\;) (zero? depth)) (loop rest #f 0 '() (flush)))
             (else (loop rest #f depth (cons c cur) out)))))))

(define (css-strings value)
  "The text of VALUE if it is only CSS strings (and an optional !important),
concatenated; otherwise #f."
  (define (hex? c) (string-index "0123456789abcdefABCDEF" c))
  (define (read-string cs q)            ; cs after the opening quote -> (text . rest) or #f
    (let loop ((cs cs) (acc '()))
      (match cs
        (() #f)
        (((? (lambda (c) (eqv? c q))) . rest) (cons (list->string (reverse acc)) rest))
        ((#\newline . _) #f)
        ((#\\ #\newline . rest) (loop rest acc))
        ((#\\ (? hex? h) . rest)
         (let hexes ((rest rest) (digits (list h)))
           (match rest
             (((? hex? h) . more) (=> next) (if (< (length digits) 6) (hexes more (cons h digits)) (next)))
             (_ (let ((rest (match rest (((? char-whitespace?) . r) r) (_ rest)))
                      (n (string->number (list->string (reverse digits)) 16)))
                  (loop rest (cons (if (or (zero? n) (> n #x10FFFF) (<= #xD800 n #xDFFF)) #\xFFFD (integer->char n))
                                   acc)))))))
        ((#\\ c . rest) (loop rest (cons c acc)))
        ((c . rest) (loop rest (cons c acc))))))
  (let* ((v (string-trim-both value))
         (v (if (string-suffix-ci? "!important" v)
                (string-trim-right (string-drop-right v (string-length "!important")))
                v)))
    (let loop ((cs (string->list v)) (parts '()))
      (match cs
        (() (and (pair? parts) (string-concatenate-reverse parts)))
        (((? char-whitespace?) . rest) (loop rest parts))
        (((and q (or #\" #\')) . rest)
         (match (read-string rest q)
           ((text . rest) (loop rest (cons text parts)))
           (#f #f)))
        (_ #f)))))

(define (element-bookmark-label el)
  "The bookmark-label in EL's style attribute, if it is made of strings;
otherwise #f.  The last bookmark-label declaration counts."
  (let ((style (element-attribute el 'style)))
    (and style
         (match (filter (lambda (d) (string=? (car d) "bookmark-label"))
                        (style-declarations style))
           (() #f)
           (decls (css-strings (cdr (last decls))))))))

;;; ---------------- Table of contents ----------------
;;; ,toc (in (uq tags)) renders <nav class="toc"> with an optional title and
;;; an empty <ol>.  (toc-pass '(h1 h2)) fills every such <ol> with links to
;;; the document's h1 and h2 elements, h2s nested under the h1 before them,
;;; giving each heading an id from its text if it has none.  An entry's text
;;; is the heading's bookmark-label, if its style attribute sets one to
;;; strings, or else its text.  Headings inside a toc are not listed.  A
;;; document without a toc is left as it is.
;;;
;;; Instead of a list of tags, toc-pass takes a procedure, called on each
;;; element in document order: #f leaves it out; a number lists it at that
;;; level (lower is outer); #t lists it at level 0.  A list of tags is
;;; shorthand for one:
;;;   (toc-pass (lambda (el) (and (has-class? el "chapter") 0)))
;;; Given a second argument, the procedure also receives the entries
;;; accepted so far, most recent first, as (level . element):
;;;   (toc-pass (lambda (el prior) (and (eq? (element-tag el) 'h1) (null? prior) 0)))

(define (toc? el) (and (eq? (element-tag el) 'nav) (has-class? el "toc")))

(define (toc-pass levels)
  "A post pass filling every ,toc of the document.  LEVELS is a list of
tags, outermost level first, or a procedure called on each element in
document order -- (proc element) or (proc element prior) -- returning #f
(leave it out), a level number (lower is outer) or #t (level 0).  PRIOR
is the entries accepted so far, most recent first, each (level . element)."
  (define decide                        ; element prior -> level or #f
    (match levels
      ((? procedure? proc)
       (let ((call (match (procedure-minimum-arity proc)
                     ((req opt rest) (if (or rest (>= (+ req opt) 2))
                                         proc
                                         (lambda (el prior) (proc el))))
                     (_ proc))))
         (lambda (el prior)
           (match (call el prior)
             (#f #f)
             (#t 0)
             ((? exact-integer? n) n)
             (other (error (format #f "toc-pass: the procedure returned ~s for <~a>; give #f, #t or a level number"
                                   other (element-tag el))))))))
      (((? symbol?) ...)
       (lambda (el prior) (list-index (lambda (t) (eq? t (element-tag el))) levels)))
      (_ (error "toc-pass: give a list of tags or a procedure" levels))))
  (lambda (doc)
    (if (null? (find-elements toc? doc)) doc (fill-tocs doc decide))))

(define (fill-tocs doc decide)
  ;; 1. decide once, in document order, each element seeing the entries
  ;;    accepted before it.  The walks below visit elements in the same
  ;;    order (outermost first, tocs skipped), so decisions match by position.
  (define levels
    (let loop ((els (find-elements (const #t) doc #:skip? toc?)) (prior '()) (out '()))
      (match els
        (() (list->vector (reverse out)))
        ((el . rest)
         (let ((level (decide el prior)))
           (loop rest (if level (cons (cons level el) prior) prior) (cons level out)))))))
  (define taken                          ; ids already in the document
    (filter-map (lambda (el) (element-attribute el 'id)) (find-elements (const #t) doc)))
  (define (fresh-id text)
    (let ((base (slugify text)))
      (let loop ((n 1))
        (let ((id (if (= n 1) base (string-append base "-" (number->string n)))))
          (if (member id taken) (loop (1+ n)) (begin (set! taken (cons id taken)) id))))))
  ;; 2. give every listed element an id
  (define with-ids
    (let ((i -1))
      (map-elements (lambda (el)
                      (set! i (1+ i))
                      (if (and (vector-ref levels i) (not (element-attribute el 'id)))
                          (set-element-attribute el 'id (fresh-id (element-text el)))
                          el))
                    doc #:skip? toc?)))
  ;; 3. the entries, (level . element) in document order, nested by level
  (define entries
    (filter car (map cons (vector->list levels) (find-elements (const #t) with-ids #:skip? toc?))))
  (define (items entries)
    (match entries
      (() '())
      (((level . el) . rest)
       (let-values (((kids rest) (span (lambda (x) (> (car x) level)) rest)))
         (cons `(li (a (@ (href ,(string-append "#" (element-attribute el 'id))))
                       ,(string-trim-both (or (element-bookmark-label el) (element-text el))))
                    ,@(if (null? kids) '() `((ol ,@(items kids)))))
               (items rest))))))
  (define listed (items entries))
  ;; 4. fill each toc's list
  (map-elements (lambda (el)
                  (if (toc? el)
                      `(nav (@ ,@(element-attributes el))
                            ,@(map (lambda (kid)
                                     (if (and (element? kid) (eq? (element-tag kid) 'ol))
                                         `(ol ,@(element-children kid) ,@listed)
                                         kid))
                                   (element-children el)))
                      el))
                with-ids))
