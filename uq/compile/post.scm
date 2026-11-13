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
         (lambda (c) (member class (string-tokenize c)) #t)))

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

;;; ---------------- Table of contents ----------------
;;; ,toc (in (uq tags)) renders <nav class="toc"> with an optional title and
;;; an empty <ol>.  (toc-pass '(h1 h2)) fills every such <ol> with links to
;;; the document's h1 and h2 elements, h2s nested under the h1 before them,
;;; giving each heading an id from its text if it has none.  Headings inside
;;; a toc are not listed.  A document without a toc is left as it is.

(define (toc? el) (and (eq? (element-tag el) 'nav) (has-class? el "toc")))

(define (toc-pass levels)
  "A post pass listing the elements whose tags are in LEVELS, outermost
level first, in every ,toc of the document."
  (lambda (doc)
    (if (null? (find-elements toc? doc)) doc (fill-tocs doc levels))))

(define (fill-tocs doc levels)
    (define (heading? el) (memq (element-tag el) levels))
    (define taken                          ; ids already in the document
      (filter-map (lambda (el) (element-attribute el 'id)) (find-elements (const #t) doc)))
    (define (fresh-id text)
      (let ((base (slugify text)))
        (let loop ((n 1))
          (let ((id (if (= n 1) base (string-append base "-" (number->string n)))))
            (if (member id taken) (loop (1+ n)) (begin (set! taken (cons id taken)) id))))))
    ;; 1. give every listed heading an id
    (define with-ids
      (map-elements (lambda (el)
                      (if (and (heading? el) (not (element-attribute el 'id)))
                          (set-element-attribute el 'id (fresh-id (element-text el)))
                          el))
                    doc #:skip? toc?))
    ;; 2. the entries, nested by level
    (define (depth el) (list-index (lambda (t) (eq? t (element-tag el))) levels))
    (define (items entries)
      (match entries
        (() '())
        ((e . rest)
         (let-values (((kids rest) (span (lambda (x) (> (depth x) (depth e))) rest)))
           (cons `(li (a (@ (href ,(string-append "#" (element-attribute e 'id))))
                         ,(string-trim-both (element-text e)))
                      ,@(if (null? kids) '() `((ol ,@(items kids)))))
                 (items rest))))))
    (define entries (items (find-elements heading? with-ids #:skip? toc?)))
    ;; 3. fill each toc's list
    (map-elements (lambda (el)
                    (if (toc? el)
                        `(nav (@ ,@(element-attributes el))
                              ,@(map (lambda (kid)
                                       (if (and (element? kid) (eq? (element-tag kid) 'ol))
                                           `(ol ,@(element-children kid) ,@entries)
                                           kid))
                                     (element-children el)))
                        el))
                  with-ids
                  #:skip? (lambda (el) #f)))
