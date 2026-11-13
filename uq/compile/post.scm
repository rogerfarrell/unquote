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
  #:use-module (uq sxml)
  ;; the SXML utilities are re-exported, so a pass needs only this module
  #:re-export (element? element-tag element-attributes element-children
               element-attribute set-element-attribute remove-element-attribute
               add-class has-class? element-text map-elements find-elements)
  #:export (slugify toc-pass))

;;; ---------------- ids ----------------

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
;;; giving each heading an id from its text if it has none.  An entry's text
;;; is the heading's data-toc attribute, if it has one, or else its text.
;;; Headings inside a toc are not listed.  A document without a toc is left
;;; as it is.
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
                       ,(string-trim-both (or (element-attribute el 'data-toc) (element-text el))))
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
