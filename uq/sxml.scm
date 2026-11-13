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

;;; (uq sxml) -- utilities for SXML elements, shared by components and post
;;; passes.  An element is (tag [(@ (name value) …)] child …).  Nothing here
;;; changes an element in place: each procedure returns a new one.
;;;
;;;   (add-class (link '("https://example.com") "jars") "external")

(define-module (uq sxml)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (element? element-tag element-attributes element-children
            element-attribute set-element-attribute remove-element-attribute
            add-class has-class? element-text map-elements find-elements))

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

(define (remove-element-attribute el name)
  "EL without attribute NAME."
  (match (cdr el)
    ((('@ . attrs) . kids)
     `(,(element-tag el) (@ ,@(remove (lambda (a) (eq? (car a) name)) attrs)) ,@kids))
    (_ el)))

(define (add-class el class)
  "EL with CLASS added to its class attribute, unless it is already there."
  (match (element-attribute el 'class)
    (#f (set-element-attribute el 'class class))
    (c (if (member class (string-tokenize c))
           el
           (set-element-attribute el 'class (string-append c " " class))))))

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
