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

;;; (uq compile) -- compile .uq files to HTML.  For build scripts:
;;;
;;;   (use-modules (uq compile))
;;;   (compile-uq "recipe.uq" #:tags '((recipe tags)))   ; -> recipe.html, a fragment
;;;
;;; Output is an HTML fragment.  Anything around it -- doctype, head,
;;; site header -- comes from a #:page procedure in the build script.
;;;
;;; Documents are evaluated in a module that uses (uq tags) plus the
;;; modules in #:tags, later ones taking precedence.  Those modules are
;;; loaded before parsing, because the parser needs their tags registered.

(define-module (uq compile)
  #:use-module (uq parse)
  #:use-module (sxml simple)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (component-module
            uq-file->expr uq-file->sxml
            page-title write-sxml write-html
            compile-uq)
  #:re-export (uq-error-message))

(define (component-module tags)
  "A fresh module using (uq tags) and each module name in TAGS.  A name
defined in more than one of them resolves to the later module, quietly:
a project may redefine a generic component such as em."
  (let ((m (make-fresh-user-module)))
    ;; as #:duplicates (replace last) in define-module
    (set-module-duplicates-handlers! m (lookup-duplicates-handlers '(replace last)))
    (for-each (lambda (spec) (module-use! m (resolve-interface spec)))
              (cons '(uq tags) tags))
    m))

(define* (uq-file->expr file #:key (tags '()))
  "The Scheme expression FILE reads as.  Loads TAGS first."
  (component-module tags)
  (call-with-input-file file read-markup-document))

(define* (uq-file->sxml file #:key (tags '()))
  "FILE parsed and evaluated: its top-level nodes, as a list of SXML
siblings with no root element.  Errors are raised."
  (let ((env (component-module tags)))
    (eval (call-with-input-file file read-markup-document) env)))

;;; Helpers for #:page procedures

(define (page-title sxml)
  "The text of the first h1 in SXML (a node or a list of nodes), or #f."
  (define (text x)
    (cond ((string? x) x)
          ((and (pair? x) (eq? (car x) '@)) "")
          ((pair? x) (string-concatenate (map text (cdr x))))
          (else "")))
  (let find ((x sxml))
    (cond ((not (pair? x)) #f)
          ((eq? (car x) 'h1) (string-trim-both (text x)))
          ((symbol? (car x)) (any find (cdr x)))      ; an element: its children
          (else (any find x)))))                      ; a list of nodes

;;; Indented output
;;
;; sxml->xml writes everything on one line, and has no option to indent.
;; Whitespace in HTML text is significant, so indentation is added only
;; where it cannot change the rendering: an element's children go on their
;; own lines when they are all elements and the element itself is not
;; phrasing (inline) content or pre-formatted.  Neighbouring phrasing
;; children share one line with nothing between them, since a line break
;; there would show as a space; at the start or end of a line next to a
;; block, the break is collapsed away.  Anything else -- a p or li with
;; text, an em, a pre -- is written on one line by sxml->xml, as before.

(define phrasing-tags
  '(a abbr audio b bdi bdo br button canvas cite code data datalist del dfn
    em embed i iframe img input ins kbd label map mark math meter noscript
    object output picture progress q ruby s samp select slot small span
    strong sub sup svg template textarea time u var video wbr))
(define verbatim-tags '(pre textarea script style))

(define (element? x)
  (and (pair? x) (symbol? (car x)) (not (memq (car x) '(@ *TOP* *ENTITY* *PI* *COMMENT*)))))
(define (phrasing? x) (and (element? x) (memq (car x) phrasing-tags) #t))

;; SXML allows nested lists of nodes; flatten them.
(define (node-list x)
  (cond ((null? x) '())
        ((and (pair? x) (not (symbol? (car x)))) (append-map node-list x))
        (else (list x))))

(define (attributes+children el)
  (let ((rest (cdr el)))
    (if (and (pair? rest) (pair? (car rest)) (eq? (caar rest) '@))
        (values (car rest) (node-list (cdr rest)))
        (values #f (node-list rest)))))

(define (indentable? el)
  (and (element? el)
       (not (memq (car el) phrasing-tags))
       (not (memq (car el) verbatim-tags))
       (call-with-values (lambda () (attributes+children el))
         (lambda (attrs kids)
           (and (pair? kids) (every element? kids))))))

;; KIDS grouped into lines: each block element alone, each run of
;; neighbouring phrasing elements together.
(define (lines kids)
  (let loop ((kids kids) (run '()) (out '()))
    (define (flush) (if (null? run) out (cons (reverse run) out)))
    (cond ((null? kids) (reverse (flush)))
          ((phrasing? (car kids)) (loop (cdr kids) (cons (car kids) run) out))
          (else (loop (cdr kids) '() (cons (list (car kids)) (flush)))))))

;; The open and close tags of EL, written by sxml->xml around a placeholder
;; so attribute escaping stays sxml->xml's own.
(define placeholder "uq-children-placeholder-7f3a")
(define (open+close-tags el attrs)
  (let* ((s (call-with-output-string
              (lambda (p) (sxml->xml `(,(car el) ,@(if attrs (list attrs) '()) ,placeholder) p))))
         (i (string-contains s placeholder)))
    (values (substring s 0 i) (substring s (+ i (string-length placeholder))))))

(define (write-indented node depth step port)
  (define (pad d) (display (make-string (* d step) #\space) port))
  (pad depth)
  (if (indentable? node)
      (call-with-values (lambda () (attributes+children node))
        (lambda (attrs kids)
          (call-with-values (lambda () (open+close-tags node attrs))
            (lambda (open close)
              (display open port)
              (newline port)
              (for-each (lambda (line)
                          (if (null? (cdr line))
                              (write-indented (car line) (1+ depth) step port)
                              (begin (pad (1+ depth))
                                     (for-each (lambda (k) (sxml->xml k port)) line)))
                          (newline port))
                        (lines kids))
              (pad depth)
              (display close port)))))
      (sxml->xml node port)))

(define* (write-sxml sxml #:optional (port (current-output-port)) #:key (indent #f))
  "Like sxml->xml, but with INDENT (#t for 2, or a number of spaces) put
block structure on indented lines.  Text and inline markup are unchanged."
  (let ((step (cond ((eq? indent #t) 2) ((integer? indent) indent) (else #f))))
    (if step
        (let ((nodes (node-list sxml)))
          (for-each (lambda (n i)
                      (unless (zero? i) (newline port))
                      (write-indented n 0 step port))
                    nodes (iota (length nodes))))
        (sxml->xml sxml port))))

(define* (write-html sxml port #:key (indent #f))
  "Write SXML to PORT as UTF-8 HTML, ending with a newline.  If its root
is an html element, an HTML5 doctype comes first; anything else is
written as a fragment.  INDENT is passed to write-sxml."
  (set-port-encoding! port "UTF-8")
  (when (and (pair? sxml) (eq? (car sxml) 'html))
    (display "<!DOCTYPE html>\n" port))
  (write-sxml sxml port #:indent indent)
  (newline port))

;;; Compiling

(define (default-output in)
  (string-append (if (string-suffix? ".uq" in) (string-drop-right in 3) in) ".html"))

(define* (compile-uq in #:key out (tags '()) (page #f) (indent #f))
  "Compile the .uq file IN to the HTML file OUT (default: IN with .html).
The output is a fragment.  PAGE, if given, is a procedure from the
document's nodes (a list of siblings) to the SXML to write instead --
splice them with ,@ -- and if the result is an html element,
a doctype is added.  INDENT (#t, or a number of spaces) writes indented
HTML; see write-sxml.  On success,
report IN -> OUT and return OUT.  On an error, report FILE: MESSAGE on
the error port, write nothing, and return #f."
  (define target (or out (default-output in)))
  (catch #t
    (lambda ()
      (let* ((doc  (uq-file->sxml in #:tags tags))
             (html (if page (page doc) doc)))              ; finish before opening OUT
        (call-with-output-file target (lambda (port) (write-html html port #:indent indent)))
        (format #t "~a -> ~a~%" in target)
        target))
    (lambda (key . args)
      (format (current-error-port) "~a: ~a~%" in (uq-error-message key args))
      #f)))
