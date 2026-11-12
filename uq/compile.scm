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
;;;   (compile-uq "recipe.uq" #:tags '((recipe tags)))   ; -> recipe.html
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
            page-title default-page write-html-page
            compile-uq uq-error-message))

(define (component-module tags)
  "A fresh module using (uq tags) and each module name in TAGS."
  (let ((m (make-fresh-user-module)))
    (for-each (lambda (spec) (module-use! m (resolve-interface spec)))
              (cons '(uq tags) tags))
    m))

(define* (uq-file->expr file #:key (tags '()))
  "The Scheme expression FILE reads as.  Loads TAGS first."
  (component-module tags)
  (call-with-input-file file read-markup-document))

(define* (uq-file->sxml file #:key (tags '()))
  "FILE parsed and evaluated: the SXML for its document.  Errors are raised."
  (let ((env (component-module tags)))
    (eval (call-with-input-file file read-markup-document) env)))

;;; Pages

(define (page-title sxml)
  "The text of the first h1 in SXML, or #f."
  (define (text x)
    (cond ((string? x) x)
          ((and (pair? x) (eq? (car x) '@)) "")
          ((pair? x) (string-concatenate (map text (cdr x))))
          (else "")))
  (let find ((x sxml))
    (cond ((not (pair? x)) #f)
          ((eq? (car x) 'h1) (string-trim-both (text x)))
          (else (any find (cdr x))))))

(define* (default-page body #:key (lang "en") (title (page-title body)) (stylesheets '()))
  "BODY wrapped in a complete html element.  The title defaults to the
first h1."
  `(html (@ (lang ,lang))
     (head (meta (@ (charset "utf-8")))
           (meta (@ (name "viewport") (content "width=device-width, initial-scale=1")))
           ,@(if title `((title ,title)) '())
           ,@(map (lambda (href) `(link (@ (rel "stylesheet") (href ,href)))) stylesheets))
     (body ,body)))

(define (write-html-page sxml port)
  "Write SXML (an html element) to PORT as UTF-8 with an HTML5 doctype."
  (set-port-encoding! port "UTF-8")
  (display "<!DOCTYPE html>\n" port)
  (sxml->xml sxml port)
  (newline port))

;;; Compiling

(define (uq-error-message key args)
  "A one-line message for an exception raised while compiling."
  (match args
    ((_ (? string? fmt) (? list? fargs) . _) (apply format #f fmt fargs))
    (_ (format #f "~a ~s" key args))))

(define (default-output in)
  (string-append (if (string-suffix? ".uq" in) (string-drop-right in 3) in) ".html"))

(define* (compile-uq in #:key out (tags '()) (page default-page))
  "Compile the .uq file IN to the HTML file OUT (default: IN with .html).
PAGE turns the document's SXML into the html element.  On success,
report IN -> OUT and return OUT.  On an error, report FILE: MESSAGE on
the error port, write nothing, and return #f."
  (define target (or out (default-output in)))
  (catch #t
    (lambda ()
      (let ((html (page (uq-file->sxml in #:tags tags))))   ; finish before opening OUT
        (call-with-output-file target (lambda (port) (write-html-page html port)))
        (format #t "~a -> ~a~%" in target)
        target))
    (lambda (key . args)
      (format (current-error-port) "~a: ~a~%" in (uq-error-message key args))
      #f)))
