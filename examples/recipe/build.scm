#!/usr/bin/env sh
exec guile --no-auto-compile -s "$0" "$@"
!#
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

;;; build.scm -- compile recipe.uq into a standalone HTML page.
;;;
;;;   examples/recipe/build.scm [INPUT.uq [OUTPUT.html]]
;;;
;;; Defaults to recipe.uq -> recipe.html next to this script.  Works from
;;; any directory: the load path is set relative to the script.

(eval-when (expand load eval)
  (let ((here (dirname (current-filename))))
    (add-to-load-path (canonicalize-path (string-append here "/../..")))   ; (uq …)
    (add-to-load-path (canonicalize-path (string-append here "/..")))))   ; (recipe …)

(use-modules (uq parse) (uq tags) (recipe tags)
             (sxml simple) (ice-9 match) (srfi srfi-1))

(define here (dirname (current-filename)))

;; The first <h1> in the page, as plain text, for the <title>.
(define (page-title tree)
  (define (text x)
    (cond ((string? x) x)
          ((and (pair? x) (eq? (car x) '@)) "")
          ((pair? x) (string-concatenate (map text (cdr x))))
          (else "")))
  (let find ((x tree))
    (cond ((not (pair? x)) #f)
          ((eq? (car x) 'h1) (string-trim-both (text x)))
          (else (any find (cdr x))))))

(define (page body)
  `(html (@ (lang "en"))
     (head (meta (@ (charset "utf-8")))
           (meta (@ (name "viewport") (content "width=device-width, initial-scale=1")))
           (title ,(or (page-title body) "Recipe")))
     (body ,body)))

(define (compile-uq in out)
  (let* ((expr (catch #t
                 (lambda () (call-with-input-file in read-markup-document))
                 (lambda (key . args)
                   ;; parse errors read "line N: …"; prefix the file name
                   (match args
                     ((_ (? string? fmt) (? list? fargs) . _)
                      (format (current-error-port) "~a: ~a~%" in (apply format #f fmt fargs)))
                     (_ (format (current-error-port) "~a: ~a ~s~%" in key args)))
                   (exit 1))))
         (body (eval expr (current-module))))
    (call-with-output-file out
      (lambda (port)
        (set-port-encoding! port "UTF-8")
        (display "<!DOCTYPE html>\n" port)
        (sxml->xml (page body) port)
        (newline port)))
    (format #t "~a -> ~a~%" in out)))

(match (command-line)
  ((_)            (compile-uq (string-append here "/recipe.uq") (string-append here "/recipe.html")))
  ((_ in)         (compile-uq in (string-append (if (string-suffix? ".uq" in)
                                                    (string-drop-right in 3) in)
                                                ".html")))
  ((_ in out)     (compile-uq in out))
  (_ (format (current-error-port) "usage: build.scm [INPUT.uq [OUTPUT.html]]~%") (exit 2)))
