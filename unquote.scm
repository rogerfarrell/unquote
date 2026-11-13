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

;; unquote.scm -- command-line renderer
;;
;;   guile -L . unquote.scm [--expr] [--tags MODULE]... FILE
;;
;; The document is evaluated in a module that uses (uq tags) and each
;; --tags module, later ones taking precedence; e.g.
;;   guile -L . -L examples unquote.scm --tags "(recipe tags)" examples/recipe/recipe.uq
(use-modules (uq parse) (uq compile) (ice-9 match) (ice-9 pretty-print) (sxml simple))
(fluid-set! %default-port-encoding "UTF-8")
(set-port-encoding! (current-output-port) "UTF-8")

(define (usage)
  (format (current-error-port) "usage: unquote.scm [--expr] [--tags \"(MODULE NAME)\"]... FILE~%")
  (exit 2))

(define (render file show? specs)
  (let* ((env  (component-module specs))
         (expr (call-with-input-file file read-markup-document #:encoding "UTF-8")))
    (when show? (pretty-print (strip-line-wrappers expr) (current-error-port)))
    (sxml->xml (eval expr env))
    (newline)))

(let loop ((args (cdr (command-line))) (show? #f) (specs '()))
  (match args
    (("--expr" . rest) (loop rest #t specs))
    (("--tags" spec . rest)
     (loop rest show? (append specs (list (call-with-input-string spec read)))))
    ((file) (render file show? specs))
    (_ (usage))))
