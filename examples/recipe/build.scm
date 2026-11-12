#!/usr/bin/env sh
exec guile --no-auto-compile -e main -s "$0" "$@"
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

(use-modules (uq compile) (ice-9 match))

(define here (dirname (current-filename)))

(define* (build in #:optional out)
  (unless (compile-uq in #:out out #:tags '((recipe tags)))
    (exit 1)))

;; Entry point, called by the -e main in the header with (command-line).
(define (main args)
  (match args
    ((_)        (build (string-append here "/recipe.uq")))
    ((_ in)     (build in))
    ((_ in out) (build in out))
    (_ (format (current-error-port) "usage: build.scm [INPUT.uq [OUTPUT.html]]~%") (exit 2))))
