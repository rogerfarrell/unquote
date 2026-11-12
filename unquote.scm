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

;; unquote.scm — command-line renderer:  guile -L . unquote.scm [--expr] FILE
(use-modules (uq parse) (uq tags) (ice-9 match) (ice-9 pretty-print) (sxml simple))
(fluid-set! %default-port-encoding "UTF-8")
(set-port-encoding! (current-output-port) "UTF-8")
(define (render file show?)
  (let ((expr (call-with-input-file file read-markup-document)))
    (when show? (pretty-print expr (current-error-port)))
    (sxml->xml (eval expr (resolve-module '(uq tags))))
    (newline)))
(match (command-line)
  ((_ "--expr" file) (render file #t))
  ((_ file) (render file #f)))
