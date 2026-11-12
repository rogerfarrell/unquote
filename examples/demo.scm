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

;; demo.scm -- markup inside an ordinary Scheme file
;;   guile -L . -L examples examples/demo.scm
(use-modules (uq parse) (uq tags) (recipe tags) (sxml simple))
(eval-when (expand load eval) (enable-markup-reader!))
(set-port-encoding! (current-output-port) "UTF-8")

;; 1. A string through the markup macro: expanded at compile time.
(define jars 12)
(define card
  (markup ",tip
  Makes ,(number->string jars) jars. Keep them ,em[dry].
  ,ul
    ,frac[1/2] cup milk powder

    ,link[https://example.com][more]"))

;; 2. Reader syntax: no quotes at all, and ,(expr) sees local variables.
(define (shelf-note weeks)
  #m<<END
,tip
  Best within ,(number->string weeks) weeks; ,strong[label the lid].
END
  )

;; 3. Your own component, defined right here; markup finds it.
(define-tag inline (warn . body) `(span (@ (class "warn")) "⚠ " ,@body))
(define notice (markup "Do not ,warn[microwave the lid]."))

(for-each (lambda (x) (sxml->xml x) (newline))
          (append card (shelf-note 6) notice))
