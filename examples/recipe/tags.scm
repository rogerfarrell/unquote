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

;;; (recipe tags) -- supplementary components for one-page recipe cards.
;;; Load alongside (uq tags):
;;;   guile -L . -L examples unquote.scm --tags "(recipe tags)" examples/recipe/recipe.uq

(define-module (recipe tags)
  #:use-module (uq parse)
  #:use-module ((uq tags) #:select (plain)))

(define-tag block (recipe . body) `(div (@ (class "recipe")) ,@body))
(define-tag block:inline (title . body) `(h1 ,@body))
;; ,photo[file][alt] or ,photo[file :width 3in][alt]
(define-tag block:inline (photo . alt) #:attributes attrs #:editor-attributes required
  `(figure (@ (class "photo"))
     (img (@ (src ,(main-attribute attrs 'src))
             (alt ,(plain alt))
             ,@(attribute-list attrs '(src alt))))))
(define-tag block:inline (yield . body) `(span (@ (class "yield")) "Yield: " ,@body))
(define-tag block:inline (prep . body)  `(span (@ (class "prep")) "Prep time: " ,@body))
(define-tag block (ingredients . body) `(section (@ (class "ingredients")) (h2 "Ingredients") ,@body))
(define-tag block (directions . body)  `(section (@ (class "directions")) (h2 "Directions") ,@body))
(define-tag block (tip . body) `(aside (@ (class "tip")) (h3 "Tip!") ,@body))
