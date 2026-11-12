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

;;; (uq tags) -- the generic components: paragraphs, lists, tables and
;;; inline markup.  Project-specific components go in their own module;
;;; see examples/recipe/tags.scm.

(define-module (uq tags)
  #:replace (link)          ; deliberately shadows POSIX `link' in user modules
  #:use-module (uq parse) #:use-module (ice-9 match) #:use-module (srfi srfi-1)
  #:export (plain))

;; The text of a body with any markup dropped, trimmed: for alt text,
;; attribute values.
(define (plain body) (string-trim-both (apply string-append (filter string? body))))

;;; structure
(define-tag block:inline (p . body) #:default-wrap #t `(p ,@body))   ; wraps loose text
(define-tag block (ul . body) #:wrap li `(ul ,@body))
(define-tag block (ol . body) #:wrap li `(ol ,@body))
(define-tag block (li . body) `(li ,@body))

;;; tables: rows are explicit ,tr; in a row each blank-line group is a
;;; cell, a td unless it is a lone ,th.  A cell's text stays inline, and a
;;; cell may hold blocks such as lists.  Loose text directly in ,table is
;;; not wrapped; it gives invalid HTML, which is left to the editor.
(define-tag block (table . body) `(table ,@body))
(define-tag block (tr . body) #:wrap (td th) `(tr ,@body))
(define-tag block (td . body) #:wrap #f `(td ,@body))
(define-tag block (th . body) #:wrap #f `(th ,@body))

;;; inline
(define-tag inline (em . body)     `(em ,@body))
(define-tag inline (strong . body) `(strong ,@body))
;; ,link[url][text] or ,link[:href url :title Hint][text]; no text shows the URL
(define-tag inline (link . body) #:required-attributes attrs
  (let ((href (main-attribute attrs 'href 'link)))
    `(a (@ (href ,href) ,@(attribute-list attrs '(href)))
        ,@(if (null? body) (list href) body))))
;; ,image[file][alt] or ,image[:src file :width 40][alt]
(define-tag inline (image . alt) #:required-attributes attrs
  `(img (@ (src ,(main-attribute attrs 'src 'image)) (alt ,(plain alt))
           ,@(attribute-list attrs '(src alt)))))
(define-tag word (frac . body)
  (let ((s (plain body)))
    (or (assoc-ref '(("1/2" . "½") ("1/3" . "⅓") ("2/3" . "⅔") ("1/4" . "¼") ("3/4" . "¾")) s)
        (match (string-split s #\/)
          ((n d) `(span (@ (class "frac")) (sup ,n) "⁄" (sub ,d)))
          (_ s)))))
