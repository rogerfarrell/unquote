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

(define-module (uq tags)
  #:replace (link)          ; deliberately shadows POSIX `link' in user modules
  #:use-module (uq parse) #:use-module (ice-9 match) #:use-module (srfi srfi-1))
;; Component definitions; each one is also exported from this module.
(define-syntax-rule (define-tag* kind (name . args) body ...)
  (begin (define-tag kind (name . args) body ...) (export name)))
(define-syntax-rule (define-tag/address* kind (name . args) body ...)
  (begin (define-tag/address kind (name . args) body ...) (export name)))

;;; ================= Components (the part you own) =================
;; Attributes live here, never in the editor's file.
(define (plain body) (string-trim-both (apply string-append (filter string? body))))

;; structural
(define-tag* block (document . body) `(article (@ (class "recipe")) ,@body))
(define-tag* block:inline (p . body)  `(p ,@body))
(define-tag* block (ul . body) #:wrap li `(ul ,@body))
(define-tag* block (ol . body) #:wrap li `(ol ,@body))
(define-tag* block (li . body) `(li ,@body))

;; recipe card
(define-tag* block (recipe . body) `(div (@ (class "card")) ,@body))
(define-tag* block:inline (title . body)  `(h1 ,@body))
(define-tag/address* block:inline (photo src . alt)
  `(figure (@ (class "photo")) (img (@ (src ,src) (alt ,(plain alt))))))
(define-tag* block:inline (yield . body) `(span (@ (class "yield")) "Yield: " ,@body))
(define-tag* block:inline (prep . body)  `(span (@ (class "prep")) "Prep time: " ,@body))
(define-tag* block (ingredients . body) `(section (@ (class "ingredients")) (h2 "Ingredients") ,@body))
(define-tag* block (directions . body)  `(section (@ (class "directions")) (h2 "Directions") ,@body))
(define-tag* block (tip . body) `(aside (@ (class "tip")) (h3 "Tip!") ,@body))

;; inline
(define-tag* inline (em . body)     `(em ,@body))
(define-tag* inline (strong . body) `(strong ,@body))
(define-tag/address* inline (link href . body)          ; no text: show the address
  `(a (@ (href ,href)) ,@(if (null? body) (list href) body)))
(define-tag/address* inline (image src . alt)  `(img (@ (src ,src) (alt ,(plain alt)))))
(define-tag* word (frac . body)
  (let ((s (plain body)))
    (or (assoc-ref '(("1/2" . "½") ("1/3" . "⅓") ("2/3" . "⅔") ("1/4" . "¼") ("3/4" . "¾")) s)
        (match (string-split s #\/)
          ((n d) `(span (@ (class "frac")) (sup ,n) "⁄" (sub ,d)))
          (_ s)))))

