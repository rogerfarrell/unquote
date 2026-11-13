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

;;; (uq qr) -- an optional QR code component.  Needs Guile-QR-Code (and
;;; the Guile-Lib it uses); load it with --tags "(uq qr)" or #:tags.
;;;
;;;   ,qr[DATA][label]
;;;   ,qr[DATA :ecc high][label]
;;;   ,qr[:data DATA][label]
;;;
;;; DATA is any text: a URL, a "WIFI:…" or vCard string (quote it if it has
;;; spaces or brackets; \n for line breaks), digits.  The label is the
;;; accessible name; without one, DATA is used.  :ecc is the error
;;; correction level -- low (the default), medium, quartile or high.
;;; The result is an inline SVG with class "qr"; size it with CSS.

(define-module (uq qr)
  #:use-module (uq parse)
  #:use-module ((uq tags) #:select (plain))
  #:use-module (qr-code)
  #:use-module ((qr-code encoder) #:select (QR-code-size qr-code-module))
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define ecc-levels
  `(("low" . ,ECC-LOW) ("medium" . ,ECC-MEDIUM) ("quartile" . ,ECC-QUARTILE) ("high" . ,ECC-HIGH)))

(define quiet-zone 4)                   ; modules of white border, as the standard asks

;; An SVG with one path: a unit square for each dark module.
(define (qr-svg data ecc label)
  (let* ((qr   (qr-encode-text data #:ecl ecc))
         (n    (QR-code-size qr))
         (side (number->string (+ n quiet-zone quiet-zone)))
         (d    (string-concatenate
                (append-map (lambda (y)
                              (filter-map (lambda (x)
                                            (and (qr-code-module qr x y)
                                                 (format #f "M~a,~ah1v1h-1z"
                                                         (+ x quiet-zone) (+ y quiet-zone))))
                                          (iota n)))
                            (iota n)))))
    `(svg (@ (xmlns "http://www.w3.org/2000/svg")
             (viewBox ,(string-append "0 0 " side " " side))
             ;; No shape-rendering="crispEdges": renderers may snap each
             ;; module's edges separately, which at a non-integer scale
             ;; leaves one-pixel notches where modules meet diagonally.
             (class "qr") (role "img") (aria-label ,label))
          (rect (@ (width "100%") (height "100%") (fill "#fff")))
          (path (@ (d ,d) (fill "#000"))))))

(define-tag inline (qr . label) #:required-attributes attrs
  (let* ((data (main-attribute attrs 'data))
         (name (string-downcase (or (attribute attrs 'ecc) "low")))
         (ecc  (or (assoc-ref ecc-levels name)
                   (error (format #f ",qr: :ecc is low, medium, quartile or high, not ~a" name)))))
    (qr-svg data ecc (if (null? label) data (plain label)))))
