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

;;; (uq qr) -- optional QR code components.  Need Guile-QR-Code (and the
;;; Guile-Lib it uses); load with --tags "(uq qr)" or #:tags.
;;;
;;;   ,qr[DATA][label]                 black on white (RGB)
;;;   ,qr-cmyk[DATA][label]            black on white as DeviceCMYK, for print
;;;   ,qr[DATA :ecc high][label]       either takes :ecc
;;;   ,qr[:data DATA][label]
;;;
;;; DATA is any text: a URL, a "WIFI:…" or vCard string (quote it if it has
;;; spaces or brackets; \n for line breaks), digits.  The label is the
;;; accessible name; without one, DATA is used.  :ecc is the minimum error
;;; correction level -- low (the default), medium, quartile or high; the
;;; library raises it when that costs no extra size.
;;; The result is an inline SVG with class "qr"; size it with CSS.
;;;
;;; Colours.  The modules are a path with class "qr-dark", the background a
;;; rect with class "qr-light", with fill="#000" and fill="#fff".  A builder
;;; given colours sets them as style attributes on those two elements:
;;;   - WeasyPrint honours them, device-cmyk() included (page CSS does not
;;;     reach inside inline SVG there, so this is the way to colour print);
;;;   - they stay on their own element, where a <style> would apply to every
;;;     QR code on the page;
;;;   - a browser drops a colour it does not support, such as device-cmyk(),
;;;     and falls back to the fill attribute.
;;; Uncoloured codes (,qr) can still be restyled by page CSS in a browser.
;;; For other colours, define a component with qr-element:
;;;
;;;   (define-tag inline (qr . label) #:required-attributes attrs
;;;     (qr-element attrs label #:dark "device-cmyk(1 0.6 0 0.2)"))

(define-module (uq qr)
  #:use-module (uq parse)
  #:use-module ((uq tags) #:select (plain))
  #:use-module (qr-code)
  #:use-module ((qr-code encoder) #:select (QR-code-size qr-code-module))
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (qr-svg qr-element))

(define ecc-levels
  `(("low" . ,ECC-LOW) ("medium" . ,ECC-MEDIUM) ("quartile" . ,ECC-QUARTILE) ("high" . ,ECC-HIGH)))

(define quiet-zone 4)                   ; modules of white border, as the standard asks

(define* (qr-svg data ecc label #:key dark light)
  "An SVG QR code for DATA: one path, a unit square per dark module, over a
background rect.  DARK and LIGHT, if given, are CSS colours set as style
attributes over the black and white fill attributes."
  (define (colour c) (if c `((style ,(string-append "fill: " c))) '()))
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
             (class "qr") (role "img") (aria-label ,label)
             (shape-rendering "crispEdges"))
          (rect (@ (class "qr-light") (width "100%") (height "100%") (fill "#fff") ,@(colour light)))
          (path (@ (class "qr-dark") (d ,d) (fill "#000") ,@(colour dark))))))

(define* (qr-element attrs label #:key dark light)
  "What a QR component returns, given its ATTRS and LABEL: the data (first
word or :data), :ecc, and the label, with optional DARK and LIGHT colours."
  (let* ((data (main-attribute attrs 'data))
         (name (string-downcase (or (attribute attrs 'ecc) "low")))
         (ecc  (or (assoc-ref ecc-levels name)
                   (error (format #f ",~a: :ecc is low, medium, quartile or high, not ~a"
                                  (current-command) name)))))
    (qr-svg data ecc (if (null? label) data (plain label)) #:dark dark #:light light)))

(define-tag inline (qr . label) #:required-attributes attrs
  (qr-element attrs label))

(define-tag inline (qr-cmyk . label) #:required-attributes attrs
  (qr-element attrs label #:dark "device-cmyk(0 0 0 1)" #:light "device-cmyk(0 0 0 0)"))
