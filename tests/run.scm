;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright (C) 2026 Roger Farrell
;;;
;;; Regression tests:  guile -L . -L examples tests/run.scm
;;; Uses the generic (uq tags) and the example (recipe tags) components.
;;; Each case is markup -> expected HTML, or markup -> expected error text.

(use-modules (uq parse) (uq tags) (recipe tags) (sxml simple) (ice-9 textual-ports)
             (ice-9 match) (srfi srfi-1))
(set-port-encoding! (current-output-port) "UTF-8")

(define (render str)
  (with-output-to-string
    (lambda () (for-each sxml->xml (eval `(list ,@(markup-string->exprs str))
                                         (current-module))))))

(define (error-text str)
  (catch #t
    (lambda () (render str) #f)
    (lambda (key . args)
      (match args
        ((_ (? string? fmt) (? list? fargs) . _) (apply format #f fmt fargs))
        (_ (format #f "~a" key))))))

(define failures 0)
(define (check label got want)
  (unless (equal? got want)
    (set! failures (1+ failures))
    (format #t "FAIL ~a~%  want: ~s~%  got:  ~s~%" label want got)))

(define-syntax-rule (html markup want) (check markup (render markup) want))
(define-syntax-rule (fails markup want) (check markup (error-text markup) want))

;;; Commands and literal commas
(html "salt, pepper; salt,pepper; 1,000 jars" "<p>salt, pepper; salt,pepper; 1,000 jars</p>")
(html "a < b & ,strong[c]" "<p>a &lt; b &amp; <strong>c</strong></p>")
(fails "plain ,and word" "line 1: unknown command ,and")
(fails "Some text ,ul here" "line 1: ,ul has to start its own line")

;;; Inline extents
(html "Keep them ,em dry and cool.\nThen store." "<p>Keep them <em>dry and cool.</em> Then store.</p>")
(html "Place in a jar.\n,em Optional:\nVacuum seal." "<p>Place in a jar. <em>Optional:</em> Vacuum seal.</p>")
(html "a ,em b ,strong c d\ne" "<p>a <em>b <strong>c d</strong></em> e</p>")
(html "a ,frac 1/2 b ,frac 3/8 c" "<p>a ½ b <span class=\"frac\"><sup>3</sup>⁄<sub>8</sub></span> c</p>")
;; a word tag takes everything up to whitespace, punctuation included;
;; brackets are the way to leave punctuation outside
(html "add ,frac[1/2], then stir" "<p>add ½, then stir</p>")
(html "use ,strong[salt]. Then" "<p>use <strong>salt</strong>. Then</p>")

;;; Brackets
(html "a ,em[x [y] z] w" "<p>a <em>x [y] z</em> w</p>")
(html "its ,strong[store-bought\nbox]. Then seal it." "<p>its <strong>store-bought box</strong>. Then seal it.</p>")
(html "see [sic] here" "<p>see [sic] here</p>")
(fails "a ,em[never closed" "line 1: ,em[ is never closed with ]")

;;; Addresses
(html ",link[https://example.com/jars][,em[jar] guide]"
      "<p><a href=\"https://example.com/jars\"><em>jar</em> guide</a></p>")
(html ",link[https://example.com]" "<p><a href=\"https://example.com\">https://example.com</a></p>")
(html ",image[lid.svg][lid icon]" "<p><img src=\"lid.svg\" alt=\"lid icon\" /></p>")
(html "footnote ,em[one][1]" "<p>footnote <em>one</em>[1]</p>")
(html ",photo[jar.jpg][A jar]" "<figure class=\"photo\"><img src=\"jar.jpg\" alt=\"A jar\" /></figure>")
(fails "see ,link here" "line 1: ,link needs brackets: ,link[attributes][text]")

;;; Attributes: bare values in order, :key value pairs as attributes
(html ",link[:href https://example.com :title Sizes][jars]"
      "<p><a href=\"https://example.com\" title=\"Sizes\">jars</a></p>")
(html ",link[https://example.com :title Sizes][jars]"
      "<p><a href=\"https://example.com\" title=\"Sizes\">jars</a></p>")
(html ",image[lid.svg :width 40][lid]" "<p><img src=\"lid.svg\" alt=\"lid\" width=\"40\" /></p>")
(html ",photo[jar.jpg :width 3in :alt ignored][A jar]"
      "<figure class=\"photo\"><img src=\"jar.jpg\" alt=\"A jar\" width=\"3in\" /></figure>")
(fails "a ,image[lid.svg :width][lid]" "line 1: ,image: :width needs a value")
(check "parse attributes"
       (eval (car (markup-string->exprs ",link[a b :x 1 c][t]")) (current-module))
       '(p (a (@ (href "a") (x "1")) "t")))

;;; block:inline
(html ",title Cheeseburger\n  Helper" "<h1>Cheeseburger Helper</h1>")
(html ",title Cheese ,em[burger\n  Helper]" "<h1>Cheese <em>burger Helper</em></h1>")
(fails ",title Cheeseburger\n\n  Helper"
       "line 3: ,title holds one run of text; no blank lines or blocks inside it")

;;; block:block with the default wrap (p)
(html ",tip Keep it ,em dry\n  and cool.\n\n  More."
      "<aside class=\"tip\"><h3>Tip!</h3><p>Keep it <em>dry</em> and cool.</p><p>More.</p></aside>")
(html ",tip\n  text\n  ,ul\n    x\n  after"
      "<aside class=\"tip\"><h3>Tip!</h3><p>text</p><ul><li>x</li></ul><p>after</p></aside>")

;;; Lists: block:block with #:wrap li
(html ",ul\n  4 cups pasta\n\n  ½ cup milk\n  powder"
      "<ul><li>4 cups pasta</li><li>½ cup milk powder</li></ul>")
(html ",ul\n  one\n  ,ol\n    a\n\n    b\n\n  two"
      "<ul><li>one<ol><li>a</li><li>b</li></ol></li><li>two</li></ul>")
(html ",ul\n  ,li\n    para one\n\n    para two\n\n  plain"
      "<ul><li><p>para one</p><p>para two</p></li><li>plain</li></ul>")
(html ",ul first ,em[item]\n\n  second" "<ul><li>first <em>item</em></li><li>second</li></ul>")

;;; Scheme escape
(html "Total: ,(number->string (+ 2 3)) items" "<p>Total: 5 items</p>")

;;; (uq compile)
(use-modules (uq compile))
(let* ((dir (string-append (or (getenv "TMPDIR") "/tmp") "/uq-test-" (number->string (getpid))))
       (good (string-append dir "/good.uq"))
       (bad  (string-append dir "/bad.uq")))
  (mkdir dir)
  (with-output-to-file good (lambda () (display ",recipe\n  ,title Cheese ,em[Helper]\n")))
  (with-output-to-file bad  (lambda () (display ",recipe\n  ,titel oops\n")))
  (let ((out (with-output-to-string
               (lambda () (compile-uq good #:tags '((recipe tags)))))))
    (check "compile-uq reports" out (string-append good " -> " dir "/good.html\n")))
  (check "compile-uq page"
         (call-with-input-file (string-append dir "/good.html")
           (lambda (p) (set-port-encoding! p "UTF-8") (get-string-all p)))
         (string-append "<!DOCTYPE html>\n<html lang=\"en\"><head><meta charset=\"utf-8\" />"
                        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />"
                        "<title>Cheese Helper</title></head><body><article><div class=\"recipe\">"
                        "<h1>Cheese <em>Helper</em></h1></div></article></body></html>\n"))
  (let* ((err (open-output-string))
         (result (with-error-to-port err
                   (lambda () (compile-uq bad #:tags '((recipe tags)))))))
    (check "compile-uq error result" result #f)
    (check "compile-uq error message" (get-output-string err)
           (string-append bad ": line 2: unknown command ,titel\n"))
    (check "compile-uq writes nothing on error"
           (file-exists? (string-append dir "/bad.html")) #f))
  (for-each (lambda (f) (when (file-exists? f) (delete-file f)))
            (list good bad (string-append dir "/good.html")))
  (rmdir dir))

(if (zero? failures)
    (format #t "all tests passed~%")
    (begin (format #t "~a failed~%" failures) (exit 1)))
