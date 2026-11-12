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

;;; #:wrap and #:attributes together
(define-tag block (steps . body) #:wrap li #:attributes attrs
  `(ol (@ (class "steps") ,@(attribute-list attrs)) ,@body))
(define-tag block (note . body) #:attributes attrs #:wrap p
  `(aside (@ ,@(attribute-list attrs)) ,@body))
(html ",steps[:id fill]\n  Fill the jar\n\n  Seal it"
      "<ol class=\"steps\" id=\"fill\"><li>Fill the jar</li><li>Seal it</li></ol>")
(html ",steps[][Fill the jar]\n\n  Seal it"
      "<ol class=\"steps\"><li>Fill the jar</li><li>Seal it</li></ol>")
(html ",note[:id keep][Keep it dry.]\n  Really.\n\n  Second."
      "<aside id=\"keep\"><p>Keep it dry. Really.</p><p>Second.</p></aside>")
(fails ",steps\n  x" "line 1: ,steps needs brackets: ,steps[attributes][text]")

;;; Quoted attribute values
(html ",link[https://example.com :title \"Jar sizes for every recipe\"][jars]"
      "<p><a href=\"https://example.com\" title=\"Jar sizes for every recipe\">jars</a></p>")
(html ",photo[\"jar photo.jpg\" :width 3in][A jar]"
      "<figure class=\"photo\"><img src=\"jar photo.jpg\" alt=\"A jar\" width=\"3in\" /></figure>")
(html ",link[:title \"say \\\"hi\\\" [1]\" https://example.com][x]"
      "<p><a href=\"https://example.com\" title=\"say &quot;hi&quot; [1]\">x</a></p>")
(html ",link[\":not-a-key\" :title t][x]" "<p><a href=\":not-a-key\" title=\"t\">x</a></p>")
;; a quoted value may not wrap; words of the bracket may
(fails "see ,link[\"https://example.com/a/very/\n  long/path\"][it]"
       "line 1: ,link: a \" is not closed on the same line")
(fails "see ,link[\"https://example.com/a\\\n  b\"][it]"
       "line 1: ,link: a \" is not closed on the same line")
(html "see ,link[https://example.com\n  :title \"Jar sizes\"][it]"
      "<p>see <a href=\"https://example.com\" title=\"Jar sizes\">it</a></p>")
;; quotes are only special in attribute brackets (sxml->xml escapes them)
(html "a \"quoted\" word ,em[and \"this\"]" "<p>a &quot;quoted&quot; word <em>and &quot;this&quot;</em></p>")
(fails "a ,link[:title \"never closed][x]" "line 1: ,link: a \" is not closed on the same line")
(fails ",tip\n  First line\n  see ,link[:title \"never\n  closed][x]" "line 3: ,link: a \" is not closed on the same line")
(fails "a ,link[:title :href x][y]" "line 1: ,link: :title needs a value")

;;; ,(expr) with non-ASCII text inside
(html "½ ,(string-append \"é\" \"½\") after" "<p>½ é½ after</p>")
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
  (with-output-to-file good (lambda () (display ",title Cheese ,em[Helper]\n\nSome text.\n")))
  (with-output-to-file bad  (lambda () (display ",recipe\n  ,titel oops\n")))
  (let ((out (with-output-to-string
               (lambda () (compile-uq good #:tags '((recipe tags)))))))
    (check "compile-uq reports" out (string-append good " -> " dir "/good.html\n")))
  (define (slurp f) (call-with-input-file f (lambda (p) (set-port-encoding! p "UTF-8") (get-string-all p))))
  (check "compile-uq writes a fragment by default"
         (slurp (string-append dir "/good.html"))
         "<h1>Cheese <em>Helper</em></h1><p>Some text.</p>\n")
  (check "page-title in a list of nodes" (page-title '((p "x") (h1 "T") (h1 "U"))) "T")
  (check "page-title, h1 first" (page-title '((h1 "T") (p "x"))) "T")
  (with-output-to-string
    (lambda ()
      (compile-uq good #:tags '((recipe tags))
                  #:page (lambda (doc) `(html (head (title ,(page-title doc))) (body (main ,@doc))))
                  #:indent #t)))
  (check "compile-uq #:page with an html root adds a doctype"
         (slurp (string-append dir "/good.html"))
         (string-append "<!DOCTYPE html>\n<html>\n  <head>\n    <title>Cheese Helper</title>\n  </head>\n"
                        "  <body>\n    <main>\n      <h1>Cheese <em>Helper</em></h1>\n"
                        "      <p>Some text.</p>\n    </main>\n  </body>\n</html>\n"))
  (with-output-to-string
    (lambda () (compile-uq good #:tags '((recipe tags)) #:page (lambda (doc) `(section ,@doc)))))
  (check "compile-uq #:page with another root stays a fragment"
         (slurp (string-append dir "/good.html"))
         "<section><h1>Cheese <em>Helper</em></h1><p>Some text.</p></section>\n")
  ;; a later #:tags module may redefine a generic component, without warnings
  (mkdir (string-append dir "/override"))
  (with-output-to-file (string-append dir "/override/tags.scm")
    (lambda ()
      (write '(define-module (override tags) #:use-module (uq parse)))
      (write '(define-tag inline (em . body) `(i ,@body)))))
  (add-to-load-path dir)
  (let* ((err (open-output-string))
         (out (with-error-to-port err
                (lambda () (with-output-to-string
                             (lambda () (compile-uq good #:tags '((recipe tags) (override tags)))))))))
    (check "compile-uq: later tags module wins" (slurp (string-append dir "/good.html"))
           "<h1>Cheese <i>Helper</i></h1><p>Some text.</p>\n")
    (check "compile-uq: override is quiet" (get-output-string err) ""))

  (let* ((err (open-output-string))
         (result (with-error-to-port err
                   (lambda () (compile-uq bad #:tags '((recipe tags)))))))
    (check "compile-uq error result" result #f)
    (check "compile-uq error message" (get-output-string err)
           (string-append bad ": line 2: unknown command ,titel\n"))
    (check "compile-uq writes nothing on error"
           (file-exists? (string-append dir "/bad.html")) #f))
  (for-each (lambda (f) (when (file-exists? f) (delete-file f)))
            (list good bad (string-append dir "/good.html") (string-append dir "/override/tags.scm")))
  (rmdir (string-append dir "/override"))
  (rmdir dir))

;;; write-sxml: indentation only where it cannot change rendering
(let ((doc '(body
             (p (em "a") (strong "b"))
             (div (span "x") (span "y") (p "block") (span "z"))
             (ul (li (a (@ (href "#")) "only link")) (li "text ,em" (em "x")))
             (pre (code "  keep\n    this"))
             (div (@ (title "a \"q\" <v> & m")) (p "esc")))))
  (check "write-sxml without #:indent is sxml->xml"
         (with-output-to-string (lambda () (write-sxml doc)))
         (with-output-to-string (lambda () (sxml->xml doc))))
  (check "write-sxml #:indent"
         (with-output-to-string (lambda () (write-sxml doc #:indent #t)))
         (string-append
          "<body>\n"
          "  <p>\n    <em>a</em><strong>b</strong>\n  </p>\n"
          "  <div>\n    <span>x</span><span>y</span>\n    <p>block</p>\n    <span>z</span>\n  </div>\n"
          "  <ul>\n    <li>\n      <a href=\"#\">only link</a>\n    </li>\n    <li>text ,em<em>x</em></li>\n  </ul>\n"
          "  <pre><code>  keep\n    this</code></pre>\n"
          "  <div title=\"a &quot;q&quot; &lt;v&gt; &amp; m\">\n    <p>esc</p>\n  </div>\n"
          "</body>"))
  (check "write-sxml #:indent 4"
         (with-output-to-string (lambda () (write-sxml '(ul (li "a")) #:indent 4)))
         "<ul>\n    <li>a</li>\n</ul>"))

(if (zero? failures)
    (format #t "all tests passed~%")
    (begin (format #t "~a failed~%" failures) (exit 1)))
