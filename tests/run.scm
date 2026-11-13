;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright (C) 2026 Roger Farrell
;;;
;;; Regression tests:  guile -L . -L examples tests/run.scm
;;; Uses the generic (uq tags) and the example (recipe tags) components.
;;; Each case is markup -> expected HTML, or markup -> expected error text.

(use-modules (uq parse) (uq tags) (uq compile) (recipe tags) (sxml simple) (ice-9 textual-ports)
             (ice-9 match) (srfi srfi-1))
(set-port-encoding! (current-output-port) "UTF-8")

(use-modules (ice-9 match))

(define (render str)
  (with-output-to-string
    (lambda () (write-sxml (eval `(list ,@(markup-string->exprs str))
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
(fails "see ,link here" "line 1: ,link needs attributes: ,link[attributes][text]")

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
;; attribute blocks may omit the brackets: no attributes
(html ",steps\n  Fill the jar\n\n  Seal it" "<ol class=\"steps\"><li>Fill the jar</li><li>Seal it</li></ol>")
(html ",note Keep it dry.\n  Really." "<aside><p>Keep it dry. Really.</p></aside>")
;; #:attributes is optional, inline too: no brackets means no attributes
(define-tag inline (hl . body) #:attributes attrs `(mark (@ ,@(attribute-list attrs)) ,@body))
(html "a ,hl bright idea\nnext" "<p>a <mark>bright idea</mark> next</p>")
(html "a ,hl[:class x][bright] idea" "<p>a <mark class=\"x\">bright</mark> idea</p>")
;; #:required-attributes: the parser insists, with a line number
(fails "a ,link[][text]" "line 1: ,link needs attributes: ,link[attributes][text]")
(fails "x\n\n,photo A jar" "line 3: ,photo needs attributes: ,photo[attributes][text]")
(fails ",photo[][A jar]" "line 1: ,photo needs attributes: ,photo[attributes][text]")
(define-tag block (checklist . body) #:required-attributes attrs #:wrap li
  `(ul (@ ,@(attribute-list attrs)) ,@body))
(html ",checklist[:id pack]\n  Jar\n\n  Lid" "<ul id=\"pack\"><li>Jar</li><li>Lid</li></ul>")
(fails ",checklist\n  Jar" "line 1: ,checklist needs attributes: ,checklist[attributes][text]")
;; options that conflict are syntax errors
(for-each
 (lambda (form)
   (check (format #f "define-tag rejects ~s" (cddr form))
          (catch #t (lambda () (eval form (current-module)) #f) (lambda _ #t))
          #t))
 '((define-tag block (x . b) #:attributes a #:required-attributes c b)
   (define-tag block (x . b) #:wrap li #:wrap p b)))

;;; The default wrapper is declared (#:default-wrap), not built in
(define saved-wrap (@@ (uq parse) default-wrap))
(define (set-default-wrap! v) (module-set! (resolve-module '(uq parse)) 'default-wrap v))
(check "(uq tags) declares p as the default wrapper" saved-wrap 'p)
;; none declared: blocks still work, loose text is an error
(set-default-wrap! #f)
(fails ",ul\n  a\n\nLoose text" "line 4: text needs a wrapper, and none is declared (#:wrap or #:default-wrap)")
(fails ",tip\n  Inside a tip" "line 2: text needs a wrapper, and none is declared (#:wrap or #:default-wrap)")
(html ",ul\n  a" "<ul><li>a</li></ul>")
;; a project may declare its own; the latest declaration wins
(define-tag block:inline (para . body) #:default-wrap #t `(div (@ (class "para")) ,@body))
(html "Hello ,em[there]" "<div class=\"para\">Hello <em>there</em></div>")
(set-default-wrap! saved-wrap)
(check "define-tag rejects #:default-wrap on an inline tag"
       (catch #t (lambda () (eval '(define-tag inline (x . b) #:default-wrap #t b) (current-module)) #f)
         (lambda _ #t))
       #t)
(html "back to p" "<p>back to p</p>")

;;; Tables: ,tr wraps cells; #:wrap (td th) lets a lone ,th through;
;;; cells (#:wrap #f) keep their text inline
(html ",table\n  ,tr\n    ,th Ingredient\n\n    ,th Amount\n  ,tr\n    Pasta (small\n    shells)\n\n    4 ,em[cups]"
      "<table><tr><th>Ingredient</th><th>Amount</th></tr><tr><td>Pasta (small shells)</td><td>4 <em>cups</em></td></tr></table>")
(html ",table\n  ,tr\n    Spices\n    ,ul\n      salt\n\n      pepper\n\n    ,td to taste"
      "<table><tr><td>Spices<ul><li>salt</li><li>pepper</li></ul></td><td>to taste</td></tr></table>")
(html ",table\n  ,tr\n    ,td\n      one\n\n      two" "<table><tr><td>one two</td></tr></table>")
(html ",table\n  ,tr\n    ,td\n      ,em[a]\n\n      ,em[b]" "<table><tr><td><em>a</em> <em>b</em></td></tr></table>")
(html ",table\n  ,tr\n    ,td\n      ,p one\n\n      ,p two" "<table><tr><td><p>one</p><p>two</p></td></tr></table>")
;; loose text directly in a table is not wrapped (invalid HTML, by choice)
(html ",table\n  Oops" "<table><p>Oops</p></table>")
;; #:wrap values
(for-each
 (lambda (form)
   (check (format #f "define-tag rejects ~s" (cdddr form))
          (catch #t (lambda () (eval form (current-module)) #f) (lambda _ #t))
          #t))
 '((define-tag block:inline (x . b) #:wrap #f b)
   (define-tag block (x . b) #:wrap "td" b)
   (define-tag block (x . b) #:wrap () b)))

;;; Tables by column: ,columns pivots ,column blocks into rows
(html ",columns\n  ,column\n    ,th Ingredient\n\n    Pasta\n\n    Salt\n  ,column\n    ,th Amount\n\n    4 ,em[cups]\n\n    ,frac[1/2] tsp"
      (string-append "<table><tr><th>Ingredient</th><th>Amount</th></tr>"
                     "<tr><td>Pasta</td><td>4 <em>cups</em></td></tr>"
                     "<tr><td>Salt</td><td>½ tsp</td></tr></table>"))
(html ",columns\n  ,column\n    a\n\n    b\n  ,column\n    1"
      "<table><tr><td>a</td><td>1</td></tr><tr><td>b</td><td></td></tr></table>")
(html ",columns\n  ,column\n    Spices\n    ,ul\n      salt\n\n      pepper"
      "<table><tr><td>Spices<ul><li>salt</li><li>pepper</li></ul></td></tr></table>")
(html ",columns" "<table></table>")
(fails "Intro.\n\n,columns\n  Oops\n  ,column\n    a" "line 3: ,columns holds only ,column blocks")
(fails ",columns\n  ,tr\n    a" "line 1: ,columns holds only ,column blocks")

;;; write-sxml closes empty non-void elements (browsers misread <td />)
(check "write-sxml: empty elements"
       (with-output-to-string (lambda () (write-sxml '(div (table) (br) (p (@ (class "x"))) (img (@ (src "a")))))))
       "<div><table></table><br /><p class=\"x\"></p><img src=\"a\" /></div>")
(html "a ,em[] b" "<p>a <em></em> b</p>")

;;; current-command: the command the editor wrote, for messages
(define-tag inline (whoami . body) `(code ,(symbol->string (current-command))))
(html "a ,whoami[] b" "<p>a <code>whoami</code> b</p>")
(html "a ,em[,(symbol->string (current-command))] b" "<p>a <em>em</em> b</p>")
;; a component that calls another directly: errors name the outer command
(define-tag inline (email . body) #:required-attributes attrs (apply link attrs body))
(fails "a\n\nb ,email[a b][x]" "line 3: ,email takes one href; quote it if it has spaces")
(check "main-attribute: an explicit command name still wins"
       (catch #t (lambda () (main-attribute '() 'src 'custom) #f)
         (lambda (k . a) (uq-error-message k a)))
       ",custom needs its src: as the first word or as :src")

;;; define-tag reports bad options by name
(for-each
 (match-lambda
   ((form want)
    (check (format #f "define-tag error for ~s" (cdddr form))
           (catch #t (lambda () (eval form (current-module)) #f)
             (lambda (k . a) (let ((m (uq-error-message k a))) (and (string-prefix? want m) want))))
           want)))
 '(((define-tag block (x . b) #:wrapp li b) "unknown option #:wrapp")
   ((define-tag block (x . b) #:wrap) "#:wrap needs a value")
   ((define-tag block (x . b) #:wrap li) "x has no body")
   ((define-tag inline (x . b) #:attributes "a" b) "#:attributes takes a variable name")
   ((define-tag block (x . b) #:wrap li #:wrap p b) "#:wrap given twice")
   ((define-tag block (x . b) #:attributes a #:required-attributes c b)
    "only one of #:attributes and #:required-attributes")))

;;; ,toc and (uq compile post)
(use-modules (uq compile post))
(define-tag block:inline (section . b) `(h1 ,@b))
(define-tag block:inline (subsection . b) `(h2 ,@b))
(define (render-post str . passes)
  (with-output-to-string
    (lambda ()
      (write-sxml (fold (lambda (pass doc) (pass doc))
                        (eval `(list ,@(markup-string->exprs str)) (current-module))
                        passes)))))
(define toc2 (toc-pass '(h1 h2)))
(html ",toc Contents" "<nav class=\"toc\"><h1 class=\"toc-title\">Contents</h1><ol></ol></nav>")
(html ",toc" "<nav class=\"toc\"><ol></ol></nav>")
(check "toc-pass: nested entries, ids from text, duplicates made unique, own title skipped"
       (render-post ",toc Contents\n\n,section Ingredients\n,subsection Dry goods\n,section Ingredients" toc2)
       (string-append
        "<nav class=\"toc\"><h1 class=\"toc-title\">Contents</h1><ol>"
        "<li><a href=\"#ingredients\">Ingredients</a><ol><li><a href=\"#dry-goods\">Dry goods</a></li></ol></li>"
        "<li><a href=\"#ingredients-2\">Ingredients</a></li></ol></nav>"
        "<h1 id=\"ingredients\">Ingredients</h1><h2 id=\"dry-goods\">Dry goods</h2><h1 id=\"ingredients-2\">Ingredients</h1>"))
(check "toc-pass: link text without markup; the heading keeps it"
       (render-post ",toc\n\n,section Jar ,em[sizes]" toc2)
       (string-append "<nav class=\"toc\"><ol><li><a href=\"#jar-sizes\">Jar sizes</a></li></ol></nav>"
                      "<h1 id=\"jar-sizes\">Jar <em>sizes</em></h1>"))
(check "toc-pass: a heading with an id keeps it"
       (and (string-contains
             (render-post ",toc\n\n,(quote (h1 (@ (id \"top\")) \"Start\"))" toc2)
             "<li><a href=\"#top\">Start</a></li>") #t)
       #t)
(check "toc-pass: a new id avoids one already in the document"
       (and (string-contains
             (render-post ",toc\n\n,(quote (p (@ (id \"spices\")) \"x\"))\n\n,section Spices" toc2)
             "<h1 id=\"spices-2\">Spices</h1>") #t)
       #t)
(check "toc-pass: levels not listed are ignored; a leading h2 stays at the top"
       (render-post ",toc\n\n,subsection Early\n,section Main\n,(quote (h3 \"Deep\"))" toc2)
       (string-append "<nav class=\"toc\"><ol><li><a href=\"#early\">Early</a></li><li><a href=\"#main\">Main</a></li></ol></nav>"
                      "<h2 id=\"early\">Early</h2><h1 id=\"main\">Main</h1><p><h3>Deep</h3></p>"))
(check "toc-pass: every toc is filled"
       (length (find-elements (lambda (el) (eq? (element-tag el) 'li))
                              (toc2 (eval `(list ,@(markup-string->exprs ",toc A\n\n,section One\n\n,toc B"))
                                          (current-module)))))
       2)
(check "toc-pass: without a toc the document is unchanged"
       (render-post ",section One" toc2) "<h1>One</h1>")
(check "toc-pass: levels are the pass's own"
       (render-post ",toc\n\n,section One\n,subsection Two" (toc-pass '(h2)))
       "<nav class=\"toc\"><ol><li><a href=\"#two\">Two</a></li></ol></nav><h1>One</h1><h2 id=\"two\">Two</h2>")
;; toc-pass with a procedure: #f skips, a number is the level, #t is level 0
(define-tag block:inline (chapter . b) `(h1 (@ (class "chapter")) ,@b))
(check "toc-pass: a predicate choosing elements and levels"
       (render-post ",toc\n\n,chapter One\n,section Not listed\n,(quote (p (@ (class \"key\")) \"Key point\"))\n\n,chapter Two"
                    (toc-pass (lambda (el) (cond ((has-class? el "chapter") 0)
                                                 ((has-class? el "key") 1)
                                                 (else #f)))))
       (string-append
        "<nav class=\"toc\"><ol><li><a href=\"#one\">One</a><ol><li><a href=\"#key-point\">Key point</a></li></ol></li>"
        "<li><a href=\"#two\">Two</a></li></ol></nav>"
        "<h1 class=\"chapter\" id=\"one\">One</h1><h1>Not listed</h1><p><p class=\"key\" id=\"key-point\">Key point</p></p>"
        "<h1 class=\"chapter\" id=\"two\">Two</h1>"))
(check "toc-pass: #t lists flat"
       (render-post ",toc\n\n,section A\n,subsection B" (toc-pass (lambda (el) (and (memq (element-tag el) '(h1 h2)) #t))))
       (string-append "<nav class=\"toc\"><ol><li><a href=\"#a\">A</a></li><li><a href=\"#b\">B</a></li></ol></nav>"
                      "<h1 id=\"a\">A</h1><h2 id=\"b\">B</h2>"))
(check "has-class?"
       (map (lambda (el) (has-class? el "toc"))
            '((nav (@ (class "toc"))) (nav (@ (class "main toc wide"))) (nav (@ (class "menu"))) (nav) (nav (@ (class "tocx")))))
       '(#t #t #f #f #f))
(check "toc-pass: a nav with another class is not a toc"
       (render-post ",(quote (nav (@ (class \"menu\")) (ol)))\n\n,toc\n\n,section A" toc2)
       (string-append "<p><nav class=\"menu\"><ol></ol></nav></p><nav class=\"toc\"><ol><li><a href=\"#a\">A</a></li></ol></nav>"
                      "<h1 id=\"a\">A</h1>"))
(check "toc-pass: a bad return value is an error"
       (catch #t (lambda () (render-post ",toc\n\n,section A" (toc-pass (lambda (el) "x"))) #f)
         (lambda (k . a) (uq-error-message k a)))
       "toc-pass: the procedure returned \"x\" for <h1>; give #f, #t or a level number")

(check "slugify" (map slugify '("Dry goods" "Crème brûlée!" "  " "3 Steps")) '("dry-goods" "crème-brûlée" "section" "3-steps"))

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

;;; ,(expr) escapes
(html "Total: ,(+ 2 3) items" "<p>Total: 5 items</p>")
(html "x ,(> 1 2) y" "<p>x  y</p>")
(html "See ,(em \"this\") and ,(quote (b \"that\"))" "<p>See <em>this</em> and <b>that</b></p>")
(html "x ,em[a ,(string-append \"]\" \"[\") c] y" "<p>x <em>a ][ c</em> y</p>")
(html "x ,(string-append \"a\"\n  \"b\") y" "<p>x ab y</p>")
(fails "Some text.\n\nx ,(string-append \"a\" y" "line 3: ,( is not a complete Scheme expression (unbalanced parentheses?)")
(fails ",tip\n  x ,em[,(car \"a\" y]" "line 2: ,( is not a complete Scheme expression (unbalanced parentheses?)")

;;; Errors while evaluating carry the line they came from
;; (Guile words its own errors differently when compiled, so for those
;; only the line is checked.)
(define (fails-at markup prefix)
  (let ((msg (error-text markup)))
    (check markup (and msg (string-prefix? prefix msg)) #t)))
(fails-at "Some text.\n\nx ,(car '()) y" "line 3: ")
(fails "x\n\n,photo[:width 3in][A jar]" "line 3: ,photo needs its src: as the first word or as :src")
;; main-attribute: the main value as a word or as its key, once
(html ",link[:href https://example.com][x]" "<p><a href=\"https://example.com\">x</a></p>")
(html ",image[:src lid.svg][lid]" "<p><img src=\"lid.svg\" alt=\"lid\" /></p>")
(fails "a ,link[https://a :href https://b][x]" "line 1: ,link: give the href once, as a word or as :href")
(fails "a ,link[https://a extra][x]" "line 1: ,link takes one href; quote it if it has spaces")
(fails "a ,image[:width 40][lid]" "line 1: ,image needs its src: as the first word or as :src")
(fails-at ",tip\n  a\n  b ,em[x ,(car '())]" "line 3: ")
(fails-at ",tip\n  a ,em[x\n  y ,(vector-ref (vector) 0)]" "line 3: ")
(check "strip-line-wrappers shows the plain expression"
       (map strip-line-wrappers (markup-string->exprs "a ,em[b] ,(+ 1 2)"))
       '((p "a " (em "b") " " (+ 1 2))))

;;; .uq files are UTF-8 whatever the locale (the tests may run under C)
(let* ((dir  (string-append (or (getenv "TMPDIR") "/tmp") "/uq-utf8-" (number->string (getpid))))
       (in   (string-append dir "/enc.uq"))
       (out  (string-append dir "/enc.html")))
  (mkdir dir)
  (call-with-output-file in (lambda (p) (display "Penzey\u2019s cr\u00e8me\n" p)) #:encoding "UTF-8")
  (with-output-to-string (lambda () (compile-uq in)))
  (check "compile-uq reads .uq files as UTF-8"
         (call-with-input-file out get-string-all #:encoding "UTF-8")
         "<p>Penzey\u2019s cr\u00e8me</p>\n")
  (delete-file in) (delete-file out) (rmdir dir))

;;; Markup embedded in a Scheme file reports that file and its lines
(let* ((dir  (string-append (or (getenv "TMPDIR") "/tmp") "/uq-embed-" (number->string (getpid))))
       (file (string-append dir "/embedded.scm"))
       (out  (string-append dir "/out")))
  (mkdir dir)
  (call-with-output-file file
    (lambda (p)
      (display (string-append
                "(use-modules (uq parse) (uq tags))\n"                    ; 1
                "(eval-when (expand load eval) (enable-markup-reader!))\n" ; 2
                "(define (try thunk)\n"                                    ; 3
                "  (catch #t thunk (lambda (k . a) (display (uq-error-message k a)) (newline))))\n"
                "(try (lambda () (markup \"x ,(car '()) y\")))\n"          ; 5
                "(try (lambda ()\n"                                        ; 6
                "  (markup\n"                                              ; 7
                "   \"first\n"                                            ; 8
                "second ,(car '())\")))\n"                                ; 9
                "(try (lambda () #m<<END\n"                                ; 10
                "one\n"                                                    ; 11
                "two ,(vector-ref (vector) 0)\n"                           ; 12
                "END\n"
                "))\n")
               p)))
  (with-output-to-file out (lambda () (load file)))
  (let ((lines (string-split (string-trim-right (call-with-input-file out get-string-all)) #\newline)))
    (check "embedded: three errors reported" (length lines) 3)
    (for-each (lambda (line want)
                (check (string-append "embedded: " want) (string-prefix? (string-append file ": " want) line) #t))
              lines '("line 5: " "line 9: " "line 12: ")))
  (delete-file file) (delete-file out) (rmdir dir))

;;; uq-error-message never fails on an unusual error shape
(use-modules (uq compile))
(check "uq-error-message: syntax error"
       (uq-error-message 'syntax-error '(#f "definition in expression context, where definitions are not allowed,"
                                         ((line . 0)) (define x 1) #f))
       "definition in expression context, where definitions are not allowed (define x 1)")
(check "uq-error-message: format mismatch falls back"
       (uq-error-message 'misc-error '(#f "~a ~a" (1)))
       "misc-error: (#f \"~a ~a\" (1))")

;;; ,(expr) with non-ASCII text inside
(html "½ ,(string-append \"é\" \"½\") after" "<p>½ é½ after</p>")
(check "parse attributes"
       (strip-line-wrappers (car (markup-string->exprs ",link[a b :x 1 c][t]")))
       '(p (link '("a" "b" (x "1") "c") "t")))

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

;;; (uq qr): optional, tested only when Guile-QR-Code is installed
(if (false-if-exception (resolve-interface '(qr-code)))
    (begin
      (use-modules (uq qr))
      (let ((svg (lambda (src) (render src))))
        (check "qr: an inline svg with its label"
               (and (string-contains (svg "Scan ,qr[https://example.com/r][the recipe].")
                                     "<p>Scan <svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 33 33\" class=\"qr\" role=\"img\" aria-label=\"the recipe\"")
                    #t)
               #t)
        (check "qr: colour hooks are classes over default fills"
               (and (string-contains (svg ",qr[0123456789]") "<rect class=\"qr-light\" width=\"100%\" height=\"100%\" fill=\"#fff\"></rect>")
                    (string-contains (svg ",qr[0123456789]") "<path class=\"qr-dark\" d=\"")
                    #t)
               #t)
        (check "qr: no <style> and no colour styles"
               (or (string-contains (svg ",qr[0123456789]") "<style")
                   (string-contains (svg ",qr[0123456789]") "style=\""))
               #f)
        (check "qr-cmyk: CMYK style attributes over the black and white fills"
               (and (string-contains (svg ",qr-cmyk[0123456789]")
                                     "<rect class=\"qr-light\" width=\"100%\" height=\"100%\" fill=\"#fff\" style=\"fill: device-cmyk(0 0 0 0)\"></rect>")
                    (string-contains (svg ",qr-cmyk[0123456789]")
                                     "fill=\"#000\" style=\"fill: device-cmyk(0 0 0 1)\"></path>")
                    #t)
               #t)
        (check "qr-cmyk: same code as qr"
               (and (string-contains (svg ",qr-cmyk[0123456789]")
                                (let* ((s (svg ",qr[0123456789]")) (i (string-contains s " d=\"")))
                                  (substring s i (string-index s #\" (+ i 4)))))
                    #t)
               #t)
        (fails "a ,qr-cmyk[x :ecc best]" "line 1: ,qr-cmyk: :ecc is low, medium, quartile or high, not best")
        (check "qr: the data is the label when there is none"
               (and (string-contains (svg ",qr[0123456789]") "aria-label=\"0123456789\"") #t) #t)
        (check "qr: :ecc changes the code"
               (equal? (svg ",qr[https://example.com/r :ecc high]") (svg ",qr[https://example.com/r]"))
               #f)
        (check "qr: :ecc is case-insensitive"
               (svg ",qr[https://example.com/r :ecc HIGH]") (svg ",qr[https://example.com/r :ecc high]"))
        (check "qr: quoted data with spaces"
               (and (string-contains (svg ",qr[\"WIFI:S:Kitchen Net;;\"][wifi]") "aria-label=\"wifi\"") #t) #t))
      (fails "x\n\n,qr[x :ecc best]" "line 3: ,qr: :ecc is low, medium, quartile or high, not best")
      (fails ",qr[:ecc high]" "line 1: ,qr needs its data: as the first word or as :data")
      (check "qr: :data works like the first word"
             (render ",qr[:data https://example.com/r]") (render ",qr[https://example.com/r]"))
      (fails ",qr[x :data y]" "line 1: ,qr: give the data once, as a word or as :data")
      (fails ",qr[WIFI:S:Kitchen Net;;]" "line 1: ,qr takes one data; quote it if it has spaces")
      (fails ",qr" "line 1: ,qr needs attributes: ,qr[attributes][text]"))
    (format #t "skipping (uq qr) tests: Guile-QR-Code is not installed~%"))

(if (zero? failures)
    (format #t "all tests passed~%")
    (begin (format #t "~a failed~%" failures) (exit 1)))
