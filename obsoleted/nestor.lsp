#!/usr/bin/env newlisp

;; @module Nestor
;; @author cormullion
;; @description experimental newLISP source code scanner and syntax colourizer
;; now obsolete 2011-09-16 18:08:26
;; @version 0.3 beta 2010-09-29 18:09:49 lutz broke my code again - 'read'
;; @version 0.2 beta 2009-11-18 17:10:05
;; @version 0.1 beta 2008-11-19 17:38:11
;;<h4>About this module</h4>
;;<p>The Nestor module scans newLISP source code and stores it
;; in a hierarchical list suitable for further processing.</p>
;;<p>It requires > newLISP 9.9 and a UTF8 system.</p>
;;<p>To-Do: commas in decimal numbers; weird bracketed symbol names 
;; and generally more compatibility with the newLISP reader. :)</p>
;;<p>this version can generate rainbow code listings</p>
;;<p><b>Usage</b></p>
;;
;;<p>To scan a file:</p>
;;<pre>
;;(load {nestor.lsp})
;;(set 'file {test.lsp})
;;(set 'nlx (Nestor:read-from-file file))
;;</pre>
;;<p>and to scan a string:</p>
;;<pre>
;;(set 's {(set 'x (+ 2 2)})
;;(Nestor:read-s s)
;;=> 
;; ((("open-paren" "(") 
;;   ("symbol" "set") 
;;   ("whitespace" "IA==") 
;;   ("quote" "'") 
;;   ("symbol" "x") 
;;   ("whitespace" "IA==") 
;;   (("open-paren" "(") 
;;    ("symbol" "+") 
;;    ("whitespace" "IA==")
;;    ("integer" 2) 
;;    ("whitespace" "IA==")
;;    ("integer" 2) 
;;    ("close-paren" ")"))))
;;
;;(Nestor:read-s {"this is a multi-line
;;string"})
;;=>
;; (("quoted-string" "this is a multi-line\nstring"))
;;</pre>
;;<p>The result in both cases is a <b>newLISP-expression</b> or <b>nlx</b>.</p>
;;
;;<h4>Processing an nlx (newLISP expression)</h4>
;;
;;<p>To reverse the process and convert an nlx list to text:</p>
;;<pre>
;;(Nestor:nlx-to-text nlx)
;;</pre>
;;
;;<p>To output an nlx in HTML:</p>
;;<pre>
;;(Nestor:nlx-to-html nlx mode)
;;</pre>
;;<p>where mode is either 'page' (a complete HTML page), 'css' (a code section which includes suitable css style definitions) or default (just a code section, no css definitions)</p>
;;<p>To output an nlx in ConTeXt (TEX) format:</p>
;;<pre>
;;(Nestor:nlx-to-context nlx)
;;</pre>
;;<p>

(define nestor-version "0.0.2")

(context 'Stack)
(define stack '()) ; the stack
(define sp '(0))   ; the stack pointer list

(define (Stack:Stack i) ; put i at end of stack
  ; increase stack pointer
  (push i stack sp)
  ;(setf (sp -1) (+ 1 (sp -1)))
  (setf (sp -1) (+ 1 $it)))

(define (Stack:init) ; init or reset
  (set 'stack '() 'sp '(0)))

(define (Stack:inc-nesting)
  (push 0 sp -1)
  (push '() stack sp))

(define (Stack:dec-nesting)
  (pop sp -1)
  (setf (sp -1) (+ 1 (last sp))))

(context 'Nestor)
(define syntax-colouring-ready nil) ; to remember whether the syntax stuff has run

(define (get-next-char)
 (let ((nch ""))
   (if (< *cursor* *source-length*)
       (begin
          (set 'nch (source-string *cursor*))
          (inc *cursor* (utf8len nch)))
       (set 'nch nil))
   nch))

(define (peek-char)
   (let ((pch ""))
   (if (< *cursor* *source-length*)
       (set 'pch (source-string *cursor*))
       (set 'pch nil))))

(define (char-identifier? c)
  ; TODO A symbol name starting with [ (left square bracket) and ending with ] 
  ; (right square bracket) may contain any character except the right square bracket.
  (not (find (lower-case (string c)) { "':,()})))

(define (char-identifier-first? c)
  (not (find (lower-case (string c)) [text] #;"'(){}.0123456789[/text])))

(define (char-numeric-first? c)
   (find c {+-.0123456789}))

(define (char-numeric? c)
   (find c {0123456789+-.xXabcdefABCDEF}))

(define (char-whitespace? c)
  (or (= c " ") (= c "\n") (= c "\t")))

(define (open-paren-token)
  (Stack:inc-nesting)
  (Stack (list "open-paren" "(")))

(define (close-paren-token)
  (Stack (list "close-paren" ")"))
  (Stack:dec-nesting))

(define (read-comment c)
  (let ((res c) (ch ""))
     (while (and (!= (set 'ch (get-next-char)) "\n") ch)
        (push ch res -1))
    (Stack (list "comment" res))))
    
(define (read-identifier c)
  (let ((res c) (ch ""))
   ; look for end of identifier
    (while (and (not (find (set 'ch (peek-char)) " \"',()\n\t\r")) (!= ch nil))
      (push (get-next-char) res -1))
    (Stack (list "symbol" res))))

(define (read-number-scanner list-so-far)
    (let ((next-char (peek-char)))
      ;; if next-char is a digit then call self recursively
      (if (and (char-numeric? next-char) next-char)
            (read-number-scanner (cons (get-next-char) list-so-far))
            (reverse list-so-far))))

(define (precise-float str)
; more faithful to original format than newLISP's float
  (let ((p "") (q ""))
    (map set '(p q) (parse str "."))
    (append p "." q)))

(define (read-number c)
  (let ((res '() number-as-string ""))
     (set 'number-as-string (join (read-number-scanner (list c))))
     (cond
       ; try hex first
       ((starts-with (lower-case number-as-string) "0x")
          (set 'res (list "hex" number-as-string)))
       ; float?
       ((find "." number-as-string)
          ; float function isn't quite what we want here     
          (set 'res (list "float" (precise-float number-as-string))))
       ; octal and not hex or float?
       ; 017 is OK, 019 is read as 10
       ((and (starts-with (lower-case number-as-string) "0") 
             (> (length number-as-string) 1)
             (empty? (difference (explode number-as-string) (explode "01234567"))))
          (set 'res (list "octal" number-as-string)))
       ; perhaps an integer?
       ; 019 is read as 19 ...
       ((integer? (int number-as-string 0 10))
         (set 'res (list "integer" (int number-as-string 0 10))))
       ; give up
       (true
         (set 'res (list "string" "NaN"))))
  (Stack res)))

(define (read-quote)
   (Stack (list "quote" "'")))

(define (read-quoted-string)
  (let ((res {}) (ch {}))
     (while (and (!= (set 'ch (get-next-char)) {"}) ch)
        (push ch res -1)
        ; check for backslashed quotes
        (when (= ch {\}) 
              (set 'ch (get-next-char))
              (push ch res -1)))
    (Stack (list "quoted-string" res))))

(define (read-braced-string)
  (let ((res "") (ch {}) (level 1)) 
  ; we've already seen the first { so we're up to level 1
     (while (> level 0)
         (set 'ch (get-next-char))
         (if (= ch "{") (inc level))
         (if (= ch "}") (dec level))
         (if (or (< level 0) (= ch nil)) (throw-error (string "error in a braced string at character " *cursor*)))
         ; don't push final "}"
         (if (and (> level 0)) (push ch res -1))
         )
    (Stack (list "braced-string" res))))

(define (read-bracketed-string ch)
  (let ((res "") (ch {}))
 ; TODO: brackets could also be that weird syntax for symbol names or CMD
 ; TODO: add checks for safety, in case lookahead or behind causes an error...?
    (if (= (lower-case (slice source-string (- *cursor* 1) 6)) "[text]")
       (begin
         (inc *cursor* 5)
         ; look for end
         (while (and  (< *cursor* (- *source-length* 7)) 
                      (!= (lower-case ( *cursor* 7 source-string)) "[/text]")
                      ch)
                (push (get-next-char) res -1))
         (inc *cursor* 7)
         (Stack (list "bracketed-string" res))))))

(define (read-whitespace ch)
  (let ((res ch))
     (while (find (set 'ch (peek-char)) " \n\t")
        (push (get-next-char) res -1))
    (Stack (list "whitespace" (base64-enc res)))))

(define (read-token)
 (let ((first-char (get-next-char)))
    (if first-char
      (cond 
            ; a - or + could be the start of a symbol or a number, so look at the next char
            ((or (= first-char "-") (= first-char "+"))
                (if (find (peek-char) "0123456789")
                    (read-number first-char)
                    (read-identifier first-char)))
            ((char-whitespace? first-char)   
               (read-whitespace first-char))
            ((= first-char {(})
               (open-paren-token))
            ((= first-char {)})
               (close-paren-token))
            ((= first-char {#})
               (read-comment first-char))
            ((= first-char {;})
               (read-comment first-char))
            ((= first-char {"})
               (read-quoted-string))
            ((= first-char "{")
               (read-braced-string))
            ((= first-char "[")
               (read-bracketed-string first-char))
            ((= first-char {'})
               (read-quote))
            ((char-numeric-first? first-char)
               (read-number first-char))
            ((char-identifier-first? first-char)
               (read-identifier first-char))
            (true (throw-error (string ">" first-char "< is an unrecognized token")))))))

;; @syntax (read-s <source-string>)
;; @return Returns a nested list representing the newLISP source code in source-string

(define (read-s source-string)
 (Stack:init)
 (set '*cursor* 0 '*source-length* (utf8len source-string))
 (while (< *cursor* *source-length*)
      (read-token))
 Stack:stack)

;; @syntax (read-from-file <file>)
;; @return Returns a nested list representing the newLISP source code in file

(define (read-from-file file)
  (Stack:init)
  (set 'source-string (read-file file))
  (set '*cursor* 0 '*source-length* (utf8len source-string))
  (while (< *cursor* *source-length*)
     (read-token))
  Stack:stack)

(define (nlx-to-text-helper nlxp)
 ; given nlx, create plain text version
 (dolist (i nlxp)
    (if (atom? (first i))
       (begin
         (cond 
           ((= (first i) "symbol") 
                (write-buffer buff (string (last i))))
           ((= (first i) "open-paren") 
                (write-buffer buff (string  {(})))
           ((= (first i) "close-paren") 
                (write-buffer buff (string  {)})))
           ((= (first i) "whitespace") 
                (dostring (s (base64-dec (last i))) 
                  (write-buffer buff (string  (char s)))))
           ((= (first i) "braced-string")
                (write-buffer buff (string  "{" (last i) "}")))
           ((= (first i) "quoted-string")
                (write-buffer buff (string  {"} (last i) {"})))
           ((= (first i) "bracketed-string")
                (write-buffer buff (string  {[text]} (last i) {[/text]})))
           ((= (first i) "quote")
                (write-buffer buff (string  "'")))
           ((= (first i) "comment")
                (write-buffer buff (string (last i) "\n")))
           ((= (first i) "integer")
                (write-buffer buff (string  (int (last i)))))
           ((= (first i) "float")
                (write-buffer buff (string  (precise-float (last i)))))
           ((= (first i) "hex")
                (write-buffer buff (string (last i))))
           ((= (first i) "octal")
                (write-buffer buff (string (last i))))
           )
         )
       ; not an atom, recurse
       (nlx-to-text-helper i))))

;; @syntax (nlx-to-text <nlx>)
;; @return Returns a string of the source code stored in newLISP expression list nlx

(define (nlx-to-text nlx)
  (let ((buff ""))
      (nlx-to-text-helper nlx)
      buff))

(define (nlx-to-ascii nlx (level -1))
 (cond 
   ((atom? (first nlx))
      (print (dup " " (* level 2)))
      (println nlx))
   ((list? nlx)
      (print "(")
      (nlx-to-ascii (first nlx) (+ level 1))
      (dolist (s (rest nlx)) (nlx-to-ascii s (+ level 1)))))
      (if (= level -1) (print ")")))
      
(define (escape-html txt)
 (if txt
  (begin
   (replace {&} txt {&amp;} 0)
   (replace {<} txt {&lt;} 0)
   (replace {>} txt {&gt;} 0)))
 txt)

(define (set-up-syntax)
; set up the various lists required for syntax-colouring
; we might not need this so do it only when required
  (if-not syntax-colouring-ready
     (begin
     (set 'css-def [text]<style>
pre, code
{
	font-family: Monaco, 'Andale Mono', 'Lucida Console', monospace;
  font-size: 10pt;
  /* http://users.tkk.fi/~tkarvine/pre-wrap-css3-mozilla-opera-ie.html */
	/* css-3 */
	/* Mozilla, since 1999 */
	/* Opera 4-6 */
	white-space: -o-pre-wrap;
	/* Opera 7 */
	word-wrap: break-word;
	/* Internet Explorer 5.5+ */
}

body {color: #ccc; background-color: #222; }

.sym  {	color:	#3300ff;	 }
.built-in  {	color:	#660044;	font-weight: bold }
.obsolete  {	color:	#ffff00; background:	#411;}
.variable  {	color:	#880077; }
.open-paren	  {	color:	#777777;	}	 
.close-paren	  {	color:	#777777;	}                        
.braced-string	  {	color:	#226666;  }	                      
.quoted-string	  {	color:	#226666;  }
.bracketed-string	  {	color:	#226666;  }
.quote	  {	color:	#220000;	}	 
.comment	  {	color:	#334455;	 font-family: serif;} 
.integer	  {	color:	#113366;}	 
.float	  {	color:	#335533;}
.hex	  {	color:	#336633;}
.octal	  {	color:	#336699;}

span.open-paren1 { background-color : inherit; -webkit-transition: background-color 0.3s linear; }
span.open-paren1:hover { color : inherit; background-color : #BAFFFF; }
span.open-paren2 { background-color : inherit; -webkit-transition: background-color 0.3s linear; }
span.open-paren2:hover { color : inherit; background-color : #FFCACA; }
span.open-paren3 { background-color : inherit; -webkit-transition: background-color 0.3s linear; }
span.open-paren3:hover { color : inherit; background-color : #FFFFBA; }
span.open-paren4 { background-color : inherit; -webkit-transition: background-color 0.3s linear; }
span.open-paren4:hover { color : inherit; background-color : #CACAFF; }
span.open-paren5 { background-color : inherit; -webkit-transition: background-color 0.3s linear; }
span.open-paren5:hover { color : inherit; background-color : #CAFFCA; }
span.open-paren6 { background-color : inherit; -webkit-transition: background-color 0.3s linear; }
span.open-paren6:hover { color : inherit; background-color : #FFBAFF; }
span.open-paren7 { background-color : inherit; -webkit-transition: background-color 0.3s linear; }
span.open-paren7:hover { color : inherit; background-color : #FFBCFF; }
span.open-paren8 { background-color : inherit; -webkit-transition: background-color 0.3s linear; }
span.open-paren8:hover { color : inherit; background-color : #FFCDFF; }


</style>[/text])
  (set 'built-in-functions (map string (symbols 'MAIN)))
  (set 'obsolete-functions (map string '(assoc-set nth-set ref-set replace-assoc set-assoc set-nth)))
  (set 'newlisp-variables  (map string '(ostype $0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $13 $14 $15 $args $idx $it $main-args))))
  (set 'syntax-colouring-ready true)))

(define (nlx-to-html-helper l)
   (dolist (i l)
   
   
   (println {i is } i)
      (if (atom? (first i))
            (begin 
              (set 'first-elt (first i) 'last-elt (last i))
              (cond 
               ((= first-elt "symbol")
                    (cond
                      ((find last-elt newlisp-variables)
                          (write-buffer buff (string {<span class="variable">} (escape-html last-elt) {</span>})))
                      
                      ((find last-elt obsolete-functions)
                          (write-buffer buff (string {<span class="obsolete">} (escape-html last-elt) {</span>})))
                        
                      ((find last-elt built-in-functions)
                          (write-buffer buff (string {<span class="built-in">} (escape-html last-elt) {</span>})))
                      (true
                          (write-buffer buff (string {<span class="sym">}   (escape-html last-elt) {</span>})))))                       
               ((= first-elt "open-paren")
                    (inc paren-level)
                    (write-buffer buff (format {<span class="open-paren%d">(} paren-level)))
               ((= first-elt "close-paren") 
                    (dec paren-level)
                    (write-buffer buff {)</span>}))
               ((= first-elt "whitespace") 
                    (dostring (s (base64-dec last-elt)) 
                      (write-buffer buff (char s))))
               ((= first-elt "braced-string")
                    (write-buffer buff (string {<span class="braced-string">} "{" (escape-html last-elt) "}" {</span>})))
               ((= first-elt "quoted-string")
                    (write-buffer buff (string {<span class="quoted-string">} {"} (escape-html last-elt) {"} {</span>})))
               ((= (first i) "bracketed-string")
                  (write-buffer buff (string {<span class="bracketed-string">} {[text]} (escape-html  last-elt) {[/text]} {</span>})))
               ((= first-elt "quote")
                    (write-buffer buff (string {<span class="quote">'</span>})))
               ((= first-elt "comment")
                    (write-buffer buff (string {<span class="comment">} (escape-html last-elt)  {</span>} "\n" )))
               ((= first-elt "integer")
                    (write-buffer buff (string {<span class="integer">} (int last-elt) {</span>})))
               ((= first-elt "float")
                    (write-buffer buff (string {<span class="float">} (precise-float last-elt) {</span>})))
               ((= first-elt "hex")
                    (write-buffer buff (string {<span class="hex">} (escape-html last-elt) {</span>})))
               (true
                    (write-buffer buff (string (escape-html last-elt))))
                    ))
         ; not an atom, so recurse
         (nlx-to-html-helper i))))

;; @syntax (nlx-to-html <nlx> [<mode>])
;; @return Returns an HTML representation of newLISP expression list in nlx
;; @param <nlx> newLISP expression list generated by read or read-from-file
;; @param <mode> (optional) <page> generates a complete HTML page, 
;; <css> generates a pre/code section which includes css style defs
;; The default is to generate a pre/code section without css style defs

(define (nlx-to-html nlx mode)
  (let ((buff "") (css-markup ""))
    (if-not syntax-colouring-ready (set-up-syntax))
    (set 'css-markup css-def)
    (set 'parenlevel 0)
    (nlx-to-html-helper nlx)
    (cond
      ((= mode "page")
         (format [text]<html>
<head>
%s
</head>
<body>
<pre><code>%s</code></pre>
</body>
</html>[/text] css-markup buff))
        ((= mode "css")
           (format "%s<pre><code>%s</code></pre>" css-markup buff))
        (true
           (format "<pre><code>%s</code></pre>" buff)))))

; ConTeXt syntax colourizer stuff

(define (escape-context x)
  (let ((str (string x)))
    (replace {\} str {\letterbackslash{}})
    (replace {$} str {\letterdollar{}})
    (replace {#} str {\letterhash{}})
    (replace {!} str {\letterexclamationmark{}})
    (replace {|} str {\letterbar{}})
    (replace {@} str {\letterat{}})
    (replace {^} str {\letterhat{}})
    (replace "%" str {\letterpercent{}})
    (replace "/" str {\letterslash{}})
    (replace "<" str {\letterless{}})
    (replace ">" str {\lettermore{}})
    (replace "~" str {\lettertilde{}})
    (replace "&" str {\letterampersand{}})
    (replace "?" str {\letterquestionmark{}})
    (replace "_" str {\letterunderscore{}})
    (replace "'" str {\lettersinglequote{}})))

(define (nlx-to-context-helper l)
   (dolist (i l)
      (if (atom? (first i))
            (begin 
              (set 'first-elt (first i) 'last-elt (last i))
              (cond 
               ((= first-elt "symbol")
                    (cond
                      ((find last-elt newlisp-variables)
                          (write-buffer buff (string "\\color[darkblue]{" (escape-context last-elt)  "}")))
                      
                      ((find last-elt obsolete-functions)
                          (write-buffer buff (string "\\color[green]{" (escape-context last-elt)  "}")))
                        
                      ((find last-elt built-in-functions)
                          (write-buffer buff (string "\\bf\\color[newlispred]{" (escape-context last-elt)  "}\\tf")))
                      (true
                          (write-buffer buff (string "\\bf\\color[darkblue]{" (escape-context last-elt) "}\\tf")))))
               ((= first-elt "open-paren")
                    (write-buffer buff "\\color[newlispgrey]{(}")) 
               ((= first-elt "close-paren")
                    (write-buffer buff "\\color[newlispgrey]{)}"))
               ((= first-elt "whitespace") 
                    (dostring (s (base64-dec (escape-context last-elt)))
                       (cond 
                          ((= s 32) 
                              (write-buffer buff "\\type{ }"))
                          ((or (= s 13) (= s 10))
                              (write-buffer buff "\\crlf{}"))
                          )))
               ((= first-elt "braced-string")
                    (write-buffer buff (string "\\letteropenbrace{}\\color[newlispgreen]{" (escape-context last-elt)  "}\\letterclosebrace{}")))
               ((= first-elt "quoted-string")
                    (write-buffer buff (string  "\\letterdoublequote{}\\color[newlispgreen]{" (escape-context last-elt) "}\\letterdoublequote{}")))
               ((= (first i) "bracketed-string")
                  (write-buffer buff (string  "\\color[newlispgreen]{" {[text]} (escape-context last-elt)  {[/text]} "}")))
               ((= first-elt "quote")
                    (write-buffer buff (string (escape-context last-elt))))
               ((= first-elt "comment")
                    (write-buffer buff (string "\\color[newlispgrey]{" (escape-context last-elt) "}\n")))
               ((= first-elt "integer")
                    (write-buffer buff (string "\\color[darkred]{" (int (escape-context last-elt) ) "}")))
               ((= first-elt "float")
                    (write-buffer buff (string "\\color[darkred]{"  (precise-float (escape-context last-elt) ) "}")))
               ((= first-elt "hex")
                    (write-buffer buff (string "\\color[darkgreen]{" (escape-context last-elt)  "}")))
               (true
                    (write-buffer buff (string  (escape-context last-elt) )))
                    ))
         ; not an atom, so recurse
         (nlx-to-context-helper i))))

(define (nlx-to-context nlx)
  (let ((buff ""))
     (if-not syntax-colouring-ready (set-up-syntax))
     (nlx-to-context-helper nlx)
     buff))

(context MAIN)

; eof
