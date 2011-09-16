#!/usr/bin/env newlisp

;; @module rainbow-code
;; @description test out pretty colour schemes for newLISP code and export them
;; @version 0.1
;; @author cormullion
;;
;; <h2>About</h2>
;; <p>This is just a quick hack to try out colour schemes for newLISP code. Uses newLISP-GS to display
;; and export to HTML.</p>

(if-not unicode (println "wants UTF version (or change utf8len to length) " (exit)))
(load (append (env "NEWLISPDIR") "/guiserver.lsp"))

; globals

(set '*styles* '(
("font-size" "16pt")
("background-color" "#dddddd")
("built-in" "#882222")
("sym" "#111111")
("obsolete" "#444444")
("variable" "#000000")
("open-paren" "#777777")
("close-paren" "#777777")
("braced-string" "#55555")
("quoted-string" "#666666")
("bracketed-string" "#444444")
("quote" "#000000")
("comment" "#333333")
("integer" "#000000")
("float" "#000000")
("hex" "#000000")
("octal" "#000000")))

(set '*source-tokens* '())
(set '*sentinel* true)

; interface

(gs:init)
(gs:frame 'Rainbow-Code 100 100 900 500 "Rainbow Code")
(gs:panel 'Main-panel)
(gs:set-border-layout 'Main-panel)

(gs:panel 'Text-panes)
(gs:set-grid-layout 'Text-panes 2 1)

; input pane for newlisp source
(gs:text-pane 'String-input 'textpane-handler "text/plain")

; output pane for html output
(gs:text-pane 'Html-output 'gs:no-action "text/html")
(gs:set-editable 'Html-output nil) 

; panel for colour fields
(gs:panel 'Input)
(gs:set-grid-layout 'Input (length *styles*) 1)

; one day, an Export function...
(gs:panel 'Export)
; button
(gs:button 'Export-button 'export-button-handler "Export")

; create text fields for entering the colours
(dolist (style *styles*)
   (gs:label (sym (string "label-" (first style))) (first style))
   (gs:text-field (sym (string "input-" (first style))) 'textfield-handler 10)
   ; add both label and input field here
   (gs:add-to 'Input (sym (string "label-" (first style))) (sym (string "input-" (first style)))))

; assemble layout 
(gs:add-to 'Text-panes 'String-input 'Html-output ); 'Input)
(gs:add-to 'Export 'Export-button)
(gs:add-to 'Main-panel  'Text-panes "center" 'Export "south" 'Input "west")
(gs:add-to 'Rainbow-Code 'Main-panel)

; initial setting
(gs:set-text 'String-input "; type some newLISP here \n (define (hello-world) \n   (println {hello world})\n")

(gs:set-visible 'MAIN:Rainbow-Code true)

; handlers

(define (build-styles)
; create a CSS HTML section based on the *styles* assoc list
  (let ((buff ""))
    (write-buffer buff [text]<style>
    pre, code
    {
      font-family: Monaco, 'Andale Mono', 'Lucida Console', monospace;[/text])
    (write-buffer buff "\n")
    
    ; font-size is set here
    (write-buffer buff (string "font-size: " (lookup "font-size" *styles*) ";"))
    
    ; and background colour
    (write-buffer buff (string "background-color: " (lookup "background-color" *styles*) ";"))
    
    ; other stuff
    (write-buffer buff [text]
    /* http://users.tkk.fi/~tkarvine/pre-wrap-css3-mozilla-opera-ie.html */
    /* css-3 */
    /* Mozilla, since 1999 */
    /* Opera 4-6 */
    white-space: -o-pre-wrap;
    /* Opera 7 */
    word-wrap: break-word;
    /* Internet Explorer 5.5+ */
    }
    [/text])
    (write-buffer buff "\n")
    
    ; now write out each style definition in the  following form
    ; .variable  {  color:  #880077; }
    (dolist (style (filter (fn (s) (!= (first s) "font-size")) *styles*)) ; skip font-size...
       (set 'new-colour (trim (lookup (first style) *styles*) "#"))
       (write-buffer buff (string "." (first style) " {color: #" new-colour "; }\n")))
       
    ; finished
    (write-buffer buff [text]
    </style>[/text])
    ; update css definition with new version
    (set 'Nestor:css-def buff)
    
    ; and update fields with new values
    (dolist (style *styles*)
     (gs:set-text (sym (string "input-" (first style))) (last style)))))

(define (export-button-handler)
; at the moment this prints the HTML code to the terminal... :/
  (println (gs:get-text 'Html-output)))

(define (tokenize-newlisp txt)
; don't run unless we can
  (let (result)
    (if *sentinel*
      (begin
        (set '*sentinel* nil)
        (if-not 
            (catch (set 'result (Nestor:read txt)) 'error) 
            (set 'result "newLISP syntax error"))
        (set '*sentinel* true)
        result)
      (set 'result {}))))

(define (textpane-handler id)
; input has changed
  (if (= id "MAIN:String-input")
      (and
        (!= key 65535) ; not cursor
        *sentinel*
        (gs:get-text id 'gettextcallback-handler))))

(define (gettextcallback-handler id text)
; input has changed so create a new set of tokens
  (and
     text
     (= id "MAIN:String-input")
     (set 'strng (base64-dec text))
     (set 'start (time-of-day))
     (set '*source-tokens* (tokenize-newlisp strng))
     ; and then update html display
     (update)))

(define (textfield-handler id text)
; a colour def has changed
  (let ((field (replace "MAIN:input-" id "")) 
        (field-value (string (base64-dec text))))
    ; check colours, should be six hex chars; we'll add the # if necessary
    (when (!= field "font-size")
          (set 'field-value (string "#" (0 6 (append (trim field-value "#") "000000")))))
    (setf (assoc field *styles*) (list field field-value))
    (update)))

(define (update)
  ; update the html window using current set of tokens
  (let ((result ""))
    ; make a set of style defs
    (build-styles)
    ; convert current set of tokens to HTML
    (if-not
      (catch
          (set 'result (Nestor:nlx-to-html *source-tokens* "page"))
          'error)
      (set 'result "HTML conversion error; sorry"))
    (gs:set-text 'Html-output result)))

;
; next is snapshot of the development version of Nestor, needed for source tokenizing
; You'd normally load this from another file, but you don't have the file .. :)
; there are still some bugs etc. 
; And it needs Unicode. But you could try changing utf8len to length...

(context 'Stack)
(define stack '())
(define sp '(0))
(define (Stack:Stack i) (push i stack sp) (setf (sp -1) (+ 1 $it)))
(define (Stack:init) (set 'stack '() 'sp '(0)))
(define (Stack:inc-nesting) (push 0 sp -1) (push '() stack sp))
(define (Stack:dec-nesting) (pop sp -1)
  (setf (sp -1) (+ 1 (last sp))))
(context 'Nestor)
(define syntax-colouring-ready nil)
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

(define (char-identifier? c) (not (find (lower-case (string c)) { "':,()})))

(define (char-identifier-first? c)
  (not (find (lower-case (string c)) [text] #;"'(){}.0123456789[/text])))

(define (char-numeric-first? c) (find c {+-.0123456789}))

(define (char-numeric? c) (find c {0123456789+-.xXabcdefABCDEF}))

(define (char-whitespace? c) (or (= c " ") (= c "\n") (= c "\t")))

(define (open-paren-token) (Stack:inc-nesting)
  (Stack (list "open-paren" "(")))

(define (close-paren-token) (Stack (list "close-paren" ")"))
  (Stack:dec-nesting))

(define (read-comment c) 
  (let ((res c) (ch ""))
     (while (and (!= (set 'ch (get-next-char)) "\n") ch)
        (push ch res -1))
    (Stack (list "comment" res))))
    
(define (read-identifier c)
  (let ((res c) (ch ""))
    (while (and (not (find (set 'ch (peek-char)) " \"',()\n\t\r")) (!= ch nil))
      (push (get-next-char) res -1))
    (Stack (list "symbol" res))))

(define (read-number-scanner list-so-far)
    (let ((next-char (peek-char)))
      (if (and (char-numeric? next-char) next-char)
            (read-number-scanner (cons (get-next-char) list-so-far))
            (reverse list-so-far))))

(define (precise-float str)
  (let ((p "") (q ""))
    (map set '(p q) (parse str "."))
    (append p "." q)))

(define (read-number c)
  (let ((res '() number-as-string ""))
     (set 'number-as-string (join (read-number-scanner (list c))))
     (cond
       ((starts-with (lower-case number-as-string) "0x")
          (set 'res (list "hex" number-as-string)))
       ((find "." number-as-string)
          (set 'res (list "float" (precise-float number-as-string))))
       ((and (starts-with (lower-case number-as-string) "0") (> (length number-as-string) 1))
          (set 'res (list "octal" number-as-string)))
       ((integer? (int number-as-string 0 10))
         (set 'res (list "integer" (int number-as-string 0 10))))
       (true
         (set 'res (list "string" "NaN"))))
  (Stack res)))

(define (read-quote)
   (Stack (list "quote" "'")))

(define (read-quoted-string)
  (let ((res {}) (ch {}))
     (while (and (!= (set 'ch (get-next-char)) {"}) ch)
        (push ch res -1)
        (when (= ch {\}) 
              (set 'ch (get-next-char))
              (push ch res -1)))
    (Stack (list "quoted-string" res))))

(define (read-braced-string)
  (let ((res "") (ch {}) (level 1)) 
     (while (> level 0)
         (set 'ch (get-next-char))
         (if (= ch "{") (inc level))
         (if (= ch "}") (dec level))
         (if (or (< level 0) (= ch nil)) (throw-error (string "error in a braced string at character " *cursor*)))
         (if (and (> level 0)) (push ch res -1))
         )
    (Stack (list "braced-string" res))))

(define (read-bracketed-string ch)
  (let ((res "") (ch {}))
    (if (= (lower-case (slice source-string (- *cursor* 1) 6)) "[text]")
       (begin
         (inc *cursor* 5)
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

(define (read source-string)
 (Stack:init)
 (set '*cursor* 0 '*source-length* (utf8len source-string))
 (while (< *cursor* *source-length*)
      (read-token))
 Stack:stack)

(define (read-from-file file)
  (Stack:init)
  (set 'source-string (read-file file))
  (set '*cursor* 0 '*source-length* (utf8len source-string))
  (while (< *cursor* *source-length*)
     (read-token))
  Stack:stack)


(define (escape-html txt)
 (if txt
  (begin
   (replace {&} txt {&amp;} 0)
   (replace {<} txt {&lt;} 0)
   (replace {>} txt {&gt;} 0)))
 txt)

(define (set-up-syntax)
  (if-not syntax-colouring-ready
     (begin
      (set 'css-def [text]<style></style>[/text])
      (set 'built-in-functions (map string (symbols 'MAIN)))
      (set 'obsolete-functions (map string '(assoc-set nth-set ref-set replace-assoc set-assoc set-nth)))
      (set 'newlisp-variables  (map string '(ostype $0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $13 $14 $15 $args $idx $it $main-args))))
      (set 'syntax-colouring-ready true)))

(define (nlx-to-html-helper l)
   (dolist (i l)
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
                    (write-buffer buff {<span class="open-paren">(</span>}))
               ((= first-elt "close-paren") 
                    (write-buffer buff {<span class="close-paren">)</span>}))
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
         (nlx-to-html-helper i))))

(define (nlx-to-html nlx mode)
  (let ((buff "") (css-markup (string css-def)))
    (if-not syntax-colouring-ready (set-up-syntax))
    (nlx-to-html-helper nlx)
    (cond
      ((= mode "page")
         (format [text]<html>
<head>
%s
</head>
<body bgcolor="#eeeeee">
<pre><code>%s</code></pre>
</body>
</html>[/text] css-markup buff))
        ((= mode "css")
           (format "%s<pre><code>%s</code></pre>" css-markup buff))
        (true
           (format "<pre><code>%s</code></pre>" buff)))))
           
           
;
; that was it
;

(context MAIN)

(update)
(gs:listen)

; eof
