#!/usr/bin/env newlisp

;; @module mycroft
;; @author cormullion at mac dot com
;; @description newLISP script profiler
;; @location somewhere on github
;; @version 0.0.1 2011-09-15 16:47:19
;; Use Mycroft to profile the performance of a single newLISP script. 
;; Use at the command line:
;; newlisp mycroft.lsp file-to-profile
;; or
;; mycroft.lsp file-to-profile
;;
;; Your script should (exit) when it's finished, so that Mycroft can report.
;; Only functions defined with 'define' are profiled... :(
;;
;; This was my entry in the Christmas 2008 newLISP programming competition. 
;; I was the only entrant. :( I think I won an imaginary T shirt. :)
;; I don't know how to profile this script.

(unless unicode (println "using a non-Unicode version of newLISP; things may go wrong..."))
(global '*file*)
(unless (set '*file* (main-args 2)) (println "specify a newLISP script to profile\n" (exit)))
(unless (file? *file*) (println "...that file doesn't exist\n" (exit)))

;; The script is disorganized and a bit long. It's divided into about 5 sections.
;; Stage 1: First, we load the newlisp-parser and add some functions for html output
;; Stage 2: we define some HTML output functions and CSS styles
;; Stage 3: we define our analysis routines, then redefine some newLISP primitives so that execution timings are recorded.
;; Stage 4: we run the script, and hope it exits when it's finished. Otherwise we're stuffed.
;; Stage 5: we analyse the timings and produce an HTML report.

; Stage 1: load parser and add some more definitions to Nlex
(context 'Nlex)
(load (string (env {HOME}) {/projects/programming/newlisp-projects/newlisp-parser.lsp}))
(define (set-up-syntax)
  (set 'built-in-functions (map string  (symbols 'MAIN)))
  (set 'obsolete-functions (map string '(write-buffer read-buffer name parse-date assoc-set nth-set ref-set replace-assoc set-assoc set-nth)))
  (set 'newlisp-variables  (map string '(ostype $0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $13 $14 $15 $args $idx $it $main-args)))
  (set 'parenlevel 0))

(define (nlx-to-html nlx (depth 0))
   (when (= depth 0) 
         (set 'buff {}) ; if first pass, initialize a buffer
         (set-up-syntax))
   (dolist (i nlx)
       (set 'token-type (first i) 'token-value (last i))
       (if  (atom? token-type)
            (cond 
               ((= token-type 'LeftParen)  
                    (inc paren-level)
                    (write buff (format {<span class="open-paren%d">(} paren-level)))
               ((= token-type 'RightParen) 
                    (dec paren-level)
                    (write buff (format {)</span>})))
               ((= token-type 'Symbol)
                    (cond
                      ((find token-value newlisp-variables)
                          (write buff (string {<span class="variable">} (Html:escape-html token-value) {</span>})))
                      ((find token-value obsolete-functions)
                          (write buff (string {<span class="obsolete">} (Html:escape-html token-value) {</span>})))
                      ((find token-value built-in-functions)
                          (write buff (string {<span class="built-in">} (Html:escape-html token-value) {</span>})))
                      (true
                          (write buff (string {<span class="symbol">}   (Html:escape-html token-value) {</span>})))))
               ((= token-type 'WhiteSpace)  
                    (dostring (s (base64-dec (string token-value))) 
                        (write buff (char s))))
               ((= token-type 'BracedString)  
                    (write buff (string {<span class="braced-string">} "{" (Html:escape-html token-value) "}" {</span>})))
               ((= token-type 'QuotedString)  
                    (write buff (string {<span class="quoted-string">} {"} (Html:escape-html token-value) {"} {</span>})))
               ((= token-type 'BracketedText)  
                    (write buff (string {<span class="bracketed-string">} {[text]} (Html:escape-html  token-value) {[/text]} {</span>})))        
               ((= token-type 'Quote)
                    (write buff (string {<span class="quote">'</span>})))
               ((= token-type 'Comment)  
                    (write buff (string {<span class="comment">} (Html:escape-html token-value) {</span>} "\n" )))
               ((= token-type 'Integer)  
                    (write buff (string {<span class="integer">} (int token-value) {</span>})))
               ((= token-type 'Float)  
                    (write buff (string {<span class="float">} (Html:escape-html token-value) {</span>})))
               ((= token-type 'Scientific)  
                    (write buff (string {<span class="scientific">} (Html:escape-html token-value) {</span>})))
               ((= token-type 'Hex)  
                    (write buff (string {<span class="hex-string">} (Html:escape-html token-value) {</span>})))                
               ((= token-type 'BracketedCommand)  
                    (write buff (string {<span class="bracketed-command">} token-value  {</span>})))                
               ((= token-type 'NaN) ; not a number
                    (write buff (string {<span class="NaN">} token-value  {</span>})))
               ((= token-type 'Octal)
                    (write buff (string {<span class="octal">}  token-value {</span>})))
               ((= token-type 'BracketedIdentifier) ; bracketed identifier
                    (write buff (string {<span class="octal">[}  token-value {]</span>}))))                    
            ; not an atom, so recurse but don't initialize buffer
            (nlx-to-html i 1)))
   buff)

;  Stage 2 Define some HTML routines
;  HTML 

(define (Html:Html str)
  (if (not Html:html-page)
    (set 'Html:html-page str)
    (write-line Html:html-page str)))

(context 'Html)

(define (Html:escape-html txt)
 (if txt
  (begin
   (replace {&} txt {&amp;} 0)
   (replace {<} txt {&lt;} 0)
   (replace {>} txt {&gt;} 0)))
 txt)

(define (Html:header)
  (Html [text]<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN"
  "http://www.w3.org/TR/html4/strict.dtd">
<html lang="en">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
  <title>[/text])
 (Html "Mycroft: report") ; To-Do: title to include file name
 (Html [text]</title>
 <style type="text/css" media="screen">
* { margin: 0;
	padding: 0;
	list-style-type: none; }
body {
	font-family: Helvetica, Arial, sans-serif;
	font-size: 11pt;
	color: #222; }
a {
	color: #555;
	text-decoration: none;
	font-weight: bold; }
/* hovering */
a:hover { color: #000; }
a span { display: none; }
a:hover span {
	display: block;
	float: right;
	z-index: 100;
	border: 0px dotted #c0c0c0;
	font-size: 1em;
	color: #282; }
h2, h3, h4 { clear: both; }
h2, h3 {
	border-bottom: 1px solid #555;
	margin-bottom: .9em; }
h3 { padding-top: 1em; }
h4 {
	font-weight: normal;
	font-size: 0.9em;
	color: #900000;
	margin-left: 1em; }
.main {
	float: left;
	clear: left;
	padding: 1em 2em; }
.section {
	float: left;
	clear: left; }
.main .key {
	clear: left;
	float: left;
	display: block;
	width: 220px;
	text-align: left;
	color: #111;
	font-weight: bold;
	font-size: 0.875em; }
.main .value {
	float: left;
	display: block;
	text-align: left;
	color: #222;
	font-weight: normal;
	font-size: 0.875em; }
.chartlist {
	float: left;
	border-top: 1px solid #ccc;
	width: 800px; }
.chartlist li {
	position: relative;
	display: block;
	border-bottom: 1px solid #fff;
	_zoom: 1; }
.chartlist li a {
	display: block;
	padding: 0.4em 4.5em 0.4em 0.5em;
	position: relative;
	z-index: 2; }
/* text at right of bar */
.chartlist .count {
	display: block;
	position: absolute;
	top: 0;
	right: 0;
	margin: 0 0.3em;
	text-align: right;
	color: #333;
	font-weight: bold;
	font-size: 0.875em;
	line-height: 2em;
	z-index: 2; }
/* the bar */
.chartlist .index {
	display: block;
	position: absolute;
	top: 0;
	left: 0;
	height: 100%;
	background: #9f9;
	text-indent: -9999px;
	overflow: hidden;
	line-height: 2em; }
.chartlist li:hover { background: #dddddd; }
p {
	font-size: 0.7 em;
	margin: 1em 0 1em 0;
	color: #444; }
blockquote { margin-left: 2em; }
p span { display: normal; }
pre, code {
	font-family: Monaco, 'Andale Mono', 'Lucida Console', monospace;
	font-size: 10pt;
	/* http://users.tkk.fi/~tkarvine/pre-wrap-css3-mozilla-opera-ie.html */
	/* css-3 */
	/* Mozilla, since 1999 */
	/* Opera 4-6 */
	white-space: -o-pre-wrap;
	/* Opera 7 */
	word-wrap: break-word;
	/* Internet Explorer 5.5+ */ }
.symbol { color: #3300ff; }
.built-in {color: #660044; font-weight: bold; }
.obsolete {color: #ffff00; background: #000;  opacity: 0.5}
.variable { color: #880077; }
.open-paren { color: #777777; }
.close-paren { color: #777777; }
.braced-string {color: #226666; background: #eeffff; opacity: 0.5}
.quoted-string {color: #226666; background: #eeffff;  opacity: 0.5}
.bracketed-string {color: #226666; background: #eeffff;  opacity: 0.5}
.quote { color: #224400; }
.comment {color: #666666; font-family: serif; }
.integer { color: #113366; }
.float { color: #335533; }
.hex { color: #336633; }
.octal { color: #336699; }
span.open-paren1 {
	background-color: #FFF;
	-webkit-transition: background-color 0.1s linear; }
span.open-paren1:hover {
	color: inherit;
	background-color: #6FC;}
span.open-paren2 {
	background-color: inherit;
	-webkit-transition: background-color 0.1s linear; }
span.open-paren2:hover {
	color: inherit;
	background-color: #6C6; }
span.open-paren3 {
	background-color: inherit;
	-webkit-transition: background-color 0.1s linear; }
span.open-paren3:hover {
	color: inherit;
	background-color: #0F0; }
span.open-paren4 {
	background-color: inherit;
	-webkit-transition: background-color 0.1s linear; }
span.open-paren4:hover {
	color: inherit;
	background-color: #3F3; }
span.open-paren5 {
	background-color: inherit;
	-webkit-transition: background-color 0.1s linear; }
span.open-paren5:hover {
	color: inherit;
	background-color: #6F6; }
span.open-paren6 {
	background-color: inherit;
	-webkit-transition: background-color 0.1s linear; }
span.open-paren6:hover {
	color: inherit;
	background-color: #9F9; }
span.open-paren7 {
	background-color: inherit;
	-webkit-transition: background-color 0.1s linear; }
span.open-paren7:hover {
	color: inherit;
	background-color: #9C6; }
span.open-paren8 {
	background-color: inherit;
	-webkit-transition: background-color 0.1s linear; }
span.open-paren8:hover {
	color: inherit;
	background-color: #CF3; }
span.open-paren9 {
	background-color: inherit;
	-webkit-transition: background-color 0.1s linear; }
span.open-paren9:hover {
	color: inherit;
	background-color: #FF6; }
</style>
</head>
[/text]))

(define (Html:body)
  (Html {<body>
  <div class="main">
  }))
  
(define (Html:heading level text)
   (Html (string {<h} level {><a name="} text {">} text {</a></h} level {>})))

(define (Html:para text)
   (Html (string {<p>} text {</p>})))

(define (Html:key-value key value)
   (Html (string {<p><span class="key">} key {</span>} {<span class="value">} value {</span></p>})))

(define (Html:start-bar-chart title title2)
  (Html {<div class="section">
  })
  (heading 3 title)
  (heading 4 title2)
  (Html {  <ul class="chartlist">}))

(define (Html:add-bar item count-value unit-string index-value hover-text link-text)
; hover text shows additional info
; could really do with named parameters for passing values to this function :)
  (Html (string {
    <li> 
      <a href="} (Html:escape-html link-text) {">} (Html:escape-html item) {<span>} hover-text {</span>}{</a>
      <span class="count">} count-value unit-string {</span>
      <span class="index" style="width: } index-value {%">} index-value {"</span>
    </li>})))

(define (Html:end-bar-chart)
   (Html [text]
   </ul>
   </div>
   [/text]))

(define (Html:end-page)
   (Html [text]
</div>
</body>
</html>
[/text]))

; Stage 3 Analysis functions and redefine newLISP primitives for profiling
(context 'Call-list) ; hold timings for each user-defined function
(context 'Mycroft)
(set 'version {0.0.1})
(set 'built-in-functions (map string (symbols 'MAIN)))
(set 'function-data '())
(set 'results '())

(define (time-list-to-microseconds l)
   ; convert list of (minute second microseconds) to microseconds
   (+ (* 1000000 60 (first l)) (* 1000000 (first (rest l))) (last l)))

(define (start)
    (set 'results (list (list 'start (4 3 (now))))))

(define (stop)
    (push (list 'stop (4 3 (now))) results -1))

(define (crunch-numbers)
  (set 'time-taken
      (-  (time-list-to-microseconds (last (last results))) 
          (time-list-to-microseconds (last (first results)))))
  ; convert last item of each entry in results to elapsed time for that call
  ; by subtracting it from the following one
  (set 'l (time-list-to-microseconds (last (first results))))
    ; but don't do last one
  (for (i 0 (dec (length results) 2))
       (set 'current (results i))
       (setf (last (results i)) 
          (-  (time-list-to-microseconds (last (results (+ i 1)))) 
              (time-list-to-microseconds (last current))))
       (set 'l (time-list-to-microseconds (last current))))
  (setf (last (last results)) 0)
  ; results now contains every call to each function in the order it was called, with time taken each call  
  ; gather into new dictionary Call-list, one entry per function
  (dolist (t results)
     (if (set 'tm (Call-list (string (first t))))
         (Call-list  (string (first t)) (inc (last t) tm))
         (Call-list  (string (first t)) (last t))))
  ; to tidy output, remove the ones we added
  ; no longer need these in the list
  (Call-list "start" nil)
  (Call-list "stop" nil)
  (set 'total-function-calls (- (length results) 2))
  
  ; functions are sorted by name and have accumulated duration totals
  ; add up durations - another view of total elapsed time! 
  (set 'total-function-call-time 0)
  (map (fn (pr) (inc total-function-call-time (last pr))) (Call-list))
  ; add extra data to the list
  ; don't need to copy - the generated assoc list is an on-the-fly copy, not the original
  (set 'function-data (Call-list))
  (replace
    '(+ +)
    function-data
    (begin
        (set 'fname (first $it))
        (set 'total-time (last $it))
    (list fname ; function-name
          (round (mul 100 (div (last $it) total-function-call-time)) -1) ; time as percentage of total time
          ; number of times function was called
          ; results holds symbols but function-data (call-list) holds strings...
          ; read-expr translates string to symbol in context but does not evaluate it
          (length (find-all (list (read-expr fname) '+) results))
          ; total microseconds for this function
          total-time))
    match))

; add time data stuff to each function defined with 'define'
(define-macro (Mycroft:define farg)
    (if (list? farg)
        (set  (farg 0)
              (letex (@fn (farg 0)
                      arg (rest farg)
                      @arg-p (cons 'list
                          (map
                              (fn   (x) (if (list? x) (first x) x))
                              (rest farg)))
                      body (cons 'begin (args)))
                  (lambda arg 
                    (push (list '@fn (4 3 (now))) Mycroft:results -1) body)))
        (if (args) (set farg (eval (first (args)))) (set farg nil))))

(define (Mycroft:exit)
   (println "(exit) - program exited"))

(context MAIN)

; switch over some functions

(constant (global 'newLISP-define) define)
(constant (global 'define) Mycroft:define)
(constant (global 'newLISP-exit) exit)
(constant (global 'exit) Mycroft:exit)

; Stage 4: run the script, wait till exit.

(context Mycroft)
(println "...loaded profiling code")
(println "...loading file " *file*)
(println "...starting execution")
; run the file
(set 'start-timing (time-of-day))
(start)
(unless (catch (load *file*) 'error) 
        (println (string "sorry the file didn't load and execute correctly:\n\t" error))
        (newLISP-exit))
(stop)
(set 'finish-timing (- (time-of-day) start-timing))

; Stage 5: analyse data and produce report 

(println "...file has finished executing")
(println "...analysing results")

(crunch-numbers)

(println "...preparing report")
(Html:header)
(Html:body)
(Html:heading 2 (string {Mycroft: report: } *file*))
(Html:key-value {file:} (Html:escape-html (real-path *file*)))
(Html:key-value {date:} (date))
(Html:key-value {time:} (string (round (div total-function-call-time 1000000) -3)  " seconds"))
(Html:key-value {function calls:} (string total-function-calls))
(Html:key-value {operating system:} ostype)
(Html:key-value {newLISP version} (sys-info -2))
(Html:key-value {mycroft version} version)
(Html:heading 3 {Contents})

(Html:para {<a href="#Timings">Timings</a> <a href="#Calls">Calls</a> <a href="#Symbols">Symbols</a> 
 <a href="#Source">Source</a> <a href="#Trivia">Trivia</a>})

(Html:start-bar-chart "Timings" " % of total execution time")
(dolist (f-data (sort function-data (fn (a b) (> (last a) (last b)))))
 ; f-data is: function-name | time as % of total time | number of times function was called | total microseconds
  (set 'avg-time (round (div (f-data 3) (f-data 2)) 0))
  (Html:add-bar 
       (f-data 0)    ; item
       (f-data 1)    ; count-value
       "%"           ; unit-string 
       (f-data 1)    ; index-value
      (string 
        (f-data 2) (if (= (f-data 2) 1) { call; (} { calls; (}) ; shouldn't have "1 calls" ! :)
        (round (mul (div (f-data 2) total-function-calls) 100) -1) {%);}
        { average: } avg-time { &#956;s; }
        { total: } (f-data 3) { &#956;s } 
      ) 
      "#Source"))
(Html:end-bar-chart)

(Html:start-bar-chart "Calls" "the number of times each function was called")
(dolist (f-data (sort function-data (fn (a b) (> (a 2) (b 2)))))
  (Html:add-bar 
         (f-data 0)        ; item
         (f-data 2)        ; count-value
         " x"              ; unit-string 
         (mul 100 (div (f-data 2) ((first function-data) 2))) ; index-value
         (string { (} (round (mul (div (f-data 2) total-function-calls) 100) -1) {%)}) ; hover-text
         "#Source"         ; link-text
         ))
(Html:end-bar-chart)

(println {... analyzing source})

(define Symbol-list:Symbol-list)

(set 'nlx (Nlex:parse-newlisp (read-file *file*)))

(set-ref-all '(Nlex:Symbol +) (copy nlx) 
  (begin
    (if (set 'total (Symbol-list (last $it)))
        (Symbol-list (last $it) (inc total))
        (Symbol-list (last $it) 1))
    $it)
  match)

(Html:start-bar-chart "Symbols" (string "the number of occurrences of a symbol in " *file*))

(dolist (symbl (sort (Symbol-list) (fn (a b) (> (a 1) (b 1)))))
  (Html:add-bar 
   (first symbl)   ; item
   (symbl 1)       ; count-value
   " x"            ; unit-string 
   (symbl 1)       ; index-value   
   {}              ; hover-text
   "#Source"       ; link-text
   ))

(Html:end-bar-chart)

(println "...formatting source")

(Html:heading 3 {Source})

(Html (format "<pre><code>%s</code></pre>" (Nlex:nlx-to-html nlx)))

(println {...formatting succeeded, now generating trivia})

(Html:heading 3 {Trivia})

; source trivia now, just to fill up the page :)
(println {...symbols})

; symbol counting
(set 'user-syms '())
; symbols in parsed source have to be prefixed from here
(set-ref-all '(Nlex:Symbol +) (copy nlx) (push (last $it) user-syms -1) match)

(Html:key-value {user-defined symbols} (string (length (difference user-syms built-in-functions))))
(Html:key-value {built-in primitives}  (string (length (intersect  user-syms built-in-functions))))

; parenthesis counting
(println {...parentheses})
(set-ref-all '(Nlex:LeftParen +) (copy nlx) (push (last $it) open-parens -1) match)
(Html:key-value {number of open parentheses} (length open-parens))
(set-ref-all '(Nlex:RightParen +) (copy nlx) (push (last $it) close-parens -1) match)
(Html:key-value {number of close parentheses} (length close-parens))

; the number of characters...
(println {...character count})
(set 'file-char-count (if unicode (utf8len (read-file *file*)) (length (read-file *file*))))
(Html:key-value {characters} (string file-char-count))

; white space characters
(println {...white space})
(set 'white-stuff "")
(set-ref-all '(Nlex:WhiteSpace +) (copy nlx)  (push (last $it) white-space -1) match)
(map (fn (c) (push (base64-dec c) white-stuff -1)) white-space)
(Html:key-value {whitespace characters} (format {%d spaces, %d returns, and %d tabs} (count '({ } "\n" "\t") (explode white-stuff))))

; comments
(println {...comments})
(set-ref-all '(Nlex:Comment +) (copy nlx) (push (last $it) comments -1) match)
(map (fn (c) (inc comment-chars (if unicode (utf8len c) (length c)))) comments)
(Html:key-value {comments} 
  (format {%d characters in %d comment%s} comment-chars (length comments) (if (= (length comments) 1) {} {s})))

(Html:end-page)
(println "saving report as " (set 'report-file (string "/tmp/" *file* (date (date-value) 0 {%Y%m%d%H%M%S}) "-my.html")))

(write-file report-file Html:html-page)

; open the report file ...?
(cond
  ((= ostype "OSX")     (exec (string "open " report-file)))
  ((= ostype "Win32")   (exec (string "c:/" report-file)))
  (true                 (println {report file is } report-file)))

(newLISP-exit)
