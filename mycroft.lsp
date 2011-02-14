#!/usr/bin/env newlisp

;; @module mycroft
;; @author cormullion at mac dot com
;; @description newLISP script profiler
;; @location http://unbalanced-parentheses.nfshost.com/downloads
;; @version of 2008-12-17 16:43:01
;; Use at the command line:
;; newlisp mycroft.nl file-to-profile
;; or
;; mycroft.nl file-to-profile
;;
;; Your file should (exit) when it's finished.
;; Only functions defined with 'define' are profiled... :(

(unless (> (sys-info -2) 9999) (println "requires newLISP version 10!\n" (exit)))
(unless unicode (println "using a non-Unicode version of newLISP; things may go wrong..."))

(new Tree 'Call-list)      ; hashictionary to hold timings for each user-defined function

; there are four contexts: Mycroft, Stack, Nestor, Html

(context 'Mycroft)

(set 'built-in-functions (map string (symbols 'MAIN)))

(unless (set 'file (main-args 2)) (println "specify a file\n" (exit)))
(unless (file? file) (println "file doesn't exist\n" (exit)))

(set 'function-data '())
(set 'results '())
(set 'version {0.0.1})

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
  ; gather into new hashictionary Call-list, one entry per function
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
    (list 
          ; function-name
          fname
          ; time as percentage of total time
          (round (mul 100 (div (last $it) total-function-call-time)) -1)
          ; number of times function was called
          ; results holds symbols but function-data (call-list) holds strings...
          ; read-expr translates string to symbol in context but does not evaluate it
          (length (find-all (list (read-expr fname) '+) results))
          ; total microseconds for this function
          total-time))
    match)
    )

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

(define (string-length s)
    (if unicode (utf8len s) (length s)))

(define (get-next-char)
 (let ((nch ""))
   (if (< *cursor* *source-length*)
       (begin
          (set 'nch (source-string *cursor*))
          (inc *cursor* (string-length nch)))
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

;; @syntax (read <source-string>)
;; @return Returns a nested list representing the newLISP source code in source-string

(define (read source-string)
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

(define (set-up-syntax)
  (set 'built-in-functions (map string (symbols 'MAIN)))
  (set 'obsolete-functions (map string '(assoc-set nth-set ref-set replace-assoc set-assoc set-nth)))
  (set 'newlisp-variables  (map string '(ostype $0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $13 $14 $15 $args $idx $it $main-args)))
  (set 'syntax-colouring-ready true))

(define (nlx-to-html-helper l)
   (dolist (i l)
      (if (atom? (first i))
            (begin 
              (set 'first-elt (first i) 'last-elt (last i))
              (cond 
               ((= first-elt "symbol")
                    (cond
                      ((find last-elt newlisp-variables)
                          (write-buffer buff (string {<span class="variable">} (Html:escape-html last-elt) {</span>})))
                      
                      ((find last-elt obsolete-functions)
                          (write-buffer buff (string {<span class="obsolete">} (Html:escape-html last-elt) {</span>})))
                        
                      ((find last-elt built-in-functions)
                          (write-buffer buff (string {<span class="built-in">} (Html:escape-html last-elt) {</span>})))
                      (true
                          (write-buffer buff (string {<span class="sym">}   (Html:escape-html last-elt) {</span>})))))                       
               ((= first-elt "open-paren") 
                    (write-buffer buff {<span class="open-paren">(</span>}))
               ((= first-elt "close-paren") 
                    (write-buffer buff {<span class="close-paren">)</span>}))
               ((= first-elt "whitespace") 
                    (dostring (s (base64-dec last-elt)) 
                      (write-buffer buff (char s))))
               ((= first-elt "braced-string")
                    (write-buffer buff (string {<span class="braced-string">} "{" (Html:escape-html last-elt) "}" {</span>})))
               ((= first-elt "quoted-string")
                    (write-buffer buff (string {<span class="quoted-string">} {"} (Html:escape-html last-elt) {"} {</span>})))
               ((= (first i) "bracketed-string")
                  (write-buffer buff (string {<span class="bracketed-string">} {[text]} (Html:escape-html  last-elt) {[/text]} {</span>})))
               ((= first-elt "quote")
                    (write-buffer buff (string {<span class="quote">'</span>})))
               ((= first-elt "comment")
                    (write-buffer buff (string {<span class="comment">} (Html:escape-html last-elt)  {</span>} "\n" )))
               ((= first-elt "integer")
                    (write-buffer buff (string {<span class="integer">} (int last-elt) {</span>})))
               ((= first-elt "float")
                    (write-buffer buff (string {<span class="float">} (precise-float last-elt) {</span>})))
               ((= first-elt "hex")
                    (write-buffer buff (string {<span class="hex">} (Html:escape-html last-elt) {</span>})))
               (true
                    (write-buffer buff (string (Html:escape-html last-elt))))
                    ))
         ; not an atom, so recurse
         (nlx-to-html-helper i))))

;; @syntax (nlx-to-html <nlx> )
;; @return Returns an HTML representation of newLISP expression list in nlx
;; @param <nlx> newLISP expression list generated by read or read-from-file

(define (nlx-to-html nlx)
  (let ((buff "") (css-markup ""))
    (if-not syntax-colouring-ready (set-up-syntax))
    (nlx-to-html-helper nlx)
    (format "<pre><code>%s</code></pre>" buff)))

(context MAIN)

; switch over some functions

(constant (global 'newLISP-define) define)
(constant (global 'define) Mycroft:define)
(constant (global 'newLISP-exit) exit)
(constant (global 'exit) Mycroft:exit)

(context Mycroft)

(println "...loaded profiling code")
(println "...loading file " file)
(println "...starting execution")

; run the file
(set 'start-timing (time-of-day))
(start)
(unless (catch (load file) 'error) 
        (println (string "sorry the file didn't load and execute correctly:\n\t" error))
        (newLISP-exit))

(stop)
(set 'finish-timing (- (time-of-day) start-timing))
(println "...file has finished executing")
(println "...analysing results")

(crunch-numbers)

(println "...preparing report")

; some HTML output routines

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
  *
  {
    margin: 0;
    padding: 0;
    list-style-type: none;
  }
  
  body
  {
    font-family: Helvetica, Arial, sans-serif;
    font-size: 11pt;
    color: #222;
  }
  
  a
  {
    color: #555;
    text-decoration: none;
    font-weight: bold;
  }

  /* hovering */
  a:hover
  {
    color: #000;
  }
  
  a span { display: none; }
  
  a:hover span
  {
    display: block;
    float: right;
    z-index: 100;
    border: 0px dotted #c0c0c0;
    font-size: 1em;
    color: #282;
  }
  
  h2, h3, h4 { clear: both; }
  
  h2, h3
  {
    border-bottom: 1px solid #555;
    margin-bottom: .9em;
  }
  
  h3 { padding-top: 1em; }
  
  h4
  {
    font-weight: normal;
    font-size: 0.9em;
    color: #900000;
    margin-left: 1em;
  }
  
  .main
  {
    float: left;
    clear: left;
    padding: 1em 2em;
  }
  
  .section
  {
    float: left;
    clear: left;
  }
  
  .main .key
  {
    clear: left;
    float: left;
    display: block;
    width: 220px;
    text-align: left;
    color: #111;
    font-weight: bold;
    font-size: 0.875em;
  }
  
  .main .value
  {
    float: left;
    display: block;
    text-align: left;
    color: #222;
    font-weight: normal;
    font-size: 0.875em;
  }
  
  .chartlist
  {
    float: left;
    border-top: 1px solid #ccc;
    width: 800px;
  }
  
  .chartlist li
  {
    position: relative;
    display: block;
    border-bottom: 1px solid #fff;
    _zoom: 1;
  }
  
  .chartlist li a
  {
    display: block;
    padding: 0.4em 4.5em 0.4em 0.5em;
    position: relative;
    z-index: 2;
  }
  
  /* text at right of bar */
  .chartlist .count
  {
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
    z-index: 2;
  }
  
  /* the bar */
  .chartlist .index
  {
    display: block;
    position: absolute;
    top: 0;
    left: 0;
    height: 100%;
    background: #9f9;
    text-indent: -9999px;
    overflow: hidden;
    line-height: 2em;
  }
  
  .chartlist li:hover { background: #dddddd; }
  
  p
  {
    font-size: 0.7 em;
    margin: 1em 0 1em 0;
    color: #444;
  }
  
  blockquote { margin-left: 2em; }
  p span { display: normal; }
  
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
  
  .sym { color: #3300ff; }
  
  .built-in
  {
    color: #660044;
    font-weight: bold;
  }
  
  .obsolete
  {
    color: #ffff00;
    background: #000;
  }
  
  .variable { color: #880077; }
  .open-paren { color: #777777; }
  .close-paren { color: #777777; }
  
  .braced-string
  {
    color: #226666;
    background: #eeffff;
  }
  
  .quoted-string
  {
    color: #226666;
    background: #eeffff;
  }
  
  .bracketed-string
  {
    color: #226666;
    background: #eeffff;
  }
  
  .quote { color: #220000; }
  
  .comment
  {
    color: #666666;
    font-family: serif;
  }
  
  .integer { color: #113366; }
  .float { color: #335533; }
  .hex { color: #336633; }
  .octal { color: #336699; }

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

(context Mycroft)

(Html:header)
(Html:body)
(Html:heading 2 (string {Mycroft: report: } file))
(Html:key-value {file:} (Html:escape-html (real-path file)))
(Html:key-value {date:} (date))
(Html:key-value {time:} (string (round (div total-function-call-time 1000000) -3)  " seconds"))
(Html:key-value {function calls:} (string total-function-calls))
(Html:key-value {operating system:} ostype)
(Html:key-value {newLISP version} (sys-info -2))
(Html:key-value {mycroft version} version)
(Html:heading 3 {Contents})

(Html:para {<a href="#Timings">Timings</a> <a href="#Calls">Calls</a> <a href="#Symbols">Symbols</a> 
 <a href="#Source">Source</a> <a href="#Trivia">Trivia</a>})

(Html:start-bar-chart "Timings" " % of total time in each function")
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

;; source analysis phase

(define Symbol-list:Symbol-list)
(set 'nlx (Nestor:read-from-file file))
(set-ref-all '("symbol" +) nlx 
  (begin
    (if (set 'total (Symbol-list (last $0)))
       (Symbol-list (last $0) (inc total))
       (Symbol-list (last $0) 1))
    $0)
  match)

(Html:start-bar-chart "Symbols" (string "the number of occurrences of a symbol in " file))
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

(unless (catch (Html (Nestor:nlx-to-html nlx)) 'error) 
        (println (string "sorry the source formatting failed: " error)))

(Html:heading 3 {Trivia})

; source trivia now, just to fill up the page :)
; symbol counting
(set-ref-all '("symbol" +) (copy nlx) (push (last $it) user-syms -1) match)

(Html:key-value {user-defined symbols} (string (length (difference user-syms built-in-functions))))
(Html:key-value {built-in primitives} (string (length (intersect user-syms built-in-functions))))

; parenthesis counting
(set-ref-all '("open-paren" +) (copy nlx) (push (last $it) open-parens -1) match)
(Html:key-value {number of open parentheses} (length open-parens))
(set-ref-all '("close-paren" +) (copy nlx) (push (last $it) close-parens -1) match)
(Html:key-value {number of close parentheses} (length close-parens))

; the number of characters...
(set 'file-char-count (if unicode (utf8len (read-file file)) (length (read-file file))))
(Html:key-value {characters} (string file-char-count))

; white space characters
(set 'white-stuff "")
(set-ref-all '("whitespace" +) (copy nlx) (push (last $it) white-space -1) match)
(map (fn (c) (push (base64-dec c) white-stuff -1)) white-space)
(Html:key-value {whitespace characters} (format {%d spaces, %d returns, and %d tabs} (count '({ } "\n" "\t") (explode white-stuff))))

; comments
(set-ref-all '("comment" +) (copy nlx) (push (last $it) comments -1) match)
(map (fn (c) (inc comment-chars (if unicode (utf8len c) (length c)))) comments)
(Html:key-value {comments} 
  (format {%d characters in %d comment%s} comment-chars (length comments) (if (= (length comments) 1) {} {s})))

(Html:end-page)
(println "saving report as " (set 'report-file (string file "-my.html")))

(write-file report-file Html:html-page)

; open the report file ...?
(cond
  ((= ostype "OSX")     (exec (string "open " report-file)))
  ((= ostype "Win32")   (exec (string "c:/" report-file))))
; Linux?

(newLISP-exit)
