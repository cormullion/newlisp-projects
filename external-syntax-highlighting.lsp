; this file converts newLISP source to an HTML page 
; (syntax-highlight source-text title) returns HTML page of source
; as used by extended version of newlispdoc, newlispdoc-ext. 

; requires newLISP parser
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
               ((= token-type 'Nlex:LeftParen)  
                    (inc paren-level)
                    (write buff (format {<span class="open-paren%d">(<span class="plain">} paren-level )))
               ((= token-type 'Nlex:RightParen)
                    (write buff (format {</span>)</span>}))
                    (dec paren-level))
               ((= token-type 'Nlex:Symbol)
                    (cond
                      ((find token-value newlisp-variables)
                          (write buff (string {<span class="variable">} (escape-html token-value) {</span>})))
                      ((find token-value obsolete-functions)
                          (write buff (string {<span class="obsolete">} (escape-html token-value) {</span>})))
                      ((find token-value built-in-functions)
                          (write buff (string {<span class="built-in">} (escape-html token-value) {</span>})))
                      (true
                          (write buff (string {<span class="symbol">}   (escape-html token-value) {</span>})))))
               ((= token-type 'Nlex:WhiteSpace)  
                    (write buff {<span class="white-space">})
                    (dostring (s (base64-dec (string token-value))) 
                        (write buff (char s)))
                    (write buff {</span>}))
               ((= token-type 'Nlex:BracedString)  
                    (write buff (string {<span class="braced-string">} "{" (escape-html token-value) "}" {</span>})))
               ((= token-type 'Nlex:QuotedString)  
                    (write buff (string {<span class="quoted-string">} {"} (escape-html token-value) {"} {</span>})))
               ((= token-type 'Nlex:BracketedText)  
                    (write buff (string {<span class="bracketed-string">} {[text]} (escape-html  token-value) {[/text]} {</span>})))        
               ((= token-type 'Nlex:Quote)
                    (write buff (string {<span class="quote">'</span>})))
               ((= token-type 'Nlex:Comment)  
                    (write buff (string {<span class="comment">} (escape-html token-value) {</span>})))
               ((= token-type 'Nlex:Integer)  
                    (write buff (string {<span class="integer">} (int token-value) {</span>})))
               ((= token-type 'Nlex:Float)  
                    (write buff (string {<span class="float">} (escape-html token-value) {</span>})))
               ((= token-type 'Nlex:Scientific)  
                    (write buff (string {<span class="scientific">} (escape-html token-value) {</span>})))
               ((= token-type 'Nlex:Hex)  
                    (write buff (string {<span class="hex-string">} (escape-html token-value) {</span>})))                
               ((= token-type 'Nlex:BracketedCommand)  
                    (write buff (string {<span class="bracketed-command">} token-value  {</span>})))                
               ((= token-type 'Nlex:NaN) ; not a number
                    (write buff (string {<span class="NaN">} token-value  {</span>})))
               ((= token-type 'Nlex:Octal)
                    (write buff (string {<span class="octal">}  token-value {</span>})))
               ((= token-type 'Nlex:BracketedIdentifier) ; bracketed identifier
                    (write buff (string {<span class="octal">[}  token-value {]</span>}))))                    
            ; not an atom, so recurse but don't initialize buffer
            (nlx-to-html i 1)))
   buff)

(define (escape-html txt)
 (if txt
  (begin
   (replace {&} txt {&amp;} 0)
   (replace {<} txt {&lt;} 0)
   (replace {>} txt {&gt;} 0)))
 txt)

(set 'HTML-header-1 [text]<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN"
  "http://www.w3.org/TR/html4/strict.dtd">
<html lang="en">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
 [/text])
 
(set 'HTML-header-2 [text]
            <style type="text/css" media="screen">
            body {color: #000; padding:3px ; margin:0px;}
            a {color: #666; text-decoration: none; font-weight: bold;  }
            a:hover {color: #000; }
            a span {display: none; }
            pre, code { margin: 0px; white-space: pre-wrap; font-family: mono, 'Inconsolata', Monaco, 'Lucida Console'; }
            .symbol { color: #dd3333; background: #ffffff;}
            .built-in {color: #550000; font-weight: bold;}
            .obsolete {color: #ffff00; background: #100;}
            .variable { color: #ff8888; font-weight: bold;}
            .braced-string {color: #226666; background: #fffffe;}
            .quoted-string {color: #226666; background: #fffffe;}
            .bracketed-string {color: #226666; background: #efffff;}
            .quote {color: #224400; background: #efffff;}
            .comment  {color: #666666; font-family: serif;  font-size: 0.9em;  background: #feffff;  }
            .integer {color: #113366; background: #eeffbb; }
            .float {color: #635533; background: #eeffcd;}
            .hex {color: #636633;  background: #eeffdc;}
            .octal {color: #736699; }
            .white-space {background: #ffffff;}
            .plain {background-color: #ffffff}
            
            span.open-paren1 {  color:  #666; }
            span.open-paren1:hover {
                -webkit-transition: background-color 0.7s linear; 
                color: #000;
                background-color: #f90; }
            
            span.open-paren2 {   color:  #666; }
            span.open-paren2:hover {
                -webkit-transition: background-color 0.7s linear; 
                color: #111;
                background-color: #f20; }
            
            span.open-paren3 { color:  #666; }
            span.open-paren3:hover {
                -webkit-transition: background-color 0.7s linear; 
                color: #222;
                background-color: #59f; }
            
            span.open-paren4 {   color:  #666; }
            span.open-paren4:hover {
                -webkit-transition: background-color 0.7s linear; 
                color: #333;
                background-color: #FFA3CF; }
            
            span.open-paren5 {  color:  #666; }
            span.open-paren5:hover {
                -webkit-transition: background-color 0.7s linear; 
                background-color: #BCA9FF; }
            
            span.open-paren6 {  color:  #666; }
            span.open-paren6:hover {
                -webkit-transition: background-color 0.7s linear; 
                background-color: #FFDCA1; }
            
            span.open-paren7 {  color:  #666; }
            span.open-paren7:hover {
                -webkit-transition: background-color 0.7s linear; 
                background-color: #9DFFAA; }
                
            span.open-paren8 {  color:  #666; }
            span.open-paren8:hover {
                -webkit-transition: background-color 0.7s linear; 
                background-color: #ACD2FF; }
            
            span.open-paren9 {   color:  #666; }
            span.open-paren9:hover {
                -webkit-transition: background-color 0.7s linear; 
                background-color: #AFFFFB; }
            
            span.open-paren10 { color:  #666; }
            span.open-paren10:hover {
                -webkit-transition: background-color 0.7s linear; 
                background-color: #EBFFD6; }
            </style>
</head>
[/text])

(define (syntax-highlight source-text title)
    (let ((HTML-text {}))
        (set-up-syntax)
        ; header
        (extend HTML-text HTML-header-1)
        (extend HTML-text (format {<title>%s</title>} title))
        (extend HTML-text HTML-header-2)
        ; start body
        (extend HTML-text {<body><pre><code>})        
        (extend HTML-text (nlx-to-html (Nlex:parse-newlisp source-text)))
        (extend HTML-text {</code></pre>})
        (extend HTML-text {<p>; syntax highlighting by newlisp-parser.lsp </p></body>})
        (extend HTML-text {</html>})
    HTML-text))

; eof