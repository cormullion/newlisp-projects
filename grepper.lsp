#!/usr/bin/env newlisp

;; @module grepper
;; @author cormullion 
;; @version 0.0.1 2007-11-12 18:27:31
;;
;; This newLISP-GS file is an interactive regex tester.
;; You type some text in one pane, the regex in another,
;; and the match is displayed in the third.

; works in both newLISP 10 and newlisp 9

(load (string (env "NEWLISPDIR") "/guiserver.lsp"))

(gs:init) 
(gs:frame 'RegexTester 200 200 650 650 "RegexTester")
(gs:panel 'MainPanel)
(gs:set-grid-layout 'MainPanel 2 2)

(gs:text-area 'string-input 'textfield-handler)
(gs:text-pane 'regex-input 'textfield-handler)
(gs:text-pane 'output 'gs:no-action)
 
(gs:set-background 'string-input '(0.9 0.9 0.8))
(gs:set-background 'regex-input '(0.9 0.7 0.7))
(gs:set-background 'output '(0.8 0.9 0.8))

(gs:set-font 'string-input "Sans Serif" 14)
(gs:set-font 'regex-input  "Sans Serif" 14)

(gs:panel 'ControlPanel)
(gs:set-grid-layout 'ControlPanel 13 1)

; The options checkboxes, defined in a list.
(set 'checkboxes 
  '((CheckBox1  {caseless}) 
    (CheckBox2  {multiline})
    (CheckBox3  {. matches all})
    (CheckBox4  {extended})
    (CheckBox5  {anchored})
    (CheckBox6  {$ matches at end of string, not before newline})
    (CheckBox7  {-})
    (CheckBox8  {first ch, not start of line ^ shouldn't match})
    (CheckBox9  {last char, not end of line $ shouldn't match})
    (CheckBox10 {invert greediness of quantifiers})
    (CheckBox11 {empty string considered invalid})
    (CheckBox12 {UTF8})))

; make the checkboxes
(dolist (cb checkboxes)
   (gs:check-box (first cb) 'checkboxing (last cb))
   (gs:set-selected (first cb) nil)
   (gs:add-to 'ControlPanel (first cb)))

(gs:add-to 'MainPanel 'string-input 'regex-input 'output 'ControlPanel)
(gs:add-to 'RegexTester 'MainPanel)
(gs:set-visible 'RegexTester true)

; checkbox handler
(define (checkboxing)
  (let ((widget (args 0))
        (value  (args 1))
        (option-number {})
       )
  (if (find {MAIN:CheckBox} widget)
    (begin
      (set 'option-number (replace {.*?(\d+)} widget $1 0)) 
      ; option-number should now be 1 2 3 4 5 to identify the checkbox... 
      (set (sym (string {option} option-number)) (if value 1 0))
      (if (= (eval (sym (string {option} option-number))) 1)
          (gs:set-selected (string {MAIN:CheckBox} option-number) true)
          (gs:set-selected (string {MAIN:CheckBox} option-number) nil))
      ; set the option global
      (set '*opt* 
        (+ (* 1 option1) 
           (* 2 option2) 
           (* 4 option3) 
           (* 8 option4) 
           (* 16 option5)
           (* 32 option6)
           (* 64 option7)
           (* 128 option8)
           (* 256 option9)
           (* 512 option10)
           (* 1024 option11)
           (* 2048 option12)
           ))))
  ; perform the regex again with the new options
  (do-match)
 )
) 

(define (do-match)
  (set 'results {})
  (set '$1 {} '$2 {} '$3 {} '$4 {} '$5 {} '$6 {} '$7 {} '$8 {} '$9 {})
  (if (catch (regex *rgx* *strng* *opt*) 'error)
      (begin
          (for (i 0 9) (push  (string {$} i { } (if ($ i) ($ i) {}) "\n") results -1))
          (gs:set-text 'output (string "Option: " *opt* "\n" results "\n" error))
          )
      (gs:set-text 'output (string {Problem: } error))))    
      
(define (textfield-handler id text)
	(gs:get-text id 'gettextcallback-handler))

(define (gettextcallback-handler id text)
  (and
    text
    (cond
       ((= id "MAIN:string-input")
          (set '*strng* (base64-dec text)))
       ((= id "MAIN:regex-input")
          (set '*rgx* (base64-dec text)))
       true
          (gs:no-action))
    (not (for-all empty? (list *rgx* *strng* *opt*)))
    (do-match *rgx* *strng* *opt*)
   )
)

(map set '(option1 option2 option3 option4 option5 option6 option7 option8 option9 option10 option11 option12) (dup 0 12))

(set 
 '*opt* 0 
 '*strng* {newLISP supports Perl-compatible regular expressions.} 
 '*rgx* "(n.{1,3}).*(rl-)")

; initial display
(gs:set-text 'string-input *strng*)
(gs:set-text 'regex-input *rgx*)
(do-match)

(gs:listen)
