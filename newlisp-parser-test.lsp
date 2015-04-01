#!/usr/bin/env newlisp

(load (string (env {HOME}) {/projects/programming/newlisp-projects/newlisp-parser.lsp}))

(define (test test-name original (display? nil))
    (letn ((parsed       (Nlex:parse-newlisp original)) 
           (new-original (Nlex:nlx-to-plaintext parsed)))
        (set 'parsed-original (parse original) 'parsed-new-original (parse new-original)) 
        (if-not (and (empty? (difference parsed-original parsed-new-original))
                     (empty? (difference parsed-new-original parsed-original )))
            (begin
                (println 
                    (format "test %-40s failed: in old: \t %-60s" test-name  
                        (string (difference (parse original) (parse new-original)))))
                (println 
                    (format "test %-40s failed: in new: \t %-60s" test-name  
                        (string (difference (parse new-original) (parse original))))))
             (println   (format "test %-40s passed" test-name)))
        (set 'original-lines (parse original "\n") 'new-original-lines (parse new-original "\n"))
        (if display? (dolist (line original-lines)
           (print (format "\t%-60s \t %-60s \n" line (new-original-lines $idx) ))))))

; numbers
(test {integers }   {(map + 1 2 3 123 -1212312312 -13 -14 0 3 6 01 02 03 04 05 06 07 08 09 11 16)})
(test {floats }     {(map add 1.2 -1.42 56. )})
(test {scientific } {123 0xE8 123e2 -1.3e-12 1.23 123e-3 45 1.23e4 42 1.123e-54} 'display)
(test {bigint }     {1234623827134691238746213947 12341293847621398476L} 'display)

; bracketed 
(test {[text] brackets }            {[text](+ 1 1)[/text]})
(test {[cmd] brackets }             {[cmd](+ 1 1)[/cmd]})
(test {[CMD] brackets should fail } {[CMD](+ 1 1)[/CMD]})
(test {both brackets }              {[text][cmd](+ 1 1)[/cmd][/text]})
(test {weird bracketed symbols }    [text]myvar A-name [blah] X34-zz [* 7 5 ()};] *111*[/text] 'display)

; strings

(test {strings braced}              [text]{this is a braced string}[/text])
(test {strings brackets}            {[text]this is a bracketed string[/text]})
(test {nested braces}               [text]{this is {nested} braced string}[/text])

; unicode
(test {unicode }                    {{\unnnn} (utf8len "我能吞下玻璃而不伤身体。")} 'display)

; try parsing smaller files
(test {this parser script }         (read-file (string (env {HOME}) {/projects/programming/newlisp-projects/newlisp-parser.lsp})))

;(test {markdown } (read-file (string (env {HOME}) {/projects/programming/newlisp-projects/markdown.lsp})))

;(test {life } (read-file (string (env {HOME}) {/projects/programming/newlisp-projects/life.lsp})))

; bigger files can take some time
;(test {qa-bench} (read-file (string (env {HOME}) {/Downloads/newlisp-10.5.7/qa-specific-tests/qa-bench})))

; test all qa files in newLISP distribution

(map 
    (fn (file)
        (println { processing } file)
        (if-not (catch (test {batch} (read-file (string (env {HOME}) {/projects/programming/lisp/newlisp-10.6.2/qa-specific-tests/} file))) 'error)
                (println " failed " file " " error)))
    (directory 
        (string (env {HOME}) {/projects/programming/lisp/newlisp-10.6.2/qa-specific-tests/}) {qa*}))

(println "\n" {all tests completed})

(exit)
