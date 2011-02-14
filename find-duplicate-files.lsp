#!/usr/bin/env newlisp
;; @module find-duplicate-files
;; @author cormullion@mac.com
;; @version 0.0.5  2009-12-03 17:26:24
;;
;; This newLISP finds duplicate files. MacOS-specific but could probably
;; be adapted. It ignores filenames, instead testing sizes and MacOS resource forks.
;; Options: move to a duplicates folder, add comment to spotlight, set finder label
;; Usage: find-duplicate-files.lsp [folder1 folder2 ...] [-usage] [-move] [-move-dummy] [-spotlight] [-label]
;; doesn't follow Finder aliases either... :(
;; Good for 50000 files and possibly more.

(define (set-spotlight-comment file comment)
;; @syntax (set-spotlight-comment file comment)
;; @param file pathname of file
;; @param comment text to assign to file
;; Set the comment field of a file so that Spotlight can find it. (MacOS X only)
  (exec (format [text]osascript -e 'set pf to POSIX file "%s" ' -e 'tell application "Finder" to set comment of pf to "%s" ' [/text] file comment)))

(define (set-finder-label file (color 0))
;; @syntax (set-finder-label <file> <int-label-index>)
;; @param <file> pathname of file
;; @param <int-label-index> label index (colour)
;; Set the Finder label of a file. (MacOS X only)
  (let (colors '(nil orange red yellow blue purple green gray))
    (set 'color (find color colors))
    (exec (format [text]osascript -e 'tell application "Finder" to set label index of alias POSIX file "%s" to %d'[/text] file color))))

(define (walk-tree dir)
;; @syntax (walk-tree <dir>)
;; @param <dir> a directory to walk
;; Recursively examine folder and built a list of the
;; files, their sizes, and their resource fork sizes too.
(if (set 'folder (directory dir {^[^.]}))
   (dolist (nde folder)
     (if (directory? (append dir "/" nde))
         (walk-tree (real-path (append dir "/" nde)))
         (do-file (append dir "/" nde))))))
       
(define (do-file item)
;; @syntax (do-file <item>)
;; @param <item> a file
;; Process a file. Checks it then adds it to a list for comparison.
   (and
          (not (starts-with item ".")) ; skip hidden files
          (set 'path-name (real-path item))
          (file-info path-name) ; skip symlinks...
          (set 'dataforksize (first (file-info path-name))) 
          (if (file? (format {%s/..namedfork/rsrc} path-name )) 
           ; add resource fork size if one exists at /..namedfork/rsrc
             (begin
               (set 'resourceforksize (first (file-info (format {%s/..namedfork/rsrc} path-name ))))
               )
             (set 'resourceforksize 0))
          ; put composite file size and file name into dupe-list
          (push (cons (+ dataforksize resourceforksize ) path-name ) dupe-list -1 )))

(set 'blank (curry dup " "))

; start

(if (find "-usage" (main-args))
  (println [text]
Usage: find-duplicate-files.lsp [folder1 folder2 ...] [-usage] [-move] [-move-dummy] [-spotlight] [-label]
Find duplicate files in the specified folders ... or in the current folder and subfolders.
-move       moves one of the duplicates to a folder 'duplicates' in the current folder
-move-dummy pretends to move - you can see what might happen...
-spotlight  puts the word 'duplicate' in the comments field of the file
-label      sets the Finder label to red
[/text] (exit)))

(if (find "-move" (main-args)) (set 'move true) (set 'move nil))
(if (find "-label" (main-args)) (set 'label true) (set 'label nil))
(if (find "-move-dummy" (main-args)) (set 'move-dummy true) (set 'move-dummy nil))
(if (find "-spotlight" (main-args)) (set 'spotlight true) (set 'spotlight nil))

; drop first argument (newlisp) and remove options 
(set 'file-args (rest (clean (fn (arg) (starts-with arg "-")) (main-args))))

(if (> (length file-args) 1)
(begin
  (dolist (folder (rest file-args))
    (println "... gathering files in folder " (real-path folder) "\n") 
      (walk-tree folder)))
  (begin 
    (println "... gathering files in current folder " (real-path) "\n") 
    (walk-tree (real-path))))

(println "... sorting " (length dupe-list) " items\n")

(if-not dupe-list (exit))

(set 'dupe-list (sort dupe-list )) ; sort by size - very important!
(println "... duplicates are: \n")

; see if two adjacent items have the same size

; this is a kludge to avoid an error
; If we start with item 1, we have no 'previous' pair for comparison
; I'd really like to start at item 2...

(set 'previous (last dupe-list))
(set 'dupe-counter 0)

(dolist (current dupe-list)
 (if (= (first current) (first previous)) ; current same size as previous?
  (and
   ; same size, compare md5 checksums

   ; hack to fix single quotes in file names
   ; escape single quotes and then use $' ' in bash (ANSI-C Quoting)
   (set 'current-file-name (last current))
   (set 'previous-file-name (last previous))
   (replace "'" current-file-name "\\'")
   (replace "'" previous-file-name "\\'")
   
   (set 'current-dataforkmd5 
    (exec (format {md5 -q $'%s'} current-file-name)))
    
   (set 'current-resourceforkmd5  
    (exec (format {md5 -q $'%s/..namedfork/rsrc'} current-file-name)))
    
   (set 'previous-dataforkmd5 
    (exec (format {md5 -q $'%s'} previous-file-name)))
   
   (set 'previous-resourceforkmd5 
    (exec (format {md5 -q $'%s/..namedfork/rsrc'} previous-file-name)))

   (and 
     (> (+ (first current) (first previous) 0)) ; not 0 
     (= current-dataforkmd5 previous-dataforkmd5 )
     (= current-resourceforkmd5 previous-resourceforkmd5)
     
     ; yay, it's a duplicate
     
     (inc dupe-counter)
     
     (println (format "     %12d %s"   (first previous) previous-file-name))
     (println (format "   = %12d %s\n" (first current)  current-file-name))
     
     ; do we want to set the spotlight comments?
     (if spotlight
       (set-spotlight-comment current-file-name (string "duplicate " previous-file-name))
       true) ; continue the 'and' if we didn't do comments
     
     ; or we can set Finder colors...?
     (if label
       (begin
        (set-finder-label (last current) 'red)
        (println (blank 10) {labelled!}))
       true)
     
     ; if we're moving or pretending to move
     (if (or move move-dummy)
        (begin
          (set 'parent 
            (join (reverse (rest 
              (reverse (parse (real-path (last current)) {/}))) ) "/"))
          (set 'path (string (last (parse (real-path (last current)) {/}))))
          
          (if (not move-dummy)
            (set 'rename-result (rename-file 
              (string parent "/" path) ; old name
              ; (string parent {/} (string {dup} path)) ; rename with 'dup' in front?
              (string {duplicates/} path)))) ; or move. This folder must exist...
          (if move-dummy 
            (println 
              (blank 10) {You would have tried to rename } "\n" 
              (blank 10) (string parent "/" path)  "\n"
              (blank 10) { to } (string {duplicates/} path)))
              
          (println 
            (blank 10)
            (if rename-result 
              {You succeeded in renaming }
              {You didn't rename }) 
            (blank 10) (string parent "/" path)  "\n"
            (blank 10) { to } (string {duplicates/} path) "\n"))
        true)
  ))
 
  ; remember this one for the next comparison
  (set 'previous current)))

(println dupe-counter { duplicates found})
(println "... finished")
(exit)
