#!/usr/bin/newlisp 
;; @module life.lsp
;; @description A simple version of the well-known Life diversion. 
;; @version 0.2
;; @author cormullion
;;
;; 2008-10-08 21:48:55
;; updated for newLISP version 10. Does not now work with version 9.
;;
;; The well-known Life game (originally devised by John Horton-Conway) is a good way
;; to explore the basics of the newLISP-GS system. The Life part of the code consists 
;; of an array, and a function that updates the array according to the rules of Life. 
;; For the graphical display, there's a loop that sets up the board as a canvas 
;; with a bunch of tagged circles, and a function that scans the array and shows or hides these 
;; tagged graphics according to the value of the array's cell.
;;
;; To-Do: some minor bugs around the edge of the board. Flat-earthers beware.

(set 'rows 30 'cols 30 'cell-size 20 'cell-radius 10) 

;; @syntax (new-world)
;;
;; Set up a new "world" and fill with random values.
(define (new-world) 
   (seed (date-value)) 
   (set '*world* (array rows cols (rand 2 124))) 
   (set '*generation-counter* 0)) 

;; @syntax (cell-lives? <r> <c>)
;;
;; @param <r> the row
;; @param <c> the column
;;
;; Is this cell alive?
(define (cell-lives? r c) 
   (and 
      (< r rows) 
      (< c cols) 
      (> r 0) 
      (> c 0)
      (> (*world* r c) 0))) 

;; @syntax (generation)
;; Update the array. Return true if there's been a change. (So you could detect
;; a stagnant world.
;; @return boolean
(define (generation) 
   (let 
      ((next-world (array rows cols '(0))) 
       (changed nil) 
       (r 0) 
       (c 0) 
       (neighbours 0) 
       (old-state 0) 
       (new-state 0)) 
   (for (r 0 (- rows 1)) 
      (for (c 0 (- cols 1)) 
         (set 'neighbours 0) 
         (if (cell-lives? (- r 1)     (- c 1))    (inc neighbours)) 
         (if (cell-lives? (- r 1)     c)          (inc neighbours)) 
         (if (cell-lives? (- r 1)     (+ c 1))    (inc neighbours)) 
         (if (cell-lives? r           (- c 1))    (inc neighbours)) 
         (if (cell-lives? r           (+ c 1))    (inc neighbours)) 
         (if (cell-lives? (+ r 1)     (- c 1))    (inc neighbours)) 
         (if (cell-lives? (+ r 1)     c)          (inc neighbours)) 
         (if (cell-lives? (+ r 1)     (+ c 1))    (inc neighbours)) 
         (set 'old-state (*world* r c)) 
         (set 'new-state 
            (or 
               (and (= old-state 0) (= neighbours 3)) 
               (and (= old-state 1 ) (or (= neighbours 2) (= neighbours 3)))))          
         (if new-state (set 'new-state 1) (set 'new-state 0)) 
         (if (and 
               (= changed nil) 
               (!= new-state old-state)) 
             (set 'changed true)) 
         (setf (next-world r c) new-state)) ; cols    ;;;; !!!! newLISP 10 only!!!
         ) ; rows 
   (set '*world* next-world) 
   (set 'next-world '()) 
   changed) 
) 

;; @syntax (restart-button-action)
;; Create a new world.
(define (restart-button-action) 
   (new-world))

;; @syntax (draw-world)
;; Draw the current population by showing or hiding the cells of the grid.
;; The tags are "Cell" row column...
(define (draw-world) 
   (let ((r 0) 
         (c 0)) 
   (for (r 0 (- rows 1)) 
      (for (c 0 (- cols 1)) 
         (if (= (*world* r c) 0) 
            (gs:hide-tag (string {Cell} r c)) 
            (gs:show-tag (string {Cell} r c)) 
            ))))) 

(if (= ostype "Win32") 
    (load (string (env "PROGRAMFILES") "/newlisp/guiserver.lsp")) 
    (load "/usr/share/newlisp/guiserver.lsp") 
) 

(gs:init)
(gs:frame 'ConwayLife 100 100 640 640 "Life") 
(gs:set-border-layout 'ConwayLife ) 
(gs:canvas 'MyCanvas  'ConwayLife) 
(gs:panel 'Panel) 
(gs:label 'T " ") 
(gs:button 'Restart 'restart-button-action "restart") 

(gs:add-to 'Panel 'Restart 'T) 
(gs:add-to 'ConwayLife 'MyCanvas "center" 'Panel "south") 

(gs:set-background 'MyCanvas '(.8 .9 .7 .8)) 
(gs:set-anti-aliasing true) 
(gs:set-visible 'ConwayLife true) 

; move down so that you can see the top blobs properly 
(gs:set-translation (/ cell-size 2) (/ cell-size 2) ) 

(new-world) 

; set up the graphical display, tagging the circles so that we can show or hide them
; later

(for (r 0 (- rows 1)) 
   (for (c 0 (- cols 1)) 
      (gs:fill-circle (string {Cell} r c) 
         (* c cell-size) 
         (* r cell-size) 
         cell-radius 
         '(.2 .5 .4 )))) 

(while (gs:check-event 1000) 
      (draw-world) 
      (generation) 
      (gs:set-text 'T (string {generation } (inc *generation-counter*)))) 
