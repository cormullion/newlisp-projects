#!/usr/bin/env newlisp				

;; @module TicTacToe
;; @author cormullion
;; @description simple tic tac toe
;; @version 0.0.2 2008-10-08 21:33:37
;;  version 0.0.1 2008-05-10 22:48:36
;; just waiting for you to add some intelligence to (generate-computer-move) ! 

(load "/usr/share/newlisp/guiserver.lsp")

(gs:init)
(gs:frame 'TicTacToe 100 100 360 400 "TicTacToe")
(gs:set-border-layout 'TicTacToe)
(gs:canvas 'Ttt-Canvas)
(gs:set-size 'Ttt-Canvas 360 360)
(gs:set-background 'Ttt-Canvas '(.3 .4 .5 .4))
(gs:mouse-released 'Ttt-Canvas 'mouse-released-action true)
(gs:label 'Status "Loading...")
(gs:set-font 'Ttt-Canvas  "Sans Serif" 60 "bold")

; playing grid
(set '*grid* (dup nil 9))

; size of squares in pixels
(set 'size 120)

; winning positions
(set '*winning-positions* '((0 1 2) (3 4 5) (6 7 8) (0 3 6) (1 4 7) (2 5 8) (0 4 8) (2 4 6)))

; game functions

;; @syntax (mouse-released-action x y button modifiers tags)
;; 
(define (mouse-released-action x y button modifiers tags)
  { extract the tag of the clicked square }
   (let ((move (int (string (first tags)) -1 10)))
        (do-human-move move)))

;; @syntax (flash-the-win)
;; 
(define (flash-the-win)
  { flash the winning line }
  (let ((winning-line (nth (last *winner*) *winning-positions* )))
    (dotimes (x 40)
       (if (< ((now) 6) 500000) ; less than 500k milliseconds 
          (map (fn (tg) (gs:hide-tag (string tg))) winning-line)
          (map (fn (tg) (gs:show-tag (string tg))) winning-line))
       (sleep 100))))

;; @syntax (square-to-xy square)
;; 
(define (square-to-xy square)
  { convert grid square number to x/y (column row) }
  (list (mod square 3) (/ square 3)))

;; @syntax (display)
;; 
(define (display)
  { draw grid and plays }
  (local (x y)
     (for (i 0 8)
        (map set '(x y) (square-to-xy i))
        (cond 
          ((= (*grid* i) 'X)    (set 'colour gs:white))
          ((= (*grid* i) 'O)    (set 'colour gs:black))
          (true                 (set 'colour gs:gray)))
        ; delete previous squares and redraw
        (gs:delete-tag (string i))
        (gs:fill-rect  (string i) (* x size) (* y size) (- size 2) (- size 2) colour)
        ; delete previous X or O text and redraw
        (gs:delete-tag (string "text" i))
        (gs:draw-text  
           (string "text" i) ; tag
           (if (*grid* i) (string (*grid* i)) "") ; text to display
           (+ 35 (* x size)) ; move text slightly right
           (+ 78 (* y size)) ; move text down
           (if (= colour gs:white) gs:black gs:white) ; invert colour
        ))))

;; @syntax (available-moves)
;; 
(define (available-moves)
  (index nil? *grid*))

;; @syntax (generate-computer-move)
;; 
(define (generate-computer-move)
  { Here you could add code to find the best move :) }
  { this just chooses at random... }
  (apply amb (available-moves)))

;; @syntax (check-move move)
;; 
(define (check-move move)
 { check if move is valid }
 (and (<= 0 move 8) (find move (available-moves))))

;; @syntax (won? player)
;; 
(define (won? player)
  { check if a player has won }
  ; get all squares marked by this player
  (letn ((player-squares (index (fn (x) (= x player)) *grid*))
         ; are these squares found one of the winning positions?
         (wins-for-player (map (fn (win) (= win (intersect win player-squares))) *winning-positions*)))
      ; wins-for-player is something like {nil nil nil true nil nil nil... )
      (if (exists true? wins-for-player) 
          ; return *winner* and index of winning position
          (set '*winner* (list player (find true wins-for-player))))))

;; @syntax (game-over?)
;; 
(define (game-over?)
  { is the game over yet? }
  (or (won? 'X) (won? 'O) (empty? (available-moves))))

;; @syntax (do-computer-move)
;; 
(define (do-computer-move)
  { the computer has a move }
  (and (not (game-over?) (= *turn* "computer"))
       (gs:set-text 'Status "thinking...")
       (gs:update)
       (sleep 2000)
       (setf (*grid* (generate-computer-move)) 'O) 
       (set '*turn* "human")
       (gs:set-text 'Status "Your move")))

;; @syntax (do-human-move move)
;; 
(define (do-human-move move)
 { the human has made a move }
 (and (not (game-over?) (= *turn* "human"))
      (check-move move)
      (setf (*grid* move) 'X)
      (display)
      (set '*turn* "computer")
      (do-computer-move)
      (display)))

(gs:add-to 'TicTacToe 'Ttt-Canvas "center" 'Status "south")
(gs:set-visible 'TicTacToe true)

(while true
  (display)
  (set '*turn* "human" '*winner* nil)
  (gs:set-text 'Status "New game. Your move")
  (do-until (game-over?) (gs:check-event 1000000))  
  (cond
    (*winner* (gs:set-text 'Status (string (first *winner*) " wins")) 
              (flash-the-win))
    (true     (gs:set-text 'Status "it's a draw")
              (sleep 3000)))
  ; reset for another game
  (set '*grid* (dup nil 9)))

; eof
