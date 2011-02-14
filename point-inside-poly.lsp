#!/usr/bin/env newlisp

(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp"))

(gs:init)
(gs:frame 'SuperCollider 100 100 550 550 "SuperCollider")
(gs:set-border-layout 'SuperCollider)
(gs:canvas 'MyCanvas  'SuperCollider)
(gs:add-to 'SuperCollider 'MyCanvas "center")
(gs:set-background 'MyCanvas gs:black)
(gs:set-anti-aliasing true)

(set 'width 550 'height 550
    'x (* 5 (- (rand 3) 1))  'y (* 5 (- (rand 3) 1))
    'delta-x (* 5 (- (rand 3) 1))
    'delta-y (* 5 (- (rand 3) 1))
    'circle-radius 4)

(gs:set-translation (/ width 2 ) (/ height 2))
(gs:set-size 'MyCanvas width height)

(set 'apoly1 '(
        (-0 -85) (-23 -103)
        (-55 -112) (-114 -90)
        (-114 -90) (-75 -103)
        (-38 -89) (-18 -57)
        (-18 -33) (-25 -14)
        (-52 9) (-73 14)
        (-95 11) (-125 -10)
        (-135 -50) (-134 -15)
        (-121 11) (-72 41)
        (-77 76) (-67 106)
        (-21 142) (-50 118)
        (-58 99) (-58 75)
        (-38 42) (-0 27)
        (38 42) (58 75)
        (57 99) (50 118)
        (21 142) (47 128)
        (66 106) (76 76)
        (72 40) (120 10)
        (133 -16) (133 -52)
        (123 -11) (94 11)
        (71 14) (50 9)
        (23 -14) (16 -33)
        (17 -57) (37 -89)
        (75 -103) (97 -99)
        (113 -89) (112 -90)
        (89 -105) (54 -112)
        (22 -103) (0 -85)))

(define (init)
    (gs:fill-rect 'Frame (/ (- width) 2) ( / (- height) 2) width height '(0.1 0.2 0.1))
    (gs:fill-polygon 'Container1 (flat apoly1) gs:black)
    (gs:fill-circle 'Electron x y circle-radius gs:green))

(set 'PI 3.14159 'TWOPI (add PI PI))

(define (angle2d x1 y1 x2 y2)
 ; Return the angle between two vectors 
 ;   The angle is from vector 1 to vector 2, positive anticlockwise
 ;   The result is between -pi -> pi
 (set 'theta1 (atan2 y1 x1))
 (set 'theta2 (atan2 y2 x2))
 (set 'dtheta (sub theta2 theta1))
 (while (> dtheta PI)
  (dec dtheta TWOPI))
 (while (< dtheta (- PI))
  (inc dtheta TWOPI))
 dtheta)

(define (inside? pt poly)
 ; point is a list (x y)
 ; poly is a list of points ((x y) (x y))
 ; uses "the worst algorithm in the world for testing points" 
 ; http://erich.realtimerendering.com/ptinpoly/
 (let ((inside nil)
       (len (length poly))
       (point-x (pt 0))
       (point-y (pt 1))
       (max-x 0) (min-x 0)
       (max-y 0) (min-y 0)
       (angle 0))
  (set 'max-x (first (apply (fn (a b) (if (>= (first a) (first b)) a b)) poly 2)))
  (set 'min-x (first (apply (fn (a b) (if (<= (first a) (first b)) a b)) poly 2)))
  (set 'max-y (last (apply (fn (a b) (if (>= (last a) (last b)) a b)) poly 2)))
  (set 'min-y (last (apply (fn (a b) (if (<= (last a) (last b)) a b)) poly 2)))
  (cond
   ; quick bounds check
   ((or  (< point-x min-x)
     (< point-y min-y)
     (> point-x max-x)
     (> point-y max-y)) inside)
   (true
    ; OK. Do it the hard way.
    (for (i 0 (- len 1)) 
     (set 'p1x (sub (first (nth i poly)) point-x))
     (set 'p1y (sub (last  (nth i poly))  point-y))
     (set 'p2x (sub (first (nth (% (+ i 1) len) poly))  point-x))
     (set 'p2y (sub (last  (nth (% (+ i 1) len) poly))  point-y))
     (set 'angle (add angle (angle2d p1x p1y p2x p2y))))
    (if (< (abs angle) PI)
     (set 'inside nil)
     (set 'inside true))))
  inside))

(define (init)
 (gs:fill-rect 'Frame (/ (- width) 2) ( / (- height) 2) width height '(0.2 0.2 0.3))
 (gs:fill-polygon 'Container1 (flat apoly1) gs:black)
 (gs:fill-circle  'Electron x y circle-radius gs:green))

(define (move-shape)
    ; hit walls, bounce back
    (if (<= x (+ circle-radius (- (/ width 2))))  (set 'delta-x (- delta-x)))
    (if (<= y (+ circle-radius (- (/ height 2)))) (set 'delta-y (- delta-y)))
    (if (>= x (- (/ width 2) circle-radius))      (set 'delta-x (- delta-x)))
    (if (>= y (- (/ height 2) circle-radius))     (set 'delta-y (- delta-y)))
    (inc x delta-x) (inc y delta-y)
    (gs:move-tag 'Electron delta-x delta-y)
    (if (inside? (list x y) apoly1)
        (begin
            ; change color of electron
            (gs:color-tag 'Electron gs:white)
            ; mark if we find something
            (if (= 0 (rand 6)) (gs:fill-circle 'Target x y circle-radius gs:yellow))
            ; randomize movement
            (set 'delta-x  (* 5 (- (rand 3) 1)))
            (set 'delta-y  (* 5 (- (rand 3) 1))))
        (gs:color-tag 'Electron gs:red))
    (gs:update))

(gs:set-visible 'SuperCollider true)
(init)
(while (gs:check-event 10000)
	(move-shape))