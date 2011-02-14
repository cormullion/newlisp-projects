#!/usr/bin/env newlisp

;; @module mystic-rose-canvas
;; @author cormullion
;; @description draw a mystic rose using HTML 5 canvas
;; @version 0.1 2011-02-14 12:47:34

(set 'html {}) ; output buffer

(define (h txt)
   (extend html (string txt "\n")))

(define (make-canvas canvas-name width height)
 (h (format {<canvas id="%s" width="%d" height="%d" style="border:1px solid #000; background-color: #000;"></canvas>} canvas-name width height)))

(define (draw canvas i j col-list)
    ; col-list is list of 4 values
    ; (push (random 0 0.7) col-list -1)
    (push 0.7 col-list -1)
    (h  (string         
        (format {%s.beginPath();} canvas) "\n"
        (format {%s.moveTo(%f, %f);} canvas (int (i 0)) (int (i 1))) "\n"
        (format {%s.lineTo(%f, %f);} canvas (int (j 0)) (int (j 1))) "\n"
        ; context.fillStyle = 'rgba(0,0,0,0.1)';
        (format {%s.strokeStyle = 'rgba(%d,%d,%d,%f)';} (cons canvas col-list)) "\n"
        (format {%s.stroke();} canvas) "\n"
        (format {%s.closePath();} canvas) "\n"
        )))

(h {<html>
<head>
<meta charset=utf-8>
<title>canvas</title>
</head>
<body>
})

(set 'primary-colors 
'((4 4 4)
  (255 0 0)
  (0 255 0)
  (0 0 255)
  (255 255 0)
  (0 255 255)
  (255 0 255)
  (255 255 255)))

(seed (date-value))

; set up drawing environment
(set 'canvas-name "a" 'width 400 'height 400)
(make-canvas canvas-name width height)
(h (format [text]
<script type="text/javascript" language="javascript" charset="utf-8">
 var a_canvas = document.getElementById("a");
 var %s = a_canvas.getContext("2d");
[/text]  canvas-name ))
(h (format {%s.translate(%f, %f);} canvas-name (div width 2) (div height 2)))
(h (format {%s.rotate(%f);} canvas-name (div 3.141592653 -2)))
(h (format {%s.lineWidth = %f;} canvas-name 0.2))



; draw a mystic rose

; ith point of ngon is  rad * cos ((2pi i) / n),   rad * sin ((2pi i) / n)

(set 'number-of-points 72 'rad 190 'pi 3.141592653)

; make points list

(for (point 0 (- number-of-points 1))
    (push (list  (mul rad (cos (div (mul 2 pi point) number-of-points)))
                 (mul rad (sin (div (mul 2 pi point) number-of-points)))) points-list))

; draw lines connecting points
(set 'b (- (length points-list) 1))

(set 'n 0)

(for (i 0 b) (for (j (+ i 1) b 1 (>= j (length points-list)))
    (inc n)
    (draw canvas-name (points-list i) (points-list j) (apply amb primary-colors))))

(println n)
; finish
(h {</script>
</body></html>})
(write-file "/tmp/noname.html" html)
(exec {open /tmp/noname.html}) ; MacOS X only?

(exit)
