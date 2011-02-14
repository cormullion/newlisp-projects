#!/usr/bin/env newlisp

;; @module Twitter-graph search plugin for Dragonfly
;; @author cormullion

;===============================================================================
; !Loading plugin into Dragonfly context
;===============================================================================

(context 'Twitter-graph)

; graphing utility functions (warning: dynamic scoping ahead!)

(define (out str)
  ; shortcut for sending html to buffer 
  (write-buffer html str))
  
(define (scale x)
  ; quick scaling of numbers
  (int (mul x (div graph-width how-many-days))))

(define (line-to x y)
  (out (format "a_context.lineTo(%d, %d);\n" x y)))

(define (move-to x y)
  (out (format "a_context.moveTo(%d, %d);\n" x y)))

(define (text-at str x y)
  (out (format {a_context.fillText("%s", %d, %d);} str x y)))

(define (twitter-graph data search-string)
; main function: data is a list of unix times of tweets
; search string is the string searched for - we want that cos we're cacheing newLISP tweets
    (let ((html {})
          (graph-width 500)
          (graph-height 120)
          (nl "\n")
          (lt (char 60)) ; avoids angle brackets ...
          (gt (char 62))
          (data-values '())
          (start 0)
          (end 0)
          (start-of-day 0)
          (zero-data-values '())
          (how-many-days 0)
          (canvas-coords '()))
    ; if this is a newLISP search, then we want to use data from the cache
    ; save values and reload from cache
    (cond 
      ((and (= search-string "newlisp" )
            (file? "databases/tweet-cache.lsp"))
               (load "databases/tweet-cache.lsp")
               (set 'data-values (unique (append data-values data))))
      (true
           (set 'data-values data)))
    
    ; data-values is list of int-seconds values for each tweet

    ; needs sorting again? 
    (sort data-values)
    
    ; start it off
    (out (format (string lt {canvas id="%s" width="%d" height="%d"} gt lt {/canvas} gt) "a" (+ 15 graph-width) graph-height))
    (out (string lt {script type="text/javascript" language="javascript" charset="utf-8"} gt nl {var a_canvas = document.getElementById("a");} nl {var a_context = a_canvas.getContext("2d");} nl {a_context.font = "bold 10px sans-serif";} nl {a_canvas.background = "#000";} nl))
   
    ; first value probably starts sometime during the day
    ; so find the offset of the first event from midnight
    (set 'start (Time (first data-values)) 'end (Time (last data-values)))
    
    ; get int-seconds of start of first day
    (set 'start-of-day (:unix-time (:midnight start)))

    ; convert values so that they start at 0
    (set 'zero-data-values (map (fn (x) (- x start-of-day)) data-values))
    
    ; how many days are we doing?
    (set 'how-many-days (+ (ceil (float (:show (Duration (:period start end))))) 1))
    
    ; canvas coordinates are basically the seconds from the start,  
    ; scaled by total width of graph (number of days)
    (set 'canvas-coords (map (fn (n) (div (mul graph-width (div n 86400)) how-many-days)) zero-data-values))
    
    ; graph parameters
    (set 'day-width (div graph-width how-many-days)
    'top-data-line-y 5 
    'bottom-data-line-y 64 
    'top-day-marker-y 65 
    'bottom-day-marker-y 80 
    'top-6hour-marker-y 65 
    'bottom-6hour-marker-y 70 
    'day-number-text-y 90
    ; we move month names up and down to prevent overlap
    'month-offsets '(120 110 100))
    
    ; draw the data lines
    (dolist (x canvas-coords)
      (move-to x top-data-line-y)
      (line-to x bottom-data-line-y))

    ; style and stroke the data lines
    (out (string {a_context.lineWidth = "0.7";} nl {a_context.strokeStyle = "#f00";} nl {a_context.stroke();} nl ))
    
    ; draw the dividers numbers etc. 
    (out (string {a_context.beginPath();} nl))

    (for (d 0 how-many-days 0.25) ; something every 6 hours
      ; which day is this?
      (set 'day (:day (:shift (copy (:midnight start)) d "days")))
      (cond 
        ((and (= day 1) (= d (round d)))
          ; first day of month
          ; draw divider
          (move-to (scale d) top-day-marker-y)
          (line-to (scale d) bottom-day-marker-y)
          ; draw new month name
          ; move months up and down so that they don't overlap
          (text-at (:month-name (:shift (copy (:midnight start)) d "days")) (scale d) (first (rotate month-offsets)))
          ; draw day-number if scale not too small
          (if (> day-width 5) (text-at (string day) (scale d) day-number-text-y)))
        ((= d (round d))
          ; draw divider
          (move-to (scale d) top-day-marker-y)
          (line-to (scale d) bottom-day-marker-y)
          ; for most days, draw day dividers if the scale's not too small
          (if (> day-width 5) (text-at (string day) (scale d) day-number-text-y)))
        ((> day-width 30)
          ; draw 6 hour markers if scale permits
          (move-to (scale d) top-6hour-marker-y)
          (line-to (scale d) bottom-6hour-marker-y))))

    ;style and stroke dividers etc
    (out (string {a_context.lineWidth = "0.7";} {a_context.strokeStyle = "#000";} nl {a_context.stroke();} nl ))
    
    ; always draw first month name (this will be a problem only when the first data point is on the first of the month)
    (out (format {a_context.fillText("%s", 1 , %d);} (:month-name start) (first (rotate month-offsets))))
    
    ; finish
    (out (string lt {/script} gt))
    (println html)
    
    ; don't forget to save for next time
    (if (= search-string "newlisp") (save "databases/tweet-cache.lsp" 'data-values))
    ))
    
(context MAIN)
; eof