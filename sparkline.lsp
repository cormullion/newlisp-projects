#!/usr/bin/env newlisp

;; @module sparkline
;; @description Draw little pictures - interactive sparklines from numeric data
;; @version 0.1 2008-02-26 15:54:10
;; @author cormullion

(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(define (plot data)
  ; plot every element in list 'data' as a y coordinate
  (letn ((old-x-coord 0)
          (old-y-coord 0)
          (x-coord h-scale)
          (y-coord 0))
    (set 'max-y (apply max data))
    (set 'min-y (apply min data))
    (set 'range (sub max-y min-y))
    (if (= range 0) (set 'range max-y))
    (set 'scale (mul v-scale (div sparkline-height (add 0.1 range))))
    ; set first coordinate now
    (set 'old-y-coord (int (mul (pop data) (div scale 2.5) -1)))
                  
    ; canvas grows to accommodate data - will almost fill window
    (set 'canvas-width (min (- window-width 2) (max 20  (* (length data) h-scale))))
    (gs:set-size 'sparkline-canvas canvas-width sparkline-height)
    
    ; draw x-axis
    (gs:set-stroke 0.2)
    (if show-x-axis (gs:draw-line 'L 0 0 canvas-width 0 '(.9 .1 .1)))
    (gs:set-stroke line-weight)
    
    ; set canvas translation
    (gs:set-translation 0 translation)
    
    ; if there are too many points, the excess will get plotted outside the canvas :(
    (when (> (length data) 250) 
        (gs:set-text 'status (string { too many points })) 
        (sleep 50))
    
    ; draw remaining data points
    (dolist (y data)
      ; scale y and reverse sign so that + goes up :)
      ; I can't remember why 2.5 was used
      ; it's obviously Cormullion's Constant ;/
      (set 'y-coord (int (mul y (div scale 2.5) -1)))
      (gs:draw-line 'L old-x-coord old-y-coord x-coord y-coord
          (list red green blue))
      (set 'old-x-coord x-coord 'old-y-coord y-coord)
      (inc x-coord h-scale))))

(define (sparkline data)
  (gs:delete-tag 'L)
  (plot data)
  (gs:update))

; the handlers for the controls

(define (sparkline-height-slider-handler id value)
   (set 'sparkline-height (int value))
   ; need to change translation if size changes?
   (set 'translation (/ sparkline-height 2))
   (update-spark))

(define (horizontal-slider-handler id value)
  (set 'h-scale (int value))
  (update-spark))

(define (vertical-slider-handler id value)
  (set 'v-scale (div (int value) 10))
   (update-spark))

(define (lineweight-slider-handler id value)
  (set 'line-weight (div (int value) 10))
   (update-spark))

(define (translation-slider-handler id value)
 ; shift canvas up or down
  (set 'translation (+ (div (- 50 (int value)) 2) (/ sparkline-height 2)))
  (update-spark))

(define (textarea-event)
  (set 'data (filter number? (map float (parse (gs:get-text 'data-input-area)))))
  (update-spark))

(define (checkbox-handler)
  (if (= "MAIN:show-x-axis-checkbox" (args 0))
    (if (true? (args 1))
      (begin (gs:set-selected 'show-x-axis-checkbox true)  (set 'show-x-axis true))
      (begin (gs:set-selected 'show-x-axis-checkbox nil)   (set 'show-x-axis nil))))
  (update-spark))

(define (update-spark)
    (gs:set-text 'status "updating sparkline")
    (if data
        (sparkline data))
    (gs:set-text 'sparkline-height-label
        (string {height (pixels) } sparkline-height))
    (gs:set-text 'h-scale-label
        (string {horizontal scale } h-scale))
    (gs:set-text 'v-scale-label
        (string {vertical scale } v-scale))
    ; user-friendly presentation of this translation factor ...
    (gs:set-text 'translation-label
        (string {vertical shift }
            (- (/ sparkline-height 2) translation)))
    (gs:set-text 'lineweight-label
        (string {line weight } line-weight))
    (gs:set-text 'red-slider-label
        (string "red " red))
    (gs:set-text 'green-slider-label
        (string "green " green))
    (gs:set-text 'blue-slider-label
        (string "blue " blue))
    (gs:set-text 'status "")
    (gs:update))

(define (export-handler)
   (gs:save-file-dialog 'f 'export))

(define (export)
   (and  (= "save" (args 1))
         (> (length (args)) 2)
         (set 'fname (base64-dec (args 2)))
         (gs:set-text 'status "exporting to PNG...")
         (gs:update)
         (gs:export fname)
         (gs:set-text 'status "finished export")))

(set 'h-scale 1 'v-scale 1 'sparkline-height 18 'line-weight 1  'canvas-width 380  'window-width 400 'red 0 'green 0 'blue 0)


(gs:init)
(gs:frame 'f 50 50 window-width 475)
(gs:set-resizable 'f nil)
(gs:set-flow-layout 'f)

(gs:canvas 'sparkline-canvas)
(gs:set-size 'sparkline-canvas canvas-width sparkline-height)
(gs:set-background 'sparkline-canvas 1 1 1) 
(gs:set-stroke line-weight)

; controls
(gs:panel 'controls )
(gs:set-grid-layout 'controls 10 1)

(gs:text-area 'data-input-area 'textarea-event "text/plain")
(gs:set-size 'data-input-area 400 80)

; sparkline height
(gs:slider 'sparkline-height-slider 'sparkline-height-slider-handler "horizontal" 10 50 sparkline-height)

; horizonal scale goes from 1 to 300 and isn't scaled - it's pixels 
(gs:slider 'horizontal-slider 'horizontal-slider-handler "horizontal" 1 100 h-scale)

; vertical scale slide goes from 20 to 0, and will be divided by 10 -> 2 to 1
(gs:slider 'vertical-slider 'vertical-slider-handler "horizontal" 0 20 10)

; line weight goes from 0 20 and will be divided by 10 -> 2 to 0.0
(gs:slider 'lineweight-slider 'lineweight-slider-handler "horizontal" 0 20 10)

; translation  
(gs:slider 'translation-slider 'translation-slider-handler "horizontal" 0 100 50)

(gs:check-box 'show-x-axis-checkbox 'checkbox-handler "x-axis")
(gs:set-selected 'show-x-axis-checkbox true)
(set 'show-x-axis true)

; labels

(gs:label 'sparkline-height-label "sparkline height" "right")
(gs:label 'h-scale-label "horizontal scale" "right")
(gs:label 'v-scale-label "vertical scale" "right")
(gs:label 'lineweight-label "line weight" "right")
(gs:label 'translation-label "translation" "right")
(gs:label 'red-slider-label "0" "right")
(gs:label 'green-slider-label "0" "right")
(gs:label 'blue-slider-label "0" "right" 50 10)
(gs:label 'status "status" "left")

; export
(gs:button 'Export 'export-handler "Export")

(define (colour-handler id value)
  (cond 
     ((= id "MAIN:red-slider") 
        (set 'red (div value 100))
        (gs:set-text 'red-slider-label (string "red " red)))
     ((= id "MAIN:green-slider") 
       (set 'green (div value 100))
        (gs:set-text 'green-slider-label (string "green " green)))
     ((= id "MAIN:blue-slider") 
       (set 'blue (div value 100))
       (gs:set-text 'blue-slider-label (string "blue " blue)))
     )
   (update-spark))

; colour
(gs:slider 'red-slider   'colour-handler "horizontal" 0 100 0)
(gs:slider 'green-slider 'colour-handler "horizontal" 0 100 0)
(gs:slider 'blue-slider  'colour-handler "horizontal" 0 100 0)

; add
(gs:add-to 'controls 
    'sparkline-height-slider 'sparkline-height-label
    'horizontal-slider 'h-scale-label 
    'vertical-slider 'v-scale-label 
    'translation-slider 'translation-label
    'lineweight-slider 'lineweight-label
    'red-slider 'red-slider-label
    'green-slider 'green-slider-label
    'blue-slider 'blue-slider-label
    'show-x-axis-checkbox 
    'Export
    'status)
(gs:add-to 'f 'data-input-area 'sparkline-canvas 'controls)

; set initial translation of canvas, expecting + and - data points...
(set 'translation (/ sparkline-height 2))
(gs:set-visible 'f true)
(update-spark)
(gs:listen)

(exit)
; eof