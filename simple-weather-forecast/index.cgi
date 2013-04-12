#!/usr/bin/env newlisp

(change-dir 
    (string (env "HOME") "/projects/simple-weather-forecast/")) ; for local testing only

(load "json.lsp" "cgi.lsp") ; uses standard modules

(context 'Forecast)

(set 'latitude 51 
     'longitude 0.0
     'apikey "???"
     'start-time (time-of-day))

(set 'curl-command 
    (format "curl -s https://api.forecast.io/forecast/%s/%f,%f?units=si" 
     apikey latitude longitude))

(define (render t)
  ; evaluate any newLISP code in template t between {% and %}
  (replace [text]{%(.*?)%}[/text] t 
     (if (catch (eval-string $1) 'result)
            (string result)
            (string "[error: " $1 "]")) 0))

(define (make-symbol day-details day-number key symbol-name)
  ; make and assign values to symbols for relevant elements
    (set (sym (string "day" day-number symbol-name)) (lookup key day-details)))

(set 'curl-d (exec curl-command))
(set 'data (first curl-d))
(unless data (println "no data found") (exit))
(set 'json (json2expr data))
(set 'currently (json (chop (ref "currently" json))))
(set 'time-stamp (last (currently (chop (ref "time" currently)))))
(set 'summary (last (currently (chop (ref "summary" currently)))))
(set 'icon    (last (currently (chop (ref "icon" currently)))))
(set 'pressure (last (currently (chop (ref "pressure" currently)))))
(set 'temperature (last (currently (chop (ref "temperature" currently)))))
(set 'humidity (last (currently (chop (ref "humidity" currently)))))
(set 'precipIntensity (last (currently (chop (ref "precipIntensity" currently)))))
(set 'visibility (last (currently (chop (ref "visibility" currently)))))
(set 'daily (json (chop (ref  "daily" json))))
(set 'days (ref-all "time" daily))
; daily forecasts for next 7 days
(dolist (day days)
    (set 'a-day (daily (chop (chop day))))
    (make-symbol a-day $idx "time" "date")
    (make-symbol a-day $idx "time" "time")
    (make-symbol a-day $idx "summary" "summary")
    (make-symbol a-day $idx "icon" "icon")
    (make-symbol a-day $idx "temperatureMin" "temperature-min")
    (make-symbol a-day $idx "temperatureMax" "temperature-max"))

(set 'template (read-file "template.html"))
(println "Content-Type: text/html\r\n\r\n" (render template))
(exit)
